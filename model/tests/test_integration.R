# Integration tests for the data-loading and pipeline functions in the R model.
#
# Run from the project root:
#   Rscript model/tests/test_integration.R
#
# Unlike test_pure.R (which only exercises data-independent helpers), these tests run the
# real loaders against the cached input files in model/cache/ and assert on the shape and
# sanity of what they return. Each test skips itself if the cache files it needs are absent,
# so the suite still passes on a fresh clone before the first fetch.

suppressPackageStartupMessages({
    library(testthat)
    library(data.table)
    library(jsonlite)
    library(sf)
})

# Locate the project root from this script's own path so the tests work from any cwd.
script <- grep("^--file=", commandArgs(TRUE), value = TRUE)
script <- if (length(script)) sub("^--file=", "", script[1]) else file.path("model", "tests", "test_integration.R")
root <- normalizePath(file.path(dirname(script), "..", ".."))
Sys.setenv(MODEL_ROOT = root)
suppressPackageStartupMessages(source(file.path(root, "model", "lib.R")))

# Skip a test unless every listed cache file exists.
need_cache <- function(...) {
    missing <- Filter(function(f) !file.exists(cache(f)), c(...))
    if (length(missing)) skip(paste0("missing cache file(s): ", paste(missing, collapse = ", ")))
}

# -------------------------------------------------------------------------------------------
# Presidential loaders
# -------------------------------------------------------------------------------------------
test_that("load_county_pres returns one row per county-year with sane fields", {
    need_cache("county_pres_08_16.csv", "county_pres_2020.csv", "county_pres_2024.csv")
    cp <- load_county_pres()
    expect_true(is.data.table(cp))
    expect_true(all(c("fips", "dem", "gop", "year", "state") %in% names(cp)))
    # five presidential cycles
    expect_equal(sort(unique(cp$year)), c(2008, 2012, 2016, 2020, 2024))
    # fips is a 5-digit zero-padded string; state is its first two digits
    expect_true(all(grepl("^[0-9]{5}$", cp$fips)))
    expect_true(all(cp$state == substr(cp$fips, 1, 2)))
    # every row has positive total votes (the loader drops dem+gop == 0 rows)
    expect_true(all(cp$dem + cp$gop > 0))
    expect_true(all(cp$dem >= 0 & cp$gop >= 0))
})

test_that("load_state_pres aggregates to a two-party Democratic share in (0,1)", {
    need_cache("county_pres_08_16.csv", "county_pres_2020.csv", "county_pres_2024.csv", "president_1976_2016.csv")
    sp <- load_state_pres()
    expect_true(all(c("year", "state", "dem", "gop", "d2") %in% names(sp)))
    expect_true(all(sp$d2 > 0 & sp$d2 < 1))
    # d2 is dem / (dem + gop)
    expect_equal(sp$d2, sp$dem / (sp$dem + sp$gop))
    # 2024 is present (used as the state-lean baseline)
    expect_true(2024 %in% sp$year)
})

test_that("national_pres gives one national two-party share per year", {
    need_cache("county_pres_08_16.csv", "county_pres_2020.csv", "county_pres_2024.csv", "president_1976_2016.csv")
    np <- national_pres(load_state_pres())
    expect_true(all(c("year", "nat") %in% names(np)))
    expect_equal(nrow(np), length(unique(np$year)))
    expect_true(all(np$nat > 0 & np$nat < 1))
})

# -------------------------------------------------------------------------------------------
# House loaders
# -------------------------------------------------------------------------------------------
test_that("load_house returns district-candidate rows with normalized keys and sides", {
    need_cache("house_1976_2018.csv", "fec_2020.xlsx", "fec_2022.xlsx")
    h <- load_house()
    expect_true(all(c("year", "st", "dn", "key", "votes", "side", "name", "dist") %in% names(h)))
    expect_equal(sort(unique(h$year)), c(2012, 2014, 2016, 2018, 2020, 2022))
    expect_true(all(h$side %in% c("D", "R", "O")))
    # dist is a 4-char GEOID-style code (state fips + 2-digit district)
    expect_true(all(grepl("^[0-9]{4}$", h$dist)))
    expect_true(all(h$votes >= 0))
    # keys are normalized uppercase identifiers (LAST_F form; a few legacy names lack a first initial)
    expect_true(all(grepl("^[A-Z]+_", h$key)))
    expect_true(all(nzchar(h$key)))
})

test_that("house_districts yields one row per district-year with an incumbent flag", {
    need_cache("house_1976_2018.csv", "fec_2020.xlsx", "fec_2022.xlsx")
    hd <- house_districts()
    expect_true(all(c("year", "dist", "st", "vD", "vR", "tot", "y", "inc") %in% names(hd)))
    expect_true(all(hd$inc %in% c(-1L, 0L, 1L)))
    # y is the two-party Democratic share
    expect_true(all(hd$y > 0 & hd$y < 1))
    expect_equal(hd$y, hd$vD / (hd$vD + hd$vR))
    # (year, dist) is unique
    expect_equal(nrow(hd), nrow(unique(hd[, .(year, dist)])))
})

test_that("national_house gives one national two-party share per year", {
    need_cache("house_1976_2018.csv", "fec_2020.xlsx", "fec_2022.xlsx")
    nh <- national_house(load_house())
    expect_true(all(c("year", "N") %in% names(nh)))
    expect_equal(nrow(nh), length(unique(nh$year)))
    expect_true(all(nh$N > 0 & nh$N < 1))
})

# -------------------------------------------------------------------------------------------
# Geography loaders
# -------------------------------------------------------------------------------------------
test_that("counties_sf returns county polygons with fips and positive area", {
    need_cache("us_counties_10m.json")
    co <- counties_sf()
    expect_true(all(c("fips", "carea") %in% names(co)))
    expect_true(nrow(co) > 3000)
    expect_true(all(grepl("^[0-9]{5}$", co$fips)))
    # (nearly) every county has positive area
    expect_gt(sum(co$carea > 0), 0.99 * nrow(co))
})

test_that("cd_sf returns congressional district polygons with GEOID-style ids", {
    need_cache("cd119.zip")
    cd <- cd_sf("cd119")
    expect_true(all("dist" %in% names(cd)))
    expect_true(nrow(cd) >= 400)
    # dist is a 4-char GEOID (state fips + 2-digit district)
    expect_true(all(grepl("^[0-9]{4}$", cd$dist)))
    # excluded territories / at-large codes are dropped
    expect_false(any(substr(cd$dist, 3, 4) %in% c("98", "99", "ZZ")))
})

test_that("crosswalk gives each district-county pair a fractional area share in (0,1]", {
    need_cache("us_counties_10m.json", "cd119.zip")
    xw <- crosswalk(cd_sf("cd119"), counties_sf())
    expect_true(all(c("dist", "fips", "frac") %in% names(xw)))
    # frac is a positive finite area share; it can slightly exceed 1 where st_intersection
    # produces overlapping slivers, so only bound it loosely.
    expect_true(all(xw$frac > 0 & is.finite(xw$frac)))
    expect_true(all(xw$frac <= 1.5))
})

# -------------------------------------------------------------------------------------------
# 01_fit.R: cty_lean_for (areal interpolation of county votes into districts)
# -------------------------------------------------------------------------------------------
test_that("cty_lean_for interpolates a two-party Democratic share per district", {
    need_cache("us_counties_10m.json", "cd119.zip", "county_pres_08_16.csv", "county_pres_2020.csv", "county_pres_2024.csv")
    # cty_lean_for closes over cp (county pres), co (county sf) and xw_cache; set them up in its env.
    exprs <- parse(file.path(root, "model", "01_fit.R"))
    env <- new.env(parent = globalenv())
    for (e in exprs) {
        if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) == "cty_lean_for") eval(e, envir = env)
    }
    env$cp <- load_county_pres()
    env$co <- counties_sf()
    env$xw_cache <- list()
    out <- env$cty_lean_for("cd119", 2020)
    expect_true(all(c("dist", "cty") %in% names(out)))
    expect_gt(nrow(out), 400)
    # cty is a two-party Democratic share in (0,1)
    expect_true(all(out$cty > 0 & out$cty < 1))
    # the crosswalk is cached on the second call (no recompute)
    n1 <- length(env$xw_cache)
    env$cty_lean_for("cd119", 2016)
    expect_equal(length(env$xw_cache), n1)
})

# -------------------------------------------------------------------------------------------
# 02_simulate.R: state_sims (Monte Carlo of one chamber's statewide races)
# -------------------------------------------------------------------------------------------
# Build a minimal runtime environment for state_sims by extracting the functions/constants it
# needs from 02_simulate.R and supplying the fitted model + shared random draws it closes over.
# This exercises the real simulation path (fundamentals + poll + rating blend, incumbent effect,
# regional/state shocks) without running the full pipeline.
build_sim_env <- function(K = 50L, seed = 1L) {
    need_cache("election_data.json", "legislators.json", "us_counties_10m.json",
        "county_pres_08_16.csv", "county_pres_2020.csv", "county_pres_2024.csv", "president_1976_2016.csv")
    skip_if(!file.exists(file.path(OUT, "fit.rds")), "model/out/fit.rds not present (run the model first)")
    fit <- readRDS(file.path(OUT, "fit.rds"))
    env <- new.env(parent = globalenv())
    env$fit <- fit
    env$A <- fit$assumptions
    env$sen <- fit$senate
    env$ed <- load_election_data()
    env$ri <- load_race_inputs(env$ed)
    env$cp <- load_county_pres()
    env$sp <- load_state_pres(env$cp)
    env$nat24 <- national_pres(env$sp)[year == 2024, nat]
    sp24 <- setNames(env$sp[year == 2024, d2], env$sp[year == 2024, state])
    sp20 <- setNames(env$sp[year == 2020, d2], env$sp[year == 2020, state])
    env$state_lean <- sp24 - env$nat24
    env$K <- K
    env$N_bar <- fit$nat$mean
    set.seed(seed)
    env$N_k <- rnorm(K, fit$nat$mean, fit$nat$sd)
    all_states <- unname(FIPS)
    regions <- unique(c(region_of(all_states), "other"))
    env$R_draw <- matrix(rnorm(length(regions) * K), length(regions), K, dimnames = list(regions, NULL))
    env$U_draw <- matrix(rnorm(length(all_states) * K), length(all_states), K, dimnames = list(all_states, NULL))
    exprs <- parse(file.path(root, "model", "02_simulate.R"))
    for (e in exprs) {
        if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]])) {
            nm <- as.character(e[[2]])
            if (nm %in% c("clampm", "blend", "idio_sd", "look", "incumbent_effect", "race_table",
                "state_sims", "FALLBACK_POLLS", "FALLBACK_H2H", "state_inc_bias", "scale_g", "sd_reg_sg", "sd_st_sg")) {
                eval(e, envir = env)
            }
        }
    }
    env
}

test_that("state_sims produces a clamped two-party share per race per simulation", {
    env <- build_sim_env()
    s <- env$state_sims("senate")
    # one row per 2026 Senate race, one column per simulation
    expect_equal(nrow(s$d), 35)
    expect_equal(dim(s$S), c(35, env$K))
    # every simulated share is clamped into (0,1)
    expect_true(all(s$S > 0 & s$S < 1))
    # the race table carries the expected columns
    expect_true(all(c("st", "lean", "inc", "poll", "rating", "mean", "post_sd", "idio") %in% names(s$d)))
    # incumbent flags are -1/0/1
    expect_true(all(s$d$inc %in% c(-1L, 0L, 1L)))
    # governor mode also runs and has 36 races
    g <- env$state_sims("governor")
    expect_equal(nrow(g$d), 36)
    expect_equal(dim(g$S), c(36, env$K))
})

# -------------------------------------------------------------------------------------------
# 2026 input loaders
# -------------------------------------------------------------------------------------------
test_that("load_election_data parses the live election data with the expected sections", {
    need_cache("election_data.json")
    ed <- load_election_data()
    expect_true(all(c("races", "race_polls") %in% names(ed)))
    expect_true(all(c("senate", "governor", "house") %in% names(ed$races)))
    expect_true("races" %in% names(ed$race_polls))
})

test_that("load_race_inputs builds ratings/polls keyed by mode and state fips", {
    need_cache("election_data.json")
    ri <- load_race_inputs()
    expect_true(all(c("ratings", "polls", "ptype", "open", "holder") %in% names(ri)))
    for (mode in c("senate", "governor", "house")) {
        expect_true(mode %in% names(ri$ratings))
        expect_true(mode %in% names(ri$polls))
    }
    # every poll share is clamped into (0,1)
    for (mode in c("senate", "governor", "house")) {
        p <- ri$polls[[mode]]
        if (length(p)) expect_true(all(p > 0 & p < 1))
    }
    # every rating is a valid two-party share
    for (mode in c("senate", "governor", "house")) {
        r <- ri$ratings[[mode]]
        if (length(r)) expect_true(all(r > 0 & r < 1))
    }
    # the 2026 Senate class has 35 races and the governor class has 36
    expect_equal(length(ri$ratings$senate), 35)
    expect_equal(length(ri$ratings$governor), 36)
})

test_that("load_legislators returns current members with a side and state fips", {
    need_cache("legislators.json")
    L <- load_legislators()
    expect_true(all(c("last", "type", "state", "party", "caucus", "side", "fips") %in% names(L)))
    expect_true(all(L$side %in% c("D", "R", "I")))
    # fips is derived from the state abbreviation; territories (VI/AS/GU/MP/PR) are not in the
    # 51-jurisdiction FIPS map and so get NA
    expect_true(all(is.na(L$fips) | L$fips %in% FIPS))
    # both chambers are present
    expect_true(all(c("sen", "rep") %in% L$type))
})

test_that("load_wiki_house returns per-district PVI and incumbent side", {
    need_cache("wiki_house.json")
    wh <- load_wiki_house()
    expect_true(all(c("dist", "pvi", "inc_side") %in% names(wh)))
    expect_true(nrow(wh) > 300)
    expect_true(all(grepl("^[0-9]{4}$", wh$dist)))
    # PVI is a finite number of points (D positive) where known; incumbent side is D/R when known
    expect_true(all(is.na(wh$pvi) | is.finite(wh$pvi)))
    # incumbent side is D/R (or O for a third-party incumbent) when known
    expect_true(all(is.na(wh$inc_side) | wh$inc_side %in% c("D", "R", "O")))
    # dist is unique
    expect_equal(nrow(wh), nrow(unique(wh[, .(dist)])))
})

cat("\nAll R integration tests passed.\n")
