# Unit tests for the pure (data-independent) functions in the R model.
#
# Run from the project root:
#   Rscript model/tests/test_pure.R
#
# These tests source model/lib.R (safe: it only creates the OUT dir at top level) and
# extract the pure helper functions from 01_fit.R / 02_simulate.R by parsing the files
# and evaluating only the top-level function/constant assignments we care about. The
# pipeline code at the top level of those files (which loads data and fits models) is
# never executed.

suppressPackageStartupMessages({
    library(testthat)
    library(data.table)
})

# Locate the project root from this script's own path so the tests work from any cwd.
script <- grep("^--file=", commandArgs(TRUE), value = TRUE)
script <- if (length(script)) sub("^--file=", "", script[1]) else file.path("model", "tests", "test_pure.R")
root <- normalizePath(file.path(dirname(script), "..", ".."))
Sys.setenv(MODEL_ROOT = root)
suppressPackageStartupMessages(source(file.path(root, "model", "lib.R")))

# Extract top-level assignments (functions or constants) with the given names from a file,
# without running the rest of the file.
extract <- function(path, names) {
    exprs <- parse(path)
    env <- new.env(parent = globalenv())
    for (e in exprs) {
        if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]])) {
            nm <- as.character(e[[2]])
            if (nm %in% names) eval(e, envir = env)
        }
    }
    env
}

fit_env <- extract(file.path(root, "model", "01_fit.R"), c("tidy_lm", "sd_of", "top", "rd"))
# state_inc_bias is derived from the state lean at run time (needs sp24/nat24), so it is not
# extracted here; the incumbent_effect test pins a controlled vector in sim_env instead.
sim_env <- extract(file.path(root, "model", "02_simulate.R"), c("argv", "clampm", "incumbent_effect", "blend", "idio_sd", "look", "with_mean"))

# -------------------------------------------------------------------------------------------
# lib.R
# -------------------------------------------------------------------------------------------
test_that("clamp bounds values into [lo, hi]", {
    expect_equal(clamp(0.5, 0.1, 0.9), 0.5)
    expect_equal(clamp(0.05, 0.1, 0.9), 0.1)
    expect_equal(clamp(0.95, 0.1, 0.9), 0.9)
    expect_equal(clamp(c(0, 0.5, 1), 0.1, 0.9), c(0.1, 0.5, 0.9))
})

test_that("FIPS and ABBR are consistent inverse maps covering 51 jurisdictions", {
    expect_equal(length(FIPS), 51)
    expect_equal(FIPS[["NE"]], "31")
    expect_equal(FIPS[["DC"]], "11")
    expect_equal(ABBR[["31"]], "NE")
    expect_equal(ABBR[["11"]], "DC")
    # every FIPS code maps back to the same abbreviation
    expect_equal(unname(ABBR[FIPS]), names(FIPS))
})

test_that("region_of returns a region for known FIPS and 'other' for unknown", {
    expect_equal(region_of("31"), "plains")
    expect_equal(region_of("99"), "other")
    expect_equal(region_of(NA_character_), "other")
})

test_that("side_of classifies party strings into D / R / O", {
    expect_equal(side_of("Democrat"), "D")
    expect_equal(side_of("Republican"), "R")
    expect_equal(side_of("Libertarian"), "O")
    expect_equal(side_of("Green"), "O")
})

test_that("norm_name normalizes to LAST_F and strips suffixes", {
    expect_equal(norm_name("Smith, John Jr."), "SMITH_J")
    expect_equal(norm_name("John Smith"), "SMITH_J")
    expect_equal(norm_name("Smith, John"), "SMITH_J")
    expect_equal(norm_name("José García"), "GARCIA_J")
})

test_that("rating_center maps ratings to two-party Democratic share", {
    expect_equal(rating_center("Toss-Up"), 0.50)
    expect_equal(rating_center("Likely R"), 0.43)
    expect_equal(rating_center("Solid D"), 0.64)
    expect_equal(rating_center("Safe R"), 0.36)
    expect_equal(rating_center("Lean D"), 0.53)
    expect_equal(rating_center(NA_character_), NA_real_)
})

test_that("%||% returns the first non-null / non-empty value", {
    expect_equal(NULL %||% 5, 5)
    expect_equal(character(0) %||% 5, 5)
    expect_equal(3 %||% 5, 3)
    expect_equal("x" %||% "y", "x")
})

# -------------------------------------------------------------------------------------------
# 01_fit.R
# -------------------------------------------------------------------------------------------
test_that("sd_of returns the sdcor for a group, or 0 when absent", {
    # In the pipeline vc is as.data.frame(VarCorr(m)), so vc[vc$grp == g, "sdcor"]
    # is a plain vector; length() correctly counts its elements.
    vc <- data.frame(grp = c("a", "b"), sdcor = c(0.1, 0.2))
    expect_equal(fit_env$sd_of(vc, "a"), 0.1)
    expect_equal(fit_env$sd_of(vc, "b"), 0.2)
    expect_equal(fit_env$sd_of(vc, "zzz"), 0)
})

test_that("top returns the max-vote row for a side, or an empty table when none", {
    d <- data.table(side = c("D", "D", "R"), votes = c(100, 300, 200))
    expect_equal(fit_env$top(d, "D")$votes, 300)
    expect_equal(fit_env$top(d, "R")$votes, 200)
    expect_equal(nrow(fit_env$top(d, "O")), 0)
})

test_that("rd rounds numeric values and leaves others untouched", {
    out <- fit_env$rd(list(a = 0.123456, b = "x", c = 1), k = 3)
    expect_equal(out$a, 0.123)
    expect_equal(out$b, "x")
    expect_equal(out$c, 1)
})

test_that("tidy_lm extracts term/estimate/se from a model summary", {
    m <- lm(mpg ~ wt, data = mtcars)
    t <- fit_env$tidy_lm(m)
    expect_true(is.data.table(t))
    expect_true(all(c("term", "estimate", "se") %in% names(t)))
    expect_equal(t$term[1], "(Intercept)")
})

# -------------------------------------------------------------------------------------------
# 02_simulate.R
# -------------------------------------------------------------------------------------------
test_that("clampm clamps a matrix in place and preserves dimensions", {
    m <- matrix(c(0, 0.5, 1, 0.2), nrow = 2)
    out <- sim_env$clampm(m, 0.1, 0.9)
    expect_equal(dim(out), dim(m))
    expect_equal(as.numeric(out), c(0.1, 0.5, 0.9, 0.2))
})

test_that("blend precision-weights finite sources and ignores NA ones", {
    # equal precision -> simple mean of the two finite values
    b <- sim_env$blend(0.5, 0.05, 0.6, 0.05, NA_real_, NA_real_)
    expect_equal(b$mean, 0.55)
    # a single finite source -> that source
    b2 <- sim_env$blend(0.4, 0.05, NA_real_, 0.05, NA_real_, NA_real_)
    expect_equal(b2$mean, 0.4)
    # tighter (smaller sd) source pulls the mean toward it
    b3 <- sim_env$blend(0.5, 0.05, 0.6, 0.01, NA_real_, NA_real_)
    expect_gt(b3$mean, 0.55)
    # use_f = FALSE drops the fundamentals source
    b4 <- sim_env$blend(0.5, 0.05, 0.6, 0.05, NA_real_, NA_real_, use_f = FALSE)
    expect_equal(b4$mean, 0.6)
})

test_that("idio_sd floors the idiosyncratic standard deviation", {
    # var_post below the floor -> floor_sd
    expect_equal(sim_env$idio_sd(0.0001, 0.05, 0.05), 0.02)
    # var_post above the floor -> sqrt of the residual
    expect_equal(sim_env$idio_sd(0.04, 0.05, 0.05), sqrt(0.04 - 0.0025 - 0.0025))
})

test_that("look returns values for present keys and NA for absent ones", {
    x <- c("02" = 0.5, "06" = 0.4)
    expect_equal(sim_env$look(x, c("02", "06", "99")), c(0.5, 0.4, NA_real_))
})

test_that("with_mean appends a column of row means", {
    M <- matrix(c(0.2, 0.4, 0.6, 0.8), nrow = 2)
    out <- sim_env$with_mean(M)
    expect_equal(ncol(out), 3)
    expect_equal(out[, 3], rowMeans(M))
})

test_that("incumbent_effect scales by base + national + state bias", {
    # incumbent_effect looks up A and state_inc_bias in its enclosing env (sim_env).
    # state_inc_bias is now derived from the state lean at run time, so pin a controlled
    # vector here to test the lookup + arithmetic in isolation.
    sim_env$A <- list(inc_scale = 0.05, anti_incumbent_national = 0.02)
    sim_env$state_inc_bias <- c("48" = -0.12)
    # state with no bias: inc * (0.05 + 0.02 + 0)
    expect_equal(unname(sim_env$incumbent_effect("31", 1, base = 0.05)), 1 * (0.05 + 0.02))
    # state with a bias ("48" = -0.12)
    expect_equal(unname(sim_env$incumbent_effect("48", 1, base = 0.05)), 1 * (0.05 + 0.02 - 0.12))
    # open seat (inc = 0) -> 0
    expect_equal(unname(sim_env$incumbent_effect("31", 0, base = 0.05)), 0)
})

test_that("argv parses --name=value from command args", {
    # argv reads the global `args`; set it in the sim env and re-evaluate.
    sim_env$args <- c("--sims=100", "--seed=42")
    expect_equal(sim_env$argv("sims", 10000), 100)
    expect_equal(sim_env$argv("seed", 2026), 42)
    expect_equal(sim_env$argv("missing", 7), 7)
})

cat("\nAll R pure-function tests passed.\n")
