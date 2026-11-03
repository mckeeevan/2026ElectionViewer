# 02_simulate.R: joint Monte Carlo of the 2026 Senate, governor and House races.
#
# One simulation draws a national environment, regional and state shocks, then every race in every chamber from those shared
# shocks, so Senate / governor / House outcomes are correlated the way they are in real cycles. Each race's expected value
# is a precision-weighted blend of the fundamentals regression (01_fit.R), the poll average and the forecaster rating.
# County results inside Senate / governor races are generated from the county swing model and re-centred on the state result.
#
# Output (model/out): meta.json, senate.bin, governor.bin, house.bin. A .bin holds Uint16 little-endian Democratic two-party
# shares (x 65535), one block of n_units values per simulation; block K (the last) is the mean over all simulations.
#
# Run from the project root:  Rscript model/02_simulate.R [--sims=10000] [--seed=2026]

source(file.path(Sys.getenv("MODEL_ROOT", unset = "."), "model", "lib.R"))

args <- commandArgs(TRUE)
argv <- function(name, default) {
    m <- sub(paste0("^--", name, "="), "", args[grepl(paste0("^--", name, "="), args)])
    if (length(m)) as.numeric(m[1]) else default
}
K <- as.integer(argv("sims", 10000))
SEED <- as.integer(argv("seed", 2026))
# Uncertainty multiplier: scales the per-race idiosyncratic noise (the "we don't
# know this specific race" margin of error) on top of the shared national/region/
# state shocks. 1.0 = fitted spread; >1 widens the model's prediction interval.
UNCERT <- as.numeric(argv("uncert", 1.0))
set.seed(SEED)
logm <- function(...) cat(sprintf(...), "\n", sep = "")
SCALE <- 65535
clampm <- function(x, lo, hi) {
    x[] <- pmax(lo, pmin(hi, x))
    x
} # clamp that keeps matrix dimensions

fit <- readRDS(file.path(OUT, "fit.rds"))
A <- fit$assumptions
ed <- load_election_data()
ri <- load_race_inputs(ed)
lg <- load_legislators()
wiki <- load_wiki_house()
cty26 <- readRDS(file.path(OUT, "cty26.rds"))

# ---------------------------------------------------------------------------------------------
# Baselines
# ---------------------------------------------------------------------------------------------
cp <- load_county_pres()
sp <- load_state_pres(cp)
nat24 <- national_pres(sp)[year == 2024, nat]
sp24 <- setNames(sp[year == 2024, d2], sp[year == 2024, state])
sp20 <- setNames(sp[year == 2020, d2], sp[year == 2020, state])
state_lean <- sp24 - nat24
# Incumbency dampener for entrenched one-party states, derived from the state's 2024
# presidential lean (the principled "one-party rule" signal) rather than a hand-picked
# list. The further a state sits from the national, the more its incumbent is exposed in
# a down year. Mild on purpose: the lean already enters via the fundamentals prior, so
# this is a secondary nudge, capped at 8 pts.
state_inc_bias <- -pmin(0.3 * abs(state_lean), 0.08)
incumbent_effect <- function(st, inc, base = A$inc_scale) {
    s <- as.character(st)
    bias <- vapply(s, function(x) if (x %in% names(state_inc_bias)) state_inc_bias[[x]] else 0, numeric(1))
    inc * (base + A$anti_incumbent_national + bias)
}

# the notebook's hand-entered fallback polls (used only where no live poll exists) and its fallback head-to-head states
FALLBACK_POLLS <- list(
    senate = c(
        "02" = 0.535, "12" = 0.478, "13" = 0.545, "19" = 0.515, "20" = 0.494, "23" = 0.512, "26" = 0.512, "27" = 0.520,
        "31" = 0.495, "33" = 0.536, "35" = 0.550, "37" = 0.555, "39" = 0.528, "48" = 0.502
    ),
    governor = c(
        "02" = 0.525, "04" = 0.522, "12" = 0.470, "13" = 0.506, "19" = 0.534, "20" = 0.470, "23" = 0.545, "26" = 0.538,
        "27" = 0.535, "32" = 0.500, "33" = 0.460, "35" = 0.525, "36" = 0.528, "39" = 0.522, "41" = 0.515, "42" = 0.555,
        "48" = 0.482, "50" = 0.380, "55" = 0.518
    )
)
FALLBACK_H2H <- list(senate = c("31"), governor = character())

# ── Third-party vote share (full_split) ─────────────────────────────────────
# Historical third-party (LIBERTARIAN + OTHER) vote share per state, from the
# Senate general elections 1992–2024 (median across years, robust to single-year
# artifacts). Used as the baseline third-party share for races with no poll-derived
# third-party candidate. A hand-entered override table handles known-strong
# third-party races (e.g. CO governor / Greg Lopez). The Senate share is a proxy
# for governor/house races (third-party vote is a state-level phenomenon).
SPOILER_DRAW <- 1.0  # 1 = 3rd party draws entirely from the leading party (spoiler); 0 = proportional (no winner change)
sen <- fread(cache("senate_1976_2024.tab"))
tp_gen <- sen[stage %in% c("gen", "GEN", "GEN RUNOFF") & year >= 1992 & year <= 2024]
tp_gen[, tp := party_simplified %in% c("LIBERTARIAN", "OTHER")]
# Per-state, per-year third-party vote share, then the median across years.
tp_share_hist <- tp_gen[, .(tp = sum(candidatevotes[tp]), total = sum(candidatevotes)),
                        by = .(state_po, year)]
tp_share_hist[, share := ifelse(total > 0, tp / total, 0)]
state_third_party_prior <- tp_share_hist[, .(tp_prior = median(share)), by = state_po]
state_third_party_prior[, fips := FIPS[state_po]]
state_third_party_prior <- state_third_party_prior[!is.na(fips)]

# Hand-entered overrides for known-strong third-party races (mode → FIPS → share).
# (CO governor / Greg Lopez override removed: a poll now shows the approximate placement.)
THIRD_PARTY_OVERRIDES <- list()

# Effective third-party share for a race: override if present, else state prior.
# (Parameter named `fips_code` to avoid data.table scoping collision with the `fips` column.)
third_party_share <- function(mode, fips_code) {
  ov <- THIRD_PARTY_OVERRIDES[[mode]]
  if (!is.null(ov) && fips_code %in% names(ov)) return(ov[[fips_code]])
  row <- state_third_party_prior[state_third_party_prior$fips == fips_code]
  if (nrow(row) && is.finite(row$tp_prior[1])) row$tp_prior[1] else 0
}

# Poll-derived third-party share for a race, mirroring the dashboard's
# `topThirdParty`: the strongest non-D/R candidate above `min_pct` in any recent
# poll (poll artifacts like "Generic Candidate" ignored). Returns the share as a
# fraction (0..1), or NA when no viable third-party candidate is polled.
poll_third_party <- function(k, min_pct = 4) {
  # race_polls keys use state abbreviations (Senate-MT, Gov-MT, MT-01); the
  # callers pass FIPS-based keys (Senate-30, Gov-30, 30-01), so convert.
  m1 <- regmatches(k, regexec("^(Senate|Gov)-([0-9]{2})$", k))[[1]]
  m2 <- regmatches(k, regexec("^([0-9]{2})-([0-9]{2}|AL)$", k))[[1]]
  if (length(m1) == 3) k <- paste0(m1[2], "-", ABBR[[m1[3]]])
  else if (length(m2) == 3) k <- paste0(ABBR[[m2[2]]], "-", m2[3])
  r <- ed$race_polls$races[[k]]
  if (is.null(r) || is.null(r$latest)) return(NA_real_)
  is_third <- function(party) {
    p <- toupper(as.character(party))
    !grepl("^(DEM|DEMOCRAT|REP|REPUBLICAN|GOP)$", p)
  }
  best <- NA_real_
  for (p in r$latest) {
    if (is.null(p$others)) next
    for (o in p$others) {
      if (is.null(o$name) || grepl("generic", o$name, ignore.case = TRUE)) next
      if (!is_third(o$party)) next
      pct <- as.numeric(o$pct)
      if (!is.finite(pct) || pct <= min_pct) next
      if (is.na(best) || pct > best) best <- pct
    }
  }
  if (is.na(best)) NA_real_ else best / 100
}

# Generate the per-unit per-sim third-party/IND share matrix (n_units x K).
# Senate/governor (county units): a state-level base (poll-derived when available,
# else the historical state prior) with a shared state swing and a per-county
# character draw scaled by the county regression's size effect. House (district
# units): a poll-derived base (rare) with small per-district variance.
# Clamped to [0, 0.4] — no 4% floor, so low-base races vary below 4%.
# For ind_head_to_head races the IND candidate is already the "D" column, so
# those states get 0 here (pass h2h_by_state to skip them).
third_party_matrix <- function(mode, units, K, ck, h2h_by_state = NULL) {
  n <- nrow(units)
  I <- matrix(0, n, K)
  if (mode == "house") {
    for (i in seq_len(n)) {
      key <- paste0(substr(units$dist[i], 1, 2), "-", substr(units$dist[i], 3, 4))
      ptp <- poll_third_party(key)
      if (is.na(ptp) || ptp <= 0.01) next
      I[i, ] <- ptp + ptp * 0.3 * rnorm(K)
    }
  } else {
    prefix <- if (mode == "senate") "Senate-" else "Gov-"
    sigmaMed <- exp(ck$sd_a + ck$sd_b * ck$lv_center)
    for (st in unique(units$st)) {
      if (!is.null(h2h_by_state) && isTRUE(h2h_by_state[[st]])) next # IND is the D column
      idx <- which(units$st == st)
      ptp <- poll_third_party(paste0(prefix, st))
      base <- if (!is.na(ptp) && ptp > 0.01) ptp else third_party_share(mode, st)
      if (!is.finite(base) || base <= 0.01) next
      sdState <- base * 0.4
      countySd <- base * 0.3
      swing <- rnorm(K)
      for (j in idx) {
        lp <- units$logpop[j]
        betaC <- 1 + ck$beta0 + ck$beta1 * (lp - ck$lv_center)
        sigmaC <- exp(ck$sd_a + ck$sd_b * lp)
        I[j, ] <- base + sdState * swing * betaC + countySd * (sigmaC / sigmaMed) * rnorm(1)
      }
    }
  }
  clampm(I, 0, 0.4)
}

# ---------------------------------------------------------------------------------------------
# Shared random draws
# ---------------------------------------------------------------------------------------------
N_bar <- fit$nat$mean
N_k <- rnorm(K, N_bar, UNCERT * fit$nat$sd)
all_states <- unname(FIPS)
regions <- unique(c(region_of(all_states), "other"))
R_draw <- matrix(rnorm(length(regions) * K), length(regions), K, dimnames = list(regions, NULL)) # standard normal
U_draw <- matrix(rnorm(length(all_states) * K), length(all_states), K, dimnames = list(all_states, NULL))
logm("national House vote: mean %.3f, sd %.3f", N_bar, fit$nat$sd)

# Posterior of one race: precision-weighted blend of fundamentals, poll and rating.
blend <- function(mu_f, sd_f, poll, sd_p, rating, sd_r, use_f = TRUE) {
    w <- cbind(f = ifelse(use_f, 1 / sd_f^2, 0), p = ifelse(is.finite(poll), 1 / sd_p^2, 0), r = ifelse(is.finite(rating), 1 / sd_r^2, 0))
    v <- cbind(mu_f, ifelse(is.finite(poll), poll, 0), ifelse(is.finite(rating), rating, 0))
    list(mean = rowSums(w * v) / rowSums(w), var = 1 / rowSums(w))
}
idio_sd <- function(var_post, sd_reg, sd_st, floor_sd = 0.02) sqrt(pmax(var_post - sd_reg^2 - sd_st^2, floor_sd^2))
look <- function(x, keys) as.numeric(unname(x[keys])) # NA for keys that are absent

# ---------------------------------------------------------------------------------------------
# Statewide races (Senate, governor)
# ---------------------------------------------------------------------------------------------
race_table <- function(mode) {
    st <- names(ri$ratings[[mode]])
    d <- data.table(st = st)
    d[, lean := state_lean[st]]
    hold <- ri$holder[[mode]][st]
    open <- ri$open[[mode]][st]
    d[, inc := fifelse(open %in% TRUE, 0, fifelse(hold == "D", 1, fifelse(hold == "R", -1, 0)))]
    d[, holder := hold]
    real <- ri$polls[[mode]]
    fb <- FALLBACK_POLLS[[mode]]
    d[, poll_src := fifelse(st %in% names(real), "poll", fifelse(st %in% names(fb), "fallback", "none"))]
    d[, poll := fifelse(poll_src == "poll", look(real, st), fifelse(poll_src == "fallback", look(fb, st), NA_real_))]
    d[, h2h := st %in% names(ri$ptype[[mode]]) | st %in% FALLBACK_H2H[[mode]]]
    d[, rating := look(ri$ratings[[mode]], st)]
    d
}

sen <- fit$senate
scale_g <- c(senate = 1, governor = A$governor_scale)
sd_reg_sg <- sen$sd_region
sd_st_sg <- fit$house$sd_state

state_sims <- function(mode) {
    d <- race_table(mode)
    sc <- scale_g[[mode]]
    d[, mu_f := N_bar + sen$intercept + sen$lean * lean + incumbent_effect(st, inc) * sen$inc]
    d[, sd_f := sc * sqrt(sen$sd_resid^2 + sen$sd_region^2)]
    d[, sd_p := fifelse(poll_src == "fallback", A$sd_fallback_poll, A$sd_poll)]
    # head-to-head independent races: only the poll says anything about an independent's strength
    b <- blend(d$mu_f, d$sd_f, d$poll, d$sd_p, ifelse(d$h2h, NA_real_, d$rating), A$sd_rating, use_f = !d$h2h)
    d[, `:=`(mean = b$mean, post_sd = sqrt(b$var))]
    # an independent race with neither poll nor fallback would have no information: fall back to a wide prior at 0.45
    d[h2h & !is.finite(poll), `:=`(mean = 0.45, post_sd = 0.10)]
    d[, idio := idio_sd(post_sd^2, sd_reg_sg * sc, sd_st_sg * sc)]
    S <- matrix(NA_real_, nrow(d), K)
    for (i in seq_len(nrow(d))) {
        reg <- region_of(d$st[i])
        S[i, ] <- d$mean[i] + (N_k - N_bar) + UNCERT * sd_reg_sg * sc * R_draw[reg, ] + UNCERT * sd_st_sg * sc * U_draw[d$st[i], ] +
            UNCERT * sqrt(d$idio[i]^2) * rnorm(K)
    }
    list(d = d, S = clampm(S, 0.05, 0.95))
}

# ---------------------------------------------------------------------------------------------
# County structure inside each statewide race
# ---------------------------------------------------------------------------------------------
co <- counties_sf()
co_dt <- data.table(fips = co$fips, st = substr(co$fips, 1, 2), carea = co$carea)
sf_use_s2(FALSE)
c24 <- cp[year == 2024 & fips %in% co_dt$fips, .(fips, dem24 = dem / (dem + gop), v24 = dem + gop)]
c20 <- cp[year == 2020 & fips %in% co_dt$fips, .(fips, dem20 = dem / (dem + gop), v20 = dem + gop)]
cc <- Reduce(function(a, b) merge(a, b, by = "fips", all.x = TRUE), list(co_dt, c24, c20))
cc[, st_base := sp24[st]]
cc[, st_swing := sp24[st] - sp20[st]]
cc[, real := !is.na(v24) | !is.na(v20)]
cc[, base := fifelse(!is.na(v24), dem24, dem20 + st_swing)]
cc[, vref := fifelse(!is.na(v24), v24, v20)]
# no county data (all of Alaska): voters scale with log land area, as in the notebook; partisanship follows size slightly
miss <- which(!cc$real)
cc[miss, logpop := rnorm(.N, 9.6 - (log(carea / 6371008.8^2) + 7.5) * 0.28, 1.15)] # carea in steradians
cc[real == TRUE, logpop := log(vref * 2)]
cc[, vref := as.numeric(vref)]
cc[miss, vref := exp(logpop) * 0.5]
cc[miss, base := clamp(st_base + (logpop - 9.7) * 0.042 + rnorm(.N, 0, 0.025), 0.05, 0.95)]
cc[, votes := round(vref * fifelse(real, runif(.N, 0.92, 1.08), 1))]
cc[, u := runif(.N)]
cc[, base := clamp(base, 0.04, 0.96)]
ck <- fit$county
cc[, beta_c := 1 + ck$beta0 + ck$beta1 * (logpop - ck$lv_center)]
cc[, sd_c := exp(ck$sd_a + ck$sd_b * logpop)] # fitted noise sd is log-linear in log county size

county_sims <- function(st_sim) {
    d <- st_sim$d
    S <- st_sim$S
    units <- cc[st %in% d$st][order(fips)]
    M <- matrix(NA_real_, nrow(units), K)
    Seff <- matrix(NA_real_, nrow(d), K)
    for (i in seq_len(nrow(d))) {
        idx <- which(units$st == d$st[i])
        u <- units[idx]
        w <- u$votes / sum(u$votes)
        S0 <- sum(w * u$base)
        Ck <- u$base + outer(u$beta_c, S[i, ] - S0) + u$sd_c * matrix(rnorm(length(idx) * K), length(idx), K)
        Ck <- Ck + rep(S[i, ] - colSums(w * Ck), each = length(idx))
        Ck <- clampm(Ck, 0.04, 0.96)
        M[idx, ] <- Ck
        Seff[i, ] <- colSums(w * Ck)
    }
    list(units = units, M = M, Seff = Seff)
}

# Simulate a Louisiana-style jungle primary (top-2 advance regardless of party) for one district.
# field: data.table(name, party, side); d_share: the district's D two-party share (0-1); K: n sims.
# Returns list(general = K-vector of the D two-party share for the general election,
#              primary = n x K matrix of each candidate's primary vote share).
# In a jungle primary the top two candidates advance regardless of party, so the general is
# D-vs-R (use d_share) or same-party (locked).
jungle_sim <- function(field, d_share, K, other_frac = 0.05, noise_sd = 0.35) {
  n <- nrow(field)
  side <- field$side
  n_side <- as.integer(table(side)[side])
  # Per-sim side totals: candidate i's side total for sim k (d_share is length K).
  side_total <- matrix(0, n, K)
  for (i in seq_len(n)) {
    side_total[i, ] <- if (side[i] == "D") d_share else if (side[i] == "R") 1 - d_share else other_frac
  }
  expected <- side_total / n_side
  shares <- expected * matrix(rlnorm(n * K, 0, noise_sd), n, K)
  shares <- shares / colSums(shares)
  # For each sim, count how many D and R candidates are in the top-2.
  nD <- numeric(K)
  nR <- numeric(K)
  for (k in seq_len(K)) {
    top2 <- order(shares[, k], decreasing = TRUE)[1:2]
    s2 <- side[top2]
    nD[k] <- sum(s2 == "D")
    nR[k] <- sum(s2 == "R")
  }
  # D-vs-R general -> d_share; D-only -> D wins (1); R-only -> R wins (0); else (D+O or R+O) -> d_share.
  general <- pmin(0.97, pmax(0.03, fifelse(nD == 1 & nR == 1, d_share, fifelse(nD == 2, 1, fifelse(nR == 2, 0, d_share)))))
  list(general = general, primary = shares)
}

# ---------------------------------------------------------------------------------------------
# House districts
# ---------------------------------------------------------------------------------------------
cd <- suppressWarnings(st_read(paste0("/vsizip/", cache("cd119.zip")), quiet = TRUE))
dist_ids <- sort(setdiff(cd$GEOID[!substr(cd$GEOID, 3, 4) %in% c("98", "99", "ZZ") & !substr(cd$GEOID, 1, 2) %in% c("11", "72", "60", "66", "69", "78")], NA))
H <- data.table(dist = dist_ids, st = substr(dist_ids, 1, 2))
H <- merge(H, wiki, by = "dist", all.x = TRUE)
H <- merge(H, cty26, by = "dist", all.x = TRUE)
comp <- rbindlist(lapply(ed$races$house$competitive, function(r) {
    m <- regmatches(r$district, regexec("^([A-Z]{2})-(\\d\\d|AL)$", r$district))[[1]]
    if (!length(m)) {
        return(NULL)
    }
    pv <- r$pvp %||% ""
    pvi <- if (grepl("^EVEN", pv, ignore.case = TRUE)) 0 else if (grepl("^[DR]\\+\\d", pv)) (if (substr(pv, 1, 1) == "D") 1 else -1) * as.numeric(sub("^[DR]\\+([0-9.]+).*", "\\1", pv)) else NA_real_
    data.table(dist = paste0(FIPS[[m[2]]], if (m[3] == "AL") "00" else m[3]), pvi_comp = pvi, open_comp = isTRUE(r$open))
}))
H <- merge(H, comp, by = "dist", all.x = TRUE)
redrawn <- c("06", "29", "37", "39", "48", "49")
H[, lean_src := fifelse(is.finite(pvi), "pvi", fifelse(
    is.finite(pvi_comp), "pvi_competitive",
    fifelse(is.finite(lean26) & !st %in% redrawn, "crosswalk", "state")
))]
H[, lean := fcase(lean_src == "pvi", pvi / 100, lean_src == "pvi_competitive", pvi_comp / 100, lean_src == "crosswalk", lean26,
    default = state_lean[st]
)]
# incumbent running: Wikipedia table when present, else the sitting member (unless the seat is rated open)
leg <- lg[type == "rep", .(dist = paste0(fips, sprintf("%02d", fifelse(is.na(district), 0L, as.integer(district)))), side_leg = side)]
H <- merge(H, leg, by = "dist", all.x = TRUE)
H[, inc_side_final := fifelse(!is.na(inc_side), inc_side, fifelse(open_comp %in% TRUE, NA_character_, side_leg))]
H[, inc := fifelse(inc_side_final == "D", 1, fifelse(inc_side_final == "R", -1, 0, 0), 0)]
H[, poll := look(ri$polls$house, dist)]
H[, rating := look(ri$ratings$house, dist)]
hs <- fit$house
H[, mu_f := N_bar + hs$intercept + hs$lean * lean + incumbent_effect(st, inc) * hs$inc]
H[, sd_f := sqrt(fifelse(inc == 0, hs$sd_open, hs$sd_inc)^2 + hs$sd_region^2 + hs$sd_state^2)]
bh <- blend(H$mu_f, H$sd_f, H$poll, A$sd_poll_house, H$rating, A$sd_rating)
H[, `:=`(mean = bh$mean, post_sd = sqrt(bh$var))]
H[, idio := idio_sd(post_sd^2, hs$sd_region, hs$sd_state, floor_sd = 0.025)]
HS <- matrix(NA_real_, nrow(H), K)
jungle_primary <- list() # per-district primary vote shares (n_cand x K) for jungle-primary districts
jungle_st <- "22" # Louisiana: top-2 advance regardless of party
for (i in seq_len(nrow(H))) {
    base <- H$mean[i] + (N_k - N_bar) + hs$sd_region * R_draw[region_of(H$st[i]), ] + hs$sd_state * U_draw[H$st[i], ] +
        UNCERT * H$idio[i] * rnorm(K)
    cf <- H$cand_field[[i]]
    if (H$st[i] == jungle_st && !is.null(cf) && nrow(cf) >= 2) {
        js <- jungle_sim(cf, base, K)
        HS[i, ] <- js$general
        jungle_primary[[H$dist[i]]] <- js$primary
    } else {
        HS[i, ] <- base
    }
}
HS <- clampm(HS, 0.03, 0.97)
H[, votes := round(rnorm(.N, 305000, 32000))]
logm(
    "House: %d districts (lean from pvi %d, competitive pvp %d, crosswalk %d, state %d); %d polled, %d rated",
    nrow(H), sum(H$lean_src == "pvi"), sum(H$lean_src == "pvi_competitive"), sum(H$lean_src == "crosswalk"), sum(H$lean_src == "state"),
    sum(is.finite(H$poll)), sum(is.finite(H$rating))
)

# ---------------------------------------------------------------------------------------------
# Run Senate and governor, assemble outputs
# ---------------------------------------------------------------------------------------------
sims <- list()
for (mode in c("senate", "governor")) {
    s <- state_sims(mode)
    cs <- county_sims(s)
    sims[[mode]] <- list(d = s$d, units = cs$units, M = cs$M, share = cs$Seff)
    logm("%s: %d races, %d counties", mode, nrow(s$d), nrow(cs$units))
}
sims$house <- list(d = H, units = H, M = HS, share = HS)

with_mean <- function(M) cbind(M, rowMeans(M)) # last column = average simulation

mode_meta <- function(mode) {
    x <- sims[[mode]]
    M <- with_mean(x$M)
    share <- with_mean(x$share)
    jungle <- NULL
    if (mode == "house") {
        id <- x$d$dist
        st <- x$d$st
        d_win <- share > 0.5
        contest_ids <- id
        h2h <- rep(FALSE, length(id))
        units <- lapply(seq_along(id), function(i) list(id = id[i], state = st[i], votes = x$d$votes[i]))
        # Jungle-primary districts (Louisiana): per-candidate mean primary vote share + top-2 advance rate.
        jungle <- list()
        for (di in names(jungle_primary)) {
            cf <- H$cand_field[[match(di, H$dist)]]
            prim <- jungle_primary[[di]] # n_cand x K
            mean_share <- rowMeans(prim)
            # top-2 advance rate per candidate across sims
            adv <- numeric(nrow(prim))
            for (k in seq_len(K)) {
                top2 <- order(prim[, k], decreasing = TRUE)[1:2]
                adv[top2] <- adv[top2] + 1
            }
            adv_rate <- adv / K
            jungle[[di]] <- lapply(seq_len(nrow(cf)), function(j) list(
                name = cf$name[j], party = cf$party[j], side = cf$side[j],
                share = round(mean_share[j], 4), advance = round(adv_rate[j], 4)
            ))
        }
    } else {
        id <- x$units$fips
        st <- x$units$st
        contest_ids <- x$d$st
        h2h <- x$d$h2h
        d_win <- share > 0.5
        units <- lapply(seq_along(id), function(i) list(id = id[i], state = st[i], votes = x$units$votes[i], logPop = round(x$units$logpop[i], 4), u = round(x$units$u[i], 4)))
    }
    win_d <- colSums(d_win & !h2h)
    win_i <- colSums(d_win & h2h)
    win_r <- length(contest_ids) - win_d - win_i
    # per-contest summary across the K sims (not the mean block)
    sh <- share[, seq_len(K), drop = FALSE]
    contests <- lapply(seq_along(contest_ids), function(i) {
        base <- list(
            id = contest_ids[i], state = if (mode == "house") st[i] else contest_ids[i],
            p_dem = round(mean(sh[i, ] > 0.5), 4), mean = round(mean(sh[i, ]), 4), sd = round(sd(sh[i, ]), 4),
            q05 = round(quantile(sh[i, ], 0.05, names = FALSE), 4), q95 = round(quantile(sh[i, ], 0.95, names = FALSE), 4), h2h = h2h[i]
        )
        d <- x$d[i]
        if (mode == "house") {
            c(base, list(
                lean = round(d$lean, 4), lean_src = d$lean_src, inc = d$inc, poll = if (is.finite(d$poll)) round(d$poll, 4), rating = if (is.finite(d$rating)) round(d$rating, 4),
                prior = round(d$mean, 4)
            ))
        } else {
            # tp_share: historical third-party prior (state median 1992-2024, or a
            # hand-entered override) so no-poll races can show a real 3rd party.
            c(base, list(
                lean = round(d$lean, 4), inc = d$inc, poll = if (is.finite(d$poll)) round(d$poll, 4), poll_src = d$poll_src, rating = if (is.finite(d$rating)) round(d$rating, 4),
                fundamentals = round(d$mu_f, 4), prior = round(d$mean, 4), tp_share = round(third_party_share(mode, contest_ids[i]), 4)
            ))
        }
    })
    # Per-state win-rate across the K sims: how often each party wins and the average margin.
    # Senate/governor: one race per state (state-level D share). House: delegation majority + avg district margin.
    # margin is the signed two-party D-minus-R margin (positive = D ahead).
    state_stats <- list()
    if (mode == "house") {
        for (s in unique(st)) {
            idx <- which(st == s)
            sh_s <- sh[idx, , drop = FALSE]
            d_count <- rowSums(sh_s > 0.5)
            r_count <- nrow(sh_s) - d_count
            state_stats[[s]] <- list(
                p_dem = round(mean(d_count > r_count), 4),
                p_rep = round(mean(r_count > d_count), 4),
                p_ind = 0,
                margin = round(mean(2 * sh_s - 1), 4),
                n_districts = nrow(sh_s)
            )
        }
    } else {
        for (i in seq_along(contest_ids)) {
            s <- contest_ids[i]
            sh_i <- sh[i, ]
            p_dem <- mean(sh_i > 0.5 & !h2h[i])
            p_ind <- mean(sh_i > 0.5 & h2h[i])
            state_stats[[s]] <- list(
                p_dem = round(p_dem, 4),
                p_rep = round(1 - p_dem - p_ind, 4),
                p_ind = round(p_ind, 4),
                margin = round(mean(2 * sh_i - 1), 4)
            )
        }
    }
    q <- function(w) as.list(setNames(round(quantile(w[seq_len(K)], c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), names = FALSE), 1), c("p05", "p10", "p25", "p50", "p75", "p90", "p95")))
    # write the binary: blocks of n_units Uint16 per simulation
    bin <- file.path(OUT, paste0(mode, ".bin"))
    con <- file(bin, "wb")
    writeBin(as.integer(round(pmax(0, pmin(1, M)) * SCALE)), con, size = 2, endian = "little")
    close(con)
    # third-party (IND) share per unit per sim, written to a parallel .bin so the
    # viewer reads model-generated all-party shares instead of synthesizing them.
    h2h_by_state <- if (mode == "house") NULL else setNames(as.list(h2h), contest_ids)
    I <- with_mean(third_party_matrix(mode, if (mode == "house") x$d else x$units, K, ck, h2h_by_state))
    ind_bin <- file.path(OUT, paste0(mode, "_ind.bin"))
    con2 <- file(ind_bin, "wb")
    writeBin(as.integer(round(pmax(0, pmin(1, I)) * SCALE)), con2, size = 2, endian = "little")
    close(con2)
    list(
        n_units = nrow(M), n_contests = length(contest_ids), bin = paste0(mode, ".bin"), ind_bin = paste0(mode, "_ind.bin"),
        units = units, contests = contests, jungle = jungle, state_stats = state_stats,
        wins = list(D = as.integer(win_d), R = as.integer(win_r), I = as.integer(win_i)),
        wins_quantiles = q(win_d)
    )
}

modes <- lapply(c(senate = "senate", governor = "governor", house = "house"), mode_meta)

# national House vote of each sim, so the viewer can show it
meta <- list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M"), sims = K, seed = SEED, scale = SCALE,
    national = list(mean = N_bar, sd = fit$nat$sd, draws = round(N_k, 5)),
    assumptions = c(A, list(uncert = UNCERT)),
    modes = modes
)
writeLines(toJSON(meta, auto_unbox = TRUE, null = "null", digits = NA), file.path(OUT, "meta.json"))
for (m in names(modes)) {
    w <- modes[[m]]$wins$D[seq_len(K)]
    logm("%s: D wins mean %.1f, median %.0f, 10-90%% %s", m, mean(w), median(w), paste(quantile(w, c(0.1, 0.9)), collapse = "-"))
}
logm("wrote %s", OUT)
