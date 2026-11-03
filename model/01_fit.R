# 01_fit.R: estimate every model parameter from history and write model/out/fit.rds + model/out/regressions.json.
#
# Layers fitted here (all two-party Democratic shares):
#   A. National environment:   House vote N_t  ~  final generic ballot            (8 midterms, 1994-2022)
#   B. Senate / Governor:      y_st - N_t      ~  state lean + incumbency + year, region-year effects   (2000-2022)
#   C. House districts:        y_dt - N_t      ~  county-implied lean + incumbency + year, region-year, state-year effects (2014-2022)
#   D. Counties:               county swing    ~  state swing, with noise that falls with county size   (presidential 2008-2024)
#   E. District <-> county <-> state links, per cycle (the regressions shown in the notebook)
# Usage (from the project root): Rscript model/01_fit.R

source(file.path(Sys.getenv("MODEL_ROOT", unset = "."), "model", "lib.R"))
suppressPackageStartupMessages(library(lme4))
set.seed(20260101)

ed <- load_election_data()
cp <- load_county_pres()
sp <- load_state_pres(cp)
npr <- national_pres(sp)
logm <- function(...) message(sprintf(...))

tidy_lm <- function(m) {
    s <- summary(m)$coefficients
    data.table(term = rownames(s), estimate = s[, 1], se = s[, 2])
}
sd_of <- function(vc, g) {
    v <- vc[vc$grp == g, "sdcor"]
    if (length(v)) v[1] else 0
}

# -------------------------------------------------------------------------------------------
# A. National environment
# -------------------------------------------------------------------------------------------
logm("A. national environment")
h_all <- load_house(from = 1992)
Nh <- national_house(h_all)
mid <- rbindlist(lapply(ed$previous_elections$midterms, function(m) {
    data.table(year = m$year, gb_d = m$generic_ballot_d, gb_r = m$generic_ballot_r, president = m$president_party)
}))
mid <- merge(mid, Nh, by = "year")
mid[, gb := gb_d / (gb_d + gb_r)]
fitN <- lm(N ~ gb, mid)
loo_N <- sqrt(mean(sapply(seq_len(nrow(mid)), function(i) (mid$N[i] - predict(lm(N ~ gb, mid[-i]), mid[i]))^2)))
gb_now <- with(ed$generic_ballot, democrat / (democrat + republican))
pN <- predict(fitN, data.frame(gb = gb_now), se.fit = TRUE)
nat <- list(
    intercept = unname(coef(fitN)[1]), slope = unname(coef(fitN)[2]), sigma = summary(fitN)$sigma, loo = loo_N, n = nrow(mid),
    r2 = summary(fitN)$r.squared, gb_now = gb_now,
    mean = unname(pN$fit),
    sd = max(sqrt(summary(fitN)$sigma^2 + pN$se.fit^2), loo_N)
)
mid[, fitted := fitted(fitN)]
logm("   N = %.3f + %.3f * generic ballot   (R2 %.2f, resid sd %.2f pts, LOO %.2f pts)", nat$intercept, nat$slope, nat$r2, 100 * nat$sigma, 100 * loo_N)
logm("   2026: generic ballot %.1f%% two-party -> House vote %.1f%% +/- %.1f pts", 100 * gb_now, 100 * nat$mean, 100 * nat$sd)

# -------------------------------------------------------------------------------------------
# B. Senate (also used for governors)
# -------------------------------------------------------------------------------------------
logm("B. Senate")
sen <- fread(cache("senate_1976_2024.tab"))
sen[, wi := as.character(writein) %in% c("TRUE", "True", "true")]
sen[, sp := as.character(special) %in% c("TRUE", "True", "true")]
sen <- sen[stage == "gen" & !wi & !is.na(candidate)]
sen[, side := fifelse(party_simplified == "DEMOCRAT", "D", fifelse(party_simplified == "REPUBLICAN", "R", "O"))]
cand <- sen[, .(votes = sum(candidatevotes), side = side[which.max(candidatevotes)]), by = .(year, st = sprintf("%02d", state_fips), sp, candidate)]
cand[, key := norm_name(candidate)]
top <- function(d, s) {
    x <- d[side == s]
    if (nrow(x)) x[which.max(votes)] else d[0]
}
races <- cand[,
    {
        d <- top(.SD, "D")
        r <- top(.SD, "R")
        w <- .SD[which.max(votes)]
        .(
            vD = if (nrow(d)) d$votes else NA_real_, vR = if (nrow(r)) r$votes else NA_real_,
            keyD = if (nrow(d)) d$key else NA_character_, keyR = if (nrow(r)) r$key else NA_character_,
            win_key = w$key, win_side = w$side
        )
    },
    by = .(year, st, sp)
]
pw <- races[, .(st, py = year, win_key)]
races[, inc := mapply(function(s, y, kd, kr) {
    p <- pw[st == s & py >= y - 6 & py < y]
    if (!is.na(kd) && kd %in% p$win_key) 1L else if (!is.na(kr) && kr %in% p$win_key) -1L else 0L
}, st, year, keyD, keyR)]
sd <- races[year >= 2000 & year <= 2022 & !is.na(vD) & !is.na(vR) & vD > 0 & vR > 0 & st != "22"]
sd[, y := vD / (vD + vR)]
sd[, py := year - (year %% 4)]
sd <- merge(sd, sp[, .(py = year, st = state, pres = d2)], by = c("py", "st"))
sd <- merge(sd, npr[, .(py = year, npres = nat)], by = "py")
sd <- merge(sd, Nh, by = "year")
sd[, `:=`(lean = pres - npres, z = y - N, reg = region_of(st))]
sd[, yr_reg := paste(year, reg)][, fyear := factor(year)]
sd[, recent_w := fifelse(year >= 2018, 2.5, fifelse(year >= 2010, 1.0, 0.5))]
mS <- lmer(z ~ lean + inc + (1 | fyear) + (1 | yr_reg), sd, weights = recent_w, REML = TRUE)
vcS <- as.data.frame(VarCorr(mS))
senate <- list(
    intercept = unname(fixef(mS)["(Intercept)"]), lean = unname(fixef(mS)["lean"]), inc = unname(fixef(mS)["inc"]),
    sd_year = sd_of(vcS, "fyear"), sd_region = sd_of(vcS, "yr_reg"),
    sd_resid = mad(resid(mS)), # robust: a few Maine / Alaska / independent-dominated races would otherwise inflate the noise
    sd_resid_ols = sd_of(vcS, "Residual"), n = nrow(sd), years = range(sd$year)
)
logm(
    "   Senate z = %.3f + %.2f*lean + %.3f*inc; sd year %.3f, region-year %.3f, race %.3f (n=%d)", senate$intercept, senate$lean, senate$inc,
    senate$sd_year, senate$sd_region, senate$sd_resid, senate$n
)
reg_senate <- tidy_lm(lm(y ~ lean + inc + factor(year), sd))

# -------------------------------------------------------------------------------------------
# C/E. House districts <-> counties <-> states
# -------------------------------------------------------------------------------------------
logm("C. House districts and county crosswalks")
hd <- house_districts(load_house(from = 2012))
cycles <- data.table(year = c(2014, 2016, 2018, 2020, 2022), cd = c("cd113", "cd115", "cd116", "cd116", "cd118"), py = c(2012, 2016, 2016, 2020, 2020))
co <- counties_sf()
xw_cache <- list()
cty_lean_for <- function(cdname, py) {
    if (is.null(xw_cache[[cdname]])) xw_cache[[cdname]] <<- crosswalk(cd_sf(cdname), co)
    xw <- xw_cache[[cdname]]
    v <- cp[year == py, .(fips, dem, gop)]
    x <- merge(xw, v, by = "fips")
    x[, `:=`(vt = (dem + gop) * frac, vd = dem * frac)]
    x[, .(cty = sum(vd) / sum(vt)), by = dist]
}
panel <- rbindlist(lapply(seq_len(nrow(cycles)), function(i) {
    cy <- cycles[i]
    d <- hd[year == cy$year]
    d <- merge(d, cty_lean_for(cy$cd, cy$py), by = "dist", all.x = TRUE)
    d[, py := cy$py]
    d
}))
panel <- merge(panel, sp[, .(py = year, st = state, state_pres = d2)], by = c("py", "st"))
panel <- merge(panel, npr[, .(py = year, npres = nat)], by = "py")
panel <- merge(panel, Nh, by = "year")
panel[, `:=`(lean_state = state_pres - npres)]
panel[, lean_cty := fifelse(is.na(cty), state_pres, cty) - npres] # no county data (Alaska): fall back to the state
panel[, `:=`(z = y - N, reg = region_of(st), fyear = factor(year))]
panel[, `:=`(yr_reg = paste(year, reg), yr_st = paste(year, st))]
panel[, recent_w := fifelse(year >= 2018, 2.5, fifelse(year >= 2014, 1.0, 0.5))]
panel <- panel[is.finite(lean_cty) & is.finite(z)]

# per-cycle regressions: how much of a district's House vote do the state and county presidential vote explain?
by_cycle <- rbindlist(lapply(sort(unique(panel$year)), function(yy) {
    p <- panel[year == yy]
    m1 <- lm(y ~ lean_state, p)
    m2 <- lm(y ~ lean_cty, p)
    m3 <- lm(y ~ lean_cty + lean_state, p)
    data.table(
        year = yy, n = nrow(p),
        state_slope = coef(m1)[2], state_r2 = summary(m1)$r.squared, state_rmse = summary(m1)$sigma,
        county_slope = coef(m2)[2], county_r2 = summary(m2)$r.squared, county_rmse = summary(m2)$sigma,
        both_county = coef(m3)[2], both_state = coef(m3)[3], both_r2 = summary(m3)$r.squared
    )
}))
pooled <- list(
    state  = lm(z ~ lean_state + inc, panel),
    county = lm(z ~ lean_cty + inc, panel),
    both   = lm(z ~ lean_cty + lean_state + inc, panel)
)
pooled_tbl <- rbindlist(lapply(names(pooled), function(n) {
    t <- tidy_lm(pooled[[n]])
    t[, `:=`(model = n, r2 = summary(pooled[[n]])$r.squared, rmse = summary(pooled[[n]])$sigma, n = nobs(pooled[[n]]))]
}))
print(by_cycle[, lapply(.SD, function(x) round(x, 3))])

mH <- lmer(z ~ lean_cty + inc + (1 | fyear) + (1 | yr_reg) + (1 | yr_st), panel, weights = recent_w, REML = TRUE)
vcH <- as.data.frame(VarCorr(mH))
panel[, res := resid(mH)]
house_obs <- list(
    intercept = unname(fixef(mH)["(Intercept)"]), lean = unname(fixef(mH)["lean_cty"]), inc = unname(fixef(mH)["inc"]),
    sd_year = sd_of(vcH, "fyear"), sd_region = sd_of(vcH, "yr_reg"), sd_state = sd_of(vcH, "yr_st"),
    sd_resid = mad(panel$res), sd_resid_ols = sd_of(vcH, "Residual"),
    sd_inc = panel[inc != 0, mad(res)], sd_open = panel[inc == 0, mad(res)], n = nrow(panel), years = c(2014, 2022)
)
logm(
    "   House z = %.3f + %.2f*lean(county) + %.3f*inc; sd year %.3f, region %.3f, state %.3f, district %.3f (n=%d)", house_obs$intercept,
    house_obs$lean, house_obs$inc, house_obs$sd_year, house_obs$sd_region, house_obs$sd_state, house_obs$sd_resid, house_obs$n
)

# The simulation feeds the model each district's Cook PVI instead of the county-implied lean (it is far less noisy, and it
# exists for the 2026 lines). Correct the attenuation from the noisy county lean using PVI as the reference measurement.
wiki <- load_wiki_house()
cty26 <- merge(cty_lean_for("cd119", 2020), cty_lean_for("cd119", 2024), by = "dist", suffixes = c("20", "24"))
cty26[, lean26 := (cty20 - npr[year == 2020, nat] + cty24 - npr[year == 2024, nat]) / 2]
cal <- merge(cty26, wiki, by = "dist")[!substr(dist, 1, 2) %in% c("06", "29", "37", "39", "48", "49", "02") & is.finite(pvi)]
cal[, pvi_lean := pvi / 100] # Cook PVI is the district's two-party presidential share minus the nation's, in points
mCal <- lm(lean26 ~ pvi_lean, cal)
tau2 <- summary(mCal)$sigma^2
var_T <- var(cal$pvi_lean)
lambda <- var_T / (var_T + tau2)
house <- house_obs
house$lean <- house_obs$lean / lambda
house$sd_resid <- sqrt(max(house_obs$sd_resid^2 - house$lean^2 * lambda * tau2, (0.5 * house_obs$sd_resid)^2))
house$lambda <- lambda
house$tau <- sqrt(tau2)
house$sd_inc <- sqrt(max(house_obs$sd_inc^2 - (house$lean^2 * lambda * tau2), (0.5 * house_obs$sd_inc)^2))
house$sd_open <- sqrt(max(house_obs$sd_open^2 - (house$lean^2 * lambda * tau2), (0.5 * house_obs$sd_open)^2))
logm(
    "   PVI calibration: county lean = %.2f * PVI/100 (noise sd %.1f pts, reliability %.2f) -> lean slope %.2f -> %.2f, district sd %.3f -> %.3f",
    coef(mCal)[2], 100 * sqrt(tau2), lambda, house_obs$lean, house$lean, house_obs$sd_resid, house$sd_resid
)

# -------------------------------------------------------------------------------------------
# D. Counties (presidential swings against the state)
# -------------------------------------------------------------------------------------------
logm("D. counties")
cw <- cp[, .(fips, state, year, dem, gop)][, `:=`(v = dem + gop, d2 = dem / (dem + gop))]
pairs <- data.table(a = c(2008, 2012, 2016, 2020), b = c(2012, 2016, 2020, 2024))
sw <- rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
    x <- merge(cw[year == pairs$a[i]], cw[year == pairs$b[i]], by = c("fips", "state"), suffixes = c("0", "1"))
    x[, `:=`(dc = d21 - d20, lv = log(v1))]
    x[, ds := sum(dem1) / sum(v1) - sum(dem0) / sum(v0), by = state]
    x[, `:=`(pair = i, dev = dc - ds)]
    x[state %in% sprintf("%02d", c(1:56)) & is.finite(dev) & v1 > 50]
}))
sw[, lvc := lv - mean(lv)]
mC <- lm(dev ~ ds + ds:lvc, sw, weights = v1)
sw[, res := dev - predict(mC)]
mCs <- lm(log(abs(res) + 1e-4) ~ lvc, sw[abs(res) > 0])
county <- list(
    beta0 = unname(coef(mC)["ds"]), beta1 = unname(coef(mC)["ds:lvc"]), lv_center = mean(sw$lv),
    sd_a = unname(coef(mCs)[1]) + 0.635, sd_b = unname(coef(mCs)[2]), n = nrow(sw),
    sd_at_median = exp(coef(mCs)[1] + 0.635 + coef(mCs)[2] * median(sw$lvc)),
    sd_all = sw[, sd(res)]
)
logm(
    "   county swing = state swing x (1 + %.2f %+.2f*log-size); county noise sd %.1f pts at median size (n=%d)", county$beta0, county$beta1,
    100 * county$sd_at_median, county$n
)

# -------------------------------------------------------------------------------------------
# Save
# -------------------------------------------------------------------------------------------
fit <- list(
    nat = nat, senate = senate, house = house, house_observed = house_obs, county = county,
    assumptions = list(
        sd_poll = 0.045, sd_poll_house = 0.055, sd_rating = 0.060, sd_fallback_poll = 0.065, governor_scale = 1.15,
        inc_scale = 0.7,
        anti_incumbent_national = -0.18,
        state_inc_bias = c(
            "06" = -0.06, "12" = -0.10, "13" = -0.04, "27" = -0.02, "29" = -0.05,
            "36" = -0.08, "48" = -0.12, "51" = -0.04, "54" = -0.05
        ),
        note = paste(
            "Poll-average error and forecaster-rating error cannot be estimated from the open data used here;",
            "they are fixed assumptions. Governor noise is the Senate fit scaled by governor_scale (no open governor history).",
            "The incumbency edge is damped nationally and more so in states with entrenched partisan structures, allowing some incumbents to lose ground or even flip the sign in a hostile year."
        )
    ),
    fitted_at = format(Sys.time(), "%Y-%m-%d %H:%M")
)
saveRDS(fit, file.path(OUT, "fit.rds"))
saveRDS(cty26[, .(dist, lean26)], file.path(OUT, "cty26.rds"))

rd <- function(x, k = 4) lapply(x, function(v) if (is.numeric(v)) round(v, k) else v)
regs <- list(
    national = list(
        table = mid[, .(year, generic_ballot = round(gb, 4), house_vote = round(N, 4), fitted = round(fitted, 4), president)],
        fit = rd(nat, 5)
    ),
    senate = list(fit = rd(senate, 5), coefficients = reg_senate, n_years = uniqueN(sd$year)),
    house = list(
        by_cycle = by_cycle[, lapply(.SD, function(x) round(x, 4))], pooled = pooled_tbl[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 5) else x)],
        fit = rd(house, 5), observed = rd(house_obs, 5),
        pvi_calibration = list(slope = unname(coef(mCal)[2]), noise_sd = sqrt(tau2), reliability = lambda, n = nrow(cal))
    ),
    county = list(fit = rd(county, 5)),
    fitted_at = fit$fitted_at
)
write_json(regs, file.path(OUT, "regressions.json"), auto_unbox = TRUE, digits = NA, dataframe = "rows", na = "null")
logm("wrote model/out/fit.rds and regressions.json")
