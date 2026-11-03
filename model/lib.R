# lib.R: shared loaders and helpers for the 2026 midterm model. Sourced by 01_fit.R and 02_simulate.R.

suppressPackageStartupMessages({
    library(data.table)
    library(jsonlite)
    library(sf)
})

ROOT <- normalizePath(Sys.getenv("MODEL_ROOT", unset = "."), mustWork = TRUE)
CACHE <- file.path(ROOT, "model", "cache")
OUT <- file.path(ROOT, "model", "out")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
cache <- function(f) file.path(CACHE, f)

clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))

FIPS <- c(
    AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08", CT = "09", DE = "10", DC = "11", FL = "12",
    GA = "13", HI = "15", ID = "16", IL = "17", IN = "18", IA = "19", KS = "20", KY = "21", LA = "22", ME = "23",
    MD = "24", MA = "25", MI = "26", MN = "27", MS = "28", MO = "29", MT = "30", NE = "31", NV = "32", NH = "33",
    NJ = "34", NM = "35", NY = "36", NC = "37", ND = "38", OH = "39", OK = "40", OR = "41", PA = "42", RI = "44",
    SC = "45", SD = "46", TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53", WV = "54", WI = "55",
    WY = "56"
)
ABBR <- setNames(names(FIPS), FIPS)

# Regions that share a correlated shock (same grouping as the notebook used; the shock sizes are now estimated).
REGION <- c(
    "26" = "rust_belt", "55" = "rust_belt", "42" = "rust_belt", "39" = "rust_belt", "18" = "rust_belt",
    "19" = "plains", "20" = "plains", "31" = "plains", "38" = "plains", "46" = "plains",
    "04" = "sun_belt", "32" = "sun_belt", "48" = "sun_belt", "13" = "sun_belt", "37" = "sun_belt", "12" = "sun_belt",
    "09" = "northeast", "23" = "northeast", "25" = "northeast", "33" = "northeast", "34" = "northeast", "36" = "northeast",
    "44" = "northeast", "50" = "northeast",
    "01" = "deep_south", "05" = "deep_south", "22" = "deep_south", "28" = "deep_south", "45" = "deep_south", "47" = "deep_south",
    "08" = "mountain", "16" = "mountain", "30" = "mountain", "35" = "mountain", "49" = "mountain", "56" = "mountain",
    "06" = "pacific", "41" = "pacific", "53" = "pacific", "15" = "pacific", "02" = "pacific",
    "21" = "appalachia", "54" = "appalachia", "51" = "appalachia", "24" = "northeast", "10" = "northeast",
    "17" = "rust_belt", "27" = "plains", "29" = "plains", "40" = "plains", "51" = "appalachia"
)
region_of <- function(st) {
    r <- REGION[st]
    r[is.na(r)] <- "other"
    unname(r)
}

# ---------------------------------------------------------------------------------------------
# Presidential results
# ---------------------------------------------------------------------------------------------

# County presidential, long: fips, state, year, dem, gop (raw votes). 2008-2016 have no Alaska; 2020/2024 report Alaska
# by state house district (their "fips" will not match a county shape) and Connecticut 2024 by planning region.
load_county_pres <- function() {
    a <- fread(cache("county_pres_08_16.csv"), colClasses = list(character = "fips_code"))
    a[, fips := sprintf("%05d", as.integer(fips_code))]
    a <- melt(a[, .(fips, dem_2008, gop_2008, dem_2012, gop_2012, dem_2016, gop_2016)],
        id.vars = "fips",
        measure.vars = patterns(dem = "^dem_", gop = "^gop_"), variable.name = "yi"
    )
    a[, year := c(2008, 2012, 2016)[as.integer(yi)]][, yi := NULL]
    rd <- function(f, yr) {
        x <- fread(cache(f), colClasses = list(character = "county_fips"))
        x[, .(fips = county_fips, dem = votes_dem, gop = votes_gop, year = yr)]
    }
    out <- rbind(a, rd("county_pres_2020.csv", 2020), rd("county_pres_2024.csv", 2024))
    out[, state := substr(fips, 1, 2)]
    out[dem + gop > 0]
}

# State presidential two-party Democratic share by year (MEDSL through 2016, county sums for 2020/2024).
load_state_pres <- function(cp = load_county_pres()) {
    p <- fread(cache("president_1976_2016.csv"))
    p <- p[!writein %in% TRUE]
    p[, side := fifelse(grepl("democrat", party, ignore.case = TRUE), "dem", fifelse(grepl("republican", party, ignore.case = TRUE), "gop", NA_character_))]
    p <- p[!is.na(side), .(votes = sum(candidatevotes)), by = .(year, state = sprintf("%02d", state_fips), side)]
    p <- dcast(p, year + state ~ side, value.var = "votes")
    late <- cp[year >= 2020, .(dem = sum(dem), gop = sum(gop)), by = .(year, state)]
    rbind(p[, .(year, state, dem, gop)], late)[, `:=`(d2 = dem / (dem + gop))][order(year, state)]
}

# National presidential two-party Democratic share by year.
national_pres <- function(sp) sp[, .(nat = sum(dem) / (sum(dem) + sum(gop))), by = year]

# ---------------------------------------------------------------------------------------------
# U.S. House results by district
# ---------------------------------------------------------------------------------------------

norm_name <- function(x) {
    x <- toupper(iconv(as.character(x), to = "ASCII//TRANSLIT"))
    x <- gsub("\\b(JR|SR|II|III|IV)\\b\\.?", "", x)
    vapply(strsplit(x, ",", fixed = TRUE), function(p) {
        if (length(p) >= 2) {
            last <- gsub("[^A-Z]", "", p[1])
            first <- gsub("[^A-Z]", "", trimws(p[2]))
        } else {
            w <- strsplit(trimws(p[1]), "\\s+")[[1]]
            last <- gsub("[^A-Z]", "", w[length(w)])
            first <- gsub("[^A-Z]", "", w[1])
        }
        paste0(last, "_", substr(first, 1, 1))
    }, character(1))
}

side_of <- function(party) {
    p <- toupper(party)
    fifelse(
        grepl("^(D|DEM|DEMOCRAT|DFL|DEMOCRATIC|DEMOCRATIC-FARMER LABOR)$", p) | grepl("DEMOCRAT", p), "D",
        fifelse(grepl("^(R|REP|REPUBLICAN)$", p) | grepl("REPUBLICAN", p), "R", "O")
    )
}

# One row per district-candidate: year, state, dist (GEOID-style, "0401"), cand key, side D/R/O, votes.
load_house <- function(from = 2012) {
    m <- fread(cache("house_1976_2018.csv"))
    m <- m[year >= from & stage == "gen" & !(special %in% TRUE) & !(runoff %in% TRUE) & !(writein %in% TRUE) & !is.na(candidate)]
    m[, `:=`(st = sprintf("%02d", state_fips), dn = district, name = candidate, p = party, votes = candidatevotes)]
    m <- m[, .(year, st, dn, name, p, votes)]

    fec <- function(file, sheet, yr) {
        library(readxl)
        x <- as.data.table(read_excel(cache(file), sheet = sheet, col_types = "text"))
        setnames(x, toupper(trimws(names(x))))
        nm <- function(pat) grep(pat, names(x), value = TRUE)[1]
        out <- data.table(
            abbr = x[[nm("^STATE ABBREVIATION")]], dn = x[[nm("^DISTRICT$")]], id = x[[nm("^FEC ID")]],
            first = x[[nm("CANDIDATE NAME \\(FIRST\\)")]], last = x[[nm("CANDIDATE NAME \\(LAST\\)")]],
            p = x[[nm("^PARTY$")]], votes = suppressWarnings(as.numeric(gsub(",", "", x[[nm("^GENERAL VOTES$")]])))
        )
        out <- out[!is.na(votes) & !is.na(id) & id != "n/a" & !is.na(dn) & grepl("^[0-9]+$", dn)]
        out[, `:=`(year = yr, st = FIPS[abbr], dn = as.integer(dn), name = paste0(last, ", ", first))]
        out[!is.na(st), .(year, st, dn, name, p, votes, id)]
    }
    f20 <- fec("fec_2020.xlsx", "13. US House Results by State", 2020)
    f22 <- fec("fec_2022.xlsx", "8. US House Results by State", 2022)
    m[, id := NA_character_]
    h <- rbind(m, f20, f22, fill = TRUE)
    h[, key := norm_name(name)]
    h[, side := side_of(p)]
    # a candidate may run on several lines (fusion states): total votes, party = the line with most votes
    h <- h[order(-votes)][, .(votes = sum(votes), side = side[1], name = name[1]), by = .(year, st, dn, key)]
    # at-large states always use "00"
    nd <- h[, .(n = uniqueN(dn)), by = .(year, st)]
    h <- merge(h, nd, by = c("year", "st"))
    h[, dist := paste0(st, fifelse(n == 1L, "00", sprintf("%02d", dn)))][, n := NULL]
    h[]
}

# District-level two-party summary with incumbent flags (inc = +1 Democratic incumbent running, -1 Republican, 0 open/none).
house_districts <- function(h = load_house()) {
    w <- h[order(-votes)][, .SD[1], by = .(year, dist)][, .(year, dist, winner_key = key, winner_side = side)]
    d <- dcast(h[side %in% c("D", "R")][order(-votes)][, .SD[1], by = .(year, dist, side)],
        year + dist ~ side,
        value.var = c("votes", "key")
    )
    setnames(d, c("votes_D", "votes_R", "key_D", "key_R"), c("vD", "vR", "keyD", "keyR"))
    d <- d[!is.na(vD) & !is.na(vR) & vD > 0 & vR > 0]
    # incumbent = the winner of the same district two years earlier, running again under the same name
    prev <- copy(w)[, `:=`(year = year + 2L)]
    setnames(prev, c("winner_key", "winner_side"), c("pkey", "pside"))
    d <- merge(d, prev, by = c("year", "dist"), all.x = TRUE)
    d[, inc := fifelse(!is.na(pkey) & pkey == keyD, 1L, fifelse(!is.na(pkey) & pkey == keyR, -1L, 0L))]
    d[, `:=`(y = vD / (vD + vR), tot = vD + vR, st = substr(dist, 1, 2))]
    d[, .(year, dist, st, vD, vR, tot, y, inc)]
}

# National House two-party Democratic share by year (contested + uncontested, as cast for the two major parties).
national_house <- function(h) h[side %in% c("D", "R"), .(N = sum(votes[side == "D"]) / sum(votes)), by = year][order(year)]

# ---------------------------------------------------------------------------------------------
# Geography: district <-> county crosswalk (areal interpolation of county votes into districts)
# ---------------------------------------------------------------------------------------------

counties_sf <- function() {
  x <- suppressWarnings(st_read(cache("us_counties_10m.json"), layer = "counties", quiet = TRUE))
  st_crs(x) <- 4326
  x <- st_make_valid(st_transform(x, 5070))
  x$fips <- x$id
  x$carea <- as.numeric(st_area(x))
  x[, c("fips", "carea")]
}

cd_sf <- function(name) {
  y <- suppressWarnings(st_read(paste0("/vsizip/", cache(paste0(name, ".zip"))), quiet = TRUE))
  y <- st_make_valid(st_transform(y, 5070))
  y$dist <- y$GEOID
  y[!substr(y$dist, 3, 4) %in% c("98", "99", "ZZ") & !substr(y$dist, 1, 2) %in% c("11", "72", "60", "66", "69", "78"), "dist"]
}

# share of each county's area that falls inside each district
crosswalk <- function(cd, counties) {
  sf_use_s2(FALSE)
  ix <- suppressWarnings(st_intersection(cd, counties))
  ix <- st_collection_extract(ix, "POLYGON")
  x <- data.table(dist = ix$dist, fips = ix$fips, carea = ix$carea, a = as.numeric(st_area(ix)))
  x[, .(frac = sum(a) / carea[1]), by = .(dist, fips)][frac > 0.002]
}

# ---------------------------------------------------------------------------------------------
# 2026 inputs: ratings, polls, incumbents, district PVI
# ---------------------------------------------------------------------------------------------

# Forecaster rating -> expected Democratic two-party share. These centers are the notebook's original hand-set values;
# there is no historical rating archive to estimate them from, so they are listed as assumptions in the model output.
RATING_CENTER <- c("safe r" = 0.36, "likely r" = 0.43, "lean r" = 0.47, "toss-up" = 0.50, "lean d" = 0.53, "likely d" = 0.57, "safe d" = 0.64)
rating_center <- function(r) {
  r <- tolower(trimws(gsub("toss[ -]*up", "toss-up", gsub("^solid", "safe", tolower(r %||% "")))))
  unname(RATING_CENTER[r])
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

load_election_data <- function() fromJSON(cache("election_data.json"), simplifyVector = FALSE)

# Pollster reliability table (grade, predictive error, mean-reverted bias, lean) from the F538-style CSV.
# `bias` is in points and D-favoring: corrected_D = poll_D + bias, corrected_R = poll_R - bias.
# Unmatched pollsters (empty pred_err) get neutral quality weight and no debias.
load_pollsters <- function() {
  f <- file.path("model", "data", "pollsters.csv")
  if (!file.exists(f)) return(list(rows = data.table::data.table(pollster = character(0)), neutral = 1.0))
  p <- data.table::fread(f, fill = TRUE)
  matched <- p[!is.na(pred_err)]
  neutral <- if (nrow(matched)) median(abs(matched$pred_err)) else 1.0
  list(rows = p, neutral = neutral)
}

# Weighted + debiased poll share from a race's latest polls.
# weight = recency (14-day half-life from as_of) * pollster quality (neutral_err / abs(pred_err)).
# share_fn(p, bias) returns the (debiased) two-party share for one poll, or NA if unusable.
weighted_poll_share <- function(lst, as_of, pollsters, share_fn) {
  ref <- if (!is.null(as_of)) as.Date(as_of) else max(sapply(lst, function(p) as.Date(p$end %||% NA)), na.rm = TRUE)
  if (is.na(ref)) return(NA_real_)
  w <- numeric(length(lst)); sh <- numeric(length(lst))
  for (i in seq_along(lst)) {
    p <- lst[[i]]
    if (is.null(p$end)) next
    age <- as.numeric(ref - as.Date(p$end)); if (is.na(age) || age < 0) age <- 0
    rec <- 0.5 ^ (age / 14)
    row <- pollsters$rows[pollsters$rows$pollster == (p$pollster %||% ""), ]
    if (nrow(row) && !is.na(row$pred_err[1])) {
      qual <- pollsters$neutral / abs(row$pred_err[1])
      bias <- if (!is.na(row$bias[1])) row$bias[1] else 0
    } else { qual <- 1.0; bias <- 0 }
    s <- share_fn(p, bias)
    if (!is.finite(s)) next
    w[i] <- rec * qual; sh[i] <- s
  }
  ok <- is.finite(w) & is.finite(sh) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * sh[ok]) / sum(w[ok])
}

# Polls and ratings keyed by mode ("senate"/"governor"/"house") and state fips (district GEOID for the House).
# Mirrors the notebook's RACE_DATA cell: poll share = D / (D + R) from the 30-day average (else the latest three polls);
# when an independent leads head to head the share is the independent's two-party share instead.
load_race_inputs <- function(ed = load_election_data()) {
  name_fips <- setNames(FIPS, state.name[match(names(FIPS), state.abb)]); name_fips["District of Columbia"] <- "11"
  rd <- list(senate = list(), governor = list(), house = list())
  ratings <- list(senate = c(), governor = c(), house = c())
  open_seat <- list(senate = c(), governor = c(), house = c())
  holder <- list(senate = c(), governor = c(), house = c())
  put <- function(lst, mode, key, val) { lst[[mode]][key] <- val; lst }

  for (r in ed$races$senate$races) {
    key <- FIPS[[r$code]]
    c0 <- rating_center(r$rating)
    if (!is.na(c0)) ratings <- put(ratings, "senate", key, c0)
    open_seat <- put(open_seat, "senate", key, isTRUE(r$open) || grepl("^open", r$incumbent %||% "", ignore.case = TRUE))
    holder <- put(holder, "senate", key, r$party %||% NA_character_)
  }
  for (r in ed$races$governor$races) {
    key <- name_fips[[r$state]]
    c0 <- rating_center(r$rating)
    if (!is.na(c0)) ratings <- put(ratings, "governor", key, c0)
    open_seat <- put(open_seat, "governor", key, isTRUE(r$open) || grepl("^open", r$incumbent %||% "", ignore.case = TRUE))
    holder <- put(holder, "governor", key, r$party %||% NA_character_)
  }
  for (r in ed$races$house$competitive) {
    m <- regmatches(r$district, regexec("^([A-Z]{2})-(\\d\\d|AL)$", r$district))[[1]]
    if (!length(m)) next
    key <- paste0(FIPS[[m[2]]], if (m[3] == "AL") "00" else m[3])
    c0 <- rating_center(r$rating)
    if (!is.na(c0)) ratings <- put(ratings, "house", key, c0)
    open_seat <- put(open_seat, "house", key, isTRUE(r$open))
  }

  polls <- list(senate = c(), governor = c(), house = c())
  ptype <- list(senate = c(), governor = c(), house = c())
  pollsters <- load_pollsters()
  for (k in names(ed$race_polls$races)) {
    v <- ed$race_polls$races[[k]]
    mode <- NULL; key <- NULL
    if (grepl("^Senate-[A-Z]{2}$", k)) { mode <- "senate"; key <- FIPS[[substr(k, 8, 9)]] }
    else if (grepl("^Gov-[A-Z]{2}$", k)) { mode <- "governor"; key <- FIPS[[substr(k, 5, 6)]] }
    else if (grepl("^[A-Z]{2}-(\\d\\d|AL)$", k)) { mode <- "house"; key <- paste0(FIPS[[substr(k, 1, 2)]], if (substr(k, 4, 5) == "AL") "00" else substr(k, 4, 5)) }
    if (is.null(mode) || is.null(key) || is.na(key)) next
    lst <- v$latest
    if (!length(lst)) next
    p0 <- lst[[1]]
    ind_of <- function(p) {
      o <- Filter(function(x) grepl("^ind", x$party %||% "", ignore.case = TRUE), p$others %||% list())
      if (!length(o)) NULL else o[[which.max(sapply(o, function(x) x$pct))]]
    }
    two <- function(p) if (!is.null(p$dem) && !is.null(p$rep) && p$dem$pct + p$rep$pct > 0) p$dem$pct / (p$dem$pct + p$rep$pct) else NA_real_
    ind_two <- function(p) { i <- ind_of(p); if (!is.null(i) && !is.null(p$rep) && i$pct + p$rep$pct > 0) i$pct / (i$pct + p$rep$pct) else NA_real_ }
    ind <- ind_of(p0)
    h2h <- mode != "house" && !is.null(ind) && ind$pct > 25 && (is.null(p0$dem) || p0$dem$pct < 5)
    a <- v$average_30d
    # debiased two-party share for one poll: corrected_D = D + bias, corrected_R = R - bias
    two_debias <- function(p, bias) { if (!is.null(p$dem) && !is.null(p$rep)) { d <- p$dem$pct + bias; r <- p$rep$pct - bias; if (d + r > 0) d / (d + r) else NA_real_ } else NA_real_ }
    ind_debias <- function(p, bias) { i <- ind_of(p); if (!is.null(i) && !is.null(p$rep)) { d <- i$pct + bias; r <- p$rep$pct - bias; if (d + r > 0) d / (d + r) else NA_real_ } else NA_real_ }
    if (h2h) {
      share <- weighted_poll_share(lst, v$as_of, pollsters, ind_debias)
      if (is.na(share)) share <- mean(sapply(lst[seq_len(min(3, length(lst)))], ind_two), na.rm = TRUE)
      ptype <- put(ptype, mode, key, "ind_head_to_head")
    } else {
      share <- weighted_poll_share(lst, v$as_of, pollsters, two_debias)
      if (is.na(share) && !is.null(a) && !is.null(a$dem) && !is.null(a$rep) && a$dem + a$rep > 0) share <- a$dem / (a$dem + a$rep)
      if (is.na(share)) share <- mean(sapply(lst[seq_len(min(3, length(lst)))], two), na.rm = TRUE)
    }
    if (is.finite(share)) {
      # multi-candidate House polls (an independent leading) do not reduce to a D-vs-R share
      if (mode == "house" && (share < 0.2 || share > 0.8)) next
      polls <- put(polls, mode, key, clamp(share, 0.1, 0.9))
    }
  }
  list(ratings = ratings, polls = polls, ptype = ptype, open = open_seat, holder = holder)
}

# Current legislators: incumbent party of every seat (Senate seats up in 2026, House by district id).
load_legislators <- function() {
  L <- fromJSON(cache("legislators.json"), simplifyVector = FALSE)
  rows <- rbindlist(lapply(L, function(p) {
    t <- p$terms[[length(p$terms)]]
    data.table(last = p$name$last, type = t$type, state = t$state, district = t$district %||% NA_integer_,
               cls = t$class %||% NA_integer_, party = t$party,
               caucus = t$caucus %||% t$party)
  }), fill = TRUE)
  rows[, side := fifelse(caucus == "Democrat", "D", fifelse(caucus == "Republican", "R", "I"))]
  rows[, fips := FIPS[state]]
  rows
}

# Parse a Wikipedia candidate field like "▌Name (Republican) ▌Name (Democratic) ..." into a data.table of name/party/side.
parse_cand_field <- function(field_str) {
  if (is.na(field_str) || !grepl("\\(", field_str)) return(NULL)
  parts <- strsplit(field_str, "▌")[[1]]
  rows <- lapply(parts, function(p) {
    mm <- regmatches(p, regexec("^\\s*(.+?)\\s*\\(([^)]+)\\)\\s*$", p))[[1]]
    if (length(mm) != 3) return(NULL)
    data.table(name = trimws(mm[2]), party = trimws(mm[3]))
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  rbindlist(rows)[, side := side_of(party)]
}

# House district Cook PVI (D positive, in points of margin) and the member / whether they are running, from Wikipedia's per-state tables.
load_wiki_house <- function() {
  suppressPackageStartupMessages({library(xml2); library(rvest)})
  j <- fromJSON(cache("wiki_house.json"))
  doc <- read_html(j$parse$text[["*"]])
  xml_remove(html_elements(doc, "style, sup, .reference"))
  name_fips <- setNames(FIPS, state.name[match(names(FIPS), state.abb)])
  out <- list()
  for (tr in html_elements(doc, "table.wikitable tr")) {
    cs <- html_elements(tr, "th,td")
    if (length(cs) != 7) next
    tx <- trimws(gsub("\\s+", " ", html_text2(cs)))
    m <- regmatches(tx[1], regexec("^(.+?) (\\d+|at-large)$", tx[1]))[[1]]
    if (!length(m) || is.na(name_fips[m[2]])) next
    id <- paste0(name_fips[[m[2]]], if (grepl("at-large", m[3], ignore.case = TRUE)) "00" else sprintf("%02d", as.integer(m[3])))
    pv <- tx[2]
    pvi <- if (grepl("^EVEN", pv, ignore.case = TRUE)) 0 else if (grepl("^[DR]\\+\\d", pv)) (if (substr(pv, 1, 1) == "D") 1 else -1) * as.numeric(sub("^[DR]\\+([0-9.]+).*", "\\1", pv)) else NA_real_
    inc_txt <- tx[3]; status <- tx[6]; cands <- tx[7]
    inc_name <- trimws(sub("\\s*(\\(.*|Redistricted.*)$", "", inc_txt))
    inc_last <- tail(strsplit(gsub("[^A-Za-z' -]", "", inc_name), "\\s+")[[1]], 1)
    cands_norm <- gsub("[^A-Za-z' -]", "", cands)
    running <- !grepl("running in|retiring|withdrew|lost renomination|did not run|not seeking", paste(inc_txt, status), ignore.case = TRUE) &&
      nzchar(inc_last) && grepl(inc_last, cands_norm, ignore.case = TRUE, fixed = FALSE)
    out[[id]] <- data.table(dist = id, pvi = pvi, inc_side = if (running) side_of(tx[4]) else NA_character_,
      cand_field = I(list(parse_cand_field(cands))))
  }
  rbindlist(out)[, .SD[1], by = dist]
}
