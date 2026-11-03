// ───────────── raceLogic.mjs ─────────────
// Pure, dependency-free election data-handling logic shared by the dashboard
// (2026 Midterms.html) and the unit tests (test/raceLogic.test.mjs).
//
// Keeping this out of the notebook means the exact code that runs in the browser
// is the code the tests exercise, so a regression caught here is a regression
// caught in the product. No d3, no fetch, no DOM — just data in, data out.

// Two-letter state → 2-digit FIPS code (matches the incumbents cell).
export const FIPS = {
  AL: "01", AK: "02", AZ: "04", AR: "05", CA: "06", CO: "08", CT: "09", DE: "10",
  DC: "11", FL: "12", GA: "13", HI: "15", ID: "16", IL: "17", IN: "18", IA: "19",
  KS: "20", KY: "21", LA: "22", ME: "23", MD: "24", MA: "25", MI: "26", MN: "27",
  MS: "28", MO: "29", MT: "30", NE: "31", NV: "32", NH: "33", NJ: "34", NM: "35",
  NY: "36", NC: "37", ND: "38", OH: "39", OK: "40", OR: "41", PA: "42", RI: "44",
  SC: "45", SD: "46", TN: "47", TX: "48", UT: "49", VT: "50", VA: "51", WA: "53",
  WV: "54", WI: "55", WY: "56"
};

export const clamp = (x, lo, hi) => Math.max(lo, Math.min(hi, x));

// ── Key parsing ──────────────────────────────────────────────────────────────
// A poll key like "Senate-NE", "Gov-CO", or "MT-01" → [mode, fipsKey].
// House keys are 2-digit state FIPS + 2-digit district ("AL" → "00").
export function keyOf(k, fips = FIPS) {
  let m;
  if ((m = /^Senate-([A-Z]{2})$/.exec(k))) return ["senate", fips[m[1]]];
  if ((m = /^Gov-([A-Z]{2})$/.exec(k))) return ["governor", fips[m[1]]];
  if ((m = /^([A-Z]{2})-(\d\d|AL)$/.exec(k))) return ["house", fips[m[1]] + (m[2] === "AL" ? "00" : m[2])];
  return null;
}

// ── Rating → expected Democratic two-party share (a forecast, not a poll) ────
const CENTER = {
  "safe r": 0.36, "likely r": 0.43, "lean r": 0.47, "toss-up": 0.50,
  "lean d": 0.53, "likely d": 0.57, "safe d": 0.64
};
export function centerOf(rating) {
  const norm = String(rating || "")
    .replace(/^solid/i, "Safe")
    .replace(/toss[\s-]*up/i, "Toss-up")
    .toLowerCase()
    .trim();
  return CENTER[norm] ?? null;
}

// Strip a trailing parenthetical: "Pete Ricketts (R)" → "Pete Ricketts".
export function cleanName(s) {
  return String(s).replace(/\s*\(.*$/, "").trim();
}

// ── Poll math ────────────────────────────────────────────────────────────────
// Two-party Democratic share from a single poll, or null if D/R aren't both present.
export function two(p) {
  return p.dem && p.rep && p.dem.pct + p.rep.pct > 0 ? p.dem.pct / (p.dem.pct + p.rep.pct) : null;
}

// The strongest independent candidate in a poll (by pct), or undefined.
export function indOf(p) {
  return (p.others || []).filter((o) => /^ind/i.test(o.party || "")).sort((a, b) => b.pct - a.pct)[0];
}

// Independent's two-party share (vs the Republican), or null.
export function indTwo(p) {
  const i = indOf(p);
  return i && p.rep && i.pct + p.rep.pct > 0 ? i.pct / (i.pct + p.rep.pct) : null;
}

// Viable third-party candidate: any non-D/R candidate above `minPct` in ANY poll
// in the list. Poll artifacts like "Generic Candidate" are ignored. Returns the
// strongest such candidate {name, party, pct}, or null.
export function topThirdParty(lst, minPct = 4) {
  const isThirdParty = (o) => {
    const p = String(o.party || "").toUpperCase();
    return !/^(DEM|DEMOCRAT|REP|REPUBLICAN|GOP)$/.test(p);
  };
  const byName = new Map();
  for (const p of lst) {
    for (const o of p.others || []) {
      if (!o || !o.name || /generic/i.test(o.name) || !isThirdParty(o)) continue;
      if ((o.pct || 0) <= minPct) continue;
      const prev = byName.get(o.name);
      if (!prev || o.pct > prev.pct) byName.set(o.name, o);
    }
  }
  let top = null;
  for (const o of byName.values()) if (!top || o.pct > top.pct) top = o;
  return top;
}

// Race type derived from the ballot itself: a D candidate with a real presence
// (>=8% in the latest poll) makes it a three-way; otherwise a third-party
// candidate makes it an IND-vs-R head-to-head; else a plain two-party race.
export function deriveRaceType(tp, dPct) {
  if (!tp) return "two_party";
  return dPct >= 8 ? "three_way" : "ind_head_to_head";
}

// ── Map merging (fallback hand-entered data overlaid by poll-derived data) ───
// `cands` is the poll-derived map for one mode; `fallbackMap` the hand-entered
// one. Poll data wins on conflict; `type` defaults to "two_party".
export function buildCandidateMap(fallbackMap, cands) {
  const map = new Map(fallbackMap);
  for (const [key, remote] of cands) {
    const fb = map.get(key) || {};
    const merged = { type: "two_party", ...fb, ...remote };
    // The hand-entered fallback type is authoritative — it matches how the R
    // model actually runs the race (h2h vs three-way). The poll-derived type
    // is only a heuristic (a minor candidate's share can push it to
    // "three_way" even when the model runs a head-to-head).
    if (fb.type) merged.type = fb.type;
    map.set(key, merged);
  }
  return map;
}

// `polls` is the poll-derived share map for one mode; `fallbackMap` the
// hand-entered one. Multi-candidate House polls (an independent leading) don't
// reduce to a D-vs-R share, so they're skipped.
export function buildPollMap(fallbackMap, polls, mode) {
  const map = new Map(fallbackMap);
  for (const [key, share] of polls) {
    if (mode === "house" && (share < 0.2 || share > 0.8)) continue;
    map.set(key, clamp(share, 0.1, 0.9));
  }
  return map;
}

// ── Election-night aggregation ───────────────────────────────────────────────
// Fraction of a unit's votes reported by time T (0..1).
export function reportedFraction(sim, T) {
  if (T >= 1) return 1;
  if (T <= sim.start) return 0;
  return clamp((T - sim.start) / (sim.duration || 0.25), 0, 1);
}

// Split each unit's votes into D / R / I totals by race type.
// `list` is an array of sim rows: {votes, start, duration, dem, mailFirst,
// raceType, indBase, pD, pR, pI, tpShare}.
export function aggregate(list, T) {
  let dem = 0, rep = 0, ind = 0, total = 0;
  for (const s of list) {
    const f = reportedFraction(s, T);
    const v = s.votes * f;
    const waveShift = (s.mailFirst || 0) * 0.06 * (1 - f);

    if (s.raceType === "ind_head_to_head") {
      // The independent is the challenger. Anchor their share at the poll-derived
      // indBase (their two-party share vs the incumbent) and let the simulated
      // swing (s.dem around 0.5) nudge it, so the split tracks the model instead
      // of collapsing to the incumbent's party.
      const base = s.indBase != null ? s.indBase : 0.5;
      const indShare = clamp(base + (s.dem - 0.5) * 0.5 + waveShift, 0.05, 0.95);
      ind += v * indShare;
      rep += v * (1 - indShare);
    } else if (s.raceType === "three_way") {
      // Anchor the third-party at their poll share; the sim's two-party D share
      // drives the D/R split of the remaining vote.
      const indShare = s.pI != null ? clamp(s.pI, 0.02, 0.6) : 0.15;
      const twoParty = 1 - indShare;
      const simTwoPartyD = clamp(s.dem + waveShift, 0.02, 0.98);
      dem += v * simTwoPartyD * twoParty;
      rep += v * (1 - simTwoPartyD) * twoParty;
      ind += v * indShare;
    } else {
      const demShare = clamp(s.dem + waveShift, 0.02, 0.98);
      // Only carve out a separate third-party (I) bucket when there is a real
      // named third-party candidate on the ballot. Otherwise the model's
      // third-party share is spread over the two existing candidates (D/R)
      // rather than shown as a phantom "Independent" row.
      const tp = s.tpShare || 0;
      if (tp > 0.01 && s.hasThirdPartyCand) {
        const twoParty = 1 - tp;
        dem += v * demShare * twoParty;
        rep += v * (1 - demShare) * twoParty;
        ind += v * tp;
      } else {
        dem += v * demShare;
        rep += v * (1 - demShare);
      }
    }
    total += s.votes;
  }
  const counted = dem + rep + ind;
  return {
    dem, rep, ind, total, counted,
    demShare: counted > 0 ? dem / counted : 0.5,
    repShare: counted > 0 ? rep / counted : 0.5,
    indShare: counted > 0 ? ind / counted : 0
  };
}

// ── Election-night posterior ─────────────────────────────────────────────────
// Core of the Election Night panel: given K simulated county D-shares and the
// counties reported so far, weight each sim by how well it explains the
// observations (Gaussian likelihood, std `sigma`). This is the exact math the
// dashboard runs, extracted so the overconfidence behavior is unit-testable.
//
// `sims` is K × nCounties (each row a sim's county D-shares). `obs` is the
// reported county D-shares; `obsMask` (same length) marks which counties have
// reported (1) vs not (0). Unreported counties contribute no likelihood.
// Returns a Float64Array of log-weights (relative, max-normalized).
export function posteriorLogWeights(sims, obs, obsMask, sigma) {
  const K = sims.length, n = obs.length;
  const inv2s2 = 1 / (2 * sigma * sigma);
  const logW = new Float64Array(K);
  for (let k = 0; k < K; k++) {
    const row = sims[k];
    let lw = 0;
    for (let i = 0; i < n; i++) {
      if (!obsMask[i]) continue;
      const d = obs[i] - row[i];
      lw -= 0.5 * d * d * inv2s2;
    }
    logW[k] = lw;
  }
  // max-normalize for numerical stability (matches the dashboard's log-sum-exp)
  let maxL = -Infinity;
  for (let k = 0; k < K; k++) if (logW[k] > maxL) maxL = logW[k];
  if (maxL > -Infinity) for (let k = 0; k < K; k++) logW[k] -= maxL;
  return logW;
}

// Effective sample size of a weighted posterior: (Σw)² / Σw². Equals K when
// all weights are equal (maximum uncertainty) and 1 when a single sim dominates
// (the overconfidence failure mode). `logW` is max-normalized log-weights.
export function effectiveSampleSize(logW) {
  let sumW = 0, sumW2 = 0;
  for (let k = 0; k < logW.length; k++) {
    const w = Math.exp(logW[k]);
    sumW += w; sumW2 += w * w;
  }
  return sumW > 0 ? (sumW * sumW) / Math.max(sumW2, 1e-9) : 1;
}

// ── Candidate resolution ─────────────────────────────────────────────────────
// Resolve the on-ballot candidate names for a race. `candidates` is the merged
// candidate map for the mode (poll + fallback); `lastGov` (governor only) is the
// prior governor-election result {dem, rep, m} used to fill missing names.
// Returns {type, D, R, I, inc, baseParty, ...}.
export function candidateFor(mode, sid, inc, candidates, lastGov = null) {
  const fips = String(sid).padStart(2, "0");
  const incParty = inc?.caucus || inc?.party;
  const generic = mode === "house"
    ? {
        type: "two_party",
        D: incParty === "D" ? inc.name : "Democratic Challenger",
        R: incParty === "R" ? inc.name : "Republican Challenger",
        inc: incParty || null,
        baseParty: incParty || "D"
      }
    : {
        type: "two_party",
        D: incParty === "D" ? inc.name : "Democratic Nominee",
        R: incParty === "R" ? inc.name : "Republican Nominee",
        inc: incParty || null,
        baseParty: incParty || "D"
      };
  const specific = candidates?.get(fips);
  const baseParty = mode === "governor" && lastGov
    ? (lastGov.m >= 0 ? "D" : "R")
    : (specific?.baseParty || incParty || generic.baseParty);
  if (specific) {
    const out = { ...generic, ...specific };
    out.baseParty = baseParty || out.baseParty || generic.baseParty;
    if (!out.D && lastGov?.dem) out.D = lastGov.dem;
    if (!out.R && lastGov?.rep) out.R = lastGov.rep;
    return out;
  }
  if (lastGov) {
    return { ...generic, D: lastGov.dem || generic.D, R: lastGov.rep || generic.R, baseParty };
  }
  return { ...generic, baseParty };
}
