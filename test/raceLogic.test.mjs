// Unit tests for lib/raceLogic.mjs — the pure election data-handling logic that
// the dashboard (2026 Midterms.html) imports. Run with `npm test`.
//
// These guard the exact code paths behind the bugs we've hit:
//   • Nebraska Senate (no Democrat on the ballot → ind_head_to_head, not two_party)
//   • the "polls ~5%" note leaking onto every candidate row
//   • poll-derived race types (three_way vs ind_head_to_head vs two_party)
//   • FIPS / district key parsing, rating normalization, candidate resolution.

import { test } from "node:test";
import assert from "node:assert/strict";
import {
  FIPS, clamp, keyOf, centerOf, cleanName,
  two, indOf, indTwo, topThirdParty, deriveRaceType,
  buildCandidateMap, buildPollMap, reportedFraction, aggregate, candidateFor,
  posteriorLogWeights, effectiveSampleSize
} from "../lib/raceLogic.mjs";

// ── FIPS / clamp ─────────────────────────────────────────────────────────────
test("FIPS maps states to 2-digit codes", () => {
  assert.equal(FIPS.NE, "31");
  assert.equal(FIPS.CO, "08");
  assert.equal(FIPS.AL, "01");
  assert.equal(FIPS.WY, "56");
});

test("clamp bounds values", () => {
  assert.equal(clamp(0.5, 0, 1), 0.5);
  assert.equal(clamp(-1, 0, 1), 0);
  assert.equal(clamp(2, 0, 1), 1);
  assert.equal(clamp(0.47, 0.05, 0.95), 0.47);
});

// ── keyOf: poll key → [mode, fipsKey] ────────────────────────────────────────
test("keyOf parses Senate / Gov / House keys", () => {
  assert.deepEqual(keyOf("Senate-NE"), ["senate", "31"]);
  assert.deepEqual(keyOf("Gov-CO"), ["governor", "08"]);
  assert.deepEqual(keyOf("MT-01"), ["house", "3001"]);
  assert.deepEqual(keyOf("AL-AL"), ["house", "0100"]); // at-large → "00"
});

test("keyOf returns null for unknown keys", () => {
  assert.equal(keyOf("Bogus-XX"), null);
  assert.equal(keyOf("Senate-XXX"), null);
  assert.equal(keyOf("MT-1"), null); // single-digit district is not a valid key
});

// ── centerOf: rating → expected D two-party share ────────────────────────────
test("centerOf normalizes Cook/Sabato ratings", () => {
  assert.equal(centerOf("Toss-up"), 0.5);
  assert.equal(centerOf("toss up"), 0.5);
  assert.equal(centerOf("Solid R"), 0.36); // "Solid" → "Safe"
  assert.equal(centerOf("Likely D"), 0.57);
  assert.equal(centerOf("Lean R"), 0.47);
  assert.equal(centerOf("Safe D"), 0.64);
});

test("centerOf returns null for unknown ratings", () => {
  assert.equal(centerOf("Unknown"), null);
  assert.equal(centerOf(""), null);
  assert.equal(centerOf(null), null);
});

// ── cleanName ────────────────────────────────────────────────────────────────
test("cleanName strips trailing parentheticals", () => {
  assert.equal(cleanName("Pete Ricketts (R)"), "Pete Ricketts");
  assert.equal(cleanName("Dan Osborn (I)"), "Dan Osborn");
  assert.equal(cleanName("Plain Name"), "Plain Name");
});

// ── two / indOf / indTwo ─────────────────────────────────────────────────────
test("two returns the two-party D share", () => {
  assert.equal(two({ dem: { pct: 40 }, rep: { pct: 60 } }), 0.4);
  assert.equal(two({ dem: { pct: 50 }, rep: { pct: 50 } }), 0.5);
});

test("two returns null when D or R is missing or zero", () => {
  assert.equal(two({ dem: { pct: 40 } }), null);
  assert.equal(two({ rep: { pct: 60 } }), null);
  assert.equal(two({ dem: { pct: 0 }, rep: { pct: 0 } }), null);
});

test("indOf picks the strongest independent", () => {
  const p = { others: [{ name: "A", party: "Ind", pct: 3 }, { name: "B", party: "Independent", pct: 7 }] };
  assert.equal(indOf(p).name, "B");
  assert.equal(indOf({ others: [] }), undefined);
});

test("indTwo returns the independent's two-party share vs R", () => {
  const p = { rep: { pct: 49 }, others: [{ name: "Osborn", party: "Ind", pct: 43 }] };
  assert.ok(Math.abs(indTwo(p) - 43 / (43 + 49)) < 1e-9);
  assert.equal(indTwo({ rep: { pct: 49 } }), null);
});

// ── topThirdParty ────────────────────────────────────────────────────────────
test("topThirdParty finds the strongest non-D/R candidate above the threshold", () => {
  const lst = [
    { others: [{ name: "Sheedy", party: "Libertarian", pct: 5 }] },
    { others: [{ name: "Sheedy", party: "Libertarian", pct: 4 }] }
  ];
  const tp = topThirdParty(lst);
  assert.equal(tp.name, "Sheedy");
  assert.equal(tp.pct, 5); // strongest across all polls
});

test("topThirdParty ignores generic candidates and sub-threshold entries", () => {
  const lst = [
    { others: [{ name: "Generic Candidate", party: "Ind", pct: 9 }] },
    { others: [{ name: "Minor", party: "Green", pct: 3 }] }
  ];
  assert.equal(topThirdParty(lst), null);
});

test("topThirdParty ignores D/R candidates", () => {
  const lst = [{ others: [{ name: "Some Dem", party: "Democrat", pct: 20 }] }];
  assert.equal(topThirdParty(lst), null);
});

// ── deriveRaceType ───────────────────────────────────────────────────────────
test("deriveRaceType classifies the ballot", () => {
  assert.equal(deriveRaceType(null, 50), "two_party");
  assert.equal(deriveRaceType({ name: "X", pct: 5 }, 50), "three_way"); // D >= 8%
  assert.equal(deriveRaceType({ name: "X", pct: 5 }, 3), "ind_head_to_head"); // D < 8%
});

// ── buildCandidateMap: fallback overlaid by poll data ────────────────────────
test("buildCandidateMap overlays poll data onto fallback (poll wins)", () => {
  const fallback = new Map([["31", { type: "ind_head_to_head", R: "Pete Ricketts", I: "Dan Osborn", indBase: 0.47 }]]);
  const cands = new Map([["31", { D: "Some Dem", R: "Pete Ricketts" }]]);
  const map = buildCandidateMap(fallback, cands);
  const ne = map.get("31");
  assert.equal(ne.R, "Pete Ricketts");
  assert.equal(ne.I, "Dan Osborn"); // preserved from fallback
  assert.equal(ne.D, "Some Dem"); // added from poll
  assert.equal(ne.indBase, 0.47); // preserved from fallback
});

test("buildCandidateMap defaults type to two_party when neither provides it", () => {
  const map = buildCandidateMap(new Map(), new Map([["08", { D: "A", R: "B" }]]));
  assert.equal(map.get("08").type, "two_party");
});

test("buildCandidateMap keeps fallback entries with no poll data (the Nebraska case)", () => {
  // Nebraska has no poll, so it only exists in the fallback map.
  const fallback = new Map([["31", { type: "ind_head_to_head", R: "Pete Ricketts", I: "Dan Osborn", indBase: 0.47 }]]);
  const map = buildCandidateMap(fallback, new Map());
  assert.equal(map.get("31").type, "ind_head_to_head");
  assert.equal(map.get("31").indBase, 0.47);
});

// ── buildPollMap ─────────────────────────────────────────────────────────────
test("buildPollMap clamps shares and skips multi-candidate House polls", () => {
  const fallback = new Map([["31", 0.495]]);
  const polls = new Map([["31", 0.47], ["0401", 0.15], ["0402", 0.5]]);
  const senate = buildPollMap(fallback, new Map([["31", 0.47]]), "senate");
  assert.equal(senate.get("31"), 0.47);
  const house = buildPollMap(new Map(), polls, "house");
  assert.equal(house.get("0401"), undefined); // 0.15 < 0.2 → skipped
  assert.equal(house.get("0402"), 0.5);
});

test("buildPollMap clamps out-of-range shares", () => {
  const m = buildPollMap(new Map(), new Map([["08", 0.99]]), "senate");
  assert.equal(m.get("08"), 0.9);
});

// ── reportedFraction ─────────────────────────────────────────────────────────
test("reportedFraction is 0 before start, 1 after finish", () => {
  const sim = { start: 0.2, duration: 0.5 };
  assert.equal(reportedFraction(sim, 0.1), 0);
  assert.equal(reportedFraction(sim, 1), 1);
  assert.ok(reportedFraction(sim, 0.45) > 0 && reportedFraction(sim, 0.45) < 1);
});

// ── aggregate: the Nebraska regression guard ─────────────────────────────────
test("aggregate: ind_head_to_head anchors the independent at indBase (Nebraska)", () => {
  // Nebraska: Ricketts (R) vs Osborn (Ind). indBase = Osborn's two-party share ≈ 0.47.
  // At full reporting (T=1) with a neutral sim (dem=0.5), Osborn should get ~47%.
  const rows = [{ votes: 1000, start: 0, duration: 1, dem: 0.5, mailFirst: 0, raceType: "ind_head_to_head", indBase: 0.47 }];
  const a = aggregate(rows, 1);
  assert.ok(Math.abs(a.ind - 470) < 1, `ind=${a.ind}`);
  assert.ok(Math.abs(a.rep - 530) < 1, `rep=${a.rep}`);
  assert.equal(a.dem, 0); // no Democrat on the ballot
  // The independent must NOT be at 0 (the original bug showed Osborn at 0).
  assert.ok(a.ind > 0);
});

test("aggregate: ind_head_to_head nudges with the simulated swing", () => {
  const base = { votes: 1000, start: 0, duration: 1, mailFirst: 0, raceType: "ind_head_to_head", indBase: 0.47 };
  const a = aggregate([{ ...base, dem: 0.5 }], 1);
  const b = aggregate([{ ...base, dem: 0.7 }], 1); // D-leaning sim → independent gains
  assert.ok(b.ind > a.ind);
});

test("aggregate: ind_head_to_head falls back to 0.5 when indBase is missing", () => {
  const a = aggregate([{ votes: 1000, start: 0, duration: 1, dem: 0.5, mailFirst: 0, raceType: "ind_head_to_head" }], 1);
  assert.ok(Math.abs(a.ind - 500) < 1);
});

test("aggregate: three_way allocates the third-party at its poll share", () => {
  const rows = [{ votes: 1000, start: 0, duration: 1, dem: 0.5, mailFirst: 0, raceType: "three_way", pD: 0.5, pR: 0.45, pI: 0.05 }];
  const a = aggregate(rows, 1);
  assert.ok(Math.abs(a.ind - 50) < 1, `ind=${a.ind}`); // 5% of 1000
  assert.ok(a.dem > 0 && a.rep > 0);
  assert.ok(Math.abs(a.dem + a.rep + a.ind - 1000) < 1e-6);
});

test("aggregate: two_party splits D/R by the simulated share", () => {
  const a = aggregate([{ votes: 1000, start: 0, duration: 1, dem: 0.6, mailFirst: 0, raceType: "two_party" }], 1);
  assert.ok(Math.abs(a.dem - 600) < 1);
  assert.ok(Math.abs(a.rep - 400) < 1);
  assert.equal(a.ind, 0);
});

test("aggregate: two_party with a named third-party candidate shrinks D/R proportionally", () => {
  const a = aggregate([{ votes: 1000, start: 0, duration: 1, dem: 0.5, mailFirst: 0, raceType: "two_party", tpShare: 0.1, hasThirdPartyCand: true }], 1);
  assert.ok(Math.abs(a.ind - 100) < 1); // 10% third-party
  assert.ok(Math.abs(a.dem - 450) < 1); // 50% of the remaining 900
  assert.ok(Math.abs(a.rep - 450) < 1);
});

test("aggregate: two_party third-party vote spreads over D/R when no named candidate", () => {
  // No real independent on the ballot → the model's third-party share is
  // folded into D/R instead of a phantom "Independent" bucket.
  const a = aggregate([{ votes: 1000, start: 0, duration: 1, dem: 0.5, mailFirst: 0, raceType: "two_party", tpShare: 0.1 }], 1);
  assert.equal(a.ind, 0);
  assert.ok(Math.abs(a.dem - 500) < 1);
  assert.ok(Math.abs(a.rep - 500) < 1);
  assert.ok(Math.abs(a.dem + a.rep - 1000) < 1e-6);
});

test("aggregate: partial reporting scales the counted votes", () => {
  const a = aggregate([{ votes: 1000, start: 0, duration: 1, dem: 0.5, mailFirst: 0, raceType: "two_party" }], 0.5);
  assert.ok(Math.abs(a.dem - 250) < 1); // half reported
  assert.ok(Math.abs(a.rep - 250) < 1);
});

test("aggregate: empty list yields neutral shares", () => {
  const a = aggregate([], 1);
  assert.equal(a.dem, 0);
  assert.equal(a.total, 0);
  assert.equal(a.demShare, 0.5);
});

// ── candidateFor ─────────────────────────────────────────────────────────────
test("candidateFor returns specific candidates when present", () => {
  const cands = new Map([["31", { type: "ind_head_to_head", R: "Pete Ricketts", I: "Dan Osborn", inc: "R", baseParty: "R" }]]);
  const out = candidateFor("senate", "31", { caucus: "R", name: "Pete Ricketts" }, cands);
  assert.equal(out.R, "Pete Ricketts");
  assert.equal(out.I, "Dan Osborn");
  assert.equal(out.type, "ind_head_to_head");
});

test("candidateFor falls back to generic placeholders when no specific data", () => {
  const out = candidateFor("senate", "08", { caucus: "D", name: "Some Incumbent" }, new Map());
  assert.equal(out.D, "Some Incumbent"); // incumbent is D
  assert.equal(out.R, "Republican Nominee"); // no R candidate known
  assert.equal(out.type, "two_party");
});

test("candidateFor uses last-election names for governor when no specific data", () => {
  const lastGov = { dem: "Jane Doe", rep: "John Roe", m: 0.1 };
  const out = candidateFor("governor", "08", null, new Map(), lastGov);
  assert.equal(out.D, "Jane Doe");
  assert.equal(out.R, "John Roe");
  assert.equal(out.baseParty, "D"); // last election D won
});

test("candidateFor pads the FIPS key", () => {
  const cands = new Map([["08", { D: "A", R: "B" }]]);
  const out = candidateFor("senate", 8, null, cands); // numeric 8 → "08"
  assert.equal(out.D, "A");
});

// ── Edge cases ───────────────────────────────────────────────────────────────
test("reportedFraction uses a 0.25 default duration and is 0 exactly at start", () => {
  // duration omitted → defaults to 0.25; at T = start + 0.125 → 50%
  assert.equal(reportedFraction({ start: 0.5 }, 0.625), 0.5);
  // exactly at start → 0
  assert.equal(reportedFraction({ start: 0.5 }, 0.5), 0);
  // before start → 0
  assert.equal(reportedFraction({ start: 0.5 }, 0.1), 0);
  // after finish → 1
  assert.equal(reportedFraction({ start: 0.5, duration: 0.25 }, 1), 1);
});

test("aggregate: mail-in wave shifts the D share early in the count", () => {
  // A mailFirst unit early in the count (small f) shifts the D share up by ~0.06;
  // by the end (f=1) the wave has fully arrived and the shift is 0.
  const list = [{ votes: 1000, start: 0.5, duration: 0.5, dem: 0.5, mailFirst: 1, raceType: "two_party" }];
  const early = aggregate(list, 0.51); // f=0.02 → waveShift ≈ 0.0588
  const atEnd = aggregate(list, 1);    // f=1 → waveShift = 0
  assert.ok(early.demShare > atEnd.demShare);
  assert.ok(Math.abs(atEnd.demShare - 0.5) < 1e-9);
});

test("aggregate: three_way with no pI falls back to a 0.15 third-party share", () => {
  const list = [{ votes: 1000, start: 0, duration: 1, dem: 0.5, raceType: "three_way" }];
  const out = aggregate(list, 1);
  assert.ok(Math.abs(out.indShare - 0.15) < 1e-9);
});

test("candidateFor: governor baseParty follows the last election winner", () => {
  const cands = new Map();
  // last election R won (m < 0) → baseParty "R"
  const r = candidateFor("governor", 8, null, cands, { dem: "X", rep: "Y", m: -0.05 });
  assert.equal(r.baseParty, "R");
  // last election D won (m >= 0) → baseParty "D"
  const d = candidateFor("governor", 8, null, cands, { dem: "X", rep: "Y", m: 0.02 });
  assert.equal(d.baseParty, "D");
});

test("candidateFor: specific candidate with a null D is filled from lastGov", () => {
  // A specific candidate that explicitly has no Democrat (D: null) gets the
  // prior-election Democrat name filled in from lastGov.
  const cands = new Map([["08", { D: null, R: "Incumbent R" }]]);
  const out = candidateFor("governor", 8, null, cands, { dem: "Prior D", rep: "Prior R", m: 0.1 });
  assert.equal(out.R, "Incumbent R");
  assert.equal(out.D, "Prior D"); // filled from lastGov
});

test("buildPollMap: non-house modes keep out-of-range shares (clamped)", () => {
  // House polls outside [0.2, 0.8] are skipped; senate polls are kept (clamped).
  const house = buildPollMap(new Map(), new Map([["0101", 0.15]]), "house");
  assert.equal(house.has("0101"), false);
  const senate = buildPollMap(new Map(), new Map([["01", 0.05]]), "senate");
  assert.equal(senate.get("01"), 0.1); // clamped up to the 0.1 floor
});

test("centerOf: hyphenated and spaced toss-up forms both normalize", () => {
  assert.equal(centerOf("Toss-Up"), 0.5);
  assert.equal(centerOf("Toss Up"), 0.5);
  assert.equal(centerOf("toss  up"), 0.5);
});

test("two: returns null when the D+R sum is zero", () => {
  assert.equal(two({ dem: { pct: 0 }, rep: { pct: 0 } }), null);
});

// ── Election-night posterior: overconfidence guard ───────────────────────────
// Regression for the "219–219 seats with 22 counties in" bug: feeding the
// reality sim's exact county values as observations (zero noise) makes the
// posterior collapse to a single sim far too early. These tests pin the
// behavior of the extracted likelihood math.

// Build K sims that differ from one another at the county level (like the real
// 10,000 independent draws), with sim 0 as the "reality" sim.
function makeSims(K, nCounties, spread) {
  const sims = [];
  for (let k = 0; k < K; k++) {
    const row = new Float64Array(nCounties);
    for (let i = 0; i < nCounties; i++) {
      // deterministic pseudo-difference so sims are distinct per county
      const seed = Math.sin(k * 12.9898 + i * 78.233) * 43758.5453;
      row[i] = 0.5 + (seed - Math.floor(seed) - 0.5) * spread;
    }
    sims.push(row);
  }
  return sims;
}

test("posterior: no observations → all sims equally weighted (nEff = K)", () => {
  const K = 200, n = 50;
  const sims = makeSims(K, n, 0.1);
  const obs = new Float64Array(n);
  const mask = new Uint8Array(n); // nothing reported
  const logW = posteriorLogWeights(sims, obs, mask, 0.07);
  assert.ok(effectiveSampleSize(logW) > K * 0.99, "nEff should be ~K with no data");
});

test("posterior: zero-noise observations collapse to one sim (the bug)", () => {
  // Feeding the reality sim's exact values with sigma→0 makes it match perfectly
  // and every other sim's likelihood → -Infinity. This is the overconfidence
  // failure mode the dashboard must avoid by adding observation noise.
  const K = 200, n = 50;
  const sims = makeSims(K, n, 0.1);
  const obs = sims[0].slice(); // reality sim's exact county values
  const mask = new Uint8Array(n).fill(1);
  const logW = posteriorLogWeights(sims, obs, mask, 1e-6);
  assert.ok(effectiveSampleSize(logW) < 1.5, "zero-noise should collapse to ~1 sim");
});

test("posterior: with realistic noise, few counties keep the posterior broad", () => {
  // The fix: with observation noise (sigma ~ sim-to-sim spread) and only a
  // handful of reported counties, the posterior must NOT collapse — nEff stays
  // well above 1, so the seat range shows real uncertainty early in the night.
  const K = 200, n = 50;
  const sims = makeSims(K, n, 0.09);
  const obs = sims[0].slice();
  const mask = new Uint8Array(n);
  for (let i = 0; i < 5; i++) mask[i] = 1; // only 5 counties reported
  const logW = posteriorLogWeights(sims, obs, mask, 0.07);
  const nEff = effectiveSampleSize(logW);
  assert.ok(nEff > 20, `5 noisy counties should keep nEff high, got ${nEff}`);
});

test("posterior: more counties narrow the posterior toward the reality sim", () => {
  // As counties accumulate, the posterior should narrow (nEff falls) and the
  // reality sim should dominate — but only after enough data, not instantly.
  // Use a spread larger than sigma so the sims are genuinely distinct and the
  // reality sim's exact match stands out once enough counties report.
  const K = 200, n = 50;
  const sims = makeSims(K, n, 0.2);
  const obs = sims[0].slice();
  const mask = new Uint8Array(n).fill(1); // all counties reported
  const logW = posteriorLogWeights(sims, obs, mask, 0.07);
  const nEff = effectiveSampleSize(logW);
  // With all 50 counties and noise smaller than the sim spread, the reality
  // sim should be the clear winner (low nEff).
  assert.ok(nEff < 5, `full data should narrow the posterior, got nEff=${nEff}`);
  // And the reality sim (index 0) should carry the top weight.
  let top = 0;
  for (let k = 1; k < K; k++) if (logW[k] > logW[top]) top = k;
  assert.equal(top, 0, "reality sim should be the top-weighted sim");
});
