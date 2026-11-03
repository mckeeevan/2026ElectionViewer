// Build the notebook for static deployment (Vercel).
// 1. Runs @observablehq/notebook-kit build -> .observable/dist/
// 2. Copies runtime data (data/, model/out/) into the dist folder so the
//    app's relative fetches (./data/..., ./model/out/...) resolve.
// 3. Renames the built notebook to index.html so it serves at the site root.
//
// Output directory: .observable/dist  (set as outputDirectory in vercel.json)

import { execSync } from "node:child_process";
import { cpSync, renameSync, rmSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const dist = path.join(root, ".observable", "dist");
const notebook = "2026 Midterms.html";

console.log("== notebook-kit build ==");
execSync(`npx @observablehq/notebook-kit build --root . -- "${notebook}"`, {
  cwd: root,
  stdio: "inherit",
});

// Copy runtime data the app fetches at load time.
for (const dir of ["data", "model/out"]) {
  const src = path.join(root, dir);
  const dest = path.join(dist, dir);
  if (!existsSync(src)) {
    console.warn(`warn: ${dir} not found, skipping (run Rscript model/run_all.R first)`);
    continue;
  }
  cpSync(src, dest, { recursive: true });
  console.log(`copied ${dir} -> dist/${dir}`);
}

// Serve the notebook at the site root.
const built = path.join(dist, notebook);
const index = path.join(dist, "index.html");
if (existsSync(index)) rmSync(index);
renameSync(built, index);
console.log(`renamed "${notebook}" -> index.html`);

console.log("\nDeploy output ready at .observable/dist");
