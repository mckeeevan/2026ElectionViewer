# run_all.R: refresh inputs, refit the regressions, and re-simulate. Run from the project root:
#
#   Rscript model/run_all.R              # use cached inputs (fast)
#   Rscript model/run_all.R --refetch    # re-download every input first (polls, ratings, results)
#   Rscript model/run_all.R --sims=10000 # more simulations (default 10000)
#
# Outputs land in model/out/ (fit.rds, regressions.json, meta.json, senate.bin, governor.bin, house.bin); the notebook loads them.

args <- commandArgs(TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
run <- function(script, extra = character()) {
    cat("\n== ", script, " ==\n", sep = "")
    status <- system2(rscript, c(file.path("model", script), extra))
    if (status != 0) stop(script, " failed", call. = FALSE)
}
if ("--refetch" %in% args) run("00_fetch.R", "--force") else run("00_fetch.R")
run("01_fit.R")
run("02_simulate.R", args[grepl("^--(sims|seed)=", args)])
cat("\nDone. Reload the notebook.\n")
