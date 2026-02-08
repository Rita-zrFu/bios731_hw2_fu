args <- commandArgs(trailingOnly=TRUE)
n <- as.integer(args[1])
beta_treat <- as.numeric(args[2])
err <- args[3]

scenario_id <- sprintf("n%03d_beta%0.1f_%s", n, beta_treat, err)
files <- list.files("data/reps", pattern = paste0("^rep_", scenario_id, "_\\d+\\.rds$"),
                    full.names = TRUE)

stopifnot(length(files) > 0)

lst <- lapply(files, readRDS)
sims <- do.call(rbind, lst)
sims <- sims[order(sims$rep_id), ]
sims <- sims[!duplicated(sims$rep_id), ]

dir.create("data/scenarios", showWarnings=FALSE, recursive=TRUE)
out <- file.path("data/scenarios", paste0("sim_", scenario_id, ".rds"))
saveRDS(sims, out)
cat("Saved merged scenario:", out, "\n")
