# scripts/run_rep.R
args <- commandArgs(trailingOnly = TRUE)

n <- as.integer(args[1])
beta_treat <- as.numeric(args[2])
err <- args[3]
rep_id <- as.integer(args[4])

# optional overrides
B <- if (length(args) >= 5) as.integer(args[5]) else 500
B_inner <- if (length(args) >= 6) as.integer(args[6]) else 100
alpha <- if (length(args) >= 7) as.numeric(args[7]) else 0.05
seed0 <- if (length(args) >= 8) as.integer(args[8]) else 731

source("sim_function.R")

scenario_id <- sprintf("n%03d_beta%0.1f_%s", n, beta_treat, err)

out_dir <- file.path("data", "reps", scenario_id)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_file <- file.path(out_dir, sprintf("rep_%s_%04d.rds", scenario_id, rep_id))

# reproducible, non-overlapping seeds
set.seed(seed0 + n + round(beta_treat * 1000) + ifelse(err == "t3", 10000, 0) + rep_id)

res <- one_rep(n = n, beta_treat = beta_treat, err = err, B = B, B_inner = B_inner, alpha = alpha)

res$true_beta <- beta_treat
res$rep_id <- rep_id
res$cover_wald <- (res$wald_lo <= beta_treat) & (beta_treat <= res$wald_hi)
res$cover_pct  <- (res$pct_lo  <= beta_treat) & (beta_treat <= res$pct_hi)
res$cover_bt   <- (res$bt_lo   <= beta_treat) & (beta_treat <= res$bt_hi)

saveRDS(res, out_file)
cat("Saved:", out_file, "\n")
