# R/run_one_scenario.R
# ------------------------------------------------------------
# Run ONE scenario (n, beta_treat, err) for nSim reps and save to RDS.
# If file exists and overwrite=FALSE, skip.
# ------------------------------------------------------------

run_one_scenario <- function(
    n, beta_treat, err,
    nSim = 500,
    B = 500,
    B_inner = 100,
    alpha = 0.05,
    out_dir = "data",
    seed = 731,
    overwrite = FALSE
) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  scenario_id <- sprintf("n%03d_beta%0.1f_%s", n, beta_treat, err)
  out_file <- file.path(out_dir, paste0("sim_", scenario_id, ".rds"))
  
  if (file.exists(out_file) && !overwrite) {
    message("Skip (already exists): ", out_file)
    return(out_file)
  }
  
  set.seed(seed + n + round(beta_treat * 1000) + ifelse(err == "t3", 10000, 0))
  
  sims <- do.call(rbind, lapply(seq_len(nSim), function(i) {
    one_rep(n = n, beta_treat = beta_treat, err = err, B = B, B_inner = B_inner, alpha = alpha)
  }))
  
  sims$true_beta <- beta_treat
  sims$cover_wald <- (sims$wald_lo <= beta_treat) & (beta_treat <= sims$wald_hi)
  sims$cover_pct  <- (sims$pct_lo  <= beta_treat) & (beta_treat <= sims$pct_hi)
  sims$cover_bt   <- (sims$bt_lo   <= beta_treat) & (beta_treat <= sims$bt_hi)
  
  saveRDS(sims, out_file)
  message("Saved: ", out_file)
  
  out_file
}
