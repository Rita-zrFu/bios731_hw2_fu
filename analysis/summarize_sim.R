# analysis/summarize_sim.R
library(dplyr)
library(tidyr)

base_dir <- "data"
scenario_dirs <- list.dirs(base_dir, recursive = FALSE)

read_one_scenario <- function(dir) {
  files <- list.files(dir, full.names = TRUE)
  sims <- do.call(rbind, lapply(files, readRDS))
  
  nm <- basename(dir)
  parts <- strsplit(nm, "_")[[1]]
  
  n <- as.integer(sub("n", "", parts[1]))
  beta <- as.numeric(sub("beta", "", parts[2]))
  err <- parts[3]
  
  sims %>%
    mutate(n = n, beta = beta, err = err)
}

all_sims <- do.call(rbind, lapply(scenario_dirs, read_one_scenario))

summary <- all_sims %>%
  group_by(n, beta, err) %>%
  summarise(
    n_rep = n(),
    
    # bias / variability
    bias = mean(theta_hat - true_beta, na.rm = TRUE),
    emp_se = sd(theta_hat, na.rm = TRUE),
    
    # coverage
    cov_wald = mean(cover_wald, na.rm = TRUE),
    cov_pct  = mean(cover_pct,  na.rm = TRUE),
    cov_bt   = mean(cover_bt,   na.rm = TRUE),
    
    # interval length
    len_wald = mean(wald_hi - wald_lo, na.rm = TRUE),
    len_pct  = mean(pct_hi  - pct_lo,  na.rm = TRUE),
    len_bt   = mean(bt_hi   - bt_lo,   na.rm = TRUE),
    
    # computation time
    time_wald = mean(time_wald, na.rm = TRUE),
    time_pct  = mean(time_pct,  na.rm = TRUE),
    time_bt   = mean(time_bt,   na.rm = TRUE),
    
    # failure rates
    pct_fail = mean(is.na(pct_lo)),
    bt_fail  = mean(is.na(bt_lo)),
    
    .groups = "drop"
  )

dir.create(here("data", "summary"), showWarnings = FALSE, recursive = TRUE)
saveRDS(summary, here("data", "summary", "summary_all.rds"))
write.csv(summary, here("data", "summary", "summary_all.csv"), row.names = FALSE)

print(summary)
