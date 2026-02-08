# R/sim_functions.R
# ------------------------------------------------------------
# Simulation helper functions
# Includes: DGP, estimator, Wald CI, bootstrap percentile CI, bootstrap-t CI
# ------------------------------------------------------------

gen_data <- function(n, beta_treat, err = c("normal", "t3"), beta0 = 0, sigma2 = 2) {
  err <- match.arg(err)
  
  x <- stats::rbinom(n, 1, 0.5)
  
  if (err == "normal") {
    eps <- stats::rnorm(n, mean = 0, sd = sqrt(sigma2))
  } else {
    # t with df=3, scaled to Var = sigma2
    nu <- 3
    u <- stats::rt(n, df = nu)                       # Var(u) = nu/(nu-2)
    eps <- u * sqrt(sigma2 * (nu - 2) / nu)          # scale so Var(eps) = sigma2
  }
  
  y <- beta0 + beta_treat * x + eps
  data.frame(y = y, x = x)
}

fit_theta <- function(dat) {
  fit <- stats::lm(y ~ x, data = dat)
  co <- summary(fit)$coef
  theta_hat <- unname(co["x", "Estimate"])
  se_hat <- unname(co["x", "Std. Error"])
  list(theta_hat = theta_hat, se_hat = se_hat)
}

ci_wald <- function(theta_hat, se_hat, alpha = 0.05) {
  zcrit <- stats::qnorm(1 - alpha/2)
  c(theta_hat - zcrit * se_hat, theta_hat + zcrit * se_hat)
}

ci_boot_percentile <- function(dat, B = 500, alpha = 0.05) {
  n <- nrow(dat)
  theta_hat <- fit_theta(dat)$theta_hat
  
  thetas <- replicate(B, {
    idx <- sample.int(n, n, replace = TRUE)
    fit_theta(dat[idx, , drop = FALSE])$theta_hat
  })
  
  ci <- stats::quantile(thetas, probs = c(alpha/2, 1 - alpha/2), names = FALSE, type = 8)
  list(ci = ci, theta_hat = theta_hat, boot_thetas = thetas)
}

ci_boot_t <- function(dat, B = 500, B_inner = 100, alpha = 0.05) {
  n <- nrow(dat)
  base <- fit_theta(dat)
  theta_hat <- base$theta_hat
  se_hat <- base$se_hat
  
  tstars <- numeric(B)
  
  for (b in seq_len(B)) {
    idx_b <- sample.int(n, n, replace = TRUE)
    dat_b <- dat[idx_b, , drop = FALSE]
    
    theta_b <- fit_theta(dat_b)$theta_hat
    
    # inner bootstrap for se(theta_b)
    theta_inner <- replicate(B_inner, {
      idx_in <- sample.int(n, n, replace = TRUE)
      fit_theta(dat_b[idx_in, , drop = FALSE])$theta_hat
    })
    se_b <- stats::sd(theta_inner)
    
    if (is.na(se_b) || se_b <= 0) {
      tstars[b] <- NA_real_
    } else {
      tstars[b] <- (theta_b - theta_hat) / se_b
    }
  }
  
  tstars <- tstars[is.finite(tstars)]
  q_lo <- stats::quantile(tstars, probs = 1 - alpha/2, names = FALSE, type = 8)
  q_hi <- stats::quantile(tstars, probs = alpha/2, names = FALSE, type = 8)
  
  ci <- c(theta_hat - q_lo * se_hat,
          theta_hat - q_hi * se_hat)
  
  list(ci = ci, theta_hat = theta_hat, se_hat = se_hat, tstars = tstars)
}

one_rep <- function(n, beta_treat, err, B = 500, B_inner = 100, alpha = 0.05) {
  dat <- gen_data(n = n, beta_treat = beta_treat, err = err)
  
  base <- fit_theta(dat)
  theta_hat <- base$theta_hat
  se_hat <- base$se_hat
  
  # Wald
  t_wald <- system.time({
    ci1 <- ci_wald(theta_hat, se_hat, alpha = alpha)
  })[["elapsed"]]
  
  # Percentile bootstrap
  t_pct <- system.time({
    out2 <- ci_boot_percentile(dat, B = B, alpha = alpha)
    ci2 <- out2$ci
  })[["elapsed"]]
  
  # Bootstrap-t
  t_bt <- system.time({
    out3 <- ci_boot_t(dat, B = B, B_inner = B_inner, alpha = alpha)
    ci3 <- out3$ci
  })[["elapsed"]]
  
  data.frame(
    theta_hat = theta_hat,
    se_hat = se_hat,
    
    wald_lo = ci1[1], wald_hi = ci1[2],
    pct_lo  = ci2[1], pct_hi  = ci2[2],
    bt_lo   = ci3[1], bt_hi   = ci3[2],
    
    time_wald = t_wald,
    time_pct  = t_pct,
    time_bt   = t_bt
  )
}

run_scenario <- function(n, beta_treat, err, nSim, B = 500, B_inner = 100, alpha = 0.05) {
  sims <- do.call(rbind, lapply(seq_len(nSim), function(i) {
    one_rep(n = n, beta_treat = beta_treat, err = err, B = B, B_inner = B_inner, alpha = alpha)
  }))
  
  sims$true_beta <- beta_treat
  sims$cover_wald <- (sims$wald_lo <= beta_treat) & (beta_treat <= sims$wald_hi)
  sims$cover_pct  <- (sims$pct_lo  <= beta_treat) & (beta_treat <= sims$pct_hi)
  sims$cover_bt   <- (sims$bt_lo   <= beta_treat) & (beta_treat <= sims$bt_hi)
  
  sims
}
