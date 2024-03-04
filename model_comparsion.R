library(loo)
log_lik_1=extract_log_lik(par3, merge_chains = F)
r_eff_1=relative_eff(log_lik_1)
loo_1 <- loo(log_lik_1, r_eff=r_eff_1)
log_lik_2=extract_log_lik(par2, merge_chains = F)
r_eff_2=relative_eff(log_lik_2)
loo_2 <- loo(log_lik_2, r_eff=r_eff_2)
loo_2
