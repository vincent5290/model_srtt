library(hBayesDM)
library(tidyverse)

load('xx.Rdata')

#run model
res <- stan(file = 'bart_par2.stan', data = data_model)

# true parameter
true_par <- data.frame(gam = res$parVals$gam %>% colMeans(),
                       tau = res$parVals$tau %>% colMeans())

## tentative simulated meeting devil
tent_sim_loss <- sapply(1:31, function(t) {
  sapply(1:200, function(i) {
    e <- sapply(0:31, function(j) rbinom(1, 1, 1/(8 - j)))
    return(which(e == 1)[1])
  })
}) %>% t

## predicted open boxes and meet devil
pred_open <- array(dim = c(31, 200))
pred_loss <- array(dim = c(31, 200))

## simulation with true parameter
for (i in 1:31) {
  n_succ <- 0
  n_pump <- 0
  gam <- true_par$gam[i]
  tau <- true_par$tau[i]
  # go througth trials
  for (j in 1:200) {
    
    # omega parameter
    omega = 8.0*(gam[j] / gam[j]+1);
    
    # generate open boxes without consider meeting devil
    sim_open <- sapply(1:8 , function(t) rbinom(1, 1, min(1,1/(1 + exp(tau * (t - omega))))))
    
    # number of open boxes before the first giveup
    openb <- if(0 %in% sim_open) {which(sim_open == 0)[1] - 1} else {8}
    # check if the meeting devil early
    loss <- ifelse(openb >= tent_sim_loss[i, j], 1, 0)
    openb <- ifelse(openb >= tent_sim_loss[i, j],tent_sim_loss[i, j], openb)
    # update the open boxes matrix
    pred_open[i, j] <- openb
    # update the loss matrix
    pred_loss[i, j] <- loss
    n_succ <- n_succ + openb - loss
    n_open <- n_open + openb
  }
}

# reshape the predicted data
pred_open <-pred_pump %>% as.data.frame() %>% `rownames<-`(HC32$subjID %>% unique) %>% 
  rownames_to_column('SubjID') %>% pivot_longer(!SubjID, values_to = 'open') %>%select(-name)
pred_loss <-pred_loss %>% as.data.frame() %>% `rownames<-`(HC32$subjID %>% unique) %>% 
  rownames_to_column('SubjID') %>% pivot_longer(!SubjID, values_to = 'loss') %>%select(-name)
pred_HC32 <- cbind(pred_open, explosion = pred_loss$loss)

## run model with predicted data
pred.res <- stan(file = 'bart_par2.stan', data = pred_HC32)

# predicted parameter
pred_par <- data.frame(gam = pred.res$parVals$gam %>% colMeans(),
                       tau = pred.res$parVals$tau %>% colMeans())

#plot
pred_par <- pred_par %>% `rownames<-`(HC32$subjID %>% unique) %>% rownames_to_column('SubjID') %>% 
  pivot_longer(!SubjID, names_to = 'parameter', values_to = 'predict') 
true_par %>% `rownames<-`(HC32$subjID %>% unique) %>% rownames_to_column('SubjID') %>% 
  pivot_longer(!SubjID, names_to = 'parameter', values_to = 'true') %>% left_join(pred_par) %>%
  ggplot(aes(predict, true)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  geom_abline(slope = 1, intercept = 0, color = 'red') + 
  facet_wrap( ~ parameter, scales = 'free') +
  theme_bw()
