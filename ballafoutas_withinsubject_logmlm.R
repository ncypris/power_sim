log_sim <- function(simNum,N,t,rho){
  library(simstudy)
  library(lme4)
  library(lmerTest)
  bin_data <- as.data.frame(
    genCorGen(
      n = N,
      nvars = t,
      params1 = c(.5,.3,.1),
      dist = "binary",
      rho = rho,
      corstr = "cs" #see below.
    ))
  
  bin_data$period <- as.factor(bin_data$period)
  mod <- glmer(X~period+(1|id), data = bin_data,
               family = binomial(link = "logit"))
  summary(mod)
  p_val1 <- summary(mod)$coefficients[2,4] #Extract p-value for 3rd factor from outcome.
  p_val2 <- summary(mod)$coefficients[3,4]
  
  sig1 <- p_val1 < .05 #Returns TRUE if p-value < alpha.
  sig2 <- p_val2 < .05 #Returns TRUE if p-value < alpha.
  return(c(p_val1 = p_val1, sig1 = sig1,
           p_val2 = p_val2, sig2 = sig2))
}

set.seed(1234)
power_log <- grid_search(log_sim,
                         params = list(N = 134,
                                       t = 3,
                                       rho = c(.5,.7,.9)),
                         n.iter = 1000,
                         output = "data.frame",
                         parallel = "snow",
                         ncpus = 8,
                         beep = 1)

results(power_log) %>%
  group_by(rho.test) %>%
  summarise(
    power1 = mean(sig1),
    power2 = mean(sig2)
  )

power_log2 <- grid_search(log_sim,
                         params = list(N = c(80,100,120,134),
                                       t = 3,
                                       rho = .5),
                         n.iter = 1000,
                         output = "data.frame",
                         parallel = "snow",
                         ncpus = 8,
                         beep = 1)

results(power_log2) %>%
  group_by(N.test) %>%
  summarise(
    power1 = mean(sig1),
    power2 = mean(sig2)
  )
