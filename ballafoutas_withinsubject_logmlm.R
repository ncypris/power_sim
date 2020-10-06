library(tidyverse)
library(simstudy)
library(lme4)
library(lmerTest)
library(paramtest)

bin_data <- as.data.frame(genCorGen(
  n = 134,
  nvars = 6,
  params1 = c(.35,.3,.25,.15,.10,.05),
  dist = "binary",
  rho = .5,
  corstr = "cs" #see below.
))

log_sim <- function(simNum,N,t,rho){
  
  bin_data <- as.data.frame(
    simstudy::genCorGen(
      n = N,
      nvars = t,
      params1 = c(.35,.3,.25,.15,.10,.05),
      dist = "binary",
      rho = rho,
      corstr = "cs" #see below.
    ))
  
  first_mean <- mean(bin_data[bin_data$period == 0,]$X) # sanity check: did assigning mean work
  last_mean <- mean(bin_data[bin_data$period == 5,]$X) # sanity check: did assigning mean work
  
  bin_data$effic <- rep(c("low", "med", "high"), times = N*2) # because we have two conditions, CPUN & APUN, per participant with low, medium, and high efficiency
  
  # bin_data$effic <- ifelse(bin_data$period == 0 | bin_data$period == 3, "Low",
  #                         ifelse(bin_data$period == 1 | bin_data$period == 4, "Med",
  #                                "High"))
  bin_data$cond <- rep(c("APUN", "CPUN"), each = 3, times = N)
  
  # bin_data$cond <- ifelse(bin_data$period == 0 | bin_data$period == 1 | bin_data$period == 2, "APUN",
  #                         "CPUN")
  mod <- lme4::glmer(X~cond+(1|id)+(1|effic), data = bin_data,
               family = binomial(link = "logit"))
  
  # summary(mod)
  
  p_val1 <- summary(mod)$coefficients[2,4] #Extract p-value for 3rd factor from outcome.
 
  sig1 <- p_val1 < .05 #Returns TRUE if p-value < alpha.

  return(c(p_val1 = p_val1, sig1 = sig1, first_mean = first_mean, last_mean = last_mean))
}

set.seed(1234)
power_log <- grid_search(log_sim,
                         params = list(N = 134,
                                       t = 6,
                                       rho = .5),
                         n.iter = 1000,
                         output = "data.frame",
                         parallel = "snow",
                         ncpus = 8,
                         beep = 1)

results(power_log) %>%
  # group_by(rho.test) %>%
  summarise(
    power1 = mean(sig1),
    first_mean = mean(first_mean),
    last_mean = mean(last_mean)
  )

ggplot(power_log$results, aes(x = first_mean))+geom_histogram(binwidth = .01) #distribution of mean of first variable
ggplot(power_log$results, aes(x = last_mean))+geom_histogram(binwidth = .01) #distribution of mean of last variable

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
