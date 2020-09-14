if (!require("tidyverse")) install.packages("tidyverse")
if (!require("paramtest")) install.packages("paramtest")
library(tidyverse)
library(paramtest)

set.seed(161)
lm_sim <- function(simNum, N, b1, b0=0)
{
  
  con <- rnorm(N, 0, 1)
  rerror <- sqrt(1 - b1^2)
  y = b1*con + rnorm(N, 0, rerror)
  model1 = lm(y ~ con)
  
  
  # model 1
  est1_x1 <- coef(summary(model1))['con', 'Estimate']
  p1_x1 <- coef(summary(model1))['con', 'Pr(>|t|)']
  sig1_x1 <- p1_x1 < .05
  
  return(c(est1_x1 = est1_x1, p1_x1 = p1_x1, sig1_x1 = sig1_x1))
  
}


power_lm <- grid_search(lm_sim, params=list(N = c(150, 200, 250)),
                        n.iter = 1000, output ='data.frame', b1 = .2, parallel = 'snow', ncpus = 4)

results(power_lm) %>%
  group_by(N.test) %>%
  summarise(
    beta_con = mean(est1_x1),
    power = mean(sig1_x1)
  )


### EXERCISES ##

# 1. Vary the effect size of the b1-coefficient (.1, .2, .3) and the number of participants (50, 100, 150).

power_lm1 <- grid_search(lm_sim, params=list(N = c(50, 100, 150), b1 = c(.1, .2, .3)),
                        n.iter = 1000, output ='data.frame', parallel = 'snow', ncpus = 4)

results(power_lm1) %>%
  group_by(N.test, b1.test) %>%
  summarise(
    beta_con = mean(est1_x1),
    power = mean(sig1_x1)
  )

# 2. Add another predictor to the equation. Adjust output (e.g. est1_x2 etc) and results table.

lm_sim2 <- function(simNum, N, b1, b2, bx, b0=0)
{
  
  con <- rnorm(N, 0, 1)
  pol <- rnorm(N, 0, 1) # adding political orientation as a predictor
  rerror <- sqrt(1 - b1^2 - b2^2)
  y = b1*con + b2*pol + bx * con * pol + rnorm(N, 0, rerror)
  model1 = lm(y ~ con * pol)

  # model 1
  
  # get conscientiousness
  est1_x1 <- coef(summary(model1))['con', 'Estimate']
  p1_x1 <- coef(summary(model1))['con', 'Pr(>|t|)']
  sig1_x1 <- p1_x1 < .05
  
  # get political orientation
  est1_x2 <- coef(summary(model1))['pol', 'Estimate']
  p1_x2 <- coef(summary(model1))['pol', 'Pr(>|t|)']
  sig1_x2 <- p1_x2 < .05
  
  # get interaction
  est1_xx <- coef(summary(model1))['con:pol', 'Estimate']
  p1_xx <- coef(summary(model1))['con:pol', 'Pr(>|t|)']
  sig1_xx <- p1_xx < .05
  
  return(c(est1_x1 = est1_x1, p1_x1 = p1_x1, sig1_x1 = sig1_x1,
           est1_x2 = est1_x2, p1_x2 = p1_x2, sig1_x2 = sig1_x2,
           est1_xx = est1_xx, p1_xx = p1_xx, sig1_xx = sig1_xx))
  
}


power_lm2 <- grid_search(lm_sim2, params=list(N = c(150, 200, 250)),
                        n.iter = 1000, output ='data.frame', b1 = .2, b2 = .1, bx = .2, parallel = 'snow', ncpus = 4)

results(power_lm2) %>%
  group_by(N.test) %>%
  summarise(
    beta_con = mean(est1_x1),
    power_con = mean(sig1_x1),
    beta_pol = mean(est1_x2),
    power_pol = mean(sig1_x2),
    beta_x = mean(est1_xx),
    power_x = mean(sig1_xx)
  )

# 3. Compare model1 with one predictor (b1 = .05) and model2 with two predictors (b1 = .05, b2 = .2).

comp_sim <- function(simNum, N, b1, b2, bx, b0=0)
{
  
  con <- rnorm(N, 0, 1)
  pol <- rnorm(N, 0, 1)
  rerror <- sqrt(1 - b1^2 - b2^2)
  y = b1*con + b2*pol + bx * con * pol + rnorm(N, 0, rerror)
  model1 = lm(y ~ con)
  model2 = lm(y ~ con * pol)
  
  # model comparison
  comp_p <- anova(model1, model2)[2, "Pr(>F)"]
  comp_sig <- comp_p < .05
  
  return(c(comp_sig = comp_sig))
  
}


power_comp <- grid_search(comp_sim, params=list(N = c(150, 200, 250)),
                         n.iter = 1000, output ='data.frame', b1 = .05, b2 = .2, bx = .05, parallel = 'snow', ncpus = 4)

results(power_comp) %>%
  group_by(N.test) %>%
  summarise(
    comp_power = mean(comp_sig)
  )

# 4. Simulate Data for a t-test.

t_sim <- function(simNum, N, d) {
  x1 <- rnorm(N, 0, 1)
  x2 <- rnorm(N, d, 1)
  
  t <- t.test(x1, x2, var.equal=TRUE)
  t_stat <- t$statistic
  p <- t$p.value
  sig <- p < .05
  
  return(c(t_stat = t_stat, p = p, sig = sig))
  
}

power_t <- grid_search(t_sim, params=list(N = c(150, 200, 250)), n.iter = 1000, output='data.frame', d = .3)

results(power_t) %>%
  group_by(N.test) %>%
  summarise(power = mean(sig))


