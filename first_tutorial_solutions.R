library(tidyverse)
library(paramtest)

set.seed(161)
lm_sim <- function(simNum, N, b1,b0=0)
{
  
  con <- rnorm(N,0,1)
  rerror <- sqrt(1-b1^2)
  y = b1*pol + 0 + rnorm(N,0,rerror)
  model1 = lm(y ~ pol)
  
  
  # model 1
  est1_x1 <- coef(summary(model1))['pol', 'Estimate']
  p1_x1 <- coef(summary(model1))['pol', 'Pr(>|t|)']
  sig1_x1 <- p1_x1 < .05
  
  return(c(est1_x1 = est1_x1, p1_x1 = p1_x1, sig1_x1 = sig1_x1))
  
}


power_lm <- grid_search(lm_sim, params=list(N = c(50, 100, 150, 200, 250), b1 = c(.1, .2, .3)),
                         n.iter = 1000, output ='data.frame', parallel = 'snow', ncpus = 4)

results(power_lm) %>%
  group_by(N.test, b1.test) %>%
  summarise(
    beta_pol = mean(est1_x1),
    power = mean(sig1_x1)
  )


### EXERCISES ##

# 1. Vary the effect size of the b1-coefficient (.1, .2, .3) and the number of participants (50, 100, 150).

# 2. Add another predictor to the equation. Adjust output (e.g. est1_x2 etc) and results table.
country.origin <- rnorm(N,0,1)
rerror <- sqrt(1-b1^2)
y = b1*country.origin + 0 + rnorm(N,0,rerror)
model2 = lm(y ~ country.origin)

est1_x2 <- coef(summary(model2))['country.origin', 'Estimate']
p1_x2 <- coef(summary(model2))['country.origin', 'Pr(>|t|)']
sig1_x2 <- p1_x2 < .05

power_lm <- grid_search(lm_sim, params=list(N = c(50, 100, 150, 200, 250), b1 = c(.1, .2, .3)),
                        n.iter = 1000, output ='data.frame', parallel = 'snow', ncpus = 4)

lm_sim2 <- function(simNum, N, b1,b0=0)
{
  
  country.origin <- rnorm(N,0,1)
  rerror <- sqrt(1-b1^2)
  y = b1*country.origin + rnorm(N,0,rerror)
  model2 = lm(y ~ country.origin)
  
  est1_x2 <- coef(summary(model2))['country.origin', 'Estimate']
  p1_x2 <- coef(summary(model2))['country.origin', 'Pr(>|t|)']
  sig1_x2 <- p1_x2 < .05
  
  return(c(est1_x2 = est1_x2, p1_x2 = p1_x2, sig1_x2 = sig1_x2))
  
}

power_lm2 <- grid_search(lm_sim2, params=list(N = c(50, 100, 150, 200, 250), b1 = c(.1, .2, .3)),
                        n.iter = 1000, output ='data.frame', parallel = 'snow', ncpus = 4)

results(power_lm2) %>%
  group_by(N.test, b1.test) %>%
  summarise(
    beta_country.origin = mean(est1_x2),
    power = mean(sig1_x2)
)
# 3. Compare model1 with one predictor (b1 = .05) and model2 with two predictors (b1 = .05, b2 = .2).


# 4. Simulate Data for a t-test (a 2 x 2 ANOVA).
anova(model1, model2)
