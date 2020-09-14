if (!require("tidyverse")) install.packages("tidyverse")
if (!require("paramtest")) install.packages("paramtest")
library(tidyverse)
library(paramtest)

set.seed(161)
lm_sim <- function(simNum, N, b1,b0=0)
{
  
  con <- rnorm(N,0,1)
  rerror <- sqrt(1-b1^2)
  y = b1*con + 0 + rnorm(N,0,rerror)
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

power_lm <- grid_search(lm_sim, params=list(N = c(50, 100, 150), b1 = c(.1, .2, .3)),
                        n.iter = 1000, output ='data.frame', parallel = 'snow', ncpus = 4)

results(power_lm) %>%
  group_by(N.test, b1.test) %>%
  summarise(
    beta_con = mean(est1_x1),
    power = mean(sig1_x1)
  )

# 2. Add another predictor to the equation. Adjust output (e.g. est1_x2 etc) and results table.



# 3. Compare model1 with one predictor (b1 = .05) and model2 with two predictors (b1 = .05, b2 = .2).


# 4. Simulate Data for a t-test (a 2 x 2 ANOVA).
anova(model1, model2)
