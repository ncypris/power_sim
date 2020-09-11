library(paramtest)
library(tidyverse)

# N = 500
# b0 = 0
# b1 = .2
# var_nation_rint = .4
# var_nation_pol_slope = .2
# varRes = .1

set.seed(161)
mlm_sim <- function(simNum, N, b0 = 0, b1, 
                    var_nation_rint = .5, var_nation_pol_slope = .25,
                    varRes = 1)
  
  
{
  subj <- # create a subject variable (1,2,...N)
  nation <- # create a nation variable ("US","UK","ES","NOR","DE") rep()
  pol <- #create a political orientation variable rnorm()
  
  nation_rint <- # create a random intercept per nation
  nation_pol_slope <- # create a random slope per nation
  
  cred <- #intercept + random intercept + ( beta coefficient * random slope) + random error
  
  df <- tibble::tibble(subj, nation, pol, cred)
  
  return <- tryCatch({

    model <- nlme::lme(cred ~ pol, random= ~ 0 + pol|nation, data = df)
    
    b_value <- summary(model)$tTable[[2,1]]
    p_value <- summary(model)$tTable[[2,5]]
    sig_p <- p_value < .05
    return(c(b_value = b_value, p_value = p_value, sig_p = sig_p))
    
  },
  
  error=function(e) {
    return(c(b_value = NA, p_value = NA, sig_p = NA))
  })
  
  return(return)
}


power_fessler_all <- grid_search(mlm_sim, n.iter = 1000, params=list(N=c(600, 700, 800, 900)), output='data.frame', b1=.2, parallel='snow', ncpus=4)

results(power_fessler_all) %>%
  group_by(N.test) %>%
  summarise(
    na=sum(is.na(sig_p)),
    b_value = mean(b_value, na.rm = TRUE),
    power = mean(sig_p, na.rm = TRUE))

