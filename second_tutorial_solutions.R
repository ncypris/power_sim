if (!require("tidyverse")) install.packages("tidyverse")
if (!require("paramtest")) install.packages("paramtest")
if (!require("nlme")) install.packages("nlme")
library(tidyverse)
library(paramtest)
library(nlme)

# N = 500
# b0 = 0
# b1 = .2
# var_nation_rint = .4
# var_nation_pol_slope = .2
# varRes = .1

set.seed(161)
mlm_sim <- function(simNum, N, b0 = 0, b1, 
                    var_nation_rint = .4, var_nation_pol_slope = .2,
                    varRes = 1)
  
  
{
  subj <- c(1:N)
  nation <- rep(c("us", "uk", "es", "nor", "de"), each = N/5)
  pol <- rnorm(N,0,1)
  
  nation_rint <- rep(rnorm(1:5, 0, sqrt(var_nation_rint)), each = N/5)
  nation_pol_slope <- rep(rnorm(1:5, 0, sqrt(var_nation_pol_slope)), each = N/5)
  
  cred <- ((b0 + nation_rint) 
           + (b1*(pol + nation_pol_slope))
           + rnorm(N, 0, varRes))
  
  df <- tibble::tibble(subj, nation, pol, cred)
  
  return <- tryCatch({

    model <- nlme::lme(cred ~ pol, random= ~ 1 + pol|nation, data = df)
    
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


power_fessler_all2 <- grid_search(mlm_sim, n.iter = 1000, params=list(N=c(200, 300, 400, 500, 600, 700, 800)), output='data.frame', b1=.2, parallel='snow', ncpus=4)

results(power_fessler_all) %>%
  group_by(N.test) %>%
  summarise(
    na=sum(is.na(sig_p)),
    b_value = mean(b_value, na.rm = TRUE),
    power = mean(sig_p, na.rm = TRUE))

