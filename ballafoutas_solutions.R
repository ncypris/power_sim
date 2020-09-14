#Packages.
library(tidyverse)
library(paramtest)


#Observed frequencies of Third-Party Punishment in Experimental Conditions (Balafoutas et al., 2014).
data <- matrix(c(20,20,36,4), nrow = 2, 
               dimnames = list(c("No Punish","Punish"),
                       c("No Counterpunishment","Counterpunishment")))
fisher.test(data)
# Balafoutas et al's main result: The decision to punish significantly depend on whether or not
# there is an option for counterpunishing.

#Power Simulation based on reported effect size (i.e., difference in frequencies).
#Parameters based on Balafoutas et al. 2014.
# N = 80 (40 per cell)
# prob1 = 20/40 = .5
# prob2 = 4/40 = .1


fisher_sim <- function(simNum,N,prob1,prob2){
  
cond <- rep(0:1, N/2) #Experimental Condition. 0 = No counterpunishment, 1 = Counterpunishment.
df <- as.data.frame(cond) #Create data set.
df$punish <- ifelse(cond == 0, rbinom(N/2,1,prob = prob1),
                    rbinom(N/2,1,prob = prob2)) #Conditional binomial distribution based on Condition.
tab <- table(df$cond, df$punish) #Create contingency table.

test <- fisher.test(tab) #Run Fisher's exact test 
#Consider to change to Chi-squared with bigger samples)
# test <- chisq.test(tab)

p <- test$p.value #Extract p-value from test output.
sig <- p < .05 #Returns TRUE if p-value < alpha.
return(c(p = p, sig = sig))
}

#Detect number of cores that your Computer can use for performing parallel iterations.
library(parallel)
detectCores()

#Simulation with different Sample Size.
set.seed(1234) #Set seed to reproduce same results.
power_log <- grid_search(fisher_sim,
                         params = list(N = 80,
                                       prob1 = .5,
                                       prob2= .1),
                         n.iter = 1000,
                         output = "data.frame",
                         parallel = "snow",
                         ncpus = 8)
                         

results(power_log) %>%
  group_by(N.test) %>%
  summarise(
    power = mean(sig)
  )

#Based on our simulation, Balafoutas et al.´s study had a power of 92%.

#################################################################################
### EXERCISES ##

# We are interested in adding a condition to Ballafoutas experimental setup.
# We expect that in this new condition we will observe a lower frequency of punishment
# than in "0 - no counterpunishment", but greater than in "1 - counterpunishment".

# Thus, one first step is to check whether the sample size used by Balafoutas et al. (N = 80)
# would allow us to capture smaller differences between conditions with sufficient statistical power.

#### 1. ####
# Use the "log_sim" function we created to estimate the smallest difference we can capture
# with 90% power, given N = 80.

set.seed(123) #Set seed to reproduce same results.
ex1_power_log <- grid_search(fisher_sim,
                         params = list(N = 80,
                                       prob1 = .5,
                                       prob2 = seq(.15,.45,.05)),
                         n.iter = 1000,
                         output = "data.frame",
                         parallel = "snow",
                         ncpus = 8)

ex1 <- results(ex1_power_log) %>%
  group_by(prob2.test) %>%
  summarise(
    power = mean(sig)
  )

#In case you want to plot your results:
ggplot(ex1, aes(x = 0.5-prob2.test, y = power))+
  geom_point()+
  geom_line()

#### 2. ####
# Consider that we want to be able to capture differences of minimum .20,
# with at least 90% power. How much larger should our sample size be?

# CLUE 1: In this case, it will be more than a few, so think big!
# CLUE 2: It would be recommendable to first change Fisher's exact test
# for Chi-squared in the simulation function, since sample sizes will get big!

chi_sim <- function(simNum,N,prob1,prob2){
  
  cond <- rep(0:1, N/2) #Experimental Condition. 0 = No counterpunishment, 1 = Counterpunishment.
  df <- as.data.frame(cond) #Create data set.
  df$punish <- ifelse(cond == 0, rbinom(N/2,1,prob = prob1),
                      rbinom(N/2,1,prob = prob2)) #Conditional binomial distribution based on Condition.
  tab <- table(df$cond, df$punish) #Create contingency table.
  
  test <- chisq.test(tab)
  
  p <- test$p.value #Extract p-value from test output.
  sig <- p < .05 #Returns TRUE if p-value < alpha.
  return(c(p = p, sig = sig))
}

set.seed(1234) #Set seed to reproduce same results.
ex2_power_log <- grid_search(chi_sim,
                             params = list(N = seq(80,400,20),
                                           prob1 = .5,
                                           prob2 = .3),
                             n.iter = 1000,
                             output = "data.frame",
                             parallel = "snow",
                             ncpus = 8)

ex2 <- results(ex2_power_log) %>%
  group_by(N.test) %>%
  summarise(
    power = mean(sig)
  )

#In case you want to plot your results:
ggplot(ex2, aes(x = N.test, y = power))+
  geom_point()+
  geom_line()
