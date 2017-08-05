# === BOOTSTRAP A/B TESTING CODE ============================================ # 
# This is an example of how to analyze A/B tests via 
# bootstrap (useful for non-mean test statistics, eg. percentiles, which are common for 
# performance metrics)
# 
# Null hypothesis: No difference between group A's test-statistic and group B's test-statistic.
# P-value: Probability seeing observed difference between test-statistics given Null hypothesis is true.

# =========================================================================== #

library(boot)
library(dplyr)

# Set up groups
control <- data %>% filter( experiment_group == 'control')
treatment <- data %>% filter(experiment_group == 'treatment')



# Group split. This is hand-inputted split that we expect from control and treatment.
control.size = 0.5
treatment.size = 0.5

# Check against group split -- generally alright if actual split is within 
# 3 percentage points (eg. actual is 52% instead of 50%):

control.actual_size = nrow(control)/(nrow(control) + nrow(treatment))
if(abs(control.actual_size - control.size) < 0.03){match_pass = "PASS"}else{match_pass = "FAIL"}
print(sprintf("Control group size is %0.3f compared to ideal %0.3f -- %s", control.actual_size, control.size, match_pass))


# Assuming checks passed, now we construct hypothetical H0 scenario using bootstrapping. What if treatment = control?
# Let's say test statistic is P90 here

test_statistic_func <- function(x, d) {
  return(quantile(x[d], 0.9))
}

# Stratification
pseudo_treatment = sample(control, round(treatment.size/control.size), replace = T)


set.seed(123)
control.boot <- boot(data = control$value, statistic = test_statistic_func,R = 1000)
set.seed(090)
pseudo_treatment.boot <- boot(data = pseudo_treatment$value, statistic = test_statistic_func,R = 1000)
h0_results <- data.frame(a1 = control.boot$t, a2 = pseudo_treatment.boot$t) %>% mutate(difference = a2 - a1)

# See where the observed difference falls on the H0 distribution of differences
obs_difference = quantile(treatment$value, 0.9) - quantile(control$value, 0.9) 
obs_position = ecdf(h0_results $difference)(obs_difference)

# Probability of seeing value
if(obs_difference > 0){
    p_value = 1 - obs_position
}else{
    p_value = obs_position
}

# Check against alpha
alpha = 0.05
if(p_value < 0.05 ){
    print("Null hypothesis rejected")
}else{
    print("Cannot reject null hypothesis")
}


