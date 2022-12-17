#---------------------------------#
# simulate survival curve data ####
#---------------------------------#

# trying to simulate survival curve data

# load packages
library(coxed)
library(tidyverse)
library(survival)
library(survminer)
library(coxme)
library(rstanarm)

# following this vignette: https://cran.r-project.org/web/packages/coxed/vignettes/simulating_survival_data.html#introduction

# setup dataset

#------------------------------------#
# simple single covariate example ####
#------------------------------------#

# create a dataset
# bacteria that have been evolved with or without listening to Classic FM
# here we have 200 Galleria injected with bacteria either grown with it without listening to classic FM
d_covar <- tibble(classic_fm = rep(c(1,0), times = 10))

# simulate survival data ####
test <- sim.survdata(N = 20, T = 10, knots = 4, X = d_covar, beta = 0.4, censor = 0.1, censor.cond=TRUE, num.data.frames = 20)

# grab data from each dataset
grab_data <- function(x){
  return(x$data)
}

# extract data
d_test <- purrr::map_df(test, grab_data, .id = 'id')

head(test$data)
test$betas

# convert y into time in hours - six sampling periods 6 hours apart
d_test <- mutate(d_test, time_to_death = y * 6,
                 failed = failed[,],
                 evolution = ifelse(classic_fm == 1, 'classicFM', 'silence'),
                 status = ifelse(failed == TRUE, 1, 0)) %>%
  group_by(id, evolution) %>%
  mutate(clone = cur_group_id()) %>%
  ungroup()
head(d_test)

mutate(d_test, tot_n = n()/2) %>%
  filter(., status == 1) %>%
  group_by(evolution) %>%
  summarise(prop_dead = n()/unique(tot_n),
            ave_death_time = median(time_to_death),
            .groups = 'drop')

# model assuming all points are independent - i.e. each point is a different bacteria
model1 <- coxph(Surv(time_to_death, status) ~ evolution, data=d_test)
model2 <- coxph(Surv(time_to_death, status) ~ 1, data=d_test)
anova(model1, model2)

model_preds <- survfit(Surv(time_to_death, status)~evolution, data=d_test)
summary(model_preds)

# plot fit
ggsurvplot(model_preds,
           conf.int=TRUE,
           pval=TRUE)

# however, this is not what is routinely done
model1_me <- coxme(Surv(time_to_death, status) ~ evolution + (1|clone), d_test)
model2_me <- coxme(Surv(time_to_death, status) ~ 1 + (1|clone), d_test)
anova(model1_me, model2_me)

names(d_test)
d_test <- data.frame(d_test)
d_test <- mutate(d_test, status2 = status) %>%
  select(-status) %>%
  rename(status = status2)

# fit this with a bayesian model
mod_bayes1 <- stan_surv(Surv(time_to_death, status) ~ evolution,
                        data = d_test,
                        chains = 3,
                        cores = 3,
                        seed = 42,
                        iter = 3000)

mod_bayes2 <- stan_surv(Surv(time_to_death, status) ~ evolution + (1|clone),
                        data = d_test,
                        chains = 3,
                        cores = 3,
                        seed = 42,
                        iter = 3000)
mod_bayes1
mod_bayes2

# get a list of the variables in the model
tidybayes::get_variables(mod_bayes2)

to_plot <- tidybayes::get_variables(mod_bayes2)[1:2]

# check key mcmc trace plots
bayesplot::mcmc_trace(mod_bayes2, pars = to_plot)
# fuzzy caterpillars


# calculate the hazard ratio
# the exp() of the difference between two factors is the hazards ratio
params_bayes <- tidybayes::spread_draws(mod_bayes2, !!!syms(c(to_plot))) %>%
  janitor::clean_names() %>%
  mutate(hazard_ratio = exp(evolutionsilence)) %>%
  select(., hazard_ratio) %>%
  tidybayes::median_qi()
params_bayes
# hazard ratio crosses one so no significant difference

# predict survival curves at the treatment level ####
d_preds <- select(d_test, evolution, clone) %>%
  distinct() %>%
  mutate(id = 1:n(),
         id2 = group_indices(., evolution),
         evolution2 = evolution) %>%
  nest_legacy(-c(id2, evolution2)) %>%
  mutate(., preds = map(data, ~posterior_survfit(mod_bayes2, newdata = .x, times = 0, standardise = TRUE, extrapolate = TRUE, dynamic = TRUE)))

d_preds <- unnest(d_preds, preds) %>%
  select(-data) %>%
  rename(evolution = evolution2)

# plot
ggplot(d_preds, aes(time, median, fill = evolution2)) +
  geom_line(aes(col = evolution), show.legend = FALSE) +
  geom_ribbon(aes(time, ymin = ci_lb, ymax = ci_ub), alpha = 0.2) +
  theme_bw() +
  ylim(c(0,1))

# predict including random effects
d_preds_random <-  select(d_test, evolution, clone) %>%
  distinct() %>%
  mutate(id = 1:n())
d_preds_random <- posterior_survfit(mod_bayes2, newdata = d_preds_random, times = 0, standardise = FALSE, extrapolate = TRUE, dynamic = TRUE) %>%
  left_join(., d_preds_random)

# plot all clones
ggplot() +
  geom_line(aes(time, median, col = evolution, group = interaction(clone, evolution)), d_preds_random) +
  geom_ribbon(aes(time, ymin = ci_lb, ymax = ci_ub, fill = evolution), d_preds_random, alpha = 0.2) +
  theme_bw() +
  ylim(c(0,1)) +
  facet_wrap(~clone, ncol = 6)

# plot clones over treatment effect
ggplot(d_preds, aes(time, median, fill = evolution)) +
  geom_line(aes(col = evolution), show.legend = FALSE, size = 2) +
  geom_ribbon(aes(time, ymin = ci_lb, ymax = ci_ub), alpha = 0.2) +
  geom_line(aes(col = evolution, group = interaction(clone, evolution)), d_preds_random, alpha = 0.5) +
  theme_bw() +
  ylim(c(0,1))
