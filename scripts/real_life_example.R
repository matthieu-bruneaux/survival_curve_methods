#--------------------------------#
# example from real-life data ####
#--------------------------------#

# load in packages
library(survival)
library(survminer)
library(coxme)
library(tidyverse)
library(rstanarm)
library(tidybayes)

# clone = bacterial clone
# time_of_death = time sampled
# treat1 = "lime" or "no lime"
# treat2 = "static" or "shaken"
# status = dead (1) or alive (0)
# treatment

# set up data
d <- read.csv('data/real_life_data.csv')

# model assuming all points are independent - i.e. each point is a different bacteria ####

# do coxph regression and model simplification
model <- coxph(Surv(time_of_death, status) ~ treat1*treat2, data=d)
model2 <- coxph(Surv(time_of_death, status) ~ treat1 + treat2, data=d)
model3 <- coxph(Surv(time_of_death, status) ~ treat2, data=d)
model4 <- coxph(Surv(time_of_death, status) ~ treat1, data=d)
model5 <- coxph(Surv(time_of_death, status) ~ 1, data=d)
anova(model, model2)
anova(model2, model3)
anova(model2, model4)
anova(model3, model5)
anova(model4, model5)
model3

model_preds <- survfit(Surv(time_of_death, status)~treat2, data=d)
summary(model_preds)

ggsurvplot(model2, data = d, legend = "right", legend.title = "Treatment", ggtheme = theme_bw(), conf.int = TRUE, linetype = c(1), palette = "Paired2", censor = FALSE, legend.labs = c('shaken', 'static'), xlab = "Time (Hours)", ylab = "Proportion Alive", font.y = c(15), font.x = c(15), font.tickslab = c(15), conf.int.alpha = 0.15)
ggforest(model3, data = d, main = "Treatment impacts on Pseudomonas virulence", fontsize = 1)

# do a mixed effects model of this ####

# originally using a standard cox model
model_me <- coxme(Surv(time_of_death, status)~treat2 + (1|clone), d)
model_me
model_me2 <- coxme(Surv(time_of_death, status)~1 + (1|clone), d)
anova(model_me, model_me2)
# predict does not really work - want to get the random effects out, a prediction for each clone

# do a bayesian analysis of this
mod_bayes <- stan_surv(Surv(time_of_death, status) ~ treat2 + (1|clone),
                        data = d,
                        chains = 3,
                        cores = 3,
                        seed = 42,
                        iter = 3000)
mod_bayes

# get a list of the variables in the model
tidybayes::get_variables(mod_bayes)

to_plot <- tidybayes::get_variables(mod_bayes)[1:2]

# check key mcmc trace plots
bayesplot::mcmc_trace(mod_bayes, pars = to_plot)
# fuzzy caterpillars

# calculate the hazard ratio
# the exp() of the difference between two factors is the hazards ratio
params_bayes <- spread_draws(mod_bayes, !!!syms(c(to_plot))) %>%
  janitor::clean_names() %>%
  mutate(hazard_ratio = exp(treat2static)) %>%
  select(., hazard_ratio) %>%
  median_qi()
params_bayes
# hazard ratio crosses one so no significant difference

# predict survival curves at the treatment level ####
d_preds <- select(d, treat2, clone) %>%
  distinct() %>%
  mutate(id = 1:n(),
         id2 = group_indices(., treat2),
         treat = treat2) %>%
  nest_legacy(-c(id2, treat)) %>%
  mutate(., preds = map(data, ~posterior_survfit(mod_bayes, newdata = .x, times = 0, standardise = TRUE, extrapolate = TRUE, dynamic = TRUE)))

d_preds <- unnest(d_preds, preds) %>%
  select(-data, treat2 = treat)

# plot
ggplot(d_preds, aes(time, median, fill = treat2)) +
  geom_line(aes(col = treat2), show.legend = FALSE) +
  geom_ribbon(aes(time, ymin = ci_lb, ymax = ci_ub), alpha = 0.2) +
  theme_bw() +
  ylim(c(0,1))

# predict including random effects
d_preds_random <-  select(d, treat2, clone) %>%
  distinct() %>%
  mutate(id = 1:n())
d_preds_random <- posterior_survfit(mod_bayes, newdata = d_preds_random, times = 0, standardise = FALSE, extrapolate = TRUE, dynamic = TRUE) %>%
  left_join(., d_preds_random)

# plot all clones
ggplot() +
  geom_line(aes(time, median, col = treat2, group = interaction(clone, treat2)), d_preds_random) +
  geom_ribbon(aes(time, ymin = ci_lb, ymax = ci_ub, fill = treat2), d_preds_random, alpha = 0.2) +
  theme_bw() +
  ylim(c(0,1)) +
  facet_wrap(~clone, ncol = 6)

# plot clones over treatment effect
ggplot(d_preds, aes(time, median, fill = treat2)) +
  geom_line(aes(col = treat2), show.legend = FALSE, size = 2) +
  geom_ribbon(aes(time, ymin = ci_lb, ymax = ci_ub), alpha = 0.2) +
  geom_line(aes(col = treat2, group = interaction(clone, treat2)), d_preds_random, alpha = 0.5) +
  theme_bw() +
  ylim(c(0,1))
