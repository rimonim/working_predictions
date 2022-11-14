library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(flexplot)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

d1 <- read.csv("Exp1.csv")
d1$err <- 1 - d1$correct
d1 <- d1 %>% filter(count_TrialProc != "NA") # remove practice

d1 %>% group_by(jatosStudyResultId) %>% summarize(mean(correct))

d1$condition[d1$cue == "A" & d1$target == "X"] <- "AX"
d1$condition[d1$cue == "A" & d1$target == "Y"] <- "AY"
d1$condition[d1$cue == "B" & d1$target == "X"] <- "BX"
d1$condition[d1$cue == "B" & d1$target == "Y"] <- "BY"

d1$p_global[d1$target == "X"] <- .68
d1$p_global[d1$target == "Y"] <- .32

d1$p_conditional[d1$condition == "AX"] <- .8
d1$p_conditional[d1$condition == "AY"] <- .2
d1$p_conditional[d1$condition == "BX"] <- .2
d1$p_conditional[d1$condition == "BY"] <- .8

d1 <- d1 %>% 
  # remove funny cases
  mutate(prevCorrect = lag(correct)) %>% 
  filter(correct == 1) %>% 
  filter(prevCorrect ==1) %>% 
  filter(response_time > 100, response_time < 5000) %>% 
  group_by(jatosStudyResultId, condition) %>% 
  filter(abs(scale(response_time)) < 2) %>%
  # user-friendliness
  rename(ID = jatosStudyResultId,
         probe = target,
         RT = response_time) %>%
  select(ID, condition, cue, probe, p_global, p_conditional, correct, RT, correct) %>%
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Experiment 1.2 Data Cleanup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

d2 <- read.csv("Exp2.csv")
d2$err <- 1 - d2$correct
d2 <- d2 %>% filter(count_TrialProc != "NA") # remove practice

d2 %>% group_by(jatosStudyResultId) %>% summarize(mean(correct))

d2$condition[d2$cue == "A" & d2$target == "X"] <- "AX"
d2$condition[d2$cue == "A" & d2$target == "Y"] <- "AY"
d2$condition[d2$cue == "B" & d2$target == "X"] <- "BX"
d2$condition[d2$cue == "B" & d2$target == "Y"] <- "BY"
d2$condition[d2$cue == "C"] <- "CX"

d2$p_global[d2$target == "X"] <- .8
d2$p_global[d2$target == "Y"] <- .2

d2$p_conditional[d2$condition == "AX"] <- .67
d2$p_conditional[d2$condition == "AY"] <- .33
d2$p_conditional[d2$condition == "BX"] <- .33
d2$p_conditional[d2$condition == "BY"] <- .67
d2$p_conditional[d2$condition == "CX"] <- 1

d2 <- d2 %>% 
  # remove funny cases
  mutate(prevCorrect = lag(correct)) %>% 
  filter(correct == 1) %>% 
  filter(prevCorrect ==1) %>% 
  filter(response_time > 100, response_time < 5000) %>% 
  group_by(jatosStudyResultId, condition) %>% 
  filter(abs(scale(response_time)) < 2) %>%
  # user-friendliness
  rename(ID = jatosStudyResultId,
         probe = target,
         RT = response_time) %>%
  select(ID, condition, cue, probe, p_global, p_conditional, correct, RT) %>%
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# One Data Set to Rule them All 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

d <- d1 %>% add_row(d2) %>%
  mutate(p_posterior = p_global * p_conditional,
         best_guess = round(p_conditional))

# Aggregated by participant/condition
# Aggregated by participant/condition
  group_by(ID, condition, cue, probe, p_global, p_conditional, p_posterior, best_guess) %>%
  summarise(RT = mean(RT),
            correct = mean(correct))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Graphical Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Do we see RTs separating into two clusters on some conditions?
#  (this would support a threshold model)

d1 %>% ggplot(aes(condition, RT)) +
  geom_quasirandom(method = "pseudorandom", width = .3, alpha = .1)

d2 %>%   filter(condition != "CX") %>%
  ggplot(aes(condition, RT)) +
  geom_quasirandom(method = "pseudorandom", width = .3, alpha = .1)

# It's hard to tell, but some of these do look a bit like they have two overlapping groups,
# What about when when we group by participant? 

d1 %>% 
  ggplot(aes(condition, RT)) +
  geom_boxplot(aes(fill = factor(ID)), alpha = .5, outlier.shape = NA) +
  geom_boxplot(alpha = .2, outlier.shape = NA) +
  scale_y_continuous(limits = c(200, 800))

d2 %>% 
  filter(condition != "CX") %>%
  ggplot(aes(condition, RT)) +
  geom_boxplot(aes(fill = factor(ID)), alpha = .5, outlier.shape = NA) +
  geom_boxplot(alpha = .2, outlier.shape = NA) +
  scale_y_continuous(limits = c(200, 800))

# Main conclusion: this is tricky to visualize.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model Comparisons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Model 0: LTM = Posterior Probability (Base Rate * Conditional)
# Slope is allowed to vary because maybe some people are more or less attuned to prediction in addition to being faster or slower generally
mod_0 <- lmer(RT ~ 1 + p_posterior + (1 + p_posterior | ID), d)
estimates(mod_0)
visualize(mod_0, plot = "model", sample = 10)


mod_0.1 <- lmer(RT ~ 1 + p_global + p_conditional + (1 + p_global + p_conditional | ID), d)
estimates(mod_0.1)
visualize(mod_0.1, plot = "model", formula = RT ~ p_conditional + ID | p_global, sample = 10)

# Model 1: LTM = Base Rate, WM = Conditional Best Guess
mod_1 <- lmer(RT ~ 1 + p_global + best_guess + (1 + p_global + best_guess | ID), d)
estimates(mod_1)
visualize(mod_1, plot = "model", sample = 10)

# Model 2: LTM = Associative Model, WM = Conditional Best Guess
mod_2 <- lmer(RT ~ 1 + p_global + p_conditional + best_guess 
              + (1 + p_global + p_conditional + best_guess | ID), d)
summary(mod_2)
visualize(mod_2, plot = "model", 
          formula = RT ~ p_conditional + ID | best_guess, sample = 7)
    # WM has a huge positive effect here, which seems wrong. It looks like it's inflating the effect of p_conditional and making up for it with best_guess

model.comparison(mod_0, mod_1) # model 0 wins
model.comparison(mod_0, mod_2) # model 0 wins
model.comparison(mod_0, mod_0.1) # model 0.1 (plain spreading activation) is best


# Model 3: LTM = Associative Model, WM = Conditional Load Threshold (NOW FOR THE HARD PART)
  # Useful resources: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2007q4/000472.html
  #                   https://discourse.mc-stan.org/t/piecewise-linear-mixed-models-with-a-random-change-point/5306/15

  # fit naive nonlinear model (so that we can give multilevel model reasonable starting points for coefficients)
  # the logistic function is a practical alternative to a true step function, which spells trouble for gradient descent
  # multstart introduces some randomness to avoid converging on local maxima and to help with converging in general
naive_mod_3 <- nls.multstart::nls_multstart(RT ~ b0 + b1*p_global + b2*p_conditional + b3*plogis(100*(p_conditional + brk)),
                   data = d,
                   start_lower = c(b0 = 100, b1 = -500, b2 = -500, b3 = -500, brk  = -2),
                   start_upper = c(b0 = 1000, b1 = 500, b2 = 500, b3 = 500, brk  = 2),
                   iter = 500,
                   supp_errors = "Y")

summary(naive_mod_3)
  # Again WM has a huge positive effect here, which seems wrong. Things are not looking good for binary boosts in general.
  # Nevertheless, I'll start letting only threshold vary by participant
mod_3 <- nlme::nlme(RT ~ b0 + b1*p_global + b2*p_conditional + b3*plogis(100*(p_conditional + brk)),
              data = d,
              fixed = b0 + b1 + b2 + b3 + brk ~ 1,
              random = brk ~ 1,
              groups = ~ ID,
              start = coef(naive_mod_3))

summary(mod_3)

coef(mod_3) %>%
  mutate(ID = sort(unique(d$ID))) %>%
  right_join(tibble(ID = sample(d$ID),
                    p_global = seq(0, 1, length.out = nrow(d)),
                    p_conditional = seq(0, 1, length.out = nrow(d)))) %>%
  mutate(RT = b0 + b1*p_global + b2*p_conditional + b3*plogis(100*(p_conditional + brk))) %>%
  ggplot(aes(p_conditional, RT)) +
    geom_line(aes(group = ID)) +
    geom_quasirandom(aes(p_conditional, RT), data = d, alpha = .1)

# Model 4: LTM = Associative Model, WM = LTM Load Threshold




