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

d1$p_cue[d1$cue == "A"] <- .8
d1$p_cue[d1$cue == "B"] <- .2

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
  select(ID, condition, cue, probe, p_cue, p_global, p_conditional, correct, RT, correct) %>%
  ungroup()

d1_agg <- d1 %>%
  group_by(ID, condition, cue, probe, p_cue, p_global, p_conditional) %>%
  summarise(RT = mean(RT, na.rm = T),
            correct = mean(correct, na.rm = T)) %>%
  mutate(p_posterior = p_cue*p_conditional) %>%
  mutate(best_guess = round(p_conditional))
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

d2$p_cue[d2$cue == "A"] <- .2
d2$p_cue[d2$cue == "B"] <- .2
d2$p_cue[d2$cue == "C"] <- .6

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
  select(ID, condition, cue, probe, p_cue, p_global, p_conditional, correct, RT) %>%
  ungroup()

d2_agg <- d2 %>%
  group_by(ID, condition, cue, probe, p_cue, p_global, p_conditional) %>%
  summarise(RT = mean(RT, na.rm = T),
            correct = mean(correct, na.rm = T)) %>%
  mutate(p_posterior = p_cue*p_conditional) %>%
  mutate(best_guess = round(p_conditional))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# One Data Set to Rule them All 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

d <- d1 %>% add_row(d2) %>%
  mutate(best_guess = round(p_conditional))

# Aggregated by participant/condition
d_agg <- d %>%
  group_by(ID, condition, cue, probe, p_cue, p_global, p_conditional, best_guess) %>%
  summarise(RT = mean(RT, na.rm = T),
            correct = mean(correct, na.rm = T)) %>%
  mutate(p_posterior = p_cue*p_conditional)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Graphical Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

d1_agg %>%
  ggplot(aes(condition, RT, color = as.factor(ID), group = as.factor(ID))) +
  geom_path(alpha = .8, size = 1) +
  geom_point() +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Experiment 1.1", x = "Condition", y = "Participant Mean RT (ms)")

d2_agg %>%
  ggplot(aes(condition, RT, color = as.factor(ID), group = as.factor(ID))) +
  geom_path(alpha = .5, size = 1) +
  geom_point(alpha = .9, size = 2) +
  scale_alpha_discrete(range = c(.1, .9)) +
  scale_size_discrete(range = c(2, 3)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Experiment 1.2", x = "Condition", y = "Participant Mean RT (ms)")


# Do we see RTs separating into two clusters on some conditions?
#  (this would support a threshold model)

d1 %>% 
  group_by(ID, condition, cue, probe, p_global, p_conditional) %>%
  summarise(RT = mean(RT),
            correct = mean(correct)) %>%
  mutate(condition = factor(paste(cue, "→", probe), levels = c("A → X", "B → Y", "A → Y", "B → X"))) %>%
  ggplot(aes(condition, RT)) +
    geom_quasirandom(method = "pseudorandom", width = .2) + 
    geom_pointrange(data = d1 %>% 
                      mutate(condition = factor(paste(cue, "→", probe), levels = c("A → X", "B → Y", "A → Y", "B → X"))) %>%
                      group_by(condition) %>% summarise(RTh = mean(RT) + sd(RT),
                                                        RTl = mean(RT) - sd(RT),
                                                        RT = mean(RT)),
                    aes(y = RT, ymin = RTl, ymax = RTh), color = "orange") +
    theme_minimal() +
    labs(x = "Condition", y = "RT (ms)")

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
# Some non-causal modeling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(ggcorrplot)

# Do participants who's times go up from A→ X to A→ Y also go up from B→ X to B→ Y, and vice versa?
d2_agg %>%
  pivot_wider(id_cols = ID, names_from = condition, names_prefix = "RT_", values_from = RT) %>%
  mutate(AXtoAY_diff = RT_AX - RT_AY,
         BXtoBY_diff = RT_BX - RT_BY,
         ABXtoCX_diff = mean(c(RT_AX, RT_BX)) - RT_CX) %>%
  ungroup() %>%
  select(AXtoAY_diff:ABXtoCX_diff) %>%
  ggplot(aes(AXtoAY_diff, BXtoBY_diff)) +
    geom_point() +
    theme_minimal() +
    annotate("rect", xmin = -150, xmax = 0, ymin = -75, ymax = 125,
             alpha = .1,fill = "red") +
    annotate("rect", xmin = -150, xmax = 75, ymin = -75, ymax = 0,
             alpha = .1,fill = "red") +
    annotate("text", x = -75, y = -35, label = "X is always \nquicker", alpha = .5) +
    annotate("text", x = -75, y = 60, label = "X is quicker \nafter A only", alpha = .5) +
    annotate("text", x = 35, y = -35, label = "X is quicker \nafter B only", alpha = .5) +
    labs(title = "Participant RTs for X and Y Prompts in Experiment 1.2",
         subtitle = "Excluding C -> X condition",
         x = "Difference between X and Y RTs after A",
         y = "Difference between X and Y RTs after B")

d1_agg %>%
  pivot_wider(id_cols = ID, names_from = condition, names_prefix = "RT_", values_from = RT) %>%
  mutate(AXtoAY_diff = RT_AX - RT_AY,
         BXtoBY_diff = RT_BX - RT_BY) %>%
  ungroup() %>%
  select(AXtoAY_diff:BXtoBY_diff) %>%
  cor()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model Comparisons: Aggregated by Participant/Condition
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Model 0: LTM = Conditional Probability
# Slope is allowed to vary because maybe some people are more or less attuned to prediction in addition to being faster or slower generally
aggmod_0 <- lmer(RT ~ 1 + p_conditional + (1 + p_conditional | ID), d_agg)
summary(aggmod_0)
visualize(aggmod_0, plot = "model", jitter = .01, alpha = .8, sample = 34) +
  labs(title = "Model 0 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2, .33, .67, .8, 1), 
                     labels = c("0.2\n(Exp1.1)", "0.33\n(Exp1.2)", "0.67\n(Exp1.2)", "0.8\n(Exp1.1)", "1.0\n(Exp1.2)"))

aggmod_0.1 <- lmer(RT ~ 1 + p_conditional + (1 + p_conditional | ID), d1_agg)
summary(aggmod_0.1)
visualize(aggmod_0.1, plot = "model", alpha = .8, sample = 10)

aggmod_0.2 <- lmer(RT ~ 1 + p_conditional + (1 + p_conditional | ID), d2_agg)
summary(aggmod_0.2)
visualize(aggmod_0.2, plot = "model", alpha = .8, sample = 10)

# Model 1: Summed Parallel Predictions
aggmod_1 <- lmer(RT ~ 1 + p_global + p_conditional + (1 + p_global + p_conditional | ID), d_agg)
summary(aggmod_1)
visualize(aggmod_1, plot = "model", formula = RT ~ p_conditional + ID | p_global, jitter = .01, alpha = .8, sample = 34) +
  labs(title = "Model 1 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2, .33, .67, .8, 1), labels = c("0.2\n(1.1)", "0.33\n(1.2)", "0.67\n(1.2)", "0.8\n(1.1)", "1.0\n(1.2)"))

aggmod_1.1 <- lmer(RT ~ 1 + p_global + p_conditional + (1 + p_global + p_conditional | ID), d1_agg)
summary(aggmod_1.1)
visualize(aggmod_1.1, plot = "model", formula = RT ~ p_conditional + ID | p_global, jitter = .01, alpha = .8, sample = 34) +
  labs(title = "Model 1 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2, .33, .67, .8, 1), labels = c("0.2\n(1.1)", "0.33\n(1.2)", "0.67\n(1.2)", "0.8\n(1.1)", "1.0\n(1.2)"))

aggmod_1.2 <- lmer(RT ~ 1 + p_global + p_conditional + (1 + p_global + p_conditional | ID), d2_agg)
summary(aggmod_1.2)
visualize(aggmod_1.2)

# Model 2: Summed Proportional Stimulus-Response Associations
aggmod_2 <- lmer(RT ~ 1 + p_global + p_posterior + (1 + p_global + p_posterior | ID), d_agg)
summary(aggmod_2)
visualize(aggmod_2, plot = "model", formula = RT ~ p_posterior | p_global, jitter = .01) +
  labs(title = "Model 2 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2*.2, .2*.33, .2*.67, .8*.2, .6, .8*.8),
                     labels = c("BX(1.1)", "AY/BX(1.2)", "AX/BY(1.2)", "AY(1.1)", "CX(1.2)", "AX(1.1)"),
                     guide = guide_axis(angle = 90, n.dodge = 2))

aggmod_2.1 <- lmer(RT ~ 1 + p_global + p_posterior + (1 + p_global + p_posterior | ID), d1_agg)
summary(aggmod_2.1)
visualize(aggmod_2.1, plot = "model", formula = RT ~ p_posterior | p_global, jitter = .01)

aggmod_2.2 <- lmer(RT ~ 1 + p_global + p_posterior + (1 + p_global + p_posterior | ID), d2_agg)
summary(aggmod_2.2)
visualize(aggmod_2.2, plot = "model", formula = RT ~ p_posterior | p_global, jitter = .01)

# Model 3: LTM = Base Rate, WM = Conditional Best Guess
aggmod_3 <- lmer(RT ~ 1 + p_global + best_guess + (1 + p_global + best_guess | ID), d_agg)
summary(aggmod_3)
visualize(aggmod_3, plot = "model", sample = 35) +
  labs(title = "Model 3 (Unified Dataset)")

aggmod_3.1 <- lmer(RT ~ 1 + p_global + best_guess + (1 + p_global + best_guess | ID), d1_agg)
summary(aggmod_3.1)
visualize(aggmod_3.1)

aggmod_3.2 <- lmer(RT ~ 1 + p_global + best_guess + (1 + p_global + best_guess | ID), d2_agg)
summary(aggmod_3.2)
visualize(aggmod_3.2)

  # Same thing with p_conditional to see if best_guess is really adding anything
  
  lmer(RT ~ 1 + p_global + p_conditional + best_guess + (1 + p_global + p_conditional + best_guess | ID), d_agg)

# Model 4: LTM = Associative Model, WM = Conditional Load Threshold (NOW FOR THE HARD PART)
# Useful resources: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2007q4/000472.html
#                   https://discourse.mc-stan.org/t/piecewise-linear-mixed-models-with-a-random-change-point/5306/15

# fit naive nonlinear model (so that we can give multilevel model reasonable starting points for coefficients)
# the logistic function is a practical alternative to a true step function, which spells trouble for gradient descent
# multstart introduces some randomness to avoid converging on local maxima and to help with converging in general
naive_mod_4 <- nls.multstart::nls_multstart(RT ~ b0 + b1*p_global + b2*p_posterior + b3*plogis(500*(p_conditional + brk)),
                                            data = d_agg,
                                            start_lower = c(b0 = 100, b1 = -500, b2 = -500, b3 = -500, brk  = -2),
                                            start_upper = c(b0 = 1000, b1 = 500, b2 = 500, b3 = 500, brk  = 2),
                                            iter = 500,
                                            supp_errors = "Y")

summary(naive_mod_4)
# Again WM has a huge positive effect here, which seems wrong. Things are not looking good for binary boosts in general.
# Nevertheless, I'll start letting only threshold vary by participant
aggmod_4 <- nlme::nlme(RT ~ b0 + b1*p_global + b2*p_posterior + b3*plogis(500*(p_conditional + brk)),
                    data = d_agg,
                    fixed = b0 + b1 + b2 + b3 + brk ~ 1,
                    random = b0 + brk ~ 1,
                    groups = ~ ID,
                    start = coef(naive_mod_4))

summary(aggmod_4)

aggmod_4_fitted <- coef(aggmod_3) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  tidyr::expand(nesting(ID, b0, b1, b2, b3, brk), 
                p_global = seq(0.132, .8, length.out = 100),
                p_posterior = seq(0.06, .64, length.out = 100),
                p_conditional = seq(0.2, 1, length.out = 200)) %>%
  mutate(RT = b0 + b1*p_global + b2*p_posterior + b3*plogis(500*(p_conditional + brk)))

aggmod_4_fitted %>%
  group_by(ID, p_conditional) %>%
  summarise(RT = mean(RT)) %>%
  ggplot(aes(p_conditional, RT)) +
    geom_quasirandom(aes(p_conditional, RT, color = as.factor(ID)), data = d_agg, alpha = .5, method = "pseudorandom", width = .01) +
    geom_line(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .3) +
    theme_minimal() +
    theme(legend.position = "none")

# Model Predictions with p_conditional on x axis
coef(aggmod_4) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  right_join(d_agg %>% select(!RT)) %>%
  mutate(RT = b0 + b1*p_global + b2*p_posterior + b3*plogis(500*(p_conditional + brk))) %>%
  ggplot(aes(p_conditional, RT)) +
  geom_quasirandom(aes(p_conditional, RT, color = as.factor(ID)), data = d_agg, alpha = .3, method = "pseudorandom", width = .04) +
  geom_point(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .8, size = 2) +
  geom_point(aes(p_conditional, RT), alpha = .03, size = 4, 
             data = coef(aggmod_4) %>%
               summarise_all(mean) %>%
               right_join(d_agg %>% select(!RT), by = character()) %>%
               group_by(p_conditional) %>%
               summarise(RT = mean(b0) + mean(b1)*mean(p_global) + mean(b2)*mean(p_posterior) + mean(b3)*plogis(500*(p_conditional + brk)))) + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Model 4 Fitted Values (Full Dataset)")

# Model Predictions with p_posterior on x axis
coef(aggmod_4) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  right_join(d_agg %>% select(!RT)) %>%
  mutate(RT = b0 + b1*p_global + b2*p_posterior + b3*plogis(500*(p_conditional + brk))) %>%
  ggplot(aes(p_posterior, RT)) +
  geom_quasirandom(aes(p_posterior, RT, color = as.factor(ID)), data = d_agg, alpha = .3, method = "pseudorandom", width = .04) +
  geom_point(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .8, size = 2) +
  geom_point(aes(p_posterior, RT), alpha = .03, size = 4, 
             data = coef(aggmod_4) %>%
               summarise_all(mean) %>%
               right_join(d_agg %>% select(!RT), by = character()) %>%
               group_by(p_posterior) %>%
               summarise(RT = mean(b0) + mean(b1)*mean(p_global) + mean(b2)*p_posterior + mean(b3)*plogis(500*(mean(p_conditional) + brk)))) + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Model 4 Fitted Values (Full Dataset)")

# Model Predictions with p_global on x axis
coef(aggmod_4) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  right_join(d_agg %>% select(!RT)) %>%
  mutate(RT = b0 + b1*p_global + b2*p_posterior + b3*plogis(500*(p_conditional + brk))) %>%
  ggplot(aes(p_global, RT)) +
  geom_quasirandom(aes(p_global, RT, color = as.factor(ID)), data = d_agg, alpha = .3, method = "pseudorandom", width = .04) +
  geom_point(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .8, size = 2) +
  geom_point(aes(p_global, RT), alpha = .03, size = 4, 
             data = coef(aggmod_4) %>%
               summarise_all(mean) %>%
               right_join(d_agg %>% select(!RT), by = character()) %>%
               group_by(p_global) %>%
               summarise(RT = mean(b0) + mean(b1)*p_global + mean(b2)*mean(p_posterior) + mean(b3)*plogis(500*(mean(p_conditional) + brk)))) + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Model 4 Fitted Values (Full Dataset)")

AIC(aggmod_1, aggmod_2, aggmod_3, aggmod_4, aggmod_5)

BIC(aggmod_1, aggmod_2, aggmod_3, aggmod_4, aggmod_5)


# Model 5: LTM = Associative Model, WM = LTM Load Threshold

naive_mod_5 <- nls.multstart::nls_multstart(RT ~ b0 + b1*p_global + b2*plogis(100*(p_conditional + brk)),
                                            data = d_agg,
                                            start_lower = c(b0 = 100, b1 = -500, b2 = -500, brk  = -2),
                                            start_upper = c(b0 = 1000, b1 = 500, b2 = 500, brk  = 2),
                                            iter = 500,
                                            supp_errors = "Y")

summary(naive_mod_5)
# Again WM has a huge positive effect here, which seems wrong. Things are not looking good for binary boosts in general.
# Nevertheless, I'll start letting only threshold vary by participant
aggmod_5 <- nlme::nlme(RT ~ b0 + b1*p_global + b2*plogis(100*(p_conditional + brk)),
                       data = d_agg,
                       fixed = b0 + b1 + b2 + brk ~ 1,
                       random = b0 + b1 + b2 ~ 1,
                       groups = ~ ID,
                       start = coef(naive_mod_5))

summary(aggmod_5)



coef(aggmod_5) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  right_join(d_agg %>% select(!RT)) %>%
  mutate(RT = b0 + b1*p_global + b2*plogis(100*(p_conditional + brk))) %>%
  ggplot(aes(p_conditional, RT)) +
  geom_quasirandom(aes(p_conditional, RT, color = as.factor(ID)), data = d_agg, alpha = .3, method = "pseudorandom", width = .04) +
  geom_point(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .8, size = 2) +
  geom_point(aes(p_conditional, RT), alpha = .03, size = 4, 
             data = coef(aggmod_5) %>%
               summarise_all(mean) %>%
               right_join(d_agg %>% select(!RT), by = character()) %>%
               group_by(p_conditional) %>%
               summarise(RT = mean(b0) + mean(b1)*mean(p_global) + mean(b2)*plogis(100*(mean(p_conditional) + brk)))) + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Model 5 Fitted Values (Full Dataset)")


aggmod_5_fitted <- coef(aggmod_5) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  tidyr::expand(nesting(ID, b0, b1, b2, brk), 
                p_global = seq(0.132, .8, length.out = 50),
                p_posterior = seq(0.06, .64, length.out = 100),
                p_conditional = seq(0.2, 1, length.out = 100)) %>%
  mutate(RT = b0 + b1*p_global + b2*plogis(100*(p_conditional + brk)))

aggmod_5_fitted %>%
  group_by(ID, p_conditional) %>%
  summarise(RT = mean(RT)) %>%
  ggplot(aes(p_conditional, RT)) +
  geom_quasirandom(aes(p_conditional, RT, color = as.factor(ID)), data = d_agg, alpha = .5, method = "pseudorandom", width = .01) +
  geom_line(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .3) +
  theme_minimal() +
  theme(legend.position = "none")

aggmod_5_fitted %>%
  group_by(ID, p_global) %>%
  summarise(RT = mean(RT)) %>%
  ggplot(aes(p_global, RT)) +
  geom_quasirandom(aes(p_global, RT, color = as.factor(ID)), data = d_agg, alpha = .5, method = "pseudorandom", width = .01) +
  geom_line(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .3) +
  theme_minimal() +
  theme(legend.position = "none")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model Comparisons: Using all the data?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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




