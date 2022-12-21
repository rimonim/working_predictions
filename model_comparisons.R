library(tidyverse)
library(ggbeeswarm)
library(cowplot)
library(lme4)
library(flexplot)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model 0: LTM = Conditional Probability
# Slope is allowed to vary because maybe some people are more or less attuned to prediction in addition to being faster or slower generally
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

aggmod_0 <- lmer(RT ~ 1 + odds_conditional + (1 + odds_conditional | ID), d_agg)
summary(aggmod_0)
visualize(aggmod_0, plot = "model", jitter = .01, alpha = .8, sample = 34) +
  labs(title = "Model 0 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2, .33, .67, .8, 1), 
                     labels = c("0.2\n(Exp1.1)", "0.33\n(Exp1.2)", "0.67\n(Exp1.2)", "0.8\n(Exp1.1)", "1.0\n(Exp1.2)"))

aggmod_0.1 <- lmer(RT ~ 1 + odds_conditional + (1 + odds_conditional | ID), d1_agg)
summary(aggmod_0.1)
visualize(aggmod_0.1, plot = "model", alpha = .8, sample = 10)

aggmod_0.2 <- lmer(RT ~ 1 + odds_conditional + (1 + odds_conditional | ID), d2_agg)
summary(aggmod_0.2)
visualize(aggmod_0.2, plot = "model", alpha = .8, sample = 10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model 1: Summed Parallel Predictions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

aggmod_1 <- lmer(RT ~ 1 + odds_global + odds_conditional + (1 + odds_conditional | ID), d_agg2)
summary(aggmod_1)
visualize(aggmod_1, plot = "model", formula = RT ~ odds_conditional + ID | odds_global, jitter = .01, alpha = .8, sample = 34) +
  labs(title = "Model 1 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2, .33, .67, .8, 1), labels = c("0.2\n(1.1)", "0.33\n(1.2)", "0.67\n(1.2)", "0.8\n(1.1)", "1.0\n(1.2)"))

aggmod_1.1 <- lmer(RT ~ 1 + odds_global + odds_conditional + (1 + odds_global + odds_conditional | ID), d1_agg)
summary(aggmod_1.1)
visualize(aggmod_1.1, plot = "model", formula = RT ~ odds_conditional + ID | odds_global, jitter = .01, alpha = .8, sample = 34) +
  labs(title = "Model 1 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2, .33, .67, .8, 1), labels = c("0.2\n(1.1)", "0.33\n(1.2)", "0.67\n(1.2)", "0.8\n(1.1)", "1.0\n(1.2)"))

aggmod_1.2 <- lmer(RT ~ 1 + odds_global + odds_conditional + (1 + odds_global + odds_conditional | ID), d2_agg)
summary(aggmod_1.2)
visualize(aggmod_1.2)

  aggmod_1_exp1 <- 
    coef(aggmod_1)$ID %>% tibble() %>% 
    mutate(ID = unique(d_agg2$ID)) %>% 
    dplyr::rename(intercept = `(Intercept)`,
                  b1 = odds_global, 
                  b2 = odds_conditional) %>% 
    right_join(d_agg2 %>% select(!RT) %>% filter(experiment == "Experiment 1")) %>% 
    mutate(RT = intercept + b1*odds_global + b2*odds_conditional,
           RTfixed = coef(summary(aggmod_1))[, 1][1] + coef(summary(aggmod_1))[, 1][2]*odds_global + coef(summary(aggmod_1))[, 1][3]*odds_conditional) %>% 
    ggplot(aes(condition, RT)) + 
      geom_quasirandom(aes(color = as.factor(ID)), data = d_agg2 %>% filter(experiment == "Experiment 1"), alpha = .5, method = "pseudorandom", width = .1) +
      geom_line(aes(color = as.factor(ID), group = as.factor(ID)), alpha = .5) +
      geom_line(aes(y = RTfixed, group = 1), size = 1) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Experiment 1.1 Model Fit",
           x = "Condition")
  
  aggmod_1_exp2 <-     
    coef(aggmod_1)$ID %>% tibble() %>% 
    mutate(ID = unique(d_agg2$ID)) %>% 
    dplyr::rename(intercept = `(Intercept)`,
                  b1 = odds_global, 
                  b2 = odds_conditional) %>% 
    right_join(d_agg2 %>% select(!RT) %>% filter(experiment == "Experiment 2")) %>% 
    mutate(RT = intercept + b1*odds_global + b2*odds_conditional,
           RTfixed = coef(summary(aggmod_1))[, 1][1] + coef(summary(aggmod_1))[, 1][2]*odds_global + coef(summary(aggmod_1))[, 1][3]*odds_conditional) %>% 
    ggplot(aes(condition, RT)) + 
      geom_quasirandom(aes(color = as.factor(ID)), data = d_agg2 %>% filter(experiment == "Experiment 2"), alpha = .5, method = "pseudorandom", width = .1) +
      geom_line(aes(color = as.factor(ID), group = as.factor(ID)), alpha = .5) +
      geom_line(aes(y = RTfixed, group = 1), size = 1) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Experiment 1.2 Model Fit",
           x = "Condition")
  
plot_grid(aggmod_1_exp1, aggmod_1_exp2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model 2: Summed Proportional Stimulus-Response Associations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

aggmod_2 <- lmer(RT ~ 1 + odds_global + odds_conjunction + (1 + odds_conjunction | ID), d_agg2)
summary(aggmod_2)
visualize(aggmod_2, plot = "model", formula = RT ~ odds_conjunction | odds_global, jitter = .01) +
  labs(title = "Model 2 (Unified Dataset)") +
  scale_x_continuous(breaks = c(.2*.2, .2*.33, .2*.67, .8*.2, .6, .8*.8),
                     labels = c("BX(1.1)", "AY/BX(1.2)", "AX/BY(1.2)", "AY(1.1)", "CX(1.2)", "AX(1.1)"),
                     guide = guide_axis(angle = 90, n.dodge = 2))

aggmod_2.1 <- lmer(RT ~ 1 + odds_global + odds_conjunction + (1 + odds_global + odds_conjunction | ID), d1_agg)
summary(aggmod_2.1)
visualize(aggmod_2.1, plot = "model", formula = RT ~ odds_conjunction | odds_global, jitter = .01)

aggmod_2.2 <- lmer(RT ~ 1 + odds_global + odds_conjunction + (1 + odds_global + odds_conjunction | ID), d2_agg)
summary(aggmod_2.2)
visualize(aggmod_2.2, plot = "model", formula = RT ~ odds_conjunction | odds_global, jitter = .01)


  aggmod_2_exp1 <- 
    coef(aggmod_2)$ID %>% tibble() %>% 
    mutate(ID = unique(d_agg2$ID)) %>% 
    dplyr::rename(intercept = `(Intercept)`,
                  b1 = odds_global, 
                  b2 = odds_conjunction) %>% 
    right_join(d_agg2 %>% select(!RT) %>% filter(experiment == "Experiment 1")) %>% 
    mutate(RT = intercept + b1*odds_global + b2*odds_conjunction,
           RTfixed = coef(summary(aggmod_2))[, 1][1] + coef(summary(aggmod_2))[, 1][2]*odds_global + coef(summary(aggmod_2))[, 1][3]*odds_conjunction) %>% 
    ggplot(aes(condition, RT)) + 
      geom_quasirandom(aes(color = as.factor(ID)), data = d_agg2 %>% filter(experiment == "Experiment 1"), alpha = .5, method = "pseudorandom", width = .1) +
      geom_line(aes(color = as.factor(ID), group = as.factor(ID)), alpha = .5) +
      geom_line(aes(y = RTfixed, group = 1), size = 1) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Experiment 1.1 Model Fit",
           x = "Condition")

  aggmod_2_exp2 <- 
    coef(aggmod_2)$ID %>% tibble() %>% 
    mutate(ID = unique(d_agg2$ID)) %>% 
    dplyr::rename(intercept = `(Intercept)`,
                  b1 = odds_global, 
                  b2 = odds_conjunction) %>% 
    right_join(d_agg2 %>% select(!RT) %>% filter(experiment == "Experiment 2")) %>% 
    mutate(RT = intercept + b1*odds_global + b2*odds_conjunction,
           RTfixed = coef(summary(aggmod_2))[, 1][1] + coef(summary(aggmod_2))[, 1][2]*odds_global + coef(summary(aggmod_2))[, 1][3]*odds_conjunction) %>% 
    ggplot(aes(condition, RT)) + 
      geom_quasirandom(aes(color = as.factor(ID)), data = d_agg2 %>% filter(experiment == "Experiment 2"), alpha = .5, method = "pseudorandom", width = .1) +
      geom_line(aes(color = as.factor(ID), group = as.factor(ID)), alpha = .5) +
      geom_line(aes(y = RTfixed, group = 1), size = 1) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Experiment 1.2 Model Fit",
           x = "Condition")

plot_grid(aggmod_2_exp1, aggmod_2_exp2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Model 3: LTM = Base Rate, WM = Conditional Best Guess
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  aggmod_3 <- lmer(RT ~ 1 + odds_global + best_guess + (1 + odds_global + best_guess | ID), d_agg)
  summary(aggmod_3)
  visualize(aggmod_3, plot = "model", sample = 35) +
    labs(title = "Model 3 (Unified Dataset)")
  
  aggmod_3.1 <- lmer(RT ~ 1 + odds_global + best_guess + (1 + odds_global + best_guess | ID), d1_agg)
  summary(aggmod_3.1)
  visualize(aggmod_3.1)
  
  aggmod_3.2 <- lmer(RT ~ 1 + odds_global + best_guess + (1 + odds_global + best_guess | ID), d2_agg)
  summary(aggmod_3.2)
  visualize(aggmod_3.2)
  
    # Same thing with odds_conditional to see if best_guess is really adding anything. It's not.
    
    lmer(RT ~ 1 + odds_global + odds_conditional + best_guess + (1 + odds_global + odds_conditional + best_guess | ID), d_agg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model 4: LTM = Associative Model, WM = Conditional Load Threshold 
# Useful resources: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2007q4/000472.html
#                   https://discourse.mc-stan.org/t/piecewise-linear-mixed-models-with-a-random-change-point/5306/15
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
# fit naive nonlinear model (so that we can give multilevel model reasonable starting points for coefficients)
# the logistic function is a practical alternative to a true step function, which spells trouble for gradient descent
# multstart introduces some randomness to avoid converging on local maxima and to help with converging in general
naive_mod_4 <- nls.multstart::nls_multstart(RT ~ b0 + b1*odds_global + b2*odds_conjunction + b3*plogis(500*(odds_conditional + brk)),
                                            data = d_agg,
                                            start_lower = c(b0 = 100, b1 = -500, b2 = -500, b3 = -500, brk  = -2),
                                            start_upper = c(b0 = 1000, b1 = 500, b2 = 500, b3 = 500, brk  = 2),
                                            iter = 500,
                                            supp_errors = "Y")

summary(naive_mod_4)
# Again WM has a huge positive effect here, which seems wrong. Things are not looking good for binary boosts in general.
# Nevertheless, I'll start letting only threshold vary by participant
aggmod_4 <- nlme::nlme(RT ~ b0 + b1*odds_global + b2*odds_conjunction + b3*plogis(500*(odds_conditional + brk)),
                    data = d_agg,
                    fixed = b0 + b1 + b2 + b3 + brk ~ 1,
                    random = b0 + brk ~ 1,
                    groups = ~ ID,
                    start = coef(naive_mod_4))

summary(aggmod_4)

aggmod_4_fitted <- coef(aggmod_3) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  tidyr::expand(nesting(ID, b0, b1, b2, b3, brk), 
                odds_global = seq(0.132, .8, length.out = 100),
                odds_conjunction = seq(0.06, .64, length.out = 100),
                odds_conditional = seq(0.2, 1, length.out = 200)) %>%
  mutate(RT = b0 + b1*odds_global + b2*odds_conjunction + b3*plogis(500*(odds_conditional + brk)))

aggmod_4_fitted %>%
  group_by(ID, odds_conditional) %>%
  summarise(RT = mean(RT)) %>%
  ggplot(aes(odds_conditional, RT)) +
    geom_quasirandom(aes(odds_conditional, RT, color = as.factor(ID)), data = d_agg, alpha = .5, method = "pseudorandom", width = .01) +
    geom_line(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .3) +
    theme_minimal() +
    theme(legend.position = "none")

# Model Predictions with odds_conditional on x axis
coef(aggmod_4) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  right_join(d_agg %>% select(!RT)) %>%
  mutate(RT = b0 + b1*odds_global + b2*odds_conjunction + b3*plogis(500*(odds_conditional + brk))) %>%
  ggplot(aes(odds_conditional, RT)) +
  geom_quasirandom(aes(odds_conditional, RT, color = as.factor(ID)), data = d_agg, alpha = .3, method = "pseudorandom", width = .04) +
  geom_point(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .8, size = 2) +
  geom_point(aes(odds_conditional, RT), alpha = .03, size = 4, 
             data = coef(aggmod_4) %>%
               summarise_all(mean) %>%
               right_join(d_agg %>% select(!RT), by = character()) %>%
               group_by(odds_conditional) %>%
               summarise(RT = mean(b0) + mean(b1)*mean(odds_global) + mean(b2)*mean(odds_conjunction) + mean(b3)*plogis(500*(odds_conditional + brk)))) + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Model 4 Fitted Values (Full Dataset)")

# Model Predictions with odds_conjunction on x axis
coef(aggmod_4) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  right_join(d_agg %>% select(!RT)) %>%
  mutate(RT = b0 + b1*odds_global + b2*odds_conjunction + b3*plogis(500*(odds_conditional + brk))) %>%
  ggplot(aes(odds_conjunction, RT)) +
  geom_quasirandom(aes(odds_conjunction, RT, color = as.factor(ID)), data = d_agg, alpha = .3, method = "pseudorandom", width = .04) +
  geom_point(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .8, size = 2) +
  geom_point(aes(odds_conjunction, RT), alpha = .03, size = 4, 
             data = coef(aggmod_4) %>%
               summarise_all(mean) %>%
               right_join(d_agg %>% select(!RT), by = character()) %>%
               group_by(odds_conjunction) %>%
               summarise(RT = mean(b0) + mean(b1)*mean(odds_global) + mean(b2)*odds_conjunction + mean(b3)*plogis(500*(mean(odds_conditional) + brk)))) + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Model 4 Fitted Values (Full Dataset)")

# Model Predictions with odds_global on x axis
coef(aggmod_4) %>%
  mutate(ID = sort(unique(d_agg$ID))) %>%
  right_join(d_agg %>% select(!RT)) %>%
  mutate(RT = b0 + b1*odds_global + b2*odds_conjunction + b3*plogis(500*(odds_conditional + brk))) %>%
  ggplot(aes(odds_global, RT)) +
  geom_quasirandom(aes(odds_global, RT, color = as.factor(ID)), data = d_agg, alpha = .3, method = "pseudorandom", width = .04) +
  geom_point(aes(group = as.factor(ID), color = as.factor(ID)), alpha = .8, size = 2) +
  geom_point(aes(odds_global, RT), alpha = .03, size = 4, 
             data = coef(aggmod_4) %>%
               summarise_all(mean) %>%
               right_join(d_agg %>% select(!RT), by = character()) %>%
               group_by(odds_global) %>%
               summarise(RT = mean(b0) + mean(b1)*odds_global + mean(b2)*mean(odds_conjunction) + mean(b3)*plogis(500*(mean(odds_conditional) + brk)))) + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Model 4 Fitted Values (Full Dataset)")

AIC(aggmod_1, aggmod_2, aggmod_3, aggmod_4, aggmod_5)

BIC(aggmod_1, aggmod_2, aggmod_3, aggmod_4, aggmod_5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model 6: Automatic Model-Free, Costly Model-Based
# This time taking into account the cost of using model-based analysis (WM) to generate predictions at all.
  # model-free system - as in Mod 2: odds_global + odds_conjunction
  # model-based system: mb_utility*odds_conditional*mb_willingness
  # random variables: mb_willingness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

naive_mod_6 <- lm(RT ~ odds_global + odds_conjunction + mb_utility:odds_conditional, data = d_agg2)

summary(naive_mod_6)

    # for testing purposes
    naive_mod_1 <- lm(RT ~ odds_global + odds_conditional, data = d_agg2)
    naive_mod_2 <- lm(RT ~ odds_global + odds_conjunction, data = d_agg2)

d_agg2 %>%
  mutate(RT_pred = coef(naive_mod_6)[[1]] + coef(naive_mod_6)[[2]]*odds_global + coef(naive_mod_6)[[3]]*odds_conjunction + coef(naive_mod_6)[[4]]*mb_utility*odds_conditional) %>%
  ggplot(aes(odds_conditional, RT_pred)) +
    geom_quasirandom(aes(odds_conditional, RT), data = d_agg2, alpha = .3, method = "pseudorandom", width = .04) +
    geom_line(color = "orange") + 
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = "Linear Model Fit - Full Data Aggregated by Participant",
         subtitle = "RT ~ 1 + b1(odds_global) + b2(odds_conjunction) + b3(mb_utility*odds_conditional)",
         x = "Probability of Probe, Conditional on Cue",
         y = "RT (ms)") + 
    facet_wrap(~odds_global, labeller = "label_both")


AIC(naive_mod_1, naive_mod_2, naive_mod_6)
BIC(naive_mod_1, naive_mod_2, naive_mod_6)

d_agg2 %>% 
  mutate(RTmod1 = coef(summary(naive_mod_1))[1, 1] + coef(summary(naive_mod_1))[2, 1]*odds_global + coef(summary(naive_mod_1))[3, 1]*odds_conditional,
         RTmod2 = coef(summary(naive_mod_2))[1, 1] + coef(summary(naive_mod_2))[2, 1]*odds_global + coef(summary(naive_mod_2))[3, 1]*odds_conjunction,
         RTmod6 = coef(summary(naive_mod_6))[1, 1] + coef(summary(naive_mod_6))[2, 1]*odds_global + coef(summary(naive_mod_6))[3, 1]*odds_conjunction + coef(summary(naive_mod_6))[4, 1]*mb_utility*odds_conditional) %>% 
  ggplot(aes(factor(condition, levels = c("AX", "BY", "BX", "AY", "CX")), RT)) +
  geom_quasirandom(data = d_agg, alpha = .5, method = "pseudorandom", width = .2) +
  geom_line(aes(y = RTmod1, group = 1, color = "yellow")) +
  geom_line(aes(y = RTmod2, group = 1, color = "blue")) +
  geom_line(aes(y = RTmod6, group = 1, color = "red")) +
  theme_minimal() +
  scale_color_manual(values = c("yellow", "blue", "red"),
                     labels = c("Model 1 (Global + Conditional)", 
                                "Model 2 (Global + Conjunction)", 
                                "Model 3 (Global + Conjunction + Utility*Conditional)")) + 
  labs(title = "Model Fits",
       x = "Condition") +
  facet_wrap(~experiment)

# Now for some Multilevel Modeling
aggmod_6 <- lmer(RT ~ 1 + odds_global + odds_conjunction + mb_utility:odds_conditional + (1 + odds_conjunction + mb_utility:odds_conditional | ID), 
              data = d_agg2)

summary(aggmod_6)

AIC(aggmod_1, aggmod_2, aggmod_6)
BIC(aggmod_1, aggmod_2, aggmod_6)

    aggmod_6_exp1 <- 
      coef(aggmod_6)$ID %>% tibble() %>% 
      mutate(ID = unique(d_agg2$ID)) %>% 
      dplyr::rename(intercept = `(Intercept)`,
                    b1 = odds_global, 
                    b2 = odds_conjunction,
                    b3 = `mb_utility:odds_conditional`) %>% 
      right_join(d_agg2 %>% select(!RT) %>% filter(experiment == "Experiment 1")) %>% 
      mutate(RT = intercept + b1*odds_global + b2*odds_conjunction + b3*mb_utility*odds_conditional,
             RTfixed = coef(summary(aggmod_6))[, 1][1] + coef(summary(aggmod_6))[, 1][2]*odds_global + coef(summary(aggmod_6))[, 1][3]*odds_conjunction + coef(summary(aggmod_6))[, 1][4]*mb_utility*odds_conditional) %>% 
      ggplot(aes(condition, RT)) + 
        geom_quasirandom(aes(color = as.factor(ID)), data = d_agg2 %>% filter(experiment == "Experiment 1"), alpha = .3, method = "pseudorandom", width = .1) +
        geom_line(aes(color = as.factor(ID), group = as.factor(ID)), alpha = .5) +
        geom_line(aes(y = RTfixed, group = 1), size = 1) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "Experiment 1.1 Model Fit",
             x = "Condition")
      
    aggmod_6_exp2 <- 
      coef(aggmod_6)$ID %>% tibble() %>% 
      mutate(ID = unique(d_agg2$ID)) %>% 
      dplyr::rename(intercept = `(Intercept)`,
                    b1 = odds_global, 
                    b2 = odds_conjunction,
                    b3 = `mb_utility:odds_conditional`) %>% 
      right_join(d_agg2 %>% select(!RT) %>% filter(experiment == "Experiment 2")) %>% 
      mutate(RT = intercept + b1*odds_global + b2*odds_conjunction + b3*mb_utility*odds_conditional,
             RTfixed = coef(summary(aggmod_6))[, 1][1] + coef(summary(aggmod_6))[, 1][2]*odds_global + coef(summary(aggmod_6))[, 1][3]*odds_conjunction + coef(summary(aggmod_6))[, 1][4]*mb_utility*odds_conditional) %>% 
      ggplot(aes(condition, RT)) + 
      geom_quasirandom(aes(color = as.factor(ID)), data = d_agg2 %>% filter(experiment == "Experiment 2"), alpha = .3, method = "pseudorandom", width = .1) +
      geom_line(aes(color = as.factor(ID), group = as.factor(ID)), alpha = .5) +
      geom_line(aes(y = RTfixed, group = 1), size = 1) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Experiment 1.2 Model Fit",
           x = "Condition")

plot_grid(aggmod_6_exp1, aggmod_6_exp2)


library(brms)

brm(data = d_agg2, 
    family = gaussian,
    RT ~ 1 + odds_global + odds_conjunction + mb_utility:odds_conditional + (1 + mb_utility:odds_conditional | ID),
    prior = c(prior(normal(-44.91, 20), coef = "odds_global"),
              prior(normal(-92.54, 20), coef = "odds_conjunction"),
              prior(normal(-113.34, 20), coef = "mb_utility:odds_conditional"),
              prior(lkj(2), class = cor)),
    iter = 2000, warmup = 1000, chains = 4, cores = 4)





