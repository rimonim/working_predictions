library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(flexplot)

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
