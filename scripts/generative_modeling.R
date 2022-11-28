library(tidyverse)

# FUNCTION mod1: Generates Output Graph Given Probability Tree Structure
#   (assumptions: 2 probes in task, tree depth = 2, no more than 26 cues)
#   cue_probs: numeric vector of probabilities, of length [number of possible starting cues]

mod1 <- function(cue_probs) {
  cue_names <- LETTERS[1:length(cue_probs)]
  cat("Possible Cues  ", cue_names, "\n", sep = "    ")
  cat("Probabilities  ", format(round(cue_probs, 2), nsmall = 2))
  tree <- list()
  for (cue in 1:length(cue_probs)) {
    cue_name <- cue_names[cue]
    probe_probs <- eval(parse(text = readline(prompt = paste("Please enter the probabilities of probes X and Y occuring after cue", cue_name, "(vector of length 2): "))))
    tree[[length(tree)+1]] <- list("cue_prob" = cue_probs[cue],
                                   "probe_probs" = probe_probs)
  }
  names(tree) <- LETTERS[1:length(tree)]
 d <- tibble(cue = rep(cue_names, each = 2),
             cue_prob = rep(as.numeric(sapply(tree, "[[", 1)), each = 2),
             probe = rep(c("X", "Y"), times = length(cue_names)),
             probe_prob_local = as.numeric(sapply(tree, "[[", 2)),
             probe_prob_adjusted = cue_prob*probe_prob_local)
 d_global <- d %>%
   group_by(probe) %>%
   summarise(probe_prob_global = sum(probe_prob_adjusted))
 print(tree)
 d %>% left_join(d_global) %>%
##~~~~~~~~~~~~~~~ THIS IS WHERE THE GENERATIVE ALGORITHM GOES ~~~~~~~~~~~~~~~~~~
   mutate(RT = 1/(probe_prob_global*probe_prob_local),
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          condition = paste("LTM", probe_prob_global, ",", "WM", probe_prob_local)) %>%
   ggplot(aes(condition, RT)) +
    geom_bar(aes(fill = probe_prob_local == 0 | probe_prob_local == 1), stat = "identity") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = "Global (LTM) and Local (WM) Outcome Probability", y = "RT")
}

exp_2_reciprocal <- mod1(c(.2, .2, .6))

# This model has WM as binary and LTM as a parametric association model without interactions.
# probe_probs = list of length [number of cues], each item a vector of the probabilities of probes X and Y, respectively, following the cue

spreading_activation_wm_mod <- function(cue_probs, probe_probs) {
  cue_names <- LETTERS[1:length(cue_probs)]
  cat("Possible Cues  ", cue_names, "\n", sep = "    ")
  cat("Probabilities  ", format(round(cue_probs, 2), nsmall = 2))
  tree <- list()
  for (cue in 1:length(cue_probs)) {
    cue_name <- cue_names[cue]
    tree[[length(tree)+1]] <- list("cue_prob" = cue_probs[cue],
                                   "probe_probs" = probe_probs[[cue]])
  }
  names(tree) <- LETTERS[1:length(tree)]
  d <- tibble(cue = rep(cue_names, each = 2),
              cue_prob = rep(as.numeric(sapply(tree, "[[", 1)), each = 2),
              probe = rep(c("X", "Y"), times = length(cue_names)),
              probe_prob_local = as.numeric(sapply(tree, "[[", 2)),
              probe_prob_adjusted = cue_prob*probe_prob_local)
  d_global <- d %>%
    group_by(probe) %>%
    summarise(probe_prob_global = sum(probe_prob_adjusted))
  print(tree)
  d %>% left_join(d_global) %>%
    ##~~~~~~~~~~~~~~~ THIS IS WHERE THE GENERATIVE ALGORITHM GOES ~~~~~~~~~~~~~~~~~~
    mutate(LTM = (probe_prob_global + probe_prob_local)*.5,
           WM = as.numeric(LTM > .3),
           RT = 3 - (2*LTM + WM),
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           condition = paste(cue, "→", probe, "\n", "LTM", LTM, ",", "WM", WM)) %>%
    ggplot(aes(condition, RT)) +
    geom_bar(aes(fill = probe_prob_local == 0 | probe_prob_local == 1), stat = "identity") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = "Global (LTM) and Local (WM) Outcome Probability", y = "RT")
}

exp1.1 <- spreading_activation_wm_mod(c(.8, .2), list(c(.8, .2), c(.2, .8)))

exp1.2 <- spreading_activation_wm_mod(c(.2, .2, .6), list(c(.67, .33), c(.33, .67), c(1, 0)))
