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
  

# This is a tool for generating new experimental paradigms (with tree depth = 3)
# tree: a tibble with the following columns:
  #   start_cue = <chr>
  #   start_cue_p = <dbl>
  #   mid_cue = <list of length start_cue>
  #   mid_cue_p = <list of length start_cue>
  #   probe = <list of lists>
  #   probe_p = <list of lists>

draw_tree <- function(tree){
  tree <- tree %>%
    unnest(cols = c(mid_cue, mid_cue_p, probe, probe_p)) %>%
    unnest(cols = c(probe, probe_p)) %>%
    mutate(p_posterior_mid = start_cue_p*mid_cue_p,
           p_posterior = start_cue_p*mid_cue_p*probe_p)
  
  tree <- tree %>%
    group_by(probe) %>%
    summarise(p_global = sum(p_posterior)) %>%
    right_join(tree) %>%
    arrange(mid_cue) %>%
    arrange(start_cue)
  
  tree %>% 
    mutate(start_cue_y = rep(seq_along(rle(start_cue)$values), times = rle(start_cue)$lengths)*mean(rle(start_cue)$lengths),
           mid_cue_y = rep(seq_along(rle(mid_cue)$values), times = rle(mid_cue)$lengths)*mean(rle(mid_cue)$lengths),
           probe_y = rep(seq_along(rle(probe)$values), times = rle(probe)$lengths)*mean(rle(probe)$lengths)) %>%
    ggplot() +
    # Lines
    geom_segment(aes(x = 1, xend = 1.5, y = start_cue_y, yend = mid_cue_y), size = 3, alpha = .1) +
    geom_segment(aes(x = 1.5, xend = 2, y = mid_cue_y, yend = mid_cue_y), size = 3, alpha = .1) +
    geom_segment(aes(x = 2, xend = 2.5, y = mid_cue_y, yend = probe_y), size = 3, alpha = .1) +
    geom_segment(aes(x = 2.5, xend = 3, y = probe_y, yend = probe_y), size = 3, alpha = .1) +
    # Line Labels
    geom_text(aes(x = 0.5, y = start_cue_y, label = start_cue_p)) +
    geom_text(aes(x = 1.5, y = mid_cue_y, label = mid_cue_p)) +
    geom_text(aes(x = 2.5, y = probe_y, label = probe_p)) +
    # Text
    geom_label(aes(x = 1, y = start_cue_y, label = start_cue), size = 8) +
    geom_label(aes(x = 2, y = mid_cue_y, label = mid_cue), size = 8) +
    geom_label(aes(x = 3, y = probe_y, label = probe), size = 8) +
    # Stats
    geom_text(aes(x = 4, y = probe_y, label = paste("p_conditional = ", probe_p, "\n",
                                                    "p_global = ", p_global,  "\n"))) +
    theme_void() +
    scale_x_continuous(limits = c(0, 5))
}

tibble(start_cue =           c("A",                            "B"),
       start_cue_p =         c(.8,                             .2),
       mid_cue =   list(     c("A",         "B"),            c("A",         "B")),
       mid_cue_p = list(     c(.8,          .2),             c(.2,          .8)),
       probe =     list(list(c("X", "Y"), c("X", "Y")), list(c("X", "Y"), c("X", "Y"))),
       probe_p =   list(list(c( 1,   0), c(  0,   1)), list(c( .8,  .2), c( .2,  .8)))
       ) %>% draw_tree()


