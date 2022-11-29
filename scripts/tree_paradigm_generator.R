# This is a tool for generating new experimental paradigms (with tree depth = 3)
# tree: a tibble with the following columns (example at bottom):
  #   start_cue = <chr>
  #   start_cue_p = <dbl>
  #   mid_cue = <list of length start_cue>
  #   mid_cue_p = <list of length start_cue>
  #   probe = <list of lists>
  #   probe_p = <list of lists>

draw_tree <- function(tree){
  # Unnest and compute basic ps
  tree <- tree %>%
    unnest(cols = c(mid_cue, mid_cue_p, probe, probe_p)) %>%
    unnest(cols = c(probe, probe_p)) %>%
    mutate(p_posterior_mid = start_cue_p*mid_cue_p,
           p_posterior = start_cue_p*mid_cue_p*probe_p)
  
  # Global p and filtering out impossibilities
  tree <- tree %>%
    group_by(probe) %>%
    summarise(p_global = sum(p_posterior)) %>%
    right_join(tree) %>%
    filter(start_cue_p != 0
           & mid_cue_p != 0
           & probe_p != 0)
  
  # Associative: times x/y occurred after b/a (first or second - if both count twice) * the number of B/As in current
  tree <- tree %>%
    pivot_longer(c(start_cue, mid_cue), values_to = "start_cue") %>%
    group_by(start_cue, probe) %>%
      summarise(probe_with_start_cue = sum(p_posterior)) %>%
      ungroup() %>%
    right_join(tree, by = c("start_cue", "probe"))
  tree <- tree %>%
    pivot_longer(c(start_cue, mid_cue), values_to = "mid_cue") %>%
    group_by(mid_cue, probe) %>%
      summarise(probe_with_mid_cue = sum(p_posterior)) %>%
      ungroup() %>%
    right_join(tree, by = c("mid_cue", "probe"))
  
  tree <- tree %>%
    mutate(p_associative = (probe_with_start_cue + probe_with_mid_cue)/2)  %>%
    arrange(probe) %>%
    arrange(mid_cue) %>%
    arrange(start_cue)
  
  # Plot
  tree %>%
    mutate(start_cue_y = rep(seq_along(rle(start_cue)$values), times = rle(start_cue)$lengths)*mean(rle(start_cue)$lengths) - .25*mean(rle(start_cue)$lengths),
           mid_cue_y = rep(seq_along(rle(paste(start_cue, mid_cue))$values), times = rle(paste(start_cue, mid_cue))$lengths)*mean(rle(paste(start_cue, mid_cue))$lengths),
           probe_y = rep(seq_along(rle(paste(start_cue, mid_cue, probe))$values), times = rle(paste(start_cue, mid_cue, probe))$lengths)*mean(rle(paste(start_cue, mid_cue, probe))$lengths) + .5**mean(rle(probe)$lengths)) %>%
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
    geom_tile(aes(x = 4, y = probe_y + .2, width = 1.5, height = .2, fill = probe_p, alpha = probe_p)) +
    geom_tile(aes(x = 4, y = probe_y, width = 1.5, height = .2, fill = p_associative, alpha = p_associative)) +
    geom_tile(aes(x = 4, y = probe_y - .2, width = 1.5, height = .2, fill = p_global, alpha = p_global)) +
  
    geom_text(aes(x = 4, y = probe_y, label = paste("conditional = ", probe_p, "\n",
                                                    "associative = ", p_associative
                                                    ,"\n", "global = ", p_global
                                                    ))) +

    theme_void() +
    theme(legend.position = "none") +
    scale_fill_gradient(low = "green", high = "green") + 
    scale_alpha_continuous(range = c(.1, 1)) + 
    scale_x_continuous(limits = c(0, 5)) +
    scale_y_continuous(limits = c(1, length(tree$probe) + 1))
  }

# Example
tibble(start_cue =           c("A",                            "B"),
       start_cue_p =         c(.5,                             .5),
       mid_cue =   list(     c("A",         "B"),            c("A",         "B")),
       mid_cue_p = list(     c(.9,          .1),             c(0,          1)),
       probe =     list(list(c("X", "Y"), c("X", "Y")), list(c("X", "Y"), c("X", "Y"))),
       probe_p =   list(list(c( .9, .1), c(  .9,   .1)), list(c( .9,  .1), c( .1,  .9)))
       ) %>% draw_tree()

