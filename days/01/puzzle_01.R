# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
sonar_sweep <- data.frame(
    depth = as.integer(readLines("days/01/input.txt"))
)

# Solution ---------------------------------------------------------------
sonar_sweep %>%
    dplyr::mutate(increase = depth > dplyr::lag(depth)) %>%
    dplyr::pull(increase) %>%
    sum(na.rm = TRUE)
