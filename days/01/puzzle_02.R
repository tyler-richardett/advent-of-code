# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
sonar_sweep <- data.frame(
    depth = as.integer(readLines("days/01/input.txt"))
)

# Solution ---------------------------------------------------------------
sonar_sweep %>%
    dplyr::mutate(
        rolling_sum = zoo::rollapplyr(depth, 3, sum, fill = NA),
        increase = rolling_sum > dplyr::lag(rolling_sum)
    ) %>%
    dplyr::pull(increase) %>%
    sum(na.rm = TRUE)
