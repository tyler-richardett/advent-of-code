# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
submarine_course <- data.frame(
    direction = readLines("days/02/input.txt")
)

# Solution ---------------------------------------------------------------
submarine_course %>%
    tidyr::separate(direction, c("direction", "step")) %>%
    dplyr::mutate(
        step = as.integer(step),
        horizontal_position = dplyr::case_when(
            direction == "forward" ~ step,
            TRUE ~ 0L
        ),
        depth = dplyr::case_when(
            direction == "down" ~ step,
            direction == "up" ~ -step,
            TRUE ~ 0L
        )
    ) %>%
    dplyr::summarize_at(dplyr::vars(horizontal_position, depth), sum, na.rm = TRUE) %>%
    as.matrix() %>%
    matrixStats::rowProds()
