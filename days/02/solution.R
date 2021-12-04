# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./days/02/input.txt") {
    directions <- readLines(file_path)
    submarine_course <- data.frame(direction = directions, stringsAsFactors = FALSE) %>%
        tidyr::separate(direction, c("direction", "step"))

    return(submarine_course)
}

# Helpers ----------------------------------------------------------------
calculate_answer <- function(submarine_course) {
    answer <- submarine_course %>%
        dplyr::summarize_at(dplyr::vars(horizontal_position, depth), sum, na.rm = TRUE) %>%
        as.matrix() %>%
        matrixStats::rowProds()

    return(answer)
}

# Puzzle 1 ---------------------------------------------------------------
submarine_course <- read_input() %>%
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
    )

cat("Puzzle 1 solution:", calculate_answer(submarine_course), fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
submarine_course <- read_input() %>%
    dplyr::mutate(
        step = as.integer(step),
        horizontal_position = dplyr::case_when(
            direction == "forward" ~ step,
            TRUE ~ 0L
        ),
        aim = dplyr::case_when(
            direction == "down" ~ step,
            direction == "up" ~ -step,
            TRUE ~ 0L
        ),
        cumulative_aim = cumsum(aim),
        depth = dplyr::case_when(
            direction == "forward" ~ step * cumulative_aim,
            TRUE ~ 0L
        )
    )

cat("Puzzle 2 solution:", calculate_answer(submarine_course), fill = TRUE)
