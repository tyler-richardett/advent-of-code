# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/17/input.txt") {
    coordinates <- readLines(file_path)
    coordinates <- regmatches(coordinates, gregexpr("-?[0-9]+", coordinates)) %>%
        unlist() %>%
        as.integer()

    target_area <- data.frame(
        x_min = coordinates[1],
        x_max = coordinates[2],
        y_min = coordinates[3],
        y_max = coordinates[4]
    )

    return(target_area)
}

# Helpers ----------------------------------------------------------------
get_feasible_velocities <- function(target_area) {
    feasible_velocities <- tidyr::crossing(
        x_velocity = 1:target_area$x_max,
        y_velocity = target_area$y_min:250,
        t = 1:750
    ) %>%
        dplyr::mutate(
            x_velocity_t = pmax(x_velocity - t + 1, 0),
            y_velocity_t = y_velocity - t + 1
        ) %>%
        dplyr::group_by(x_velocity, y_velocity) %>%
        dplyr::mutate(
            x_t = cumsum(x_velocity_t),
            y_t = cumsum(y_velocity_t)
        ) %>%
        dplyr::filter(
            any(
                x_t >= target_area$x_min &
                    x_t <= target_area$x_max &
                    y_t >= target_area$y_min &
                    y_t <= target_area$y_max
            )
        ) %>%
        dplyr::ungroup()

    return(feasible_velocities)
}

# Puzzle 1 ---------------------------------------------------------------
max_y <- read_input() %>% get_feasible_velocities() %>% dplyr::pull(y_t) %>% max()
cat("Puzzle 1 solution:", max_y, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
distinct_values <- read_input() %>% get_feasible_velocities() %>% dplyr::distinct(x_velocity, y_velocity) %>% nrow()
cat("Puzzle 2 solution:", distinct_values, fill = TRUE)
