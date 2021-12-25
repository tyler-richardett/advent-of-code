# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/24/input.txt") {
    monad <- readLines(file_path) %>%
        as.data.frame() %>%
        tidyr::separate(".", c("command", "var_1", "var_2"), " ", fill = "right") %>%
        dplyr::mutate(idx = cumsum(command == "inp")) %>%
        dplyr::group_by(idx) %>%
        dplyr::mutate(
            div_z = as.integer(var_2[5]),
            add_x = as.integer(var_2[6]),
            add_y = as.integer(var_2[16])
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(idx, div_z, add_x, add_y)

    return(monad)
}

# Helpers ----------------------------------------------------------------
find_combinations <- function(monad, minimum = FALSE) {
    look_back <- monad %>% dplyr::filter(FALSE)
    combinations <- tidyr::crossing(idx = 1:14, value = 1:9)

    for (row in split(monad, 1:nrow(monad))) {
        if (row$div_z == 1) {
            look_back <- look_back %>% dplyr::bind_rows(row)
        } else {
            prev_row <- look_back %>% tail(1)
            look_back <- look_back %>% dplyr::slice(-dplyr::n())

            delta <- row$add_x + prev_row$add_y
            if (delta > 0) {
                value_exclude <- head(1:9, delta)
                prev_value_exclude <- tail(1:9, delta)
            } else {
                value_exclude <- tail(1:9, -delta)
                prev_value_exclude <- head(1:9, -delta)
            }

            combinations <- combinations %>%
                dplyr::filter(
                    !(idx == row$idx & value %in% value_exclude),
                    !(idx == prev_row$idx & value %in% prev_value_exclude),
                )
        }
    }

    combinations <- combinations %>%
        dplyr::group_by(idx) %>%
        dplyr::mutate(
            is_min = value == min(value),
            is_max = value == max(value)
        ) %>%
        dplyr::ungroup()

    if (!minimum) {
        model_number <- combinations %>%
            dplyr::filter(is_max) %>%
            dplyr::pull(value) %>%
            paste0(collapse = "")
    } else {
        model_number <- combinations %>%
            dplyr::filter(is_min) %>%
            dplyr::pull(value) %>%
            paste0(collapse = "")
    }

    return(model_number)
}

# Puzzle 1 ---------------------------------------------------------------
model_number <- read_input() %>% find_combinations()
cat("Puzzle 1 solution:", model_number, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
model_number <- read_input() %>% find_combinations(TRUE)
cat("Puzzle 2 solution:", model_number, fill = TRUE)
