# Imports ----------------------------------------------------------------
library(magrittr)
library(zeallot)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/20/input.txt") {
    trench_map <- readLines(file_path)

    algorithm <- trench_map[1] %>%
        strsplit("") %>%
        unlist() %>%
        replace(. == "#", 1L) %>%
        replace(. == ".", 0L) %>%
        as.integer()

    image_matrix <- trench_map[3:length(trench_map)] %>%
        strsplit("") %>%
        unlist() %>%
        replace(. == "#", 1L) %>%
        replace(. == ".", 0L) %>%
        as.integer() %>%
        matrix(nrow = 100, byrow = TRUE)

    rbind_padding <- matrix(0L, nrow = 55L, ncol = ncol(image_matrix))
    image_matrix <- rbind(rbind_padding, image_matrix, rbind_padding)

    cbind_padding <- matrix(0L, nrow = nrow(image_matrix), ncol = 55L)
    image_matrix <- cbind(cbind_padding, image_matrix, cbind_padding)

    lights <- tidyr::crossing(row = 1:nrow(image_matrix), col = 1:nrow(image_matrix)) %>%
        dplyr::mutate(value = purrr::map2_int(row, col, ~image_matrix[.x, .y]))

    return(list(algorithm, lights))
}

# Helpers ----------------------------------------------------------------
enhance_image <- function(input, steps) {
    c(algorithm, lights) %<-% input

    for (i in steps) {
        adjacent_values <- lights %>%
            tidyr::crossing(
                row_shift = rep(-1:1, each = 3),
                col_shift = rep(-1:1, times = 3)
            ) %>%
            dplyr::mutate(
                row2 = row + row_shift,
                col2 = col + col_shift
            ) %>%
            dplyr::select(row, col, row2, col2) %>%
            dplyr::left_join(lights, c("row2" = "row", "col2" = "col")) %>%
            dplyr::group_by(row, col) %>%
            dplyr::summarize(value = paste0(value, collapse = ""), .groups = "drop") %>%
            dplyr::mutate(value = algorithm[strtoi(value, 2) + 1]) %>%
            tidyr::fill(value, .direction = "downup")

        lights <- lights %>%
            dplyr::select(-value) %>%
            dplyr::left_join(adjacent_values, c("row", "col"))
    }

    num_lights <- sum(lights$value > 0)
    return(num_lights)
}

# Puzzle 1 ---------------------------------------------------------------
num_lights <- read_input() %>% enhance_image(1:2)
cat("Puzzle 1 solution:", num_lights, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
num_lights <- read_input() %>% enhance_image(1:50)
cat("Puzzle 2 solution:", num_lights, fill = TRUE)
