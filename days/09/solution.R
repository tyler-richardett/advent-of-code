# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./days/09/input.txt") {
    heights <- readLines(file_path)
    heights <- heights %>% strsplit("") %>% unlist() %>% as.integer()

    height_matrix <- matrix(heights, nrow = 100, ncol = 100, byrow = TRUE)
    return(height_matrix)
}

# Helpers ----------------------------------------------------------------
sum_risk_levels <- function(height_matrix) {
    height_map <- table(1:100, 1:100) %>%
        as.data.frame() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            x = as.integer(Var2),
            y = as.integer(Var1),
            height = height_matrix[x, y]
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(x:height)

    adjacent_values <- height_map %>%
        dplyr::group_by(x) %>%
        dplyr::arrange(y) %>%
        dplyr::mutate(
            left = dplyr::lag(height, default = 100),
            right = dplyr::lead(height, default = 100)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(y) %>%
        dplyr::arrange(x) %>%
        dplyr::mutate(
            above = dplyr::lag(height, default = 100),
            below = dplyr::lead(height, default = 100)
        ) %>%
        dplyr::ungroup()

    risk_levels <- adjacent_values %>%
        dplyr::filter(
            height < left,
            height < right,
            height < above,
            height < below
        ) %>%
        dplyr::mutate(risk_level = height + 1) %>%
        dplyr::pull(risk_level)

    return(sum(risk_levels))
}

prod_largest_basin_sizes <- function(height_matrix, n = 3) {
    height_matrix[height_matrix < 9] <- 1
    height_matrix[height_matrix == 9] <- 0

    basins <- wvtool::cc.label(height_matrix, 4) %>% .$summary
    prod_top_n_basins <- basins %>%
        dplyr::top_n(n, area) %>%
        dplyr::pull(area) %>%
        prod()

    return(prod_top_n_basins)
}

# Puzzle 1 ---------------------------------------------------------------
risk_level <- read_input() %>% sum_risk_levels()
cat("Puzzle 1 solution:", risk_level, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
min_distance <- read_input() %>% prod_largest_basin_sizes()
cat("Puzzle 2 solution:", min_distance, fill = TRUE)
