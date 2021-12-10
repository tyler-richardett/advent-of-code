# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./days/09/input.txt") {
    heights <- readLines(file_path)
    heights <- heights %>% strsplit("") %>% unlist() %>% as.integer()
    heights <- matrix(heights, nrow = 100, ncol = 100, byrow = TRUE)

    height_map <- table(1:100, 1:100) %>%
        as.data.frame() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            x = as.integer(Var2),
            y = as.integer(Var1),
            height = heights[x, y]
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(x:height) %>%
        get_adjacent_values()

    return(height_map)
}

# Helpers ----------------------------------------------------------------
get_adjacent_values <- function(height_map) {
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

    return(adjacent_values)
}

sum_risk_levels <- function(adjacent_values) {
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

find_basins <- function(adjacent_values) {
    edge_origins <- adjacent_values %>%
        dplyr::arrange(x, y) %>%
        dplyr::mutate(idx = dplyr::row_number()) %>%
        dplyr::filter(height < 9)

    edges <- data.frame()
    for (row in split(edge_origins, rownames(edge_origins))) {
        if (row$left < 9)  edges <- edges %>% dplyr::bind_rows(data.frame(from = row$idx, to = row$idx - 1L))
        if (row$right < 9) edges <- edges %>% dplyr::bind_rows(data.frame(from = row$idx, to = row$idx + 1L))
        if (row$above < 9) edges <- edges %>% dplyr::bind_rows(data.frame(from = row$idx, to = row$idx - 100L))
        if (row$below < 9) edges <- edges %>% dplyr::bind_rows(data.frame(from = row$idx, to = row$idx + 100L))
    }

    basins <- tidygraph::tbl_graph(edges = edges) %>% tidygraph::to_components()
    return(basins)
}

get_largest_basin_sizes <- function(basins, n = 3) {
    basin_sizes <- lapply(basins, function(x) x %>% as.data.frame() %>% nrow()) %>% unlist()
    top_n_basins <- sort(basin_sizes, decreasing = TRUE) %>% head(n)

    return(top_n_basins)
}

# Puzzle 1 ---------------------------------------------------------------
risk_level <- read_input() %>% sum_risk_levels()
cat("Puzzle 1 solution:", risk_level, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
largest_basins <- read_input() %>%
    find_basins() %>%
    get_largest_basin_sizes() %>%
    prod()

cat("Puzzle 2 solution:", largest_basins, fill = TRUE)
