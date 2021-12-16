# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/15/input.txt") {
    cavern <- readLines(file_path)
    cavern <- cavern %>% strsplit("") %>% unlist() %>% as.integer()

    cavern_matrix <- matrix(cavern, nrow = 100, ncol = 100, byrow = TRUE)
    return(cavern_matrix)
}

# Helpers ----------------------------------------------------------------
enumerate_risks <- function(cavern_matrix, five_times_larger) {
    if (five_times_larger) {
        original_matrix <- cavern_matrix
        for (i in 1:4) cavern_matrix <- cbind(cavern_matrix, original_matrix + i)

        original_row <- cavern_matrix
        for (i in 1:4) cavern_matrix <- rbind(cavern_matrix, original_row + i)

        cavern_matrix[cavern_matrix > 9] <- cavern_matrix[cavern_matrix > 9] - 9L
    }

    num_cols <- ncol(cavern_matrix)
    num_cells <- num_cols ^ 2

    cavern_risks <- data.frame(from = rep(1:num_cells, 4)) %>%
        dplyr::arrange(from) %>%
        dplyr::mutate(
            direction = rep(c("up", "down", "left", "right"), num_cells),
            to = dplyr::case_when(
                direction == "up" ~ from - 1L,
                direction == "down" ~ from + 1L,
                direction == "left" ~ from - num_cols,
                direction == "right" ~ from + num_cols
            )
        ) %>%
        dplyr::filter(
            !(from %% num_cols == 1 & direction == "up"),
            !(from %% num_cols == 0 & direction == "down"),
            !(to < 1),
            !(to > num_cells)
        ) %>%
        dplyr::mutate(weight = purrr::map_int(to, ~cavern_matrix[.x])) %>%
        dplyr::select(-direction)

    return(cavern_risks)
}

lowest_risk_path <- function(cavern_risks) {
    to <- max(cavern_risks$to) %>% as.character()
    cavern_graph <- igraph::graph_from_data_frame(cavern_risks)

    lowest_risk <- igraph::shortest_paths(cavern_graph, "1", to)
    lowest_risk <- lowest_risk$vpath[[1]] %>% names() %>% as.integer()

    risk_score <- cavern_risks %>%
        dplyr::filter(to %in% lowest_risk, to != 1) %>%
        dplyr::distinct(to, weight) %>%
        dplyr::pull(weight) %>%
        sum()

    return(risk_score)
}

# Puzzle 1 ---------------------------------------------------------------
risk_score <- read_input() %>% enumerate_risks(FALSE) %>% lowest_risk_path()
cat("Puzzle 1 solution:", risk_score, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
risk_score <- read_input() %>% enumerate_risks(TRUE) %>% lowest_risk_path()
cat("Puzzle 2 solution:", risk_score, fill = TRUE)
