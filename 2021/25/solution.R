# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/25/input.txt") {
    floor_map <- readLines(file_path)

    sea_cucumber_matrix <- floor_map %>%
        strsplit("") %>%
        unlist() %>%
        matrix(nrow = length(floor_map), ncol = nchar(floor_map[1]), byrow = TRUE)

    sea_cucumbers <- tidyr::crossing(
        row = 1:nrow(sea_cucumber_matrix),
        col = 1:ncol(sea_cucumber_matrix)
    ) %>%
        dplyr::mutate(value = purrr::map2_chr(row, col, ~sea_cucumber_matrix[.x, .y]))

    return(sea_cucumbers)
}

# Helpers ----------------------------------------------------------------
find_step <- function(sea_cucumbers) {
    prev_df <- sea_cucumbers
    i <- 1

    while (TRUE) {
        new_df <- prev_df %>%
            dplyr::group_by(row) %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    value == "." & dplyr::lag(value, default = "#") == ">" ~ ">",
                    dplyr::row_number() == 1 & value == "." & dplyr::last(value) == ">" ~ ">",
                    value == ">" & dplyr::lead(value, default = "#") == "." ~ ".",
                    dplyr::row_number() == dplyr::n() & value == ">" & dplyr::first(value) == "." ~ ".",
                    TRUE ~ value
                )
            ) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(col) %>%
            dplyr::mutate(
                value = dplyr::case_when(
                    value == "." & dplyr::lag(value, default = "#") == "v" ~ "v",
                    dplyr::row_number() == 1 & value == "." & dplyr::last(value) == "v" ~ "v",
                    value == "v" & dplyr::lead(value, default = "#") == "." ~ ".",
                    dplyr::row_number() == dplyr::n() & value == "v" & dplyr::first(value) == "." ~ ".",
                    TRUE ~ value
                )
            ) %>%
            dplyr::ungroup()

        if (identical(prev_df, new_df)) break
        prev_df <- new_df
        i <- i + 1
    }

    return(i)
}

# Puzzle 1 ---------------------------------------------------------------
first_step <- read_input() %>% find_step()
cat("Puzzle 1 solution:", first_step, fill = TRUE)
