# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./days/12/input.txt") {
    edges <- readLines(file_path)
    edges <- data.frame(
        edge = edges,
        stringsAsFactors = FALSE
    ) %>%
        tidyr::separate(edge, c("from", "to"), "-")

    edges <- edges %>%
        dplyr::bind_rows(
            edges %>%
                dplyr::rename(from_ = from, to_ = to) %>%
                dplyr::select(from = to_, to = from_)
        )

    return(edges)
}

# Helpers ----------------------------------------------------------------
get_paths <- function(edges, small_cave_twice) {
    remaining_paths <- edges %>%
        dplyr::filter(from == "start") %>%
        dplyr::mutate(path = glue::glue("{from},{to}")) %>%
        dplyr::select(path, to)

    paths <- data.frame()
    while (nrow(remaining_paths) > 0) {
        remaining_paths <- remaining_paths %>%
            dplyr::rename(from = to) %>%
            dplyr::left_join(edges, "from") %>%
            dplyr::filter(
                to != "start",
                purrr::map2_lgl(to, path, ~is_valid_route(.x, .y, small_cave_twice))
            ) %>%
            dplyr::mutate(path = glue::glue("{path},{to}")) %>%
            dplyr::select(path, to)

        paths <- paths %>%
            dplyr::bind_rows(
                remaining_paths %>%
                    dplyr::filter(to == "end") %>%
                    dplyr::select(path)
            )

        remaining_paths <- remaining_paths %>%
            dplyr::filter(to != "end")
    }

    return(paths)
}

is_valid_route <- function(to, path, small_cave_twice = FALSE) {
    destination_in_path <- grepl(to, path)
    large_cave_exception <- grepl("[A-Z]", to)

    if (!small_cave_twice) {
        valid_route <- !(destination_in_path & !large_cave_exception)
    } else {
        small_caves <- path %>% strsplit(",") %>% unlist()
        small_caves <- grep("^[a-z][a-z]$", small_caves, value = TRUE)

        small_cave_exception_met <- any(stringr::str_count(path, small_caves) == 2)
        small_cave_exception <- stringr::str_count(path, to) < 2 & grepl("^[a-z][a-z]$", to)

        valid_route <- !(destination_in_path & !(large_cave_exception | (small_cave_exception & !small_cave_exception_met)))
    }

    return(valid_route)
}

# Puzzle 1 ---------------------------------------------------------------
num_paths <- read_input() %>% get_paths(FALSE) %>% nrow()
cat("Puzzle 1 solution:", num_paths, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
num_paths <- read_input() %>% get_paths(TRUE) %>% nrow()
cat("Puzzle 2 solution:", num_paths, fill = TRUE)
