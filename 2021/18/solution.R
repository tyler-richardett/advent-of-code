# Imports ----------------------------------------------------------------
library(magrittr)
library(zeallot)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/18/input.txt") {
    snail_numbers <- readLines(file_path)
    return(snail_numbers)
}

# Helpers ----------------------------------------------------------------
snail_add <- function(n1, n2) {
    snail_number <- glue::glue("[{n1},{n2}]")
    return(snail_number)
}

can_explode <- function(snail_number) {
    snail_split <- strsplit(snail_number, "") %>% unlist()
    snail_split <- dplyr::case_when(
        snail_split == "[" ~ 1,
        snail_split == "]" ~ -1,
        TRUE ~ 0
    )
    snail_split <- cumsum(snail_split)

    if (any(snail_split > 4)) {
        start_idx <- which(snail_split > 4) %>% dplyr::first()
        end_idx <- which(snail_split > 4 & dplyr::lead(snail_split) < snail_split) %>% dplyr::first() %>% add(1)
        return(list(TRUE, start_idx, end_idx))
    } else {
        return(list(FALSE, NA, NA))
    }
}

snail_explode <- function(snail_number, start_idx, end_idx) {
    pair_numbers <- substr(snail_number, start_idx, end_idx) %>% strsplit("\\[|,|\\]") %>% unlist() %>% as.integer() %>% extract(!is.na(.))
    pair_left <- pair_numbers[1]
    pair_right <- pair_numbers[2]

    regular_numbers <- stringr::str_locate_all(snail_number, stringr::regex("[0-9]+"))[[1]] %>% as.data.frame()
    regular_left <- regular_numbers %>% dplyr::filter(end <= start_idx) %>% tail(1)
    regular_right <- regular_numbers %>% dplyr::filter(start >= end_idx) %>% head(1)

    if (nrow(regular_left) > 0) {
        regular_number <- substr(snail_number, regular_left$start, regular_left$end) %>%
            as.integer() %>%
            add(pair_left)

        snail_number <- substr(snail_number, 1, regular_left$start - 1) %>%
            paste0(regular_number, collapse = "") %>%
            paste0(substr(snail_number, regular_left$end + 1, nchar(snail_number)), collapse = "")

        if (regular_number - pair_left <= 9 & regular_number > 9) {
            regular_right <- regular_right + 1
            start_idx <- start_idx + 1
            end_idx <- end_idx + 1
        }
    }

    if (nrow(regular_right) > 0) {
        regular_number <- substr(snail_number, regular_right$start, regular_right$end) %>%
            as.integer() %>%
            add(pair_right)

        snail_number <- substr(snail_number, 1, regular_right$start - 1) %>%
            paste0(regular_number, collapse = "") %>%
            paste0(substr(snail_number, regular_right$end + 1, nchar(snail_number)), collapse = "")
    }

    snail_number <- substr(snail_number, 1, start_idx - 1) %>%
        paste0(0, collapse = "") %>%
        paste0(substr(snail_number, end_idx + 1, nchar(snail_number)), collapse = "")

    return(snail_number)
}

can_split <- function(snail_number) {
    split_number <- stringr::str_locate(snail_number, stringr::regex("[0-9]{2}")) %>% as.data.frame() %>% dplyr::filter(!is.na(start))
    if (nrow(split_number) > 0) {
        return(list(TRUE, split_number$start, split_number$end))
    } else {
        return(list(FALSE, NA, NA))
    }
}

snail_split <- function(snail_number, start_idx, end_idx) {
    regular_number <- substr(snail_number, start_idx, end_idx) %>% as.integer()

    pair_left <- floor(regular_number / 2)
    pair_right <- ceiling(regular_number / 2)

    snail_number <- substr(snail_number, 1, start_idx - 1) %>%
        paste0(glue::glue("[{pair_left},{pair_right}]"), collapse = "") %>%
        paste0(substr(snail_number, end_idx + 1, nchar(snail_number)), collapse = "")

    return(snail_number)
}

snail_reduce <- function(snail_number) {
    while (TRUE) {
        c(explode_bool, explode_start_idx, explode_end_idx) %<-% can_explode(snail_number)
        c(split_bool, split_start_idx, split_end_idx) %<-% can_split(snail_number)

        if (explode_bool) {
            snail_number <- snail_explode(snail_number, explode_start_idx, explode_end_idx)
        } else if (split_bool) {
            snail_number <- snail_split(snail_number, split_start_idx, split_end_idx)
        } else {
            break
        }
    }

    return(snail_number)
}

snail_iter <- function(snail_numbers) {
    snail_number <- snail_numbers[1]

    for (i in 2:length(snail_numbers)) {
        snail_number <- snail_number %>%
            snail_add(snail_numbers[i]) %>%
            snail_reduce()
    }

    return(snail_number)
}

snail_magnitude <- function(snail_number) {
    snail_number <- gsub("\\[", "(", snail_number)
    snail_number <- gsub("\\]", ")", snail_number)
    snail_number <- gsub(",", "* 3 + 2 *", snail_number)

    magnitude <- eval(parse(text = snail_number))
    return(magnitude)
}

snail_largest_pair <- function(snail_numbers) {
    snail_pairs <- tidyr::crossing(x = 1:100, y = 1:100) %>%
        dplyr::filter(x != y) %>%
        dplyr::mutate(
            snail_number = purrr::map2_chr(x, y, ~snail_add(snail_numbers[.x], snail_numbers[.y]) %>% snail_reduce()),
            magnitude = purrr::map_dbl(snail_number, ~snail_magnitude(.x))
        )

    largest_pair <- max(snail_pairs$magnitude)
    return(largest_pair)
}

# Puzzle 1 ---------------------------------------------------------------
magnitude <- read_input() %>% snail_iter() %>% snail_magnitude()
cat("Puzzle 1 solution:", magnitude, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
largest_pair <- read_input() %>% snail_largest_pair()
cat("Puzzle 2 solution:", largest_pair, fill = TRUE)
