# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/08/input.txt") {
    displays <- readLines(file_path)
    displays <- displays %>% strsplit(" \\| ")
    return(displays)
}

# Helpers ----------------------------------------------------------------
count_unique_segments <- function(display) {
    output_display <- display[2] %>% strsplit(" ") %>% unlist()
    unique_segments <- nchar(output_display) %in% c(2:4, 7) %>% sum()
    return(unique_segments)
}

contains_all_letters <- function(patterns, code) {
    code <- code %>% strsplit("") %>% unlist()
    code <- glue::glue("(?=.*{code})") %>% paste0(collapse = "")
    contains_mask <- grepl(code, patterns, perl = TRUE)
    return(contains_mask)
}

which_letters_missing <- function(code) {
    code <- code %>% strsplit("") %>% unlist()
    letters_missing <- letters[1:7][!letters[1:7] %in% code]
    return(letters_missing)
}

decode_output <- function(display) {
    d <- c(rep(NA, 10))
    names(d) <- as.character(0:9)

    patterns <- display[1] %>% strsplit(" ") %>% unlist()

    d[["1"]] <- patterns[nchar(patterns) == 2]
    d[["7"]] <- patterns[nchar(patterns) == 3]
    d[["4"]] <- patterns[nchar(patterns) == 4]
    d[["8"]] <- patterns[nchar(patterns) == 7]

    d[["9"]] <- patterns[nchar(patterns) == 6 & contains_all_letters(patterns, d[["4"]])]
    d[["0"]] <- patterns[nchar(patterns) == 6 & contains_all_letters(patterns, d[["7"]]) & patterns != d[["9"]]]
    d[["6"]] <- patterns[nchar(patterns) == 6 & patterns != d[["9"]] & patterns != d[["0"]]]

    d[["3"]] <- patterns[nchar(patterns) == 5 & contains_all_letters(patterns, d[["7"]])]
    d[["5"]] <- patterns[nchar(patterns) == 5 & !grepl(which_letters_missing(d[["6"]]), patterns) & !grepl(which_letters_missing(d[["9"]]), patterns)]
    d[["2"]] <- patterns[nchar(patterns) == 5 & patterns != d[["3"]] & patterns != d[["5"]]]

    output <- display[2] %>% strsplit(" ") %>% unlist()
    output <- lapply(output, function(x) names(which(contains_all_letters(d, x) & nchar(d) == nchar(x))))
    output <- output %>% unlist() %>% paste0(collapse = "") %>% as.integer()

    return(output)
}

# Puzzle 1 ---------------------------------------------------------------
unique_segments <- read_input() %>% sapply(count_unique_segments) %>% sum()
cat("Puzzle 1 solution:", unique_segments, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
min_distance <- read_input() %>% sapply(decode_output) %>% sum()
cat("Puzzle 2 solution:", min_distance, fill = TRUE)
