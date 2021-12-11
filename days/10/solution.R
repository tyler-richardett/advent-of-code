# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./days/10/input.txt") {
    subsystem_lines <- data.frame(
        subsystem_line = readLines(file_path),
        stringsAsFactors = FALSE
    ) %>%
        dplyr::mutate(
            simplified = purrr:::map_chr(subsystem_line, simplify_line),
            line_type = ifelse(
                grepl(CLOSING_CHARACTERS, simplified),
                "corrupted",
                "incomplete"
            )
        )

    return(subsystem_lines)
}

# Helpers ----------------------------------------------------------------
OPENING_CHARACTERS <- "\\(|\\[|\\{|<"
CLOSING_CHARACTERS <- "\\)|\\]|\\}|>"
COMPLETE_SETS <- "\\(\\)|\\[\\]|\\{\\}|<>"
SCORING <- data.frame(
    char = c(")", "]", "}", ">", "(", "[", "{", "<"),
    score = c(3, 57, 1197, 25137, 1:4),
    stringsAsFactors = FALSE
)

simplify_line <- function(subsystem_line) {
    while (grepl(COMPLETE_SETS, subsystem_line)) {
        subsystem_line <- gsub(COMPLETE_SETS, "", subsystem_line)
    }

    return(subsystem_line)
}

score_corrupted <- function(subsystem_lines) {
    score <- subsystem_lines %>%
        dplyr::filter(line_type == "corrupted") %>%
        dplyr::mutate(
            illegal_character = purrr::map_chr(simplified, ~substr(gsub(OPENING_CHARACTERS, "", .x), 1, 1)),
            score = purrr::map_dbl(illegal_character, ~SCORING$score[SCORING$char == .])
        ) %>%
        dplyr::pull(score)

    return(sum(score))
}

score_incomplete <- function(subsystem_lines) {
    tally_score <- function(simplified) {
        score <- 0
        for (i in nchar(simplified):1) {
            score <- score * 5
            score <- score + SCORING$score[SCORING$char == substr(simplified, i, i)]
        }
        return(score)
    }

    score <- subsystem_lines %>%
        dplyr::filter(line_type == "incomplete") %>%
        dplyr::mutate(score = purrr::map_dbl(simplified, tally_score)) %>%
        dplyr::pull(score)

    return(median(score))
}

# Puzzle 1 ---------------------------------------------------------------
syntax_error_score <- read_input() %>% score_corrupted()
cat("Puzzle 1 solution:", syntax_error_score, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
syntax_middle_score <- read_input() %>% score_incomplete()
cat("Puzzle 2 solution:", syntax_middle_score, fill = TRUE)
