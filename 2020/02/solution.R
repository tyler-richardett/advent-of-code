# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2020/02/input.txt") {
    password_rules <- readLines(file_path)
    password_rules <- data.frame(rule = password_rules, stringsAsFactors = FALSE) %>%
        tidyr::separate(rule, c("minimum", "maximum", "value", "password"), "-|:? ") %>%
        dplyr::mutate_at(c("minimum", "maximum"), as.integer)

    return(password_rules)
}

# Helpers ----------------------------------------------------------------
count_valid_passwords <- function(password_rules, between) {
    if (between) {
        valid_passwords <- password_rules %>%
            dplyr::mutate(
                count_value = stringr::str_count(password, value),
                is_valid = minimum <= count_value & count_value <= maximum
            )
    } else {
        valid_passwords <- password_rules %>%
            dplyr::mutate(
                is_min_value = stringr::str_sub(password, minimum, minimum) == value,
                is_max_value = stringr::str_sub(password, maximum, maximum) == value,
                is_valid = is_min_value + is_max_value == 1
            )
    }

    valid_passwords <- valid_passwords %>% dplyr::filter(is_valid) %>% nrow()
    return(valid_passwords)
}

# Puzzle 1 ---------------------------------------------------------------
num_valid_passwords <- read_input() %>% count_valid_passwords(TRUE)
cat("Puzzle 1 solution:", num_valid_passwords, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
num_valid_passwords <- read_input() %>% count_valid_passwords(FALSE)
cat("Puzzle 2 solution:", num_valid_passwords, fill = TRUE)
