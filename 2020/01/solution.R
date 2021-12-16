# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2020/01/input.txt") {
    expenses <- readLines(file_path)
    expense_report <- data.frame(amount = as.integer(expenses))
    return(expense_report)
}

# Helpers ----------------------------------------------------------------
find_entries <- function(expense_report, num_amounts) {
    expense_report <- expense_report %>%
        dplyr::left_join(expense_report, by = character()) %>%
        dplyr::left_join(expense_report, by = character()) %>%
        dplyr::filter(
            amount.x != amount.y,
            amount.y != amount,
            amount.x != amount,
        )

    if (num_amounts == 2) expense_report <- expense_report %>% dplyr::mutate(summed = amount.x + amount.y)
    if (num_amounts == 3) expense_report <- expense_report %>% dplyr::mutate(summed = amount.x + amount.y + amount)

    multiplied <- expense_report %>%
        dplyr::filter(summed == 2020) %>%
        extract(1:num_amounts) %>%
        as.matrix() %>%
        matrixStats::rowProds() %>%
        unique()

    return(multiplied)
}

# Puzzle 1 ---------------------------------------------------------------
multiplied <- read_input() %>% find_entries(2)
cat("Puzzle 1 solution:", multiplied, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
multiplied <- read_input() %>% find_entries(3)
cat("Puzzle 2 solution:", multiplied, fill = TRUE)
