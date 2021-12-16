# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/01/input.txt") {
    depths <- readLines(file_path)
    sonar_sweep <- data.frame(depth = as.integer(depths))
    return(sonar_sweep)
}

# Helpers ----------------------------------------------------------------
count_increases <- function(sonar_sweep, col_name) {
    num_increases <- sonar_sweep %>%
        dplyr::mutate(increase = !!as.symbol(col_name) > dplyr::lag(!!as.symbol(col_name))) %>%
        dplyr::pull(increase) %>%
        sum(na.rm = TRUE)

    return(num_increases)
}

# Puzzle 1 ---------------------------------------------------------------
sonar_sweep <- read_input()
cat("Puzzle 1 solution:", count_increases(sonar_sweep, "depth"), fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
sonar_sweep <- read_input() %>%
    dplyr::mutate(rolling_sum = zoo::rollapplyr(depth, 3, sum, fill = NA))

cat("Puzzle 2 solution:", count_increases(sonar_sweep, "rolling_sum"), fill = TRUE)
