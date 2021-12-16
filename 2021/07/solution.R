# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/07/input.txt") {
    crab_positions <- readLines(file_path)
    crab_positions <- crab_positions %>% strsplit(",") %>% unlist() %>% as.integer()
    return(crab_positions)
}

# Helpers ----------------------------------------------------------------
sum_distances <- function(start, end, triangle = FALSE) {
    distance <- abs(end - start)
    if (triangle) distance <- distance * (distance + 1) / 2
    return(sum(distance))
}

fmin <- function(crab_positions, triangle = FALSE) {
    ends <- min(crab_positions):max(crab_positions)
    sums <- purrr::map_dbl(ends, ~sum_distances(crab_positions, ., triangle))
    return(sums[which.min(sums)])
}

# Puzzle 1 ---------------------------------------------------------------
min_distance <- read_input() %>% fmin()
cat("Puzzle 1 solution:", min_distance, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
min_distance <- read_input() %>% fmin(triangle = TRUE)
cat("Puzzle 2 solution:", min_distance, fill = TRUE)
