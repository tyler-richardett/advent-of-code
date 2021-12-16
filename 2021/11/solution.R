# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/11/input.txt") {
    dumbo <- readLines(file_path)
    dumbo <- dumbo %>% strsplit("") %>% unlist() %>% as.integer()

    dumbo_matrix <- matrix(dumbo, nrow = 10, ncol = 10, byrow = TRUE)
    return(dumbo_matrix)
}

# Helpers ----------------------------------------------------------------
single_step <- function(dumbo_matrix) {
    flashed <- matrix(nrow = 0, ncol = 2)

    dumbo_matrix <- dumbo_matrix + 1
    will_flash <- which(dumbo_matrix > 9, arr.ind = TRUE)

    while (nrow(will_flash) > 0) {
        for (coords in split(will_flash, 1:nrow(will_flash))) {
            x <- coords[1]
            y <- coords[2]
            to_increase <- expand.grid(row = (x - 1):(x + 1), col = (y - 1):(y + 1)) %>%
                dplyr::filter(
                    !(row == x & col == y),
                    dplyr::between(row, 1, 10),
                    dplyr::between(col, 1, 10),
                ) %>%
                as.matrix()

            dumbo_matrix[to_increase] <- dumbo_matrix[to_increase] + 1
        }

        flashed <- flashed %>% rbind(will_flash)
        dumbo_matrix[will_flash] <- 100

        will_flash <- which(dumbo_matrix > 9 & dumbo_matrix < 100, arr.ind = TRUE)
    }

    flashes <- nrow(flashed)
    dumbo_matrix[flashed] <- 0

    return(
        list(
            dumbo_matrix = dumbo_matrix,
            flashes = flashes
        )
    )
}

# Puzzle 1 ---------------------------------------------------------------
dumbo_matrix <- read_input()
flashes <- 0

for (i in 1:100) {
    step_result <- single_step(dumbo_matrix)
    dumbo_matrix <- step_result$dumbo_matrix
    flashes <- flashes + step_result$flashes
}

cat("Puzzle 1 solution:", flashes, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
dumbo_matrix <- read_input()

for (i in 1:10000) {
    step_result <- single_step(dumbo_matrix)
    dumbo_matrix <- step_result$dumbo_matrix
    if (step_result$flashes == 100) break
}

cat("Puzzle 2 solution:", i, fill = TRUE)
