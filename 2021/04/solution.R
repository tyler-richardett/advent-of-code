# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/04/input.txt") {
    input <- readLines(file_path)
    drawn_numbers <- input[1] %>% strsplit(",") %>% unlist() %>% as.integer()

    boards <- list()
    i <- 1
    for (j in seq(3, 597, 6)) {
        boards[[i]] <- matrix(
            input[j:(j + 4)] %>%
                trimws() %>%
                strsplit("\\s+") %>%
                unlist() %>%
                as.integer(),
            nrow = 5,
            ncol = 5
        ) %>% t()

        i <- i + 1
    }

    return(
        list(
            drawn_numbers = drawn_numbers,
            boards = boards
        )
    )
}

# Helpers ----------------------------------------------------------------
mark_space <- function(board, value) {
    board[board == value] <- -1
    return(board)
}

board_wins <- function(board) {
    winning_row <- any(rowSums(board) == -5)
    winning_col <- any(colSums(board) == -5)
    return(winning_row | winning_col)
}

calculate_score <- function(boards, has_bingo, value) {
    j <- which(has_bingo)
    bingo <- boards[[j]]
    score <- sum(bingo[bingo != -1]) * value
    return(score)
}

# Puzzle 1 ---------------------------------------------------------------
input <- read_input()
drawn_numbers <- input$drawn_numbers
boards <- input$boards

for (i in drawn_numbers) {
    boards <- lapply(boards, mark_space, value = i)
    has_bingo <- lapply(boards, board_wins) %>% unlist()

    if (any(has_bingo)) {
        cat("Puzzle 1 solution:", calculate_score(boards, has_bingo, i), fill = TRUE)
        break
    }
}

# Puzzle 2 ---------------------------------------------------------------
input <- read_input()
drawn_numbers <- input$drawn_numbers
boards <- input$boards

for (i in drawn_numbers) {
    boards <- lapply(boards, mark_space, value = i)
    has_bingo <- lapply(boards, board_wins) %>% unlist()

    if (sum(!has_bingo) == 1) last_to_win <- !has_bingo
    if (sum(!has_bingo) == 0) {
        cat("Puzzle 2 solution:", calculate_score(boards, last_to_win, i), fill = TRUE)
        break
    }
}
