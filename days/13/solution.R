# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./days/13/input.txt") {
    dots <- readLines(file_path)
    dot_coords <- data.frame(
        coords = grep("fold along|^$", dots, invert = TRUE, value = TRUE),
        stringsAsFactors = FALSE
    ) %>%
        tidyr::separate(coords, c("row", "col"), ",") %>%
        dplyr::mutate_all(~as.integer(.) + 1) %>%
        as.matrix()

    matrix_nrow <- max(dot_coords[, "row"]) + (max(dot_coords[, "row"]) %% 2 == 0)
    matrix_ncol <- max(dot_coords[, "col"]) + (max(dot_coords[, "col"]) %% 2 == 0)

    dot_matrix <- matrix(0, matrix_nrow, matrix_ncol)
    dot_matrix[dot_coords] <- 1

    folds <- grep("fold along ", dots, value = TRUE)
    folds <- gsub("fold along ", "", folds)

    return(
        list(
            dot_matrix = dot_matrix,
            folds = folds
        )
    )
}

# Helpers ----------------------------------------------------------------
fold_paper <- function(input, idx, return_letters = FALSE) {
    dot_matrix <- input$dot_matrix
    folds <- input$folds

    for (i in idx) {
        fold <- folds[i] %>% strsplit("=") %>% unlist()
        direction <- fold[1]
        coord <- fold[2] %>% as.integer() + 1

        range_1 <- 1:(coord - 1)

        if (direction == "x") {
            range_2 <- (coord + 1):nrow(dot_matrix)
            dot_matrix_1 <- dot_matrix[range_1, ]
            dot_matrix_2 <- dot_matrix[range_2, ] %>% apply(2, rev)
        } else if (direction == "y") {
            range_2 <- (coord + 1):ncol(dot_matrix)
            dot_matrix_1 <- dot_matrix[, range_1]
            dot_matrix_2 <- dot_matrix[, range_2] %>% apply(1, rev) %>% t()
        }

        dot_matrix <- dot_matrix_1 + dot_matrix_2
    }

    if (!return_letters) {
        return(sum(dot_matrix > 0))
    } else {
        dot_matrix[dot_matrix > "0"] <- "#"
        dot_matrix[dot_matrix == "0"] <- "."
        return(dot_matrix)
    }
}

# Puzzle 1 ---------------------------------------------------------------
num_dots <- read_input() %>% fold_paper(1)
cat("Puzzle 1 solution:", num_dots, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
dot_matrix <- read_input() %>% fold_paper(1:12, TRUE) %>% t()
cat("Puzzle 2 solution:", fill = TRUE)
for (i in 1:nrow(dot_matrix)) cat(dot_matrix[i, ], fill = TRUE)
