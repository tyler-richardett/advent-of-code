# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./days/05/input.txt") {
    vents <- readLines(file_path)
    hydro_vents <- data.frame(
        vent = vents,
        stringsAsFactors = FALSE
    )

    hydro_vents <- hydro_vents %>%
        tidyr::separate(
            vent,
            c("x1", "y1", "x2", "y2"),
            ",| -> "
        ) %>%
        dplyr::mutate_all(as.integer)

    return(hydro_vents)
}

# Helpers ----------------------------------------------------------------
init_matrix <- function(hydro_vents) {
    vent_matrix <- matrix(
        data = 0,
        nrow = max(hydro_vents, na.rm = TRUE),
        ncol = max(hydro_vents, na.rm = TRUE)
    )

    return(vent_matrix)
}

get_coords <- function(row) {
    coords <- data.frame(
        x = row$x1:row$x2,
        y = row$y1:row$y2
    )

    coords <- coords %>% as.matrix()
    return(coords)
}

# Puzzle 1 ---------------------------------------------------------------
hydro_vents <- read_input()
vent_matrix <- init_matrix(hydro_vents)

for (row in split(hydro_vents, rownames(hydro_vents))) {
    coords <- get_coords(row)

    if (row$x1 == row$x2 | row$y1 == row$y2) {
        vent_matrix[coords] <- vent_matrix[coords] + 1
    }
}

cat("Puzzle 1 solution:", sum(vent_matrix >= 2), fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
hydro_vents <- read_input()
vent_matrix <- init_matrix(hydro_vents)

for (row in split(hydro_vents, rownames(hydro_vents))) {
    coords <- get_coords(row)
    vent_matrix[coords] <- vent_matrix[coords] + 1
}

cat("Puzzle 2 solution:", sum(vent_matrix >= 2), fill = TRUE)
