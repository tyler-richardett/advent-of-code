# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/22/input.txt") {
    steps <- readLines(file_path) %>%
        stringr::str_replace_all("[a-z]=", "")

    reboot_steps <- data.frame(step = steps, stringsAsFactors = FALSE) %>%
        tidyr::separate(step, c("toggle", "x_min", "x_max", "y_min", "y_max", "z_min", "z_max"), " |\\.\\.|,") %>%
        dplyr::mutate_at(dplyr::vars(!dplyr::matches("toggle")), as.integer)

    return(reboot_steps)
}

# Helpers ----------------------------------------------------------------
reboot_reactor <- function(reboot_steps, x_range = c(-Inf, Inf), y_range = c(-Inf, Inf), z_range = c(-Inf, Inf)) {
    reboot_steps <- reboot_steps %>%
        dplyr::filter(
            dplyr::between(x_min, x_range[1], x_range[2]) | dplyr::between(x_max, x_range[1], x_range[2]),
            dplyr::between(y_min, y_range[1], y_range[2]) | dplyr::between(y_max, y_range[1], y_range[2]),
            dplyr::between(z_min, z_range[1], z_range[2]) | dplyr::between(z_max, z_range[1], z_range[2])
        ) %>%
        dplyr::mutate(multiplier = ifelse(toggle == "on", 1, -1))

    cubes <- reboot_steps %>% dplyr::filter(FALSE) %>% dplyr::select(-toggle)
    for (row in split(reboot_steps, 1:nrow(reboot_steps))) {
        overlapping <- cubes %>%
            dplyr::left_join(row, by = character(), suffix = c("_i", "_j")) %>%
            dplyr::filter(
                x_min_i <= x_max_j & x_max_i >= x_min_j &
                    y_min_i <= y_max_j & y_max_i >= y_min_j &
                    z_min_i <= z_max_j & z_max_i >= z_min_j
            ) %>%
            dplyr::transmute(
                x_min = pmax(x_min_i, x_min_j),
                x_max = pmin(x_max_i, x_max_j),
                y_min = pmax(y_min_i, y_min_j),
                y_max = pmin(y_max_i, y_max_j),
                z_min = pmax(z_min_i, z_min_j),
                z_max = pmin(z_max_i, z_max_j),
                multiplier = -multiplier_i
            )

        cubes <- cubes %>% dplyr::bind_rows(overlapping)
        if (row$toggle == "on") cubes <- cubes %>% dplyr::bind_rows(row)
    }

    num_cubes <- cubes %>%
        dplyr::mutate(volume = (x_max - x_min + 1) * (y_max - y_min + 1) * (z_max - z_min + 1) * multiplier) %>%
        dplyr::pull(volume) %>%
        sum()

    return(num_cubes)
}

# Puzzle 1 ---------------------------------------------------------------
num_cubes <- read_input() %>% reboot_reactor(c(-50, 50), c(-50, 50), c(-50, 50))
cat("Puzzle 1 solution:", num_cubes, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
num_cubes <- read_input() %>% reboot_reactor()
cat("Puzzle 2 solution:", num_cubes %>% as.character(), fill = TRUE)
