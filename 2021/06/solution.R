# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/06/input.txt") {
    timers <- readLines(file_path)
    timers <- timers %>% strsplit(",") %>% unlist() %>% as.integer()

    lanternfish <- data.frame(timer = timers) %>%
        dplyr::group_by(timer) %>%
        dplyr::summarize(num_fish = dplyr::n()) %>%
        dplyr::ungroup() %>%
        tidyr::complete(timer = 0:8, fill = list(num_fish = 0))

    return(lanternfish)
}

# Helpers ----------------------------------------------------------------
simulate_days <- function(lanternfish, num_days) {
    for (i in 1:num_days) {
        new_spawns <- lanternfish %>%
            dplyr::filter(timer == 0L) %>%
            dplyr::pull(num_fish)

        lanternfish <- lanternfish %>%
            dplyr::mutate(
                timer = timer - 1L,
                num_fish = ifelse(timer == 6L, num_fish + new_spawns, num_fish)
            ) %>%
            dplyr::filter(timer >= 0L) %>%
            dplyr::bind_rows(
                data.frame(
                    timer = 8L,
                    num_fish = new_spawns
                )
            )
    }

    return(lanternfish)
}

# Puzzle 1 ---------------------------------------------------------------
lanternfish <- read_input() %>% simulate_days(80)
cat("Puzzle 1 solution:", sum(lanternfish$num_fish), fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
lanternfish <- read_input() %>% simulate_days(256)
cat("Puzzle 2 solution:", as.character(sum(lanternfish$num_fish)), fill = TRUE)
