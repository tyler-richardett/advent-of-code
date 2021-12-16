# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/03/input.txt", remove = TRUE) {
    binaries <- readLines(file_path)
    binary_diagnostic <- data.frame(
        binary = binaries,
        stringsAsFactors = FALSE
    )

    binary_diagnostic <- binary_diagnostic %>%
        tidyr::separate(binary, paste0("binary_", 1:12), sep = "(?<=.)", remove = remove) %>%
        dplyr::mutate_at(paste0("binary_", 1:12), as.integer)

    return(binary_diagnostic)
}

# Helpers ----------------------------------------------------------------
round <- function(x, digits = 0) {
    posneg <- sign(x)
    z <- trunc(abs(x) * 10 ^ (digits + 1)) / 10
    z <- floor(z * posneg + 0.5) / 10 ^ digits
    return(z)
}

get_decimal <- function(rate) {
    decimal_value <- rate %>%
        paste0(collapse = "") %>%
        strtoi(base = 2)

    return(decimal_value)
}

get_rating <- function(binary_diagnostic, co2 = FALSE) {
    i <- 1

    while (nrow(binary_diagnostic) > 1) {
        bit <- next_bit(binary_diagnostic[[paste0("binary_", i)]], co2 = co2)
        binary_diagnostic <- binary_diagnostic %>%
            dplyr::filter(substr(binary, i, i) == bit)
        i <- i + 1
    }

    rating <- binary_diagnostic %>%
        dplyr::pull(binary) %>%
        strtoi(base = 2)

    return(rating)
}

next_bit <- function(bits, co2 = FALSE) {
    mean_bit <- mean(bits, na.rm = TRUE)

    if (co2) {
        if (mean_bit >= 0.5) bit <- 0
        if (mean_bit < 0.5) bit <- 1
    } else {
        bit <- round(mean_bit)
    }

    return(bit)
}

# Puzzle 1 ---------------------------------------------------------------
gamma <- read_input() %>%
    dplyr::summarize_all(~round(mean(., na.rm = TRUE))) %>%
    dplyr::slice(1)

gamma_rate <- get_decimal(gamma)
epsilon_rate <- useful::binary.flip(gamma) %>% get_decimal()

cat("Puzzle 1 solution:", gamma_rate * epsilon_rate, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
oxygen_rating <- read_input(remove = FALSE) %>% get_rating()
co2_rating <- read_input(remove = FALSE) %>% get_rating(co2 = TRUE)

cat("Puzzle 2 solution:", oxygen_rating * co2_rating, fill = TRUE)
