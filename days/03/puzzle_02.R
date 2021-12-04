# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
binary_diagnostic <- data.frame(
    binary = readLines("days/03/input.txt"),
    stringsAsFactors = FALSE
)

# Solution ---------------------------------------------------------------
round.off <- function(x, digits = 0) {
    posneg = sign(x)
    z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
    z = floor(z * posneg + 0.5) / 10 ^ digits
    return(z)
}

next_bit <- function(bits, co2 = FALSE) {
    mean_bit <- mean(bits, na.rm = TRUE)

    if (co2) {
        if (mean_bit >= 0.5) bit <- 0
        if (mean_bit < 0.5) bit <- 1
    } else {
        bit <- round.off(mean_bit)
    }

    return(bit)
}

binary_diagnostic <- binary_diagnostic %>%
    tidyr::separate(binary, paste0("binary_", 1:12), sep = "(?<=.)", remove = FALSE) %>%
    dplyr::mutate_at(paste0("binary_", 1:12), as.integer)


oxygen_rating <- binary_diagnostic
i <- 1

while (nrow(oxygen_rating) > 1) {
    bit <- next_bit(oxygen_rating[[paste0("binary_", i)]])
    oxygen_rating <- oxygen_rating %>%
        dplyr::filter(substr(binary, i, i) == bit)
    i <- i + 1
}

oxygen_rating <- oxygen_rating %>%
    dplyr::pull(binary) %>%
    strtoi(base = 2)


co2_rating <- binary_diagnostic
i <- 1

while (nrow(co2_rating) > 1) {
    bit <- next_bit(co2_rating[[paste0("binary_", i)]], co2 = TRUE)
    co2_rating <- co2_rating %>%
        dplyr::filter(substr(binary, i, i) == bit)
    i <- i + 1
}

co2_rating <- co2_rating %>%
    dplyr::pull(binary) %>%
    strtoi(base = 2)


oxygen_rating * co2_rating
