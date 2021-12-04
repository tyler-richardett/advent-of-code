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

binary_diagnostic %>%
    tidyr::separate(binary, paste0("binary_", 1:12), sep = "(?<=.)") %>%
    dplyr::mutate_all(as.integer) %>%
    dplyr::summarize_all(~round.off(mean(., na.rm = TRUE))) %>%
    dplyr::slice(rep(1:dplyr::n(), each = 2)) %>%
    dplyr::mutate_all(
        ~dplyr::case_when(
            dplyr::row_number() == 1 ~ .,
            dplyr::lag(.) == 1 ~ 0,
            TRUE ~ 1
        )
    ) %>%
    tidyr::unite("binary", binary_1:binary_12, sep = "") %>%
    dplyr::mutate(binary = strtoi(binary, base = 2)) %>%
    as.matrix() %>%
    matrixStats::colProds()
