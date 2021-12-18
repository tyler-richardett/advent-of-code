# Imports ----------------------------------------------------------------
library(magrittr)
library(zeallot)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/16/input.txt") {
    hex_transmission <- readLines(file_path)
    binary_vector <- BMS::hex2bin(hex_transmission)
    return(binary_vector)
}

# Helpers ----------------------------------------------------------------
str2i <- function(x) {
    y <- as.numeric(strsplit(x, "")[[1]])
    return(sum(y * 2 ^ rev((seq_along(y) - 1))))
    # Source: https://stackoverflow.com/a/13536947
}

slice_bits <- function(binary_vector, n, to_int = TRUE) {
    value <- binary_vector[1:n]
    if (to_int) value <- value %>% paste0(collapse = "") %>% str2i()

    binary_vector <- binary_vector[-(1:n)]
    return(list(value, binary_vector))
}

define_literal <- function(binary_vector) {
    vector_length <- length(binary_vector)
    num_values <- ceiling(vector_length / 5)

    values <- split(binary_vector, rep(1:num_values, each = 5, len = vector_length))

    decimal <- c()
    for (i in 1:num_values) {
        decimal <- c(decimal, values[[i]][2:5])
        if (values[[i]][1] == 0) break
    }

    decimal <- decimal %>% paste0(collapse = "") %>% str2i()
    c(., binary_vector) %<-% slice_bits(binary_vector, i * 5)

    return(list(decimal, binary_vector))
}

define_packets <- function(binary_vector) {
    packet <- list()

    c(packet$version_id, binary_vector) %<-% slice_bits(binary_vector, 3)
    c(packet$type_id, binary_vector) %<-% slice_bits(binary_vector, 3)

    if (packet$type_id == 4) {
        c(packet$value, binary_vector) %<-% define_literal(binary_vector)
    } else {
        c(packet$length_type_id, binary_vector) %<-% slice_bits(binary_vector, 1)

        if (packet$length_type_id == 0) {
            c(length_subpackets, binary_vector) %<-% slice_bits(binary_vector, 15)
            c(subpackets, binary_vector) %<-% slice_bits(binary_vector, length_subpackets, FALSE)

            packet$packets <- list()
            while (length(subpackets) > 0) {
                c(tmp_packet, subpackets) %<-% define_packets(subpackets)
                packet$packets <- c(packet$packets, list(tmp_packet))
            }
        } else {
            c(num_subpackets, binary_vector) %<-% slice_bits(binary_vector, 11)

            packet$packets <- list()
            for (i in 1:num_subpackets) {
                c(tmp_packet, binary_vector) %<-% define_packets(binary_vector)
                packet$packets <- c(packet$packets, list(tmp_packet))
            }
        }

        packet_values <- purrr::map_dbl(packet$packets, "value")
        packet$value <- if (packet$type_id == 0) {
            sum(packet_values)
        } else if (packet$type_id == 1) {
            prod(packet_values)
        } else if (packet$type_id == 2) {
            min(packet_values)
        } else if (packet$type_id == 3) {
            max(packet_values)
        } else if (packet$type_id == 5) {
            as.integer(packet_values[1] > packet_values[2])
        } else if (packet$type_id == 6) {
            as.integer(packet_values[1] < packet_values[2])
        } else if (packet$type_id == 7) {
            as.integer(packet_values[1] == packet_values[2])
        }
    }

    return(list(packet = packet, left = binary_vector))
}

sum_versions <- function(packets) {
    summed_versions <- packets %>%
        as.data.frame() %>%
        dplyr::select(dplyr::contains("version_id")) %>%
        dplyr::distinct() %>%
        rowSums()

    return(summed_versions)
}

# Puzzle 1 ---------------------------------------------------------------
summed_versions <- read_input() %>% define_packets() %>% .$packet %>% sum_versions()
cat("Puzzle 1 solution:", summed_versions, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
packet_value <- (read_input() %>% define_packets())$packet$value
cat("Puzzle 2 solution:", packet_value %>% as.character(), fill = TRUE)
