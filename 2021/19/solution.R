# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/19/input.txt") {
    scanners <- readLines(file_path)
    beacons <- data.frame(V1 = scanners, stringsAsFactors = FALSE) %>%
        tidyr::separate(V1, c("x", "y", "z"), "---|,") %>%
        dplyr::mutate(scanner_id = ifelse(grepl("scanner", y), gsub("^\\s*scanner\\s*|\\s*$", "", y), NA)) %>%
        tidyr::fill(scanner_id) %>%
        dplyr::filter(!is.na(z), z != "") %>%
        dplyr::mutate_all(as.integer) %>%
        dplyr::mutate(beacon_id = dplyr::row_number()) %>%
        dplyr::select(scanner_id, beacon_id, x:z) %>%

        return(beacons)
}

# Helpers ----------------------------------------------------------------
reorient_scanners <- function(beacons) {
    # Get possible coordinates assuming all 24 rotated
    coords_enum <- beacons %>%
        dplyr::left_join(data.frame(direction_id = 1:24), by = character()) %>%
        dplyr::mutate(
            coords = dplyr::case_when(
                direction_id == 1  ~ glue::glue("{x}, {y}, {z}"),
                direction_id == 2  ~ glue::glue("{-y}, {x}, {z}"),
                direction_id == 3  ~ glue::glue("{-x}, {-y}, {z}"),
                direction_id == 4  ~ glue::glue("{y}, {-x}, {z}"),
                direction_id == 5  ~ glue::glue("{z}, {y}, {-x}"),
                direction_id == 6  ~ glue::glue("{z}, {x}, {y}"),
                direction_id == 7  ~ glue::glue("{z}, {-y}, {x}"),
                direction_id == 8  ~ glue::glue("{z}, {-x}, {-y}"),
                direction_id == 9  ~ glue::glue("{-x}, {y}, {-z}"),
                direction_id == 10 ~ glue::glue("{-y}, {-x}, {-z}"),
                direction_id == 11 ~ glue::glue("{x}, {-y}, {-z}"),
                direction_id == 12 ~ glue::glue("{y}, {x}, {-z}"),
                direction_id == 13 ~ glue::glue("{-z}, {y}, {x}"),
                direction_id == 14 ~ glue::glue("{-z}, {-x}, {y}"),
                direction_id == 15 ~ glue::glue("{-z}, {-y}, {-x}"),
                direction_id == 16 ~ glue::glue("{-z}, {x}, {-y}"),
                direction_id == 17 ~ glue::glue("{x}, {-z}, {y}"),
                direction_id == 18 ~ glue::glue("{y}, {-z}, {-x}"),
                direction_id == 19 ~ glue::glue("{-x}, {-z}, {-y}"),
                direction_id == 20 ~ glue::glue("{-y}, {-z}, {x}"),
                direction_id == 21 ~ glue::glue("{x}, {z}, {-y}"),
                direction_id == 22 ~ glue::glue("{-y}, {z}, {-x}"),
                direction_id == 23 ~ glue::glue("{-x}, {z}, {y}"),
                direction_id == 24 ~ glue::glue("{y}, {z}, {x}")
            )
        ) %>%
        dplyr::select(-x, -y, -z) %>%
        tidyr::separate(coords, c("x", "y", "z"), ", ") %>%
        dplyr::mutate_at(c("x", "y", "z"), as.integer)

    # Calculate x, y, and z coordinate distances
    distances <- coords_enum %>%
        dplyr::left_join(coords_enum, c("scanner_id", "direction_id"), suffix = c(".1", ".2")) %>%
        dplyr::filter(beacon_id.1 != beacon_id.2) %>%
        dplyr::mutate(x_dist = x.1 - x.2, y_dist = y.1 - y.2, z_dist = z.1 - z.2) %>%
        dplyr::select(
            scanner_id.i = scanner_id,
            direction_id.i = direction_id,
            beacon_id.i.1 = beacon_id.1,
            beacon_id.i.2 = beacon_id.2,
            x_dist,
            y_dist,
            z_dist
        )

    # Join on distances to get beacon and direction pairs
    pairs <- distances %>%
        dplyr::left_join(
            distances %>% dplyr::rename_all(~stringr::str_replace_all(., "\\.i", ".j")),
            c("x_dist", "y_dist", "z_dist")
        ) %>%
        dplyr::select(-dplyr::ends_with("dist")) %>%
        dplyr::filter(scanner_id.i < scanner_id.j, beacon_id.i.1 < beacon_id.i.2) %>%
        dplyr::group_by(scanner_id.i, direction_id.i, beacon_id.i.1, scanner_id.j, direction_id.j, beacon_id.j.1) %>%
        dplyr::filter(dplyr::n() >= 11) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(scanner_id.i, direction_id.i, beacon_id.i.1, scanner_id.j, direction_id.j, beacon_id.j.1)

    # Chain scanner pairs together to get coordinate rotated
    rotations <- pairs %>%
        dplyr::filter(scanner_id.i == 0, direction_id.i == 1) %>%
        dplyr::rename_all(~stringr::str_replace_all(., "\\.i", "")) %>%
        dplyr::distinct(scanner_id, direction_id)

    while (!all(0:24 %in% rotations$scanner_id)) {
        rotations <- rotations %>%
            dplyr::bind_rows(
                pairs %>%
                    dplyr::inner_join(rotations, c("scanner_id.i" = "scanner_id", "direction_id.i" = "direction_id")) %>%
                    dplyr::distinct(scanner_id = scanner_id.j, direction_id = direction_id.j)
            ) %>%
            dplyr::distinct() %>%
            dplyr::arrange(scanner_id)

        rotations <- rotations %>%
            dplyr::bind_rows(
                pairs %>%
                    dplyr::inner_join(rotations, c("scanner_id.j" = "scanner_id", "direction_id.j" = "direction_id")) %>%
                    dplyr::distinct(scanner_id = scanner_id.i, direction_id = direction_id.i)
            ) %>%
            dplyr::distinct() %>%
            dplyr::arrange(scanner_id)
    }

    # Rotate original coordinates
    rotated <- beacons %>%
        dplyr::left_join(rotations, "scanner_id") %>%
        dplyr::select(-x, -y, -z) %>%
        dplyr::left_join(coords_enum, c("scanner_id", "direction_id", "beacon_id"))

    # Chain scanner pairs together to get coordinate realignments
    shifts <- data.frame(scanner_id = 0, x_shift = 0, y_shift = 0, z_shift = 0)

    while (!all(0:24 %in% shifts$scanner_id)) {
        shifts <- shifts %>%
            dplyr::bind_rows(
                pairs %>%
                    dplyr::distinct(scanner_id.i, beacon_id.i.1, scanner_id.j, beacon_id.j.1) %>%
                    dplyr::inner_join(shifts, c("scanner_id.i" = "scanner_id")) %>%
                    dplyr::left_join(rotated, c("scanner_id.i" = "scanner_id", "beacon_id.i.1" = "beacon_id")) %>%
                    dplyr::left_join(rotated, c("scanner_id.j" = "scanner_id", "beacon_id.j.1" = "beacon_id"), suffix = c(".i", ".j")) %>%
                    dplyr::mutate(
                        x_shift.j = x.i - x.j + x_shift,
                        y_shift.j = y.i - y.j + y_shift,
                        z_shift.j = z.i - z.j + z_shift,
                    ) %>%
                    dplyr::distinct(
                        scanner_id = scanner_id.j,
                        x_shift = x_shift.j,
                        y_shift = y_shift.j,
                        z_shift = z_shift.j
                    )
            ) %>%
            dplyr::distinct() %>%
            dplyr::arrange(scanner_id)

        shifts <- shifts %>%
            dplyr::bind_rows(
                pairs %>%
                    dplyr::distinct(scanner_id.i, beacon_id.i.1, scanner_id.j, beacon_id.j.1) %>%
                    dplyr::inner_join(shifts, c("scanner_id.j" = "scanner_id")) %>%
                    dplyr::left_join(rotated, c("scanner_id.j" = "scanner_id", "beacon_id.j.1" = "beacon_id")) %>%
                    dplyr::left_join(rotated, c("scanner_id.i" = "scanner_id", "beacon_id.i.1" = "beacon_id"), suffix = c(".j", ".i")) %>%
                    dplyr::mutate(
                        x_shift.i = x.j - x.i + x_shift,
                        y_shift.i = y.j - y.i + y_shift,
                        z_shift.i = z.j - z.i + z_shift,
                    ) %>%
                    dplyr::distinct(
                        scanner_id = scanner_id.i,
                        x_shift = x_shift.i,
                        y_shift = y_shift.i,
                        z_shift = z_shift.i
                    )
            ) %>%
            dplyr::distinct() %>%
            dplyr::arrange(scanner_id)
    }

    # Realign original coordinates
    aligned <- rotated %>%
        dplyr::select(scanner_id, x:z) %>%
        dplyr::left_join(shifts, "scanner_id") %>%
        dplyr::mutate(
            beacon_x = x + x_shift,
            beacon_y = y + y_shift,
            beacon_z = z + z_shift
        ) %>%
        dplyr::select(
            scanner_id,
            scanner_x = x_shift,
            scanner_y = y_shift,
            scanner_z = z_shift,
            beacon_x,
            beacon_y,
            beacon_z
        )

    return(aligned)
}

largest_manhattan_distance <- function(aligned) {
    pairwise_distances <- aligned %>%
        dplyr::distinct(scanner_id, scanner_x, scanner_y, scanner_z) %>%
        dplyr::left_join(
            aligned %>%
                dplyr::distinct(scanner_id, scanner_x, scanner_y, scanner_z),
            by = character(),
            suffix = c(".i", ".j")
        ) %>%
        dplyr::filter(scanner_id.i != scanner_id.j) %>%
        dplyr::mutate(
            manhattan_distance = abs(scanner_x.i - scanner_x.j) +
                abs(scanner_y.i - scanner_y.j) +
                abs(scanner_z.i - scanner_z.j)
        )

    largest_distance <- pairwise_distances %>%
        dplyr::top_n(1, manhattan_distance) %>%
        dplyr::distinct(manhattan_distance) %>%
        dplyr::pull(manhattan_distance)

    return(largest_distance)
}

# Puzzle 1 ---------------------------------------------------------------
num_beacons <- read_input() %>%
    reorient_scanners() %>%
    dplyr::distinct(beacon_x, beacon_y, beacon_z) %>%
    nrow()

cat("Puzzle 1 solution:", num_beacons, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
largest_distance <- read_input() %>%
    reorient_scanners() %>%
    largest_manhattan_distance()

cat("Puzzle 2 solution:", largest_distance, fill = TRUE)
