# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/14/input.txt") {
    input <- readLines(file_path)

    polymer_template <- input[1]
    insertion_rules <- data.frame(
        rules = grep("->", input, value = TRUE),
        stringsAsFactors = FALSE
    ) %>%
        tidyr::separate(rules, c("pair", "insertion"), " -> ")

    return(
        list(
            polymer_template = polymer_template,
            insertion_rules = insertion_rules
        )
    )
}

# Helpers ----------------------------------------------------------------
pair_inserts <- function(input, idx) {
    polymer_template <- input$polymer_template
    insertion_rules <- input$insertion_rules

    elements <- strsplit(polymer_template, "") %>%
        table() %>%
        as.matrix() %>%
        as.data.frame() %>%
        dplyr::rename(n = V1) %>%
        tibble::rownames_to_column("element")

    polymer <- data.frame(
        start = 1:(nchar(polymer_template) - 1),
        end = 2:nchar(polymer_template)
    ) %>%
        dplyr::mutate(pair = stringr::str_sub(polymer_template, start, end)) %>%
        dplyr::count(pair)

    for (i in idx) {
        polymer <- polymer %>%
            dplyr::left_join(insertion_rules, "pair") %>%
            dplyr::mutate(
                new_pair_1 = paste0(stringr::str_sub(pair, 1, 1), insertion),
                new_pair_2 = paste0(insertion, stringr::str_sub(pair, 2, 2))
            )

        elements <- elements %>%
            dplyr::full_join(
                polymer %>%
                    dplyr::select(element = insertion, new = n) %>%
                    dplyr::group_by(element) %>%
                    dplyr::summarize(new = sum(new, na.rm = TRUE)) %>%
                    dplyr::ungroup(),
                "element"
            ) %>%
            replace(is.na(.), 0) %>%
            dplyr::mutate(n = n + new) %>%
            dplyr::select(-new)

        polymer <- polymer %>%
            dplyr::select(pair = new_pair_1, n) %>%
            dplyr::bind_rows(polymer %>% dplyr::select(pair = new_pair_2, n)) %>%
            dplyr::group_by(pair) %>%
            dplyr::summarize(n = sum(n, na.rm = TRUE)) %>%
            dplyr::ungroup()
    }

    return(elements)
}

most_minus_least_common <- function(elements) {
    polymer_score <- max(elements$n) - min(elements$n)
    return(polymer_score)
}

# Puzzle 1 ---------------------------------------------------------------
polymer_score <- read_input() %>% pair_inserts(1:10) %>% most_minus_least_common()
cat("Puzzle 1 solution:", polymer_score, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
polymer_score <- read_input() %>% pair_inserts(1:40) %>% most_minus_least_common()
cat("Puzzle 2 solution:", polymer_score %>% as.character(), fill = TRUE)
