# Imports ----------------------------------------------------------------
library(magrittr)

# Input ------------------------------------------------------------------
read_input <- function(file_path = "./2021/21/input.txt") {
    starting_spaces <- readLines(file_path) %>%
        stringr::str_replace_all("^.*position: ", "") %>%
        as.integer()

    return(starting_spaces)
}

# Helpers ----------------------------------------------------------------
DiracDice <- R6::R6Class("DiracDice",
    public = list(
        player_1 = list(
            score = 0L,
            space = NULL
        ),
        player_2 = list(
            score = 0L,
            space = NULL
        ),
        next_roll = 1:3,
        next_turn = "player_1",
        to_win = 1000L,
        game_over = FALSE,

        roll_counter = 0,
        winning_player = NULL,
        losing_player = NULL,

        initialize = function(starting_spaces) {
            self$player_1$space <- starting_spaces[1]
            self$player_2$space <- starting_spaces[2]
        },

        take_turn = function() {
            current_player <- self$next_turn
            next_player <- ifelse(current_player == "player_1", "player_2", "player_1")

            self[[current_player]][["space"]] <- (self[[current_player]][["space"]] + sum(self$next_roll) - 1) %% 10 + 1
            self[[current_player]][["score"]] <- self[[current_player]][["score"]] + self[[current_player]][["space"]]

            self$roll_counter <- self$roll_counter + 3

            self$game_over <- self[[current_player]][["score"]] >= self$to_win
            if (self$game_over) self$winning_player <- current_player
            if (self$game_over) self$losing_player <- next_player

            self$next_roll <- (self$next_roll + 3 - 1) %% 100 + 1
            self$next_turn <- next_player
        }
    )
)

simulate_game <- function(starting_spaces) {
    dirac_dice <- DiracDice$new(starting_spaces)
    while (!dirac_dice$game_over) dirac_dice$take_turn()

    losing_score <- dirac_dice[[dirac_dice$losing_player]][["score"]]
    prod_score <- losing_score * dirac_dice$roll_counter

    return(prod_score)
}

quantum_die <- function(starting_spaces) {
    possible_outcomes <- tidyr::crossing(a = 1:3, b = 1:3, c = 1:3) %>%
        dplyr::transmute(roll = a + b + c) %>%
        dplyr::count(roll)

    player_wins <- c(0, 0)
    player_idx <- 1

    games <- data.frame(
        space_1 = starting_spaces[1],
        score_1 = 0,
        space_2 = starting_spaces[2],
        score_2 = 0,
        universes = 1
    )

    while (nrow(games) > 0) {
        games <- games %>%
            tidyr::crossing(possible_outcomes) %>%
            dplyr::mutate(
                "space_{player_idx}" := (!!dplyr::sym(glue::glue("space_{player_idx}")) + roll - 1) %% 10 + 1,
                "score_{player_idx}" := !!dplyr::sym(glue::glue("score_{player_idx}")) + !!dplyr::sym(glue::glue("space_{player_idx}")),
                universes = universes * n
            ) %>%
            dplyr::group_by(space_1, score_1, space_2, score_2) %>%
            dplyr::summarize(universes = sum(universes, na.rm = TRUE), .groups = "drop")

        if (any(games[[glue::glue("score_{player_idx}")]] >= 21)) {
            player_wins[player_idx] <- games %>%
                dplyr::filter(!!dplyr::sym(glue::glue("score_{player_idx}")) >= 21) %>%
                dplyr::pull(universes) %>%
                sum() %>%
                magrittr::add(player_wins[player_idx])

            games <- games %>%
                dplyr::filter(!!dplyr::sym(glue::glue("score_{player_idx}")) < 21)
        }

        player_idx <- 3 - player_idx
    }

    max_wins <- max(player_wins)
    return(max_wins)
}

# Puzzle 1 ---------------------------------------------------------------
prod_score <- read_input() %>% simulate_game()
cat("Puzzle 1 solution:", prod_score, fill = TRUE)

# Puzzle 2 ---------------------------------------------------------------
max_wins <- read_input() %>% quantum_die()
cat("Puzzle 2 solution:", max_wins %>% as.character(), fill = TRUE)
