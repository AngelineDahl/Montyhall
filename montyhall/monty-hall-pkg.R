#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}


#' @title Select a door at random
#'
#' @description
#' Simulates the contestant's initial choice by randomly selecting
#' one of the three doors.
#'
#' @details
#' The contestant has no information about where the car is located,
#' so the selection is a simple random draw from doors 1, 2, and 3.
#'
#' @return
#' An integer (1, 2, or 3) representing the contestant's chosen door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function() {
  doors <- c(1, 2, 3)
  a.pick <- sample(doors, size = 1)
  return(a.pick)
}



#' @title Host opens a goat door
#'
#' @description
#' Determines which door the host opens, revealing a goat.
#'
#' @details
#' The host must open a door that:
#' 1. Has a goat behind it, and
#' 2. Is not the contestant's selected door.
#'
#' If the contestant initially selects the car, the host chooses
#' randomly between the two goat doors. If the contestant selects
#' a goat, the host has only one valid door to open.
#'
#' @param game A character vector of length 3 containing "goat" or "car".
#' @param a.pick The door number initially selected by the contestant.
#'
#' @return
#' An integer (1, 2, or 3) representing the door opened by the host.
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' open_goat_door(game, pick)
#'
#' @export
open_goat_door <- function(game, a.pick) {
  doors <- c(1, 2, 3)

  if (game[a.pick] == "car") {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(goat.doors, size = 1)
  } else {
    opened.door <- doors[game != "car" & doors != a.pick]
  }

  return(opened.door)
}



#' @title Change or stay with the selected door
#'
#' @description
#' Determines the contestant's final door choice based on whether
#' they choose to stay or switch.
#'
#' @details
#' If `stay = TRUE`, the contestant keeps their original door.
#' If `stay = FALSE`, the contestant switches to the only remaining
#' unopened door.
#'
#' @param stay Logical. `TRUE` to stay with the original door,
#'   `FALSE` to switch.
#' @param opened.door The door number opened by the host.
#' @param a.pick The contestant's initial door selection.
#'
#' @return
#' An integer (1, 2, or 3) representing the contestant's final choice.
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' opened <- open_goat_door(game, pick)
#' change_door(TRUE, opened, pick)
#' change_door(FALSE, opened, pick)
#'
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick) {
  doors <- c(1, 2, 3)

  if (stay) {
    final.pick <- a.pick
  } else {
    final.pick <- doors[doors != opened.door & doors != a.pick]
  }

  return(final.pick)
}


#' @title Determine whether the contestant wins
#'
#' @description
#' Evaluates the final door choice and returns whether the contestant
#' wins the car or receives a goat.
#'
#' @details
#' The function checks the game setup and compares the final door
#' selection to the location of the car.
#'
#' @param final.pick The contestant's final door choice.
#' @param game A character vector of length 3 containing "goat" or "car".
#'
#' @return
#' A character string: `"WIN"` or `"LOSE"`.
#'
#' @examples
#' game <- create_game()
#' determine_winner(1, game)
#'
#' @export
determine_winner <- function(final.pick, game) {
  if (game[final.pick] == "car") {
    return("WIN")
  } else {
    return("LOSE")
  }
}


#' @title Play one round of the Monty Hall game
#'
#' @description
#' Simulates a single play of the Monty Hall game using either
#' the "stay" or "switch" strategy.
#'
#' @details
#' This function runs the full sequence of the Monty Hall game:
#' 1. A game is created.
#' 2. The contestant selects a door.
#' 3. The host opens a goat door.
#' 4. The contestant either stays or switches.
#' 5. The outcome (WIN or LOSE) is determined.
#'
#' @param strategy A character string, either "stay" or "switch".
#'
#' @return
#' A data frame with two columns:
#' \describe{
#'   \item{strategy}{The strategy used ("stay" or "switch").}
#'   \item{outcome}{The result of the game ("WIN" or "LOSE").}
#' }
#'
#' @examples
#' play_game("stay")
#' play_game("switch")
#'
#' @export
play_game <- function(strategy = "stay") {

  game <- create_game()
  a.pick <- select_door()
  opened.door <- open_goat_door(game, a.pick)

  if (strategy == "switch") {
    final.pick <- change_door(stay = FALSE, opened.door, a.pick)
  } else {
    final.pick <- a.pick
  }

  outcome <- determine_winner(final.pick, game)

  results.df <- data.frame(
    strategy = strategy,
    outcome = outcome,
    stringsAsFactors = FALSE
  )

  return(results.df)
}



#' @title Play multiple Monty Hall games
#'
#' @description
#' Runs many simulated Monty Hall games using either the "stay"
#' or "switch" strategy.
#'
#' @details
#' This function repeatedly calls `play_game()` and collects the
#' outcomes into a single data frame. It then prints the win
#' proportions for each strategy.
#'
#' @param n The number of games to simulate. Default is 100.
#'
#' @return
#' A data frame containing the strategy and outcome for each game.
#'
#' @examples
#' play_n_games(100)
#'
#' @export
play_n_games <- function(n = 100) {

  results.list <- list()

  for (i in 1:n) {
    game.outcome <- play_game("stay")
    results.list[[i]] <- game.outcome
  }

  results.df <- dplyr::bind_rows(results.list)

  print(
    results.df |>
      dplyr::count(strategy, outcome) |>
      dplyr::group_by(strategy) |>
      dplyr::mutate(prop = round(n / sum(n), 2))
  )

  return(results.df)
}
