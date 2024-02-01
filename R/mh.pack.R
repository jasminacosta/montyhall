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



#' @title
#'  Player Selects a Door
#'
#' @description
#'  `select_door()` randomly selects one door in the game where there are 3
#'  doors total with 2 doors having a goat and 1 door having a car.
#'
#' @details
#'  Within the game the player is able to select one door when starting the game,
#'  which the player chooses between three doors, 2 of the doors having a goat
#'  and one door having the car. The player does not know what is behind each door,
#'  and is randomly selecting one door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns 1 door randomly from the game,
#'  which could be a door that either has a goat or a car behind it.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Opening a Goat Door
#'
#' @description
#'  `open_goat_door()` a goat door will always be opened, but it cannot be a
#'  door that the player has already chosen, and it cannot be a door
#'  with a car behind it if the player chose a door with a goat behind it
#'
#' @details
#'  The host within the game will always open a goat door. If the player chooses
#'  the car on the first guess then the host can open one of the two doors
#'  being either the goat door or the car door. However, if the player chooses
#'  the goat door then the host has no other choice but to open a goat door.
#'
#' @param the arguments used in this function are game & a.pick
#'
#' @return The function always returns a goat door but alters based on the
#'  scenario depending on if the player chose a car the first time or a goat.
#'
#' @examples
#' open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'  Changing Doors
#'
#' @description
#'  `change_door()` change door allows the player to switch doors from their
#'  initial pick
#'
#' @details
#'  In this game the player has the choice to change doors from their initial
#'  pick, and are able to choose the other door that is still closed. If
#'  the player chose a car door initially they could switch to a goat door
#'  without knowing what is behind each door. If the player chose a goat
#'  door then they could switch doors that could have the car.
#'
#' @param The arguments used within this function is opened.door and a.pick
#'  along with stay=TRUE
#'
#' @return The function returns whether the player decided to change doors or
#'  stay with their initial pick, which will either be a car or a goat behind
#'  the door.
#'
#' @examples
#' change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title
#'  Determining If The Player Won
#'
#' @description
#'  `determine_winner()` determines if the player won or lost the game based on
#'  their final pick being either a goat or a car
#'
#' @details
#'  Once the player makes their final choice the door will be opened to reveal
#'  whether the door they chose had a goat or a car behind it. If the player
#'  chose the door with the goat behind it then the player lost the game. If
#'  the player chose the door with the car behind it then the player won the
#'  game.
#'
#' @param The arguments within this function are final.pick and game
#'
#' @return The function returns "WIN" if the player's final pick had a car
#'  behind the door, and "LOSE" if the player's final pick had a goat behind
#'  the door.
#'
#' @examples
#'   determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}



#' @title
#'  Playing the Game
#'
#' @description
#'  `play_game()` generates the entire game play by play in order and works as a
#'  simulation set up along with returning a data frame if the player would have
#'  lost or won the game if they would have stayed or switched from their initial
#'  choice
#'
#' @details
#'  To play the game it begins with setting up the game with 3 doors where
#'  2 of the doors have a goat behind them and 1 door has a car. Then the player
#'  begins by choosing one of the three doors at random not knowing which door
#'  has the goat or car. Then one goat door is opened every time at the start
#'  whether a goat or car door is chosen initially. If a car door is chosen the
#'  player has the the choice to open either the goat or car door, not knowing
#'  which one is behind each of the doors, but if a goat door is chosen the
#'  player has no choice but to open a goat door. However, the player
#'  has the choice to change doors from the initial pick or to stay with their
#'  initial choice. Finally, at the end based on the player's final pick the
#'  game will indicate whether the player won or lost based on if they chose
#'  a door with a goat or car behind it. Based on the game a data
#'  frame will be created that indicates what the best strategy is whether
#'  that is staying or switching from the player's initial pick and this will be
#'  stored as game results.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'  The function returns a data frame with the best strategy for the game which
#'  is either the player switching or staying with their initial pick.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}




#' @title
#'  Creating a Loop for the Game
#'
#' @description
#'  `play_n_games()` generates a loop to simulate game results based on a number
#'  of times if the player were to play the game, and a table of results is
#'  created through the loop to show the number of wins and loses if the player
#'  would have stayed or switched from their inital pick
#'
#' @details
#'  This functions runs through a loop through a desired amount of times. In
#'  this case the loop is running the game 100 times. The loop plays the game
#'  100 times and provides results of how many wins and loses occurred within
#'  each strategy based on whether the player chose to stay or to switch.
#'  Then to better understand the results to be able to see if staying or
#'  switching is the dominant strategy a table is generated by creating
#'  proportions of the results by rows.
#'
#' @param The argument used in this function is n=100, but the number of loops
#'  can be modified to any number of times but the number needs to be specified
#'
#' @return
#'  The function returns a results list of the number of loses and wins based
#'  on whether the player chose to switch or stay in each game. Then the results
#'  are proportioned by rows to better understand the results to determine what
#'  the dominant strategy is.
#'
#' @examples
#'   play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}





