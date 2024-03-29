% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mh.pack.R
\name{play_n_games}
\alias{play_n_games}
\title{Creating a Loop for the Game}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{The}{argument used in this function is n=100, but the number of loops
can be modified to any number of times but the number needs to be specified}
}
\value{
The function returns a results list of the number of loses and wins based
on whether the player chose to switch or stay in each game. Then the results
are proportioned by rows to better understand the results to determine what
the dominant strategy is.
}
\description{
\code{play_n_games()} generates a loop to simulate game results based on a number
of times if the player were to play the game, and a table of results is
created through the loop to show the number of wins and loses if the player
would have stayed or switched from their inital pick
}
\details{
This functions runs through a loop through a desired amount of times. In
this case the loop is running the game 100 times. The loop plays the game
100 times and provides results of how many wins and loses occurred within
each strategy based on whether the player chose to stay or to switch.
Then to better understand the results to be able to see if staying or
switching is the dominant strategy a table is generated by creating
proportions of the results by rows.
}
\examples{
  play_n_games()

}
