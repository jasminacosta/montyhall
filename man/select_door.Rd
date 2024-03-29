% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mh.pack.R
\name{select_door}
\alias{select_door}
\title{Player Selects a Door}
\usage{
select_door()
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
The function returns 1 door randomly from the game,
which could be a door that either has a goat or a car behind it.
}
\description{
\code{select_door()} randomly selects one door in the game where there are 3
doors total with 2 doors having a goat and 1 door having a car.
}
\details{
Within the game the player is able to select one door when starting the game,
which the player chooses between three doors, 2 of the doors having a goat
and one door having the car. The player does not know what is behind each door,
and is randomly selecting one door.
}
\examples{
  select_door()

}
