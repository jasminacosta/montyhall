% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mh.pack.R
\name{change_door}
\alias{change_door}
\title{Changing Doors}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{The}{arguments used within this function is opened.door and a.pick
along with stay=TRUE}
}
\value{
The function returns whether the player decided to change doors or
stay with their initial pick, which will either be a car or a goat behind
the door.
}
\description{
\code{change_door()} change door allows the player to switch doors from their
initial pick
}
\details{
In this game the player has the choice to change doors from their initial
pick, and are able to choose the other door that is still closed. If
the player chose a car door initially they could switch to a goat door
without knowing what is behind each door. If the player chose a goat
door then they could switch doors that could have the car.
}
\examples{
change_door()

}
