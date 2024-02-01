install.packages(c("devtools", "roxygen2","usethis","testthat","knitr"))

library(devtools)
has_devel()

setwd( "some/path/here" )

usethis::create_package( "montyhall" )

setwd("/Users/jasminacosta/montyhall")
getwd()   # 'C:/Users/jasminacosta/montyhall'
devtools::document()

setwd( ".." )  # move up one level with two periods
getwd()        # should be /documents NOT /montyhall
devtools::install( "montyhall" )

library( montyhall )
create_game()

help( "create_game" )

