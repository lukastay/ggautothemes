##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- AUTOALLTHEMES---------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title autoallthemes
#'
#' @param ggplotvisual Required. ggplot2 visual that you'd like to see in various themes.
#'
#' @export autoallthemes
#'
#' @details
#'
#' Cycles through all theme packs.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' my_nested_list <- list(id=c(1,2,3,4,5),
#' weight=c(1,1,1,1,1),
#' choice1 = c(0,0,1,1,0),
#' choice2 = c(1,0,0,1,1),
#' choice3 = c(0,1,0,1,1),
#' choice4 = c(0,1,0,1,1),
#' choice5 = c(1,1,1,1,1))
#'
#' catadat <-  as.data.frame(do.call(cbind, my_nested_list))
#'
#' p <- ggplot(catadat)
#'
#' autoallthemes(p)
#'
#' @returns
#'
#' Slowly showcases all available themes for a given visual
#'

autoallthemes <- function(ggplotvisual){

  sleep <- function(x)
  {
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 # The cpu usage should be negligible
  }

  themepacks <- c("basic1", "basic2", "ggthemes1", "ggthemes2", "ggthemes3", "ggthemes4", "hrbrthemes1", "hrbrthemes2", "ggtech1", "ggtech2", "ggdark1", "ggdark2")

  for(themepack in themepacks){

    p <- ggautothemes(ggplotvisual, themecollection = themepack)
    plot(p)

    sleep(10)

  }

}
