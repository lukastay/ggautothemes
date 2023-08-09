##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- AUTOALLTHEMES---------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title autoallthemes
#'
#' @param ggplotvisual Required. ggplot2 visual that you'd like to see in various themes.
#'
#' @param HQsave Defaults to FALSE. Set to TRUE to output high quality raster images of all the plots.
#'
#' @export autoallthemes
#'
#' @details
#'
#' Cycles through all theme packs.
#'
#' @examples
#'
#' lp <- ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#' geom_point()+
#'   guides(color="none")+
#'   theme(legend.position="none")
#'
#' autoallthemes(p)
#'
#' @returns
#'
#' Slowly showcases all available themes for a given visual
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices tiff

autoallthemes <- function(ggplotvisual, HQsave = FALSE) {
  sleep <- function(x)
  {
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 # The cpu usage should be negligible
  }

  themepacks <-
    c(
      "basic1",
      "basic2",
      "ggthemes1",
      "ggthemes2",
      "ggthemes3",
      "ggthemes4",
      "ggdark1",
      "ggdark2",
      "outcasts"
    )

  for (themepack in themepacks) {
    p <- ggautothemes(ggplotvisual, themecollection = themepack)

    if (HQsave == TRUE) {
      filename <- paste(themepack, ".tiff", sep = "")
      tiff(
        filename,
        units = "in",
        width = 7,
        height = 7,
        res = 1500
      )
      plot(p)
      dev.off()
      plot(p)

    } else{
      plot(p)

    }

    sleep(5)

  }

}
