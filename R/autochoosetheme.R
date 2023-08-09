##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- AUTOCHOOSETHEME-------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title autochoosetheme
#'
#' @param ggplotvisual Required. ggplot2 visual that you'd like to see in various themes.
#'
#' @param theme No Default. Enter the theme exactly as it appears in the title of the other function's plots.
#'
#' @param HQexport Default is FALSE. If set to TRUE, it will export a professional grade raster image of your chosen themed plot.
#'
#' @param size Default is medium. Enter string as "small" or "large" to set size of image exported by HQexport. Smaller sizes have larger text relative to the plot.
#'
#' @export autochoosetheme
#'
#' @details
#'
#' Outputs a single graph based on the visuals you chose.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p <- ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))
#' p <- p + geom_point()
#' p <- p + guides(color="none")
#' p <- p + theme(legend.position="none")
#'
#' autochoosetheme(ggplotvisual = p, theme = "theme_grey")
#'
#' @returns
#'
#' Returns a plot with the theme chosen.
#'
#' @import ggplot2
#' @import ggthemes
#' @import hrbrthemes
#' @import ggdark
#' @import ggtech
#' @import extrafont
#' @importFrom grDevices dev.off
#' @importFrom grDevices tiff

autochoosetheme <- function(ggplotvisual, theme, HQexport = FALSE, size = "medium") {


  if (theme == "theme_grey") {
    g <- ggplotvisual +
      ggtitle("theme_grey") +
      theme_grey()
  } else if (theme == "theme_bw") {
    g <- p2 <- ggplotvisual +
      ggtitle("theme_bw") +
      theme_bw()
  } else if (theme == "theme_linedraw") {
    g <- ggplotvisual +
      ggtitle("theme_linedraw") +
      theme_linedraw()
  } else if (theme == "theme_light") {
    g <- ggplotvisual +
      ggtitle("theme_light") +
      theme_light()
  } else if (theme == "theme_dark") {
    g <- ggplotvisual +
      ggtitle("theme_dark") +
      theme_dark()
  } else if (theme == "theme_minimal") {
    g <- ggplotvisual +
      ggtitle("theme_minimal") +
      theme_minimal()
  } else if (theme == "theme_classic") {
    g <- ggplotvisual +
      ggtitle("theme_classic") +
      theme_classic()
  } else if (theme == "theme_void") {
    g <- ggplotvisual +
      ggtitle("theme_void") +
      theme_void()
  } else if (theme == "theme_base") {
    g <- ggplotvisual +
      ggtitle("theme_base") +
      theme_base()
  } else if (theme == "theme_calc") {
    g <- ggplotvisual +
      ggtitle("theme_calc") +
      theme_calc() +
      scale_fill_calc()
  } else if (theme == "theme_clean") {
    g <- ggplotvisual +
      ggtitle("theme_clean") +
      theme_clean()
  } else if (theme == "theme_economist") {
    g <- ggplotvisual +
      ggtitle("theme_economist") +
      theme_economist() +
      scale_fill_economist()
  } else if (theme == "theme_excel") {
    g <- ggplotvisual +
      ggtitle("theme_excel") +
      theme_excel() +
      scale_fill_excel()
  } else if (theme == "theme_excel_new") {
    g <- ggplotvisual +
      ggtitle("theme_excel_new") +
      theme_excel_new() +
      scale_fill_excel_new()
  } else if (theme == "theme_few") {
    g <- ggplotvisual +
      ggtitle("theme_few") +
      theme_few() +
      scale_fill_few()
  } else if (theme == "theme_fivethirtyeight") {
    g <- ggplotvisual +
      ggtitle("theme_fivethirtyeight") +
      theme_fivethirtyeight() +
      scale_fill_fivethirtyeight()
  } else if (theme == "theme_foundation") {
    g <- ggplotvisual +
      ggtitle("theme_foundation") +
      theme_foundation()
  } else if (theme == "theme_gdocs") {
    g <- ggplotvisual +
      ggtitle("theme_gdocs") +
      theme_gdocs() +
      scale_fill_gdocs()
  } else if (theme == "theme_hc") {
    g <- ggplotvisual +
      ggtitle("theme_hc") +
      theme_hc() +
      scale_fill_hc()
  } else if (theme == "theme_igray") {
    g <- ggplotvisual +
      ggtitle("theme_igray") +
      theme_igray()
  } else if (theme == "theme_pander") {
    g <- ggplotvisual +
      ggtitle("theme_pander") +
      theme_pander() +
      scale_fill_pander()
  } else if (theme == "theme_par") {
    g <- ggplotvisual +
      ggtitle("theme_par") +
      theme_par()
  } else if (theme == "theme_solarized") {
    g <- ggplotvisual +
      ggtitle("theme_solarized") +
      theme_solarized() +
      scale_fill_solarized()
  } else if (theme == "theme_solid") {
    g <- ggplotvisual +
      ggtitle("theme_solid") +
      theme_solid()
  } else if (theme == "theme_stata") {
    g <- ggplotvisual +
      ggtitle("theme_stata") +
      theme_stata() +
      scale_fill_stata()
  } else if (theme == "theme_ipsum") {
    g <- ggplotvisual +
      ggtitle("theme_ipsum") +
      theme_ipsum() + # Arial Narrow
      scale_fill_ipsum()
  } else if (theme == "scale_fill_ft") {
    g <- ggplotvisual +
      ggtitle("scale_fill_ft") +
      scale_fill_ft()
  } else if (theme == "etsy") {
    g <- ggplotvisual +
      ggtitle("etsy") +
      theme_tech(theme = "etsy") +
      scale_fill_tech(theme = "etsy")
  } else if (theme == "dark_theme_gray") {
    g <- ggplotvisual +
      ggtitle("dark_theme_gray") +
      dark_theme_gray()
  } else if (theme == "dark_theme_bw") {
    g <- ggplotvisual +
      ggtitle("dark_theme_bw") +
      dark_theme_bw()
  } else if (theme == "dark_theme_linedraw") {
    g <- ggplotvisual +
      ggtitle("dark_theme_linedraw") +
      dark_theme_linedraw()
  } else if (theme == "dark_theme_light") {
    g <- ggplotvisual +
      ggtitle("dark_theme_light") +
      dark_theme_light()
  } else if (theme == "dark_theme_dark") {
    g <- ggplotvisual +
      ggtitle("dark_theme_dark") +
      dark_theme_dark()
  } else if (theme == "dark_theme_minimal") {
    g <- ggplotvisual +
      ggtitle("dark_theme_minimal") +
      dark_theme_minimal()
  } else if (theme == "dark_theme_classic") {
    g <- ggplotvisual +
      ggtitle("dark_theme_classic") +
      dark_theme_classic()
  } else if (theme == "dark_theme_void") {
    g <- ggplotvisual +
      ggtitle("dark_theme_void") +
      dark_theme_void()
  } else if (theme == "theme_stata") {
    g <- ggplotvisual +
      ggtitle("theme_stata") +
      theme_stata() +
      scale_fill_stata()
  } else if (theme == "theme_ipsum") {
    g <- ggplotvisual +
      ggtitle("theme_ipsum") +
      theme_ipsum() + # Arial Narrow
      scale_fill_ipsum()
  } else if (theme == "scale_fill_ft") {
    g <- ggplotvisual +
      ggtitle("scale_fill_ft") +
      scale_fill_ft()
  } else if (theme == "etsy") {
    g <- ggplotvisual +
      ggtitle("etsy") +
      theme_tech(theme = "etsy") +
      scale_fill_tech(theme = "etsy")
  } else{
    print("Theme not found. Returning.")
    return()
  }

  if (HQexport == TRUE) {
    filename <- "ggautothemes_exported_graph.tiff"

    if(size == "small"){
      tiff(
        filename,
        units = "in",
        width = 2,
        height = 2,
        res = 1500
      )
    } else if(size == "medium"){
      tiff(
        filename,
        units = "in",
        width = 4,
        height = 4,
        res = 1500
      )
    } else if(size == "large"){
      tiff(
        filename,
        units = "in",
        width = 6,
        height = 6,
        res = 1500
      )
    } else{
      print("Size not found. Returning.")
      return()
    }

    plot(g)
    dev.off()

  }

  plot(g)
  return(g)

}
