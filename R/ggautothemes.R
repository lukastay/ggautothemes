##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- GGAUTOTHEMES----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title ggautothemes
#'
#' @param ggplotvisual Required. ggplot2 visual that you'd like to see in various themes.
#'
#' @param themecollection Optional. Here you can cycle through the various themes. Your theme options are (in lowercase): basic1 (default), basic2, ggthemes1, ggthemes2, ggthemes3, ggthemes4, ggthemes5, hrbrthemes, ggthemr1 (NOT YET IMPLREMENTED), gthemr2 (NOT YET IMPLEMENTED), ggthemr3 (NOT YET IMPLEMENTED), ggtech, ggdark1, and ggdark2.
#'
#' @export ggautothemes
#'
#' @details
#'
#' Once one chooses a plot they like the best, they can use the ______ function to select it, using the title of their preferred plot as an argument.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p <- ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
#' geom_point() +
#'   guides(color="none") +
#'   theme(legend.position="none")
#'
#' ggautothemes(p)
#'
#' @returns
#'
#' Plot of multiple versions of visual with various themes.
#'
#' @import ggplot2
#' @import patchwork
#' @import ggthemes
#' @import hrbrthemes
#' @import ggdark
#' @import ggtech
#' @import extrafont

ggautothemes <- function(ggplotvisual, themecollection = "basic1"){

  if(themecollection=="basic1"){

    p1 <- ggplotvisual +
      ggtitle("theme_grey") +
      theme_grey()

    p2 <- ggplotvisual +
      ggtitle("theme_bw") +
      theme_bw()

    p3 <- ggplotvisual +
      ggtitle("theme_linedraw") +
      theme_linedraw()

    p4 <- ggplotvisual +
      ggtitle("theme_light") +
      theme_light()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="basic2"){

    p1 <- ggplotvisual +
      ggtitle("theme_dark") +
      theme_dark()

    p2 <- ggplotvisual +
      ggtitle("theme_minimal") +
      theme_minimal()

    p3 <- ggplotvisual +
      ggtitle("theme_classic") +
      theme_classic()

    p4 <- ggplotvisual +
      ggtitle("theme_void") +
      theme_void()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="ggthemes1"){

    p1 <- ggplotvisual +
      ggtitle("theme_base") +
      theme_base()

    p2 <- ggplotvisual +
      ggtitle("theme_calc") +
      theme_calc() +
      scale_fill_calc()

    p3 <- ggplotvisual +
      ggtitle("theme_clean") +
      theme_clean()

    p4 <- ggplotvisual +
      ggtitle("theme_economist") +
      theme_economist() +
      scale_fill_economist()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="ggthemes2"){

    p1 <- ggplotvisual +
      ggtitle("theme_excel") +
      theme_excel() +
      scale_fill_excel()

    p2 <- ggplotvisual +
      ggtitle("theme_excel_new") +
      theme_excel_new() +
      scale_fill_excel_new()

    p3 <- ggplotvisual +
      ggtitle("theme_few") +
      theme_few() +
      scale_fill_few()

    p4 <- ggplotvisual +
      ggtitle("theme_fivethirtyeight") +
      theme_fivethirtyeight() +
      scale_fill_fivethirtyeight()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="ggthemes3"){

    p1 <- ggplotvisual +
      ggtitle("theme_foundation") +
      theme_foundation()

    p2 <- ggplotvisual +
      ggtitle("theme_gdocs") +
      theme_gdocs() +
      scale_fill_gdocs()

    p3 <- ggplotvisual +
      ggtitle("theme_hc") +
      theme_hc() +
      scale_fill_hc()

    p4 <- ggplotvisual +
      ggtitle("theme_igray") +
      theme_igray()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="ggthemes4"){

    p1 <- ggplotvisual +
      ggtitle("theme_pander") +
      theme_pander() +
      scale_fill_pander()

    p2 <- ggplotvisual +
      ggtitle("theme_par") +
      theme_par()

    p3 <- ggplotvisual +
      ggtitle("theme_solarized") +
      theme_solarized() +
      scale_fill_solarized()

    p4 <- ggplotvisual +
      ggtitle("theme_solid") +
      theme_solid()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="ggthemes5"){

    patched <- p5 <- ggplotvisual +
      ggtitle("theme_stata") +
      theme_stata() +
      scale_fill_stata()

  }else if(themecollection=="hrbrthemes"){

    p1 <- ggplotvisual +
      ggtitle("theme_ipsum") +
      theme_ipsum() + # Arial Narrow
      scale_fill_ipsum()

    p2 <- ggplotvisual +
      ggtitle("scale_fill_ft") +
      scale_fill_ft()

    patched <- p1 | p2

  }else if(themecollection=="hrbrthemes1"){

    print("full hrbrthemes coming soon. returning.")
    return()

    # import_public_sans()
    # import_titillium_web()
    # import_econ_sans()
    # import_tinyhand()
    # import_plex_sans()
    # import_roboto_condensed()
    # update_geom_font_defaults(family=font_es_light)
    # extrafont::loadfonts()

    # print("ERROR: hrbrthemes1 is not yet ready. Returning.")
    # return()

    # p1 <- ggplotvisual +
    #   ggtitle("theme_ipsum") +
    #   theme_ipsum() + # Arial Narrow
    #   scale_fill_ipsum()

    # p2 <- ggplotvisual +
    #   ggtitle("theme_ipsum_es") +
    #   theme_ipsum_es()

    # p3 <- ggplotvisual +
    #   ggtitle("theme_ipsum_rc") +
    #   theme_ipsum_rc()

    # p4 <- ggplotvisual +
    #   ggtitle("theme_ipsum_ps") +
    #   theme_ipsum_ps()

    # patched <- p1

    # patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="hrbrthemes2"){

    print("full hrbrthemes coming soon. returning.")
    return()

    # print("ERROR: hrbrthemes2 is not yet ready. Returning.")
    # return()

    # p1 <- ggplotvisual +
    #   ggtitle("theme_ipsum") +
    #   theme_ipsum_pub()

    # p2 <- ggplotvisual +
    #   ggtitle("theme_ipsum_tw") +
    #   theme_ipsum_tw()

    # p3 <- ggplotvisual +
    #   ggtitle("theme_modern_rc") +
    #   theme_modern_rc()

    # p4 <- ggplotvisual +
    #   ggtitle("scale_fill_ft") +
    #   scale_fill_ft()
    #
    # patched <- p4

    # patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="ggtech"){

    p1 <- ggplotvisual +
      ggtitle("etsy") +
      theme_tech(theme = "etsy") +
      scale_fill_tech(theme = "etsy")

    patched <- p1

  }else if(themecollection=="ggtech1"){

    print("Full version of ggtech coming soon. Returning.")
    return()

    # p1 <- ggplotvisual +
    #   ggtitle("airbnb") +
    #   theme_tech(theme = "airbnb") +
    #   scale_fill_tech(theme = "airbnb")

    # p2 <- ggplotvisual +
    #   ggtitle("etsy") +
    #   theme_tech(theme = "etsy") +
    #   scale_fill_tech(theme = "etsy")

    # p3 <- ggplotvisual +
    #   ggtitle("facebook") +
    #   theme_tech(theme = "facebook") +
    #   scale_fill_tech(theme = "facebook")

    # patched <- (p1 | p2) / (p3)

    # patched <- p2

  }else if(themecollection=="ggtech2"){

    print("Full version of ggtech coming soon. Returning.")
    return()

    # p1 <- ggplotvisual +
    #   ggtitle("google") +
    #   theme_tech(theme = "google") +
    #   scale_fill_tech(theme = "google")

    # p2 <- ggplotvisual +
    #   ggtitle("twitter") +
    #   theme_tech(theme = "twitter") +
    #   scale_fill_tech(theme = "twitter")

    # p3 <- ggplotvisual +
    #   ggtitle("airbnb") +
    #   theme_airbnb_fancy() +
    #   scale_fill_tech(theme = "airbnb")

    # patched <- p2

    # patched <- (p1 | p2)# / (p3)

  }else if(themecollection=="ggdark1"){

    p1 <- ggplotvisual +
      ggtitle("dark_theme_gray") +
      dark_theme_gray()

    p2 <- ggplotvisual +
      ggtitle("dark_theme_bw") +
      dark_theme_bw()

    p3 <- ggplotvisual +
      ggtitle("dark_theme_linedraw") +
      dark_theme_linedraw()

    p4 <- ggplotvisual +
      ggtitle("dark_theme_light") +
      dark_theme_light()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="ggdark2"){

    p1 <- ggplotvisual +
      ggtitle("dark_theme_dark") +
      dark_theme_dark()

    p2 <- ggplotvisual +
      ggtitle("dark_theme_minimal") +
      dark_theme_minimal()

    p3 <- ggplotvisual +
      ggtitle("dark_theme_classic") +
      dark_theme_classic()

    p4 <- ggplotvisual +
      ggtitle("dark_theme_void") +
      dark_theme_void()

    patched <- (p1 | p2) / (p3 | p4)

  }else if(themecollection=="outcasts"){

    p1 <- ggplotvisual +
      ggtitle("theme_stata") +
      theme_stata() +
      scale_fill_stata()

    p2 <- ggplotvisual +
      ggtitle("theme_ipsum") +
      theme_ipsum() + # Arial Narrow
      scale_fill_ipsum()

    p3 <- ggplotvisual +
      ggtitle("scale_fill_ft") +
      scale_fill_ft()

    p4 <- ggplotvisual +
      ggtitle("etsy") +
      theme_tech(theme = "etsy") +
      scale_fill_tech(theme = "etsy")

    patched <- (p1 | p2) / (p3 | p4)

  }else{

    print("Invalid themecollection name. Returning.")
    return()

  }

  return(patched)

}
