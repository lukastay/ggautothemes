% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autochoosetheme.R
\name{autochoosetheme}
\alias{autochoosetheme}
\title{autochoosetheme}
\usage{
autochoosetheme(ggplotvisual, theme, HQexport = FALSE, size = "medium")
}
\arguments{
\item{ggplotvisual}{Required. ggplot2 visual that you'd like to see in various themes.}

\item{theme}{No Default. Enter the theme exactly as it appears in the title of the other function's plots.}

\item{HQexport}{Default is FALSE. If set to TRUE, it will export a professional grade raster image of your chosen themed plot.}

\item{size}{Default is medium. Enter string as "small" or "large" to set size of image exported by HQexport. Smaller sizes have larger text relative to the plot.}
}
\value{
Returns a plot with the theme chosen.
}
\description{
autochoosetheme
}
\details{
Outputs a single graph based on the visuals you chose.
}
\examples{

library(ggplot2)

p <- ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))
p <- p + geom_point()
p <- p + guides(color="none")
p <- p + theme(legend.position="none")

autochoosetheme(ggplotvisual = p, theme = "theme_grey")

}
