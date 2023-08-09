# ggautothemes by Lukas Taylor
Managing ggplot themes shouldn't be such a hassle. So, I've done the work for you! Run our autoallthemes function and R will start a slideshow showing your ggplot2 plot in various themes.

Quick Installation:

```
library(devtools)
install_github("lukastay/ggautothemes")
library(ggautothemes)
```

To Run A Slideshow Of All The Themes, Simply Input A ggplot Object:
```
g <- ggplot(faithful)
autoallthemes(g)
```

![autoallthemes](https://github.com/lukastay/Lukas-Taylor-Repository/blob/master/photos/showcase.gif?raw=true)

Once You've Found A Theme You Like, You Can Plot It. Or Set HQExport To True To Export It As A Professional Grade Raster Image File:

```
autochoosetheme(g, theme = "theme_light", HQexport = TRUE, size = "small")
```

![choose_theme](https://github.com/lukastay/Lukas-Taylor-Repository/blob/master/photos/ggautothemes_exported_graph.tiff?raw=true)
