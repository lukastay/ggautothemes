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

![autoallthemes](toadd)
