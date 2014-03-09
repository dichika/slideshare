slideshare
==========
Get slideshare information.

This IS NOT a slideshare SDK.

You can just get information. You can't use other methods(upload, delete, and so on).

# Example
```
# At first, you have to get APIkey and sharedsecret at the URL below.
# http://www.slideshare.net/developers/applyforapi

devtools::install_github("dichika/slideshare")

key <- "Your APIkey"
secret <- "Your sharedsecret" 

library(slideshare)
test <- Slideshare$new(apikey=key,sharedsecret=secret)
res <- test$getSlideshow("http://www.slideshare.net/dichika/r25lt")
resTag <- test$getSlideshowByTag("tokyor")
resUser <- test$getSlideshowByUser("dichika")

# You can create slideshow from slideshare (You need to install rCharts and plyr)

# install.packages("plyr")
# devtools::install_github("ramnathv/rCharts")

createSlideshowByTag(tag="tokyor", limit=100, apikey=key, sharedsecret=secret, file="tokyor.html")
browseURL("tokyor.html")

createSlideshowByUser(user="dichika", limit=100, apikey=key, sharedsecret=secret, file="dichika.html")
browseURL("dichika.html")

```
