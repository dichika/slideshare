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
```
