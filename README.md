slideshare
==========
Get slideshare information.

This IS NOT a slideshare SDK.

You can just get information. You can't use other methods(upload, delete, and so on).

# Usage
```
key <- "Your APIkey"
secret <- "Your sharedsecret" 
test <- Slideshare$new(apikey=key,sharedsecret=secret)
res <- test$getSlideshowById(id=13343768)
```
