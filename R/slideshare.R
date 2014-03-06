Slideshare <- setRefClass(Class="Slideshare",
                  fields = list(apikey="character", 
                                sharedsecret="character")
                  )

Slideshare$methods(
  getSlideshowById = function(id=NULL) {
  require(XML)
  require(digest)
  require(RCurl)
  if(is.null(id)){
    stop("id must be required")
  }
  tstamp <- as.numeric(as.POSIXlt(Sys.time()))
  hash <- digest(paste0(sharedsecret, tstamp),algo="sha1", serialize=FALSE)
  url <- paste0(u, 
                "slideshow_id=", id,
                "&api_key=", apikey,
                "&ts=", tstamp,
                "&hash=", hash)
  docxml <- getURL(url)
  docxmlparsed <- xmlInternalTreeParse(docxml)
  r <- xmlRoot(docxmlparsed)
  res <- data.frame(t(xmlSApply(r, xmlValue)))
})