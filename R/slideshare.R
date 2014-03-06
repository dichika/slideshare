Slideshare <- setRefClass(Class="Slideshare",
                  fields = list(apikey="character", 
                                sharedsecret="character"
                                )
                  )

Slideshare$methods(
  getSlideshowById = function(id=NULL) {
  require(XML)
  require(digest)
  require(RCurl)
  if(is.null(id)){
    stop("id must be required")
  }
  u <- "https://www.slideshare.net/api/2/get_slideshow?"
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

Slideshare$methods(
  getSlideshow = function(url=NULL, id=NULL) {
    require(XML)
    require(digest)
    require(RCurl)
    if(all(is.null(url), is.null(id))){
      stop("url or id must be required")
    }
    u <- "https://www.slideshare.net/api/2/get_slideshow?"
    if(!is.null(url)){
      m <- paste0("slideshow_url=", url)
    } else {
      m <- paste0("slideshow_id=", id)      
    }
    tstamp <- as.numeric(as.POSIXlt(Sys.time()))
    hash <- digest(paste0(sharedsecret, tstamp),algo="sha1", serialize=FALSE)
    url <- paste0(u, m,
                  "&api_key=", apikey,
                  "&ts=", tstamp,
                  "&hash=", hash,
                  "&detailed=1")
    docxml <- getURL(url)
    docxmlparsed <- xmlInternalTreeParse(docxml)
    r <- xmlRoot(docxmlparsed)
    res <- data.frame(t(xmlSApply(r, xmlValue)), stringsAsFactors=FALSE)
  })