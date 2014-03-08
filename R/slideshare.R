Slideshare <- setRefClass(Class="Slideshare",
                  fields = list(apikey="character", 
                                sharedsecret="character"
                                )
                  )

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

Slideshare$methods(
  getSlideshowByTag = function(tag=NULL, limit=100) {
    require(XML)
    require(digest)
    require(RCurl)
    if(is.null(tag)){
      stop("tag must be required")
    }
    if(!(limit>0)){
      stop("limit must be greater than 0")
    }
    u <- "https://www.slideshare.net/api/2/get_slideshows_by_tag?"
    tstamp <- as.numeric(as.POSIXlt(Sys.time()))
    hash <- digest(paste0(sharedsecret, tstamp),algo="sha1", serialize=FALSE)
    url <- paste0(u,
                  "api_key=", apikey,
                  "&ts=", tstamp,
                  "&hash=", hash,
                  "&tag=", tag,
                  "&limit=", limit,
                  "&detailed=1")
    docxml <- getURL(url)
    res <- xmlToDataFrame(docxml,stringsAsFactors=FALSE)
    res <- subset(res, !is.na(ID))
    res$text <- NULL
    invisible(res)
  })

Slideshare$methods(
  getSlideshowByUser = function(user=NULL, limit=NULL) {
    require(XML)
    require(digest)
    require(RCurl)
    if(is.null(user)){
      stop("user must be required")
    }
    u <- "https://www.slideshare.net/api/2/get_slideshows_by_user?"
    tstamp <- as.numeric(as.POSIXlt(Sys.time()))
    hash <- digest(paste0(sharedsecret, tstamp),algo="sha1", serialize=FALSE)
    url <- paste0(u,
                  "api_key=", apikey,
                  "&ts=", tstamp,
                  "&hash=", hash,
                  "&username_for=", user,
                   ifelse(!(is.null(limit)), paste0("&limit=", limit), ""),
                  "&detailed=1")
    docxml <- getURL(url)
    res <- xmlToDataFrame(docxml,stringsAsFactors=FALSE)
    res <- subset(res, !is.na(ID))
    res$text <- NULL
    invisible(res)
  })

createSlideshowByTag <- function(tag=NULL, limit=100, apikey=NULL, sharedsecret=NULL, file="output.html"){
  require(rCharts)  
  # get data    
  if(is.null(tag)){
    stop("tag must be required")
  }
  if(!(limit>0)){
    stop("limit must be greater than 0")
  }
  if((is.null(apikey)||is.null(sharedsecret))){
    stop("APIkey and Sharedsecret must be required")
  }
  ss <- Slideshare$new(apikey=apikey,sharedsecret=sharedsecret)
  res <- ss$getSlideshowByTag(tag,limit=limit)
  
  res$EmbedModified <- gsub('width="427"','width="600"',res$Embed)
  releaseList <- plyr::alply(res, 1, function(x) {
    list(startDate = format(as.Date(x$Created), "%Y,%m,%d"), 
         headline = x$Title, 
         text = x$EmbedModified,
         asset=list()
    )
  })
  
  m = Timeline$new()
  m$main(headline = res[res$Created==min(res$Created),"Title"],
         type = "default", 
         text = res[res$Created==min(res$Created),"EmbedModified"],
         startDate = format(as.Date(min(res$Created)), "%Y,%m,%d"), 
         asset = list()
  )
  names(releaseList) <- NULL
  m$event(releaseList)
  tmp <- tempfile()
  m$save(tmp)
  
  x <- paste(readLines(tmp, warn = F), collapse = "\n")
  x <- gsub(paste0(system.file(package="rCharts"),"/libraries/timeline/js/storyjs-embed.js"), 
            "http://timelyportfolio.github.io/rCharts_timeline_r/compiled/js/storyjs-embed.js", 
            x)
  writeLines(x, con = file)
}

createSlideshowByUser <- function(user=NULL, limit=100, apikey=NULL, sharedsecret=NULL, file="output.html"){
  require(rCharts)  
  # get data    
  if(is.null(user)){
    stop("user must be required")
  }
  if(!(limit>0)){
    stop("limit must be greater than 0")
  }
  if((is.null(apikey)||is.null(sharedsecret))){
    stop("APIkey and Sharedsecret must be required")
  }
  ss <- Slideshare$new(apikey=apikey,sharedsecret=sharedsecret)
  res <- ss$getSlideshowByUser(user,limit=limit)
  
  res$EmbedModified <- gsub('width="427"','width="600"',res$Embed)
  releaseList <- plyr::alply(res, 1, function(x) {
    list(startDate = format(as.Date(x$Created), "%Y,%m,%d"), 
         headline = x$Title, 
         text = x$EmbedModified,
         asset=list()
    )
  })
  
  m = Timeline$new()
  m$main(headline = res[res$Created==min(res$Created),"Title"],
         type = "default", 
         text = res[res$Created==min(res$Created),"EmbedModified"],
         startDate = format(as.Date(min(res$Created)), "%Y,%m,%d"), 
         asset = list()
  )
  names(releaseList) <- NULL
  m$event(releaseList)
  tmp <- tempfile()
  m$save(tmp)
  
  x <- paste(readLines(tmp, warn = F), collapse = "\n")
  x <- gsub(paste0(system.file(package="rCharts"),"/libraries/timeline/js/storyjs-embed.js"), 
            "http://timelyportfolio.github.io/rCharts_timeline_r/compiled/js/storyjs-embed.js", 
            x)
  writeLines(x, con = file)
}


# test_file("inst/tests/test-slideshare.R")