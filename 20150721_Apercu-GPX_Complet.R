rm(list=ls())
library(RgoogleMaps)
library(rgdal)
library(knitr)
library(leaflet)

# The function 'reverseGeoCode' is taken from J. Aswani post on http://allthingsr.blogspot.com/2012/03/geocode-and-reverse-geocode-your-data.html
# Ref: J. Aswani, “All Things R: Geocode and reverse geocode your data using, R, JSON and Google Maps’ Geocoding API,” All Things R, 20-Mar-2012. .
reverseGeoCode <- function(latlng) {
  latlngStr <-  gsub(' ','%20', paste(latlng, collapse=","))#Collapse and Encode URL Parameters
  library("RJSONIO") #Load Library
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=',latlngStr, sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK")
    address <- data.json["results.formatted_address"]
  return (address)
}

### The function 'GPX_Overview()':
## * If "GPXname=NULL" the .gpx files processed are those which do not yet exist as .png overview (the names are collected in 'AF')
## One or more .gpx file names can be provided as text vector
## * "option" should be specified whether to output only the .png map, to output the html report (with leaflet interactive map) or both
GPX_Overview <- function(GPXname=NULL,option=c("SimplePNG","htmlReport","both")){
  setwd("/home/jf/Dropbox (CARAH)/_Carto⁄LIFE-ELIA[dropBox]/Fichiers GPX/GPX2PNG") ### On my laptop
  ## List of the .gpx files which were not yet processed
  GPXfile.list <- list.files(".", pattern = "\\.gpx$")
#  PNGfile.list <- list.files("./OutputPNG")[grep(".png",list.files("./OutputPNG"))]
  PNGfile.list <- list.files("./OutputPNG", pattern= "\\.png$")
#  comp <- substring(PNGfile.list,1,25) ## Adapt substring start and stop to your .gpx name
  comp <- paste0(sapply(strsplit(PNGfile.list, ".gpx_"), function(x) x[[1]]),".gpx")
  AF <- GPXfile.list[is.na(match(GPXfile.list,comp))]
  if(!is.null(GPXname)){AF <- GPXname} ## Choose .gpx files to process
  ### sapply the script on each .gpx file
  sapply(AF,function(GPXfile) {filename <<- GPXfile
  pts <<-  readOGR(GPXfile, layer = "track_points")
  ## Keep the coordinates with 'native' CRS (WGS84) in decimal degrees
  crd.gpx <<- data.frame(pts@coords)
  colnames(crd.gpx) <<- c("Long", "Lat")
  ## Reproject on metric system
  pts.crd <<- spTransform(pts,CRS("+init=epsg:3035"))@coords ### EUROPE
  ### Produce map using RGoogleMaps library
  bb <<- qbbox(pts@bbox[2,],pts@bbox[1,], TYPE = "all",
               margin = list(m = rep(1, 4),TYPE = c("perc", "abs")[1]))
  range.lat <- 3*(bb$latR[2]-bb$latR[1])
  range.lon <- 3*(bb$lonR[2]-bb$lonR[1])
  bb.w <<- list(latR=c(bb$latR[1]-range.lat,bb$latR[2]+range.lat)
                ,lonR=c(bb$lonR[1]-range.lon,bb$lonR[2]+range.lon))
  ### Create MyMap and export as .png background before plotting track points
  MyMap <<- GetMap.bbox(bb$lonR, bb$latR, destfile = paste(getwd(),"/OutputPNG/FondCarte/FondCarteGPX-",GPXfile,".png",sep="")
                        , maptype = "hybrid",size = c(640, 640))
  Ncut <<- as.integer(sqrt(length(pts)))
  COL <<- cut(1:length(pts),Ncut,labels=heat.colors(Ncut))
  
  ### Find the first identified address to get country name (used in naming the .png output)
  ads.beg <- NULL; nbeg <- 1
  while(!is.character(ads.beg)){ads.beg <- try(reverseGeoCode(crd.gpx[nbeg,2:1]))
  nbeg <- nbeg+1}
  SpltAds <- unlist(strsplit(ads.beg,", "))
  CNTRY <<- SpltAds[length(SpltAds)]
  # Simple .png output of the track on google hybrid background
  if(option=="SimplePNG" | option=="both"){
    png(paste(getwd(),"/OutputPNG/",GPXfile,"_",CNTRY, ".png",sep="")
        ,width=800,height=800)
    PlotOnStaticMap(MyMap, lat = crd.gpx$Lat, lon = crd.gpx$Long, size = c(640,640),
                    cex = 1, pch = 20, col = as.character(COL), add = F)
    dev.off()}
  ### Call .Rmd for knitting into .html
  if(option=="htmlReport" | option=="both"){
    MyMapWideHyb <<- GetMap.bbox(bb.w$lonR, bb.w$latR, destfile =
                                   paste(getwd(),"/OutputPNG/FondCarte/FondCarteGPX-",GPXfile,"Hyb_WIDE.png",sep="")
                                 , maptype = "hybrid",size = c(640, 640))
    MyMapWideTer <<- GetMap.bbox(bb.w$lonR, bb.w$latR, destfile = paste(getwd(),"/OutputPNG/FondCarte/FondCarteGPX-",GPXfile,"Ter_WIDE.png",sep="")
                                 , maptype = "terrain",size = c(640, 640))
    
    setwd('/home/jf/Dropbox (CARAH)/_Carto⁄LIFE-ELIA[dropBox]/Fichiers GPX/GPX2PNG/Output_html/')
    # Note!!: knit2html is not working with leaflet widget creation. The widget is not visible in the html page, only the message "!–html_preserve–"
    # NOT RUN #knit2html(input='/home/jf/Dropbox (CARAH)/_Carto⁄LIFE-ELIA[dropBox]/Fichiers GPX/Apercu-GPX-Full_2html.Rmd',output=GPXfile,envir=globalenv()) 
    ## The solution is explained here: http://stackoverflow.com/questions/28585238/r-rendering-html-widget-in-external-browser
    rmarkdown::render(input='/home/jf/R/GitHub/GPXView/Apercu-GPX-Full_2html.Rmd'
                      ,output_dir='.'
                      ,output_file=paste(GPXfile,'_',CNTRY,'.html',sep="")
                      ,envir=globalenv())
    setwd("/home/jf/Dropbox (CARAH)/_Carto⁄LIFE-ELIA[dropBox]/Fichiers GPX/GPX2PNG")
  }
  })
}

list.files("/home/jf/Dropbox (CARAH)/_Carto⁄LIFE-ELIA[dropBox]/Fichiers GPX/GPX2PNG", pattern = ".gpx")

GPX_Overview(option="both")
GPX_Overview(option="SimplePNG")
GPX_Overview(GPXname="2014-02-xx_nietoperze.gpx",option="both") ## 
GPX_Overview(GPXname="20170330112918.gpx",option="htmlReport") ## ?????????????
GPX_Overview(GPXname="20170330112918.gpx",option="SimplePNG") ## 


Files2View <- c("20170405170033.gpx",
                "20170405172945.gpx") # Some names
GPX_Overview(GPXname=Files2View,option="htmlReport")

