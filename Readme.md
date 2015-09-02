# GPX_Overview
An R script that produces maps of .gpx tracks file, including an interactive leaflet map, and compute the speed between points.  
Check and adapt the path setwd() **at 3 places** for your own folders.  
Open 20150721_Apercu-GPX_Complet.R script, run the two functions and play around with GPX_Overview() and its arguments.  
20150721_Apercu-GPX_Complet.R is calling Apercu-GPX-Full_2html.Rmd for the .html output.   
Note that the **.Rmd script is sourcing a short external R script** which is not provided, that I use to call my personal MapBox token. If you use MapBox, you'll have to adapt the script in the same way or remove the source() line and include the token in the .Rmd script.  
There scripts can easily be simplified to increase computing performances. Feel free to branch it!  

This repository provides examples of files produced from two different .gpx files.  
