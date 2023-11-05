# mining map 
library(readr)
library(sp)
library(ggplot2)
library(ggmap)
library(ggspatial)
library(maps)
library(sf)
library(tidyverse)

setwd("C:/Users/SAGGESE/Documents/GitHub/zambia/raws")

# read in KMLs
globalmines <- read_sf("global-mineral-depo-map.kml")
critmin <- read_sf("global-mines-depos-critical-minerals.kml")
# in crit mineral - need to 
## a) parse out only entries with zambia (13)
## b) parse xml in description column - make new cols for data 
## c) plot critical mineral reserves 

# https://mrdata.usgs.gov/major-deposits/package.php 
zambia <- read_sf("usgszambia")

ggplot() +
  labs(x="Longitude (WGS84)", y="Latitude",
       title="Zambia mineral map") + 
  geom_sf(data=zambia, col="blue", lwd=0.4, pch=21) +
  theme_bw()

boundary <- read_sf("zmb_shp")

# create plot of zambia mines over the country map 
ggplot() + 
  geom_sf(data = boundary, size = 1.5, color = "black") + 
  ggtitle("Zambia") + 
  geom_sf(data=zambia, col="blue")
  coord_sf()

                        