
rm(list = ls())
setwd("C:/Users/saptashya.ghosh/Dropbox/2.raw/GDP HDI/GDP PPP/")

#install.packages("chillR")
#install.packages("GenSA", type = "binary")
#install.packages("rgdal")
#install.packages("rgdal", repos="http://cran.r-project.org")


# Loading the libraries

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(ggplot2) # package for plotting
library(ecmwfr)
library(tidyverse)
library(lubridate)
library(abind)
library(dplyr)
library(reshape2)
library(sf)
library(dplyr)
library(raster)
library(data.table)
library(plm)
library(chillR)

#####For GDP Grid######

# Reading the nc data

data_gdp <- nc_open('gdp_dryad.nc')

lon <- ncvar_get(data_gdp, "longitude") #This gives the (N,S) grid location
lat <- ncvar_get(data_gdp, "latitude", verbose = F)
t <- ncvar_get(data_gdp, "time")
gdp_ppp <- ncvar_get(data_gdp,"GDP_per_capita_PPP")

gdp_ppp_2015 <- gdp_ppp[1:4320,1:2160,26]
rm(gdp_ppp)

#With respect to the latitudinal and longitudinal extent, 
# India lies between latitudes 8° 4'N and 37° 6'N, and the 
# longitudes 68° 7'E and 97° 25'E.
which(lon > 97 & lon < 98)
lon[3328]

which(lon > 68 & lon < 69)
lon[2984]

# So longitude in index (2984,3328) is India

which(lat>8 & lat < 9)
lat[980]

which(lat > 37 & lat < 38)
lat[630]

lon1 <- as.data.table(seq(2984,3328,1))
lat1 <- as.data.table(seq(630,980,1))

gdp_india <- expand.grid(lon1=lon1$V1, lat1=lat1$V1)

extract_value <- function(var,idx){
  return(var[idx])
}

gdp_india$lon_value <- mapply(extract_value, var= list(lon), gdp_india$lon1)
gdp_india$lat_value <- mapply(extract_value, var= list(lat), gdp_india$lat1)

extract_value_gdp <- function(var,lon,lat){
  return(var[lon,lat])
}

gdp_india$gdp_ppp <- mapply(extract_value_gdp, var=list(gdp_ppp_2015), gdp_india$lon1, gdp_india$lat1)

#####For HDI Grid######


# Reading the nc data

data_hdi <- nc_open('HDI_1990_2015_v2.nc')


lon_hdi <- ncvar_get(data_hdi, "longitude") #This gives the (N,S) grid location
lat_hdi <- ncvar_get(data_hdi, "latitude", verbose = F)
t <- ncvar_get(data_hdi, "time")
hdi <- ncvar_get(data_hdi,"HDI")

hdi_2015 <- hdi[1:4320,1:2160,26]
rm(hdi)

#With respect to the latitudinal and longitudinal extent, 
# India lies between latitudes 8° 4'N and 37° 6'N, and the 
# longitudes 68° 7'E and 97° 25'E.
which(lon_hdi > 97 & lon_hdi < 98)
lon_hdi[3328]

which(lon_hdi > 68 & lon_hdi < 69)
lon_hdi[2984]

# So longitude in index (2984,3328) is India

which(lat_hdi>8 & lat_hdi < 9)
lat_hdi[980]

which(lat > 37 & lat < 38)
lat_hdi[630]

lon1_hdi <- as.data.table(seq(2984,3328,1))
lat1_hdi <- as.data.table(seq(630,980,1))

hdi_india <- expand.grid(lon1_hdi=lon1_hdi$V1, lat1_hdi=lat1_hdi$V1)

extract_value <- function(var,idx){
  return(var[idx])
}

hdi_india$lon_value_hdi <- mapply(extract_value, var= list(lon_hdi), hdi_india$lon1_hdi)
hdi_india$lat_value_hdi <- mapply(extract_value, var= list(lat_hdi), hdi_india$lat1_hdi)

extract_value_hdi <- function(var,lon_hdi,lat_hdi){
  return(var[lon_hdi,lat_hdi])
}

hdi_india$hdi <- mapply(extract_value_hdi, var=list(hdi_2015), hdi_india$lon1_hdi, hdi_india$lat1_hdi)


#Set the directory
setwd("C:/Users/saptashya.ghosh/Dropbox/pincode 2022 - shapefiles/pincode 2022 - shapefiles/")


# Reading the shapefile

centroid_shape <- st_read('centroid.shp')

# Extract longitude and latitude from the "geometry" column
coords <- st_coordinates(centroid_shape)

# Extract longitude and latitude from the "geometry" column
centroid_shape <- st_transform(centroid_shape, crs = 4326) # Transform to WGS84 (assuming it's not already)

# Remove the geometry column
centroid_shape$geometry <- NULL

# Split geometry into longitude and latitude columns
centroid_shape$longitude <- coords[, 1]
centroid_shape$latitude <- coords[, 2]


##Method1 (OG Method)
# Convert data frames to sf objects
centroid_sf <- st_as_sf(centroid_shape, coords = c("longitude", "latitude"), crs = 4326)
gdp_sf <- st_as_sf(gdp_india, coords = c("lon_value", "lat_value"), crs = 4326)
hdi_sf <- st_as_sf(hdi_india, coords = c("lon_value_hdi", "lat_value_hdi"), crs = 4326)

# Find nearest GDP point for each centroid point
nearest_gdp <- st_nearest_feature(centroid_sf, gdp_sf)

# Find nearest hdi point for each centroid point
nearest_hdi <- st_nearest_feature(centroid_sf, hdi_sf)

# Extract nearest GDP values
closest_gdp_data <- gdp_sf[nearest_gdp, ]

# Extract nearest HDI values
closest_hdi_data <- hdi_sf[nearest_hdi, ]


# Combine the data with original centroids, for a non-spatial join(THE needed Data)
combined_data <- cbind(centroid_shape, as.data.frame(closest_gdp_data), as.data.frame(closest_hdi_data))

# Remove the geometry column
combined_data$geometry <- NULL
combined_data$geometry <- NULL


# Export the Combined data
write.csv(combined_data, "gdp_hdi.csv", row.names = FALSE)










