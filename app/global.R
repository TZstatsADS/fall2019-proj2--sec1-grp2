library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) 
library(ggplot2)
library(raster)
library(rgdal)
library(shinyWidgets)
library(maptools)
library(tigris)
library(RColorBrewer)
library(dplyr)
library(grDevices)
library(rgeos)

df = readRDS("data/sample_data.rds")
load("data/pollution.RData")
nbhood <- rgdal::readOGR("data/nbhood/geo_export_63264cca-db33-43e7-ac15-9019c83788c0.shp")

NYC_coord <- c(lon = -74.00597, lat = 40.71278)
df$full_address <- mutate(df, full_address = paste(address, zip_city, state, postcode, sep = ', '))
df$nta <- factor(df$nta, levels = nbhood@data$ntacode)


# clickable trees ------------------------------------------------------------------------------------------------------
Round <- function(x, type = c("round", "ceiling", "floor")){
  i <- floor(log10(x))
  if(type == "round")
    output <- round(x/10^i * 2) / 2 * 10^i
  else if(type == "ceiling")
    output <- ceiling(x/10^i * 2) / 2 * 10^i
  else
    output <- floor(x/10^i * 2) / 2 * 10^i
  return(output)
}

set_bins <- function(count, n_bins){
  n <- n_bins + 1
  count <- count[!is.na(count)]
  
  if(length(count) < 1000){
    bins <- c(1, Round(seq(0, Round(max(count), "ceiling"), length.out = n)[-1], "round"))
    return(bins)
  }
  
  q <- quantile(count, probs = seq(0, 1, length.out = n))
  bins <- c(1, Round(q[2:(n-1)], "round"), Round(q[n], "ceiling"))
  return(bins)
}

set_pal <- function(bins){
  # pal <- colorBin(c("#808080", colorRampPalette(c("lightgreen", "darkgreen"))(length(bins) - 2)), 
  #                bins = bins)
  pal <- colorBin(colorRampPalette(c("lightgreen", "darkgreen"))(length(bins) - 1), 
                  bins = bins)
  return(pal)
}

set_labels <- function(bins){
  legend_labels <- c(paste(bins[1], "-", bins[2], sep = ""))
  for(i in 2:(length(bins)-1)){
    next_level <- paste(bins[i]+1, "-", bins[i+1], sep = "")
    legend_labels <- c(legend_labels, next_level)
  }
  return(legend_labels)
}




