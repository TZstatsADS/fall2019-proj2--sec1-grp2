library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(sp)
library(raster)
library(rgdal)
library(shinyWidgets)
library(maptools)
library(tigris)
library(RColorBrewer)
library(dplyr)
library(grDevices)


df_2015 <- readRDS("../data/sample_data_2015.rds")
df_2005 <- readRDS("../data/sample_data_2005.rds")
df_1995 <- readRDS("../data/sample_data_1995.rds")
load("../data/pollution.RData")
NYC_coord <- c(lon = -74.00597, lat = 40.71278)

selected_cols <- c('tree_id','tree_dbh','status','health','spc_common','address','postcode','zip_city','borough','state','latitude','longitude')
processed_df <- df_2015[,selected_cols]
processed_df$full_address <- mutate(processed_df, full_address = paste(address, zip_city, state, postcode, sep = ', '))
df_2015$nta <- factor(df_2015$nta, levels = nbhood@data$ntacode)

# nbhood <- readShapePoly("../data/nbhood/geo_export_63264cca-db33-43e7-ac15-9019c83788c0.shp")
nbhood <- readShapePoly("../data/nbhood/geo_export_63264cca-db33-43e7-ac15-9019c83788c0.shp")



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

######## Data Loading
nbhood <- readShapePoly("../data/nbhood/geo_export_63264cca-db33-43e7-ac15-9019c83788c0.shp")
tree <- readRDS("../data/sample_data_2015.rds")
#tree <- readRDS("../../data/data_2015.rds")

######## Data Processing
# tree$nta <- factor(tree$nta, levels = nbhood@data$ntacode)
# tree_count <- tree %>%
#   group_by(nta, .drop = F) %>%
#   #    group_by(nta) %>%
#   tally() 
# tree_count$tree_per_km2 <- tree_count$n / nbhood@data$shape_area * 10^6
# tree_count <- tree_count %>%
#   dplyr::filter(n != 0)
# 
# combined <- geo_join(nbhood, tree_count, by_sp = "ntacode", by_df = "nta")
# save(combined, file = "../../data/combined.rdata")
# 
# data_by_borough <- list()
# 
# data_by_borough$Manhattan <- combined[combined@data$boro_name == "Manhattan",]
# data_by_borough$Brooklyn <- combined[combined@data$boro_name == "Brooklyn",]
# data_by_borough$Queens <- combined[combined@data$boro_name == "Queens",]
# data_by_borough$StatenIsland <- combined[combined@data$boro_name == "Staten Island",]
# data_by_borough$Bronx <- combined[combined@data$boro_name == "Bronx",]
# 
# save(data_by_borough, file = "../../data/data_by_borough.rdata")
load("../data/combined.rdata")
load("../data/data_by_borough.rdata")

NYC_coord <- c(lon = -74.00597, lat = 40.71278)

legend_title <- c("Tree count", "Tree density (/KM2)")

popup <- list()

for(i in 1:5){
  popup1 <- paste("<strong>Neighborhood: </strong>", data_by_borough[[i]]@data$ntaname,
                  "<br><strong>Counts: </strong>", data_by_borough[[i]]@data$n)
  popup2 <- paste("<strong>Neighborhood: </strong>", data_by_borough[[i]]@data$ntaname,
                  "<br><strong>Density: </strong>", data_by_borough[[i]]@data$tree_per_km2)
  popup[[i]] <- list(popup1, popup2)
}



# different measurements ----------------------------------------------------------------------------------------------------------------------
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

######## Data Loading
nbhood <- readShapePoly("../data/nbhood/geo_export_63264cca-db33-43e7-ac15-9019c83788c0.shp")
tree <- readRDS("../data/sample_data_2015.rds")
#tree <- readRDS("../../data/data_2015.rds")

######## Data Processing
# tree$nta <- factor(tree$nta, levels = nbhood@data$ntacode)
# tree_count <- tree %>%
#   group_by(nta, .drop = F) %>%
#   #    group_by(nta) %>%
#   tally() 
# tree_count$tree_per_km2 <- tree_count$n / nbhood@data$shape_area * 10^6
# tree_count <- tree_count %>%
#   dplyr::filter(n != 0)
# 
# combined <- geo_join(nbhood, tree_count, by_sp = "ntacode", by_df = "nta")
# save(combined, file = "../../data/combined.rdata")
# 
# data_by_borough <- list()
# 
# data_by_borough$Manhattan <- combined[combined@data$boro_name == "Manhattan",]
# data_by_borough$Brooklyn <- combined[combined@data$boro_name == "Brooklyn",]
# data_by_borough$Queens <- combined[combined@data$boro_name == "Queens",]
# data_by_borough$StatenIsland <- combined[combined@data$boro_name == "Staten Island",]
# data_by_borough$Bronx <- combined[combined@data$boro_name == "Bronx",]
# 
# save(data_by_borough, file = "../../data/data_by_borough.rdata")
load("../data/combined.rdata")
load("../data/data_by_borough.rdata")

NYC_coord <- c(lon = -74.00597, lat = 40.71278)

legend_title <- c("Tree count", "Tree density (/KM2)")

popup <- list()

for(i in 1:5){
  popup1 <- paste("<strong>Neighborhood: </strong>", data_by_borough[[i]]@data$ntaname,
                  "<br><strong>Counts: </strong>", data_by_borough[[i]]@data$n)
  popup2 <- paste("<strong>Neighborhood: </strong>", data_by_borough[[i]]@data$ntaname,
                  "<br><strong>Density: </strong>", data_by_borough[[i]]@data$tree_per_km2)
  popup[[i]] <- list(popup1, popup2)
}
