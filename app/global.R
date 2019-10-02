library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(sp)
library(raster)
library(rgdal)
library(shinyWidgets)
df_2015 <- readRDS("../data/sample_data_2015.rds")
df_2005 <- readRDS("../data/sample_data_2005.rds")
df_1995 <- readRDS("../data/sample_data_1995.rds")
load("../data/pollution.RData")
NYC_coord <- c(lon = -74.00597, lat = 40.71278)

selected_cols <- c('tree_id','tree_dbh','status','health','spc_common','address','postcode','zip_city','borough','state','latitude','longitude')
processed_df <- df_2015[,selected_cols]
processed_df$full_address <- mutate(processed_df, full_address = paste(address, zip_city, state, postcode, sep = ', '))

