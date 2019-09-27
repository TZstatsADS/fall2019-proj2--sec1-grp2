df_2015 <- read.csv("./data/2015_Street_Tree_Census.csv", stringsAsFactors = F)
df_2015$Latitude <- as.numeric(df$Latitude)
df_2015$Longitude <- as.numeric(df$Longitude)

sample_data_2015 <- df_2015[c(1:1000),]
saveRDS(sample_data_2015, "./data/2015_sample_data.rds")





















