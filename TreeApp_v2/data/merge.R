df1 = readRDS("sample_data_1995.rds")
df2 = readRDS("sample_data_2005.rds")
df3 = readRDS("sample_data_2015.rds")

n1 = dim(df1)[1]
n2 = dim(df2)[1]
n3 = dim(df3)[1]

df1["Year"] = rep(1995,n1)
df1["tree_dbh"] = rep(0,n1)
df1["status"] = df1["Condition"]
df1["zip_city"] = df1["Borough"]
df1["state"] = "New York"
df2["Year"] = rep(2005,n2)
df3["Year"] = rep(2015,n3)


tmp1<- df1[,c("RecordId","tree_dbh","status","Address","Zip_New","zip_city","state","Borough","latitude","latitude","Spc_Common","NTA_2010","Condition","Year")]
tmp2<- df2[,c("OBJECTID","tree_dbh","status","address","zipcode","zip_city","state","boroname","longitude","latitude","spc_common","nta","status","Year")]
tmp3<- df3[,c("tree_id","tree_dbh","status","address","postcode","zip_city","state","borough","longitude","latitude","spc_common","nta","health","Year")]


# c('tree_id','tree_dbh','status','health','spc_common','address','postcode','zip_city','borough','state','latitude','longitude')
colnames(tmp1)<-c("tree_id","tree_dbh","status","address","postcode","zip_city","state","borough","longitude","latitude","spc_common","nta","health","Year")
colnames(tmp2)<-c("tree_id","tree_dbh","status","address","postcode","zip_city","state","borough","longitude","latitude","spc_common","nta","health","Year")

data <- rbind(tmp1, tmp2, tmp3)
saveRDS(data, "sample_data.rds")
# unique(data$borough)
# unique(data$spc_common)
