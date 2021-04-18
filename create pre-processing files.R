### 20201020 ###
## 匯出所需檔的過程 ##

## 1. datDecoord 詳測土壤圖的所有座標
setwd("D://1091/thesis_data/DetailedSoilMap")# 筆電
setwd("C:/Users/USER/Desktop/1082/SoilMap/DetailedSoilMap/")  # 桌電

datDe = readOGR("DetailedSoilMapWGS84.shp", encoding="UTF-8")

datDecoord = list()
for(i in 1:32368){
  datDecoord[[i]] = cbind(round(datDe[i,]@polygons[[1]]@Polygons[[1]]@coords[,1],3), 
                          round(datDe[i,]@polygons[[1]]@Polygons[[1]]@coords[,2],3))
  print(round(i/32368,3))
}
#　write out file
library(erer)
write.list(datDecoord, file = "datDecoord.csv")
da = fread("datDecoord.csv")


# 2. 找出Hen 等人產製的10公里網格的點
tenkmgrid = read.table("yield(grid).txt", header = T)
colnames(tenkmgrid) = c("ID","lat","lon", "yield")
lon = as.numeric(as.character(as.factor(tenkmgrid[,3])))
lat = as.numeric(as.character(as.factor(tenkmgrid[,2])))
tenkmgridcoord = cbind(lon, lat)
write.csv(tenkmgridcoord, file = "tenkmgridcoord.csv")


# 3. 找出DSM中，符合 Hen 等人產製的10公里網格的點 (包含3位數以及2位數)
library(data.table)
da = fread("datDecoord.csv")
colnames(da) = c("NO.", "lon", "lat")

## 3 位小數點座標
d = as.data.frame(da[da$NO.!="Result"])
dd = d[ d$lon != "",]
ddd = as.data.frame(cbind(as.numeric(as.character(dd$lon)), as.numeric(as.character(dd$lat))))
colnames(ddd) = c("lon","lat")
fin = inner_join(ddd,as.data.frame(tenkmgridcoord), by = c("lon"="lon","lat"="lat"))
uni3 = unique(fin)
write.csv(uni3, file = "Corresponding coordinates for three decimal places.csv")

## 2 位小數點座標
HC27coor = as.data.frame(tenkmgridcoord)
HC27coor2 = round(HC27coor,2)

ddd2 =  round(ddd,2)
uni2 = unique(inner_join(ddd2,HC27coor2, by = c("lon"="lon","lat"="lat")))
dim(inner_join(ddd2,HC27coor2, by = c("lon"="lon","lat"="lat"))) # 17607 
dim(unique(uni2)) #154

write.csv(uni2, file = "Corresponding coordinates for two decimal places.csv")


# 4. Corresponding back to the coordinates.csv (1026 points)

## combine number and lon/lat
# Corresponding back to the coordinates
library(stringr)
clean = function(x){
  unlist(strsplit(x, split='.', fixed=TRUE))[2]
}

ddNO. = lapply(dd$NO., clean) %>% as.numeric()
combines = as.data.frame(cbind(ddNO. , 
                               round(as.numeric(dd$lon),2), 
                               round(as.numeric(dd$lat),2)))


colnames(combines) = c("NO.", "lon", "lat")

fij = inner_join(combines, uni2, by = c("lon"="lon","lat"="lat")) #17607 
uniquefij = unique(fij)
uniquefijnumber = uniquefij$NO.
dim(uniquefij) #1026
write.csv(uniquefij, file = "Corresponding back to the coordinates.csv")

# 5. three digits for 1024 points(final layercode)

lon.3 = c()
lat.3 = c()

for(i in 1:length(NO.number)){
  
  lon.3[i] = da[da$Result == paste0("result.", NO.number[i]) ,]$V1 %>% as.numeric %>% mean() %>% round(3)
  lat.3[i] = da[da$Result == paste0("result.", NO.number[i]) ,]$V2 %>% as.numeric %>% mean() %>% round(3)
}
to3 = cbind(lon.3, lat.3) %>% format( nsmall = 3)
to33 = expandRows(to3, count = 4, count.is.col = FALSE)

write.csv(to3, "three digits for 1024 points.csv")
write.csv(to33, "three digits for 1024 points(4104.csv")




