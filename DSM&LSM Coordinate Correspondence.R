library(rgdal)
library(dplyr)
library(ggplot2)
setwd("D://1091/thesis_data") # 筆電
setwd("C:/Users/user/Desktop/1091/thesisdata") # 桌電

# 
tenkmgrid = read.table("yield(grid).txt", header = T)
colnames(tenkmgrid) = c("ID","lat","lon", "yield")
lon = as.numeric(as.character(as.factor(tenkmgrid[,3])))
lat = as.numeric(as.character(as.factor(tenkmgrid[,2])))
tenkmgridcoord = cbind(lon, lat)
write.csv(tenkmgridcoord, file = "tenkmgridcoord.csv")

###

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

### test file ###
library(data.table)
da = fread("datDecoord.csv")
head(da)

colnames(da) = c("NO.", "lon", "lat")
# da[da$NO.=="result.200"]
#################


d = as.data.frame(da[da$NO.!="Result"])
dd = d[ d$lon != "",]
ddd = as.data.frame(cbind(as.numeric(as.character(dd$lon)), as.numeric(as.character(dd$lat))))
colnames(ddd) = c("lon","lat")
fin = inner_join(ddd,as.data.frame(tenkmgridcoord), by = c("lon"="lon","lat"="lat"))
uni3 = unique(fin)
#write.csv(uni3, file = "Corresponding coordinates for three decimal places.csv")
# 48點  不足
ggplot2::ggplot(fin, aes(x=lon,y=lat))+   
  geom_point(data=fin, aes(x=fin$lon, y=fin$lat), colour = "purple3")+
  labs(x = "Longitude", y = "Latitude")

## HC27

# HC27coor = as.data.frame(tenkmgridcoord)
# 
# inner_join(ddd,HC27coor, by = c("lon"="lon","lat"="lat"))
# unique(inner_join(ddd,HC27coor, by = c("lon"="lon","lat"="lat")))
# dim(unique(inner_join(ddd,HC27coor, by = c("lon"="lon","lat"="lat"))))

######兩位
HC27coor2 = round(HC27coor,2)

ddd2 =  round(ddd,2)
uni2 = unique(inner_join(ddd2,HC27coor2, by = c("lon"="lon","lat"="lat")))
dim(inner_join(ddd2,HC27coor2, by = c("lon"="lon","lat"="lat"))) # 17607 
dim(unique(uni2)) #154
### 畫各層土壤含水量/質地分類的台灣圖
### 用kriging畫出像Aus那樣的圖
write.csv(uni2, file = "Corresponding coordinates for two decimal places.csv")



ggplot(uni3, aes(x=lon,y=lat))+
  geom_point(data=uni3, aes(x=uni3$lon, y=uni3$lat), colour = "purple3")+
  labs(x = "Longitude", y = "Latitude")


ggplot(uni2, aes(x=lon,y=lat))+
  geom_point(data=uni2, aes(x=lon, y=lat), colour = "purple3")+
  labs(x = "Longitude", y = "Latitude")
#####################
## 想辦法疊圖
###################
########
##把抓出的148個點對應回darDe檔案
# uni2
## 

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

ggplot(uniquefij, aes(x=lon,y=lat))+
  geom_point(data=uniquefij, aes(x=lon, y=lat), colour = "purple3")+
  labs(x = "Longitude", y = "Latitude")
## 畫出來與uni2相同
###################
## read data
uni2 = read.csv("Corresponding coordinates for two decimal places.csv") #154

back_datDe_coords = read.csv("Corresponding back to the coordinates.csv") #1026

NO.number = back_datDe_coords$NO.
length(NO.number) #1026



# 找出質地
# data 從0開始，網格資料從1開始
NO.number_1 = NO.number-1


find_texture = function(x){
  t1 = datDe@data[x,]$第一層質地 %>% as.character() %>% as.numeric()
  t2 = datDe@data[x,]$第二層質地 %>% as.character() %>% as.numeric()
  t3 = datDe@data[x,]$第三層質地 %>% as.character() %>% as.numeric()
  t4 = datDe@data[x,]$第四層質地 %>% as.character() %>% as.numeric()
  tex = cbind(t1,t2,t3,t4) %>% as.data.frame()
  
}

code = lapply(NO.number_1[1:1026], find_texture) %>%  as.data.frame() %>% t()
layer = rep(c("L1","L2","L3","L4"),1026)
NO = rep(c(back_datDe_coords$NO.), each = 4)
lons = rep(c(back_datDe_coords$lon), each = 4)
lats = rep(c(back_datDe_coords$lat), each = 4)
output = cbind(NO, lons, lats, layer, code) %>% as.data.frame()
colnames(output) = c("NO", "lon","lat", "layer", "code") #NO為datDe的NO，對應得到個座標的質地代碼
# write.csv(output, "layer code(new).csv")

##########
# 抓出有選到的.sol
TW.sol_10km = readLines("TW.sol_10km.txt")

tenkmgrid_re = cbind(as.numeric(substr(tenkmgrid$ID, 3,11)), 
                     as.numeric(as.character(round(tenkmgrid$lon,2))), as.numeric(as.character(round(tenkmgrid$lat,2)))) %>% as.data.frame()
colnames(tenkmgrid_re) = c("ID",  "lat","lon")
jjj = inner_join(tenkmgrid_re, uni2, by = c("lon"="lon","lat"="lat")) #為.sol對應的座標
##############
find_.sol_ID = function(x){
  unlist(strsplit(TW.sol_10km[x], "    "))[1] %>% substr(4,11)
  
}
seq = seq(1,7098,13)
.sol_ID = lapply(seq, find_.sol_ID) %>% as.numeric()

.sol_ID = cbind(1:546, .sol_ID) %>% as.data.frame() 
colnames(.sol_ID) = c("NO", ".sol_ID")
#  head(.sol_ID)

colnames(jjj) = c(".sol_ID", "lon", "lat", "ori_code")
find_order = inner_join(jjj, .sol_ID, by = ".sol_ID")
find_order$NO ##選出.sol裡面符合座標的(154點) (原本148點)


ggplot(find_order, aes(x=lon,y=lat))+
  geom_point(data=find_order, aes(x=lon, y=lat), colour = "purple3")



.sol_read_by_lines =  read.table("TW.sol_10km.txt", sep = "\n")
rea = read.table("TW.sol_10km.txt", sep = "\n")
# testing how towrite out same type of .sol
# write.table(rea, "test.txt", row.names=FALSE, 
#            sep="\t", quote = FALSE,  col.names = F)

# function for get the .sol title site
to = function(x){
  (find_order$NO[x]-1)*13+1
}

titlesite = lapply(1:154, to)


## function for find out all of the .sol by title
catchall = function(x){
  aa = list()
  
  aa[x] = rea[x:(x+12),]
  
}

lapply(as.numeric(titlesite), catchall)  #可以把所有要的抓出來
