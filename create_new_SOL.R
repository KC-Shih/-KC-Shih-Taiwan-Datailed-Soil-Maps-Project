# library(rgdal)
# library(ithir)
# library(aqp)
# library(dplyr)
#
#
# setwd("D:/1091/thesis_data") # laptop

# spline_use = read.csv("spline_use1116.csv")

#
# id = "test"; lon = 120; lat = 24
# top = c(0,30,60,90)
# bottom = c(30,60,90,150)
#
# id.top.bottom = cbind(id, top,bottom)
#
#
# tt = read.csv("tt.csv")
# select = seq(1,3680,4)
# tt = tt[select,]


##　需先執行 for all 得到q，在執行以上tt等(可以去spline_use1116)，
##  再繼續以下程式
# ======= j 是 han154的ID，i 是 每個han 在DSM 使用的ID ======= #

for(j in 154:154){
  pp = list()
  for(i in 919:920){
    
    
    ###================= 改1 ======================###
    SW = cbind(id.top.bottom, spline_use[select[i]:c(select[i]+3),])
    ###================= 改1 ======================###
    
    
    ########################
    ##### each spline  #####
    
    SW.LL = ea_spline(SW, var.name="LL", 
                      lam = 0.1, d = t(c(0,5,15,30,60,100,200)), 
                      vlow = 0, vhigh = 1, show.progress=T)
    #str(SW.LL)
    
    #
    SW.DUL = ea_spline(SW, var.name="DUL", 
                       lam = 0.1, d = t(c(0,5,15,30,60,100,200)), 
                       vlow = 0, vhigh = 1, show.progress=T)
    #str(SW.DUL)
    
    #
    SW.SAT = ea_spline(SW, var.name="SAT", 
                       lam = 0.1, d = t(c(0,5,15,30,60,100,200)), 
                       vlow = 0, vhigh = 1, show.progress=T)
    #str(SW.SAT)
    
    ##############################
    #####  new water number  #####
    
    
    newLL = c(SW.LL$harmonised$`0-5 cm`, SW.LL$harmonised$`5-15 cm`, SW.LL$harmonised$`15-30 cm`,
              SW.LL$harmonised$`30-60 cm`,SW.LL$harmonised$`60-100 cm`,SW.LL$harmonised$`100-200 cm`)
    
    newDUL = c(SW.DUL$harmonised$`0-5 cm`, SW.DUL$harmonised$`5-15 cm`, SW.DUL$harmonised$`15-30 cm`,
               SW.DUL$harmonised$`30-60 cm`,SW.DUL$harmonised$`60-100 cm`,SW.DUL$harmonised$`100-200 cm`)
    
    newSAT = c(SW.SAT$harmonised$`0-5 cm`, SW.SAT$harmonised$`5-15 cm`, SW.SAT$harmonised$`15-30 cm`,
               SW.SAT$harmonised$`30-60 cm`,SW.SAT$harmonised$`60-100 cm`,SW.SAT$harmonised$`100-200 cm`)
    
    newW = cbind(newLL, newDUL, newSAT) %>% as.data.frame()
    
    
    ###================= 改2 ：須注意名字百位千位要改======================###
    y = q[[j]][1] %>% unlist() %>% as.character()
    yy = paste(c(paste0("*TW03368",i),strsplit(y, " ")[[1]][2:160]),collapse=" ")
    
    u = q[[j]][2] %>% unlist() %>% as.character()
    o = q[[j]][3] %>% unlist() %>% as.character()
    m = q[[j]][4:6] %>% unlist() %>% as.character()
    ###================= 改3 ======================###
    k = paste(c(" -99              TW      ",format(as.numeric(tt[i,1]),nsmall =3),
                strsplit(o, " ")[[1]][24],format(as.numeric(tt[i,2]),nsmall =3),strsplit(o, " ")[[1]][26:172]),collapse=" ")
    ###================= 改4 ======================###
    
    d5   = q[[j]][7] %>% unlist() %>% as.character()  
    d15  = q[[j]][8] %>% unlist() %>% as.character()  
    d30  = q[[j]][9] %>% unlist() %>% as.character()  
    d60  = q[[j]][10] %>% unlist() %>% as.character()  
    d100 = q[[j]][11] %>% unlist() %>% as.character()  
    d200 = q[[j]][12] %>% unlist() %>% as.character()  
    
    ###================= 改4 ======================###
    
    
    
    d5n   = paste(c("     5 A    ",format(round(newW[1,], 3), nsmall = 3),strsplit(d5, " ")[[1]][15:130]),collapse=" ")
    d15n  = paste(c("    15 A    ",format(round(newW[2,], 3), nsmall = 3),strsplit(d15, " ")[[1]][14:129]),collapse=" ")
    d30n  = paste(c("    30 AB   ",format(round(newW[3,], 3), nsmall = 3),strsplit(d30, " ")[[1]][13:128]),collapse=" ")
    d60n  = paste(c("    60 BA   ",format(round(newW[4,], 3), nsmall = 3),strsplit(d60, " ")[[1]][13:127]),collapse=" ")
    d100n = paste(c("   100 B    ",format(round(newW[5,], 3), nsmall = 3),strsplit(d100, " ")[[1]][13:126]),collapse=" ")
    d200n = paste(c("   200 BC   ",format(round(newW[6,], 3), nsmall = 3),strsplit(d200, " ")[[1]][12:125]),collapse=" ")
    
    pp[[i]] = c(yy,u,k,m, d5n, d15n, d30n, d60n, d100n, d200n)
    
  }
  
  setwd("D:/1091/thesis_data/test")
  lapply(pp, write, paste0("DH",i,".txt"), append=TRUE)
  
}
