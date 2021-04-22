rm(list = ls())
setwd("C:/Users/USER/Desktop/1082/機器學習與環境資料分析")
dat = read.csv("DetailedSoilMap.csv")
datDe_clean = dat[complete.cases(dat),]
attach(datDe_clean)
####################
# 共640個中文土系
# 
# meterial = datDe_clean$母質種類
# property = datDe_clean$土壤特性
# morphology = datDe_clean$土壤形態
# drainage = datDe_clean$排水等級

library(rsample)      # data splitting 
library(randomForest)
set.seed(123)
dat_split = initial_split(data = datDe_clean, prop = .7)
dat_train = training(dat_split)
dat_test  = testing(dat_split)

# default RF model
model = randomForest(
  formula = 土壤形態 ~ 母質種類+土壤特性,
                                # +factor(土壤形態)+factor(排水等級),
                                # +factor(石灰性)+factor(坡度)
                                # +factor(表土酸鹼性)+factor(第一層質地)
                                # +factor(第二層質地)+factor(第三層質地)
                                # +factor(第四層質地),
  data = dat_train
)

model
pred_randomForest = predict(model, dat_test)
head(as.numeric(pred_randomForest))
head(dat_test$土壤形態)

################################

model_morphology = randomForest(
  formula = 土壤形態 ~ 母質種類+土壤特性+排水等級+石灰性+坡度+表土酸鹼性
                      +第一層質地+第二層質地+第三層質地+第四層質地,
  data = dat_train
)

model_morphology
pred_randomForest = predict(model_morphology, dat_test)
head(as.numeric(pred_randomForest))
head(dat_test$土壤形態) # 土壤形態總數為 8496

########################
# 把土壤形態為1的去除，再跑一次
without = which(datDe_clean$土壤形態==1)
nomor1 = datDe_clean[-without,]
set.seed(456)
nomor1_split = initial_split(data = nomor1, prop = .7)
nomor1_train = training(nomor1_split)
nomor1_test  = testing(nomor1_split)

model_nomor1 = randomForest(
  formula = factor(排水等級) ~ #母質種類+土壤特性+排水等級+石灰性+坡度+表土酸鹼性
  +第一層質地+第二層質地+第三層質地+第四層質地,
  data = nomor1_train
)

model_nomor1
pred_nomor1_randomForest = predict(model_nomor1, dat_test)
round(head(as.numeric(pred_nomor1_randomForest)),0)
head(nomor1$排水等級) 
# > round(head(as.numeric(pred_nomor1_randomForest)),0)
# [1] 20 19 19 19 19 19
# > head(nomor1$土壤形態)
# [1] 19 19 19 19 19 19
# > tail(nomor1$土壤形態)
# [1] 16 19 19 19 19 16
# > round(tail(as.numeric(pred_nomor1_randomForest)),0)
# [1] 19 24 23 15 15 19

#############
# 全部都猜錯，只是因為數值很小所以error很小，還是要用factor
# 嘗試改成類別，查類別的隨機森林
# RF好了再NN
#############

model = randomForest(
  formula = as.factor(土壤形態) ~ 母質種類+土壤特性+排水等級+石灰性+坡度+表土酸鹼性
  +第一層質地+第二層質地+第三層質地+第四層質地,
  #+factor(as.character(土壤形態))
  #+factor(as.character(排水等級)),
  # +factor(石灰性)+factor(坡度)
  # +factor(表土酸鹼性)+factor(第一層質地)
  # +factor(第二層質地)+factor(第三層質地)
  # +factor(第四層質地),
  data = dat_train
)

model
pred_randomForest = predict(model, dat_test)
head(as.numeric(pred_randomForest))
head(dat_test$土壤形態)

##################
library(rpart)
library(rpart.plot)

fit <- rpart(formula =  as.factor(土壤形態) ~ 母質種類+土壤特性+排水等級+石灰性+坡度+表土酸鹼性
             +第一層質地+第二層質地+第三層質地+第四層質地
             , data = dat_train, method = 'class')

rpart.rules(x = fit,cover = TRUE)
printcp(x = fit)


plotcp(x = fit)
predicted <- predict(object = fit,newdata = dat_test,type = 'class')
tbl <- table(predicted,dat_test$土壤形態)
tbl
accuracy <- sum(diag(tbl)) / sum(tbl); accuracy

#################
# 去除1
fit1 <- rpart(formula =  as.factor(土壤形態) ~ 母質種類+土壤特性+排水等級+石灰性+坡度+表土酸鹼性
             +第一層質地+第二層質地+第三層質地+第四層質地
             , data = nomor1_train, method = 'class')

rpart.rules(x = fit1,cover = TRUE)
printcp(x = fit1)


plotcp(x = fit1)
predicted <- predict(object = fit1,newdata = nomor1_test,type = 'class')
tbl <- table(predicted,nomor1_test$土壤形態)
tbl
accuracy <- sum(diag(tbl)) / sum(tbl); accuracy


###################
fit1 <- rpart(formula =  as.factor(排水等級) ~ 母質種類+土壤特性+石灰性+坡度+表土酸鹼性
              +第一層質地+第二層質地+第三層質地+第四層質地
              , data = dat_train, method = 'class')

rpart.rules(x = fit1,cover = TRUE)
printcp(x = fit1)


plotcp(x = fit1)
predicted <- predict(object = fit1,newdata = nomor1_test,type = 'class')
tbl <- table(predicted,dat_train$排水等級)
tbl
accuracy <- sum(diag(tbl)) / sum(tbl); accuracy

######################################################
new = datDe_clean[,5:15]
colnames(new) = 1:11


new_split = initial_split(data = new, prop = .7)
new_trainx = training(new_split)[,2:11]
new_testx  = testing(new_split)[,2:11]
new_trainy = training(new_split)[1] # 16
new_testy  = testing(new_split)[1]

new_trainy = as.numeric(new_trainy[,1])

new_trainy[which(new_trainy==0)]=1
new_trainy[which(new_trainy==1)]=2
new_trainy[which(new_trainy==4)]=3
new_trainy[which(new_trainy==9)]=4
new_trainy[which(new_trainy==11)]=5
new_trainy[which(new_trainy==14)]=6
new_trainy[which(new_trainy==16)]=7
new_trainy[which(new_trainy==17)]=8
new_trainy[which(new_trainy==18)]=9
new_trainy[which(new_trainy==19)]=10
new_trainy[which(new_trainy==20)]=11
new_trainy[which(new_trainy==21)]=12
new_trainy[which(new_trainy==23)]=13
new_trainy[which(new_trainy==24)]=14
new_trainy[which(new_trainy==25)]=15

y_train = to_categorical(new_trainy, 15)
y_test = to_categorical(new_testy, 15) 


# 建立模型 MLP

library(keras)
library(magrittr)
model = keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(10)) %>%
  layer_dropout(rate = 0.4) %>%  #降低模型複雜度
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 15, activation = "softmax")  #0-9共10類各自的函數(被選到的機率)

# 查看模型
summary(model)

# 使用適當的損失函數，優化器和指標來編譯模型：

model %>% compile( #編譯
  loss = "categorical_crossentropy",  #計算交叉熵，高信息熵可以涵蓋更多的資訊量，依此機率建構損失涵數
  optimizer = optimizer_rmsprop(),  # 很多種，如adam，見補充
  metrics = c('accuracy')  #可選mse，差異??
)

# 測試及驗證 
starttime = Sys.time()
history = model %>% fit(
  new_trainx, new_trainy, 
  epochs = 3, batch_size = 128,  #迭代多少次 / 每次取多少資料
  validation_split = 0.2          #取多少training data 做驗證
)
endtime = Sys.time()
waste = round(endtime - starttime ,2)       
cat("Take", waste, "seconds", sep = " ")   #計算花費時間




