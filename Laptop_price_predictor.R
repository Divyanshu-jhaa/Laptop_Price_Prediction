library(ggplot2)
library(stringr)
library(rpart)
library(dplyr)
library(xgboost)
library(caret)            
library(randomForest)
x=read.csv("laptop_price.csv")
x[,8]=gsub('GB','',x[,8])
x[,12]=substr(x[,12],1,nchar(x[,12])-2)
x[,8]=as.numeric(x[,8])
x[,12]=as.numeric(x[,12])
#checking density
p1=ggplot(x,aes(x=Price_euros))+geom_density()
p2=ggplot(x,aes(x=Company))+geom_bar()
print(p1)
print(p2)
#adding touchscreen AND Ips column
Touchscreen=c()
Ips=c()
for(tt in x[,6]){
  if(grepl("Touchscreen",tt)){
    Touchscreen=c(Touchscreen,1)
  }else{
    Touchscreen=c(Touchscreen,0)
  }
}
for(zz in x[,6]){
  if(grepl("Touchscreen",zz)){
    Ips=c(Ips,1)
  }else{
    Ips=c(Ips,0)
  }
}
x$Touchscreen=Touchscreen
x$Ips=Ips
p3=ggplot(x,aes(x=Touchscreen))+geom_bar()
p4=ggplot(x,aes(x=Ips))+geom_bar()
print(p3)
print(p4)
x_y=regmatches(x[,6], gregexpr("[[:digit:]]+", x[,6]))
x_res=c()
y_res=c()
count=0
for(gg in x_y){
  check=TRUE
  for(ff in gg){
    if(check){
      x_res=c(x_res,ff)
      check=FALSE
    }else{
      y_res=c(y_res,ff)
   
    }
  }
}
x$x_res=x_res
x$y_res=y_res[1:1303]
x[,16]=as.numeric(x[,16])
x[,17]=as.numeric(x[,17])
x$ppi=((x$x_res)**2+(x$y_res)**2)**0.5/x$Inches
x=x[-c(5,6,16,17)]
x[,5]=word(x[,5],1,3,' ')
cp=c()
for(cpu in x[,5]){
  if(cpu=="Intel Core i5" || cpu=="Intel Core i7" || cpu=="Intel Core i3"){
    cp=c(cp,cpu)
  }else if(word(cpu,1,1,' ')=="Intel"){
    cp=c(cp,"Other Intel Processor")
  }else{
    cp=c(cp,"AMD Processor")
  }
}
x[,5]=cp
print(ggplot(x,aes(x=Cpu,y=Price_euros))+geom_bar(stat="identity"))
ssd=c()
hdd=c()
for(mem in x[,7]){
  temp=as.character(gsub("[^\\d]+", " ",mem, perl=TRUE))
  if(grepl("SSD",mem) && grepl("HDD",mem)){
    ssd=c(ssd,word(temp,1))
    hdd=c(hdd,word(temp,2))
  }else if(grepl("SSD",mem)){
    ssd=c(ssd,word(temp,1))
    hdd=c(hdd,"0")
  }else if(grepl("HDD",mem)){
    hdd=c(hdd,word(temp,1))
    ssd=c(ssd,"0")
  }else{
    hdd=c(hdd,"0")
    ssd=c(ssd,"0")
  }
}
for(i in 1:length(ssd)){
  if(ssd[i]=="1"){
    ssd[i]="1024"
  }
  if(ssd[i]=="2"){
    ssd[i]="2048"
  }
  
}
for(j in 1:length(hdd)){
  if(hdd[j]=="1"){
    hdd[j]="1024"
  }else if(hdd[j]=="2"){
    hdd[j]="2048"
  }
  
}
x$SSD=ssd
x$HDD=hdd
print(ggplot(x,aes(x=HDD))+geom_bar())
gpu=c()
for(gp in x[,8]){
  if(word(gp,1)=="Intel"){
    gpu=c(gpu,"Intel")
  }else if(word(gp,1)=="Nvidia"){
    gpu=c(gpu,"Nvidia")
  }else{
    gpu=c(gpu,"AMD")
  }
}
x$Gpu=gpu
os=c()
for(o in x$OpSys){
  if(word(o,1)=="Windows"){
    os=c(os,"Windows")
  }else if(word(o,1)=="Mac" || word(o,1)=="macOS"){
    os=c(os,"Mac")
  }else{
    os=c(os,"Other")
  }
}
x$OpSys=os
x=x[-c(1,3,7)]
x=x %>% relocate(Price_euros, .after=HDD)
# coverting char to factor
x[,1]=as.factor(x[,1])
x[,2]=as.factor(x[,2])
x[,3]=as.factor(x[,3])
x[,5]=as.factor(x[,5])
x[,6]=as.factor(x[,6])
x[,8]=as.factor(x[,8])
x[,9]=as.factor(x[,9])
x[,11]=as.factor(x[,11])
x[,12]=as.factor(x[,12])

r2=function(actual,predicted){cor(actual,predicted)**2}
mae=function(actual,predicted,n){sum(abs(actual-predicted))/n}
rmse=function(actual,predicted,n){(sum((actual-predicted)^2)/n)^(1/2)}
#testing the model


set.seed(101)
row =sample(1303)
x_shuffled=x[row,]
x_train=x_shuffled[1:1200,]
x_test=x_shuffled[1201:1303,]
#MLR

# creation of the model using decision tree regressor
# control = rpart.control(minsplit = 10,
#                          minbucket = round(5 / 3),
#                          maxdepth = 5,
#                          cp = 0)
tree=rpart(Price_euros~.,x_train,method = "anova")
mae_decision_tree=mae(x_test$Price_euros,predict(tree,x_test[-c(13)],method="anova"),nrow(x_test))
r2_decision_tree=r2(x_test$Price_euros,predict(tree,x_test[-c(13)],method="anova"))
rmse_decision_tree=rmse(x_test$Price_euros,predict(tree,x_test[-c(13)],method="anova"),nrow(x_test))
cat("The MAE value for the decision tree model is",mae_decision_tree,'\n')
cat("The R2 value for the decision tree model is",r2_decision_tree,'\n')
cat("The RMSE value for the decision tree model is",rmse_decision_tree,'\n\n')
plot(tree)

#random forest regressor
rf=randomForest(Price_euros ~ .,x_train,ntree=50)
mae_random_forest=mae(x_test$Price_euros,predict(rf,x_test[-c(13)],method="anova"),nrow(x_test))
r2_random_forest=r2(x_test$Price_euros,predict(rf,x_test[-c(13)],method="anova"))
rmse_random_forest=rmse(x_test$Price_euros,predict(rf,x_test[-c(13)],method="anova"),nrow(x_test))
cat("The MAE value for the random forest model is",mae_random_forest,'\n')
cat("The R2 value for the random forest model is",r2_random_forest,'\n')
cat("The RMSE value for the random forest model is",rmse_random_forest,'\n')


# 
# watchlist = list(train=x_train, test=x_test)
# model = xgb.train(data = x_train, max.depth = 3, watchlist=watchlist, nrounds = 100)
# model_xgboost = xgboost(data = x_train, max.depth = 3, nrounds = 86, verbose = 0)
# summary(model_xgboost)
