

set.seed(1234)
setwd("C://Users//MinSu//Desktop//생물통계(중간발표)")
#1. mushroom
##step1 setting
df1=read.table("agaricus-lepiota.data",sep=",")
head(df1)

df1=subset(df1,V12!="?")
df1$V12=factor(df1$V12)
str(df1)
df1=df1[,-17]

setnum=sample(1:5644,1000)
df1=df1[setnum,]
head(df1)
df1[1,]

tp=1:700
testdata=df1[tp,]
traindata=df1[-tp,]

l_num=sample(1:300,100)
traindata_l=traindata[l_num,]
traindata_u=traindata[-l_num,]
head(traindata_u)
traindata_u$V1=rep(0,200)
head(traindata_l)

X <- traindata_l[,2:22]
X_u <- traindata_u[1:200,2:22]
y <- traindata_l[,1]
Xe<-rbind(X,X_u)

library("RSSL")
g_sup <- SVM(X,y,x_center=FALSE,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=10,Cstar=10,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
##step2 tuning(10CV)
#X_u는 고정된채로, X만 바꿔가며..

tuningnum=sample(1:100,100)
traindata_l_cv=traindata_l[tuningnum,]

flower1=function(x){
  
  c1=x
  g_sup_vec=c()
  
  for(i in 1:10){
    a=10*i-9
    b=10*i
    vec=a:b
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,2:22]
    y=cv_train_data[,1]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,2:22]
    y_cv_test=cv_test_data[,1]
    
    g_sup=SVM(X,y,x_center=FALSE,C=c1,scale=FALSE)
    
    g_sup_vec[i]=mean(y_cv_test==predict(g_sup,X_cv_test))
    
  }
  return(mean(g_sup_vec))
}

flower1(1)

flower2=function(x){
  
  c1=x[1]
  c2=x[2]
  g_constraint_vec=c()
  
  for(i in 1:10){
    a=10*i-9
    b=10*i
    vec=a:b
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,2:22]
    y=cv_train_data[,1]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,2:22]
    y_cv_test=cv_test_data[,1]
    
    g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                         C=c1,Cstar=c2,balancing_constraint = TRUE,
                         x_center = FALSE,verbose=TRUE)
    
    g_constraint_vec[i]=mean(y_cv_test==predict(g_constraint,X_cv_test))
    
    
    
  }
  return(mean(g_constraint_vec))
}

flower1(0.01)
flower2(c(0.01,10))

lis=c(0.001,0.01,0.1,1,10,100)
lis2=c()
for(j in 1:6){
  cc=lis[j]
  lis2[j]=flower1(cc)
}
lis2 #cost 0.1 채택

l=list(lis=lis,lis=lis)
m=do.call(expand.grid,l)
m$lislis=rep(0,36)
for (i in 1:36) {
  tryCatch({
    m[i,3]=flower2(c(m[i,1],m[i,2]))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
m
#cost 0.1, 0.001 채택



str(X)
#?TSVM
##step3 testdata

g_sup <- SVM(X,y,x_center=FALSE,C=0.1,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=0.1,Cstar=0.001,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
X_test=testdata[,2:22]
mean(testdata$V1==predict(g_sup,X_test))
mean(testdata$V1==predict(g_constraint,X_test)) #성능이 살짝 좋아짐..

#2.iris
##step1 setting
set.seed(1234)
df2=read.table("iris.data",sep=",")
head(df2)
table(df2$V5)
df2=subset(df2,V5!='Iris-setosa')
df2$V5=factor(df2$V5)
str(df2)


setnum=sample(1:100,100)
df2=df2[setnum,]
head(df2)
df2[1,]

tp=1:80
testdata=df2[tp,]
traindata=df2[-tp,]

l_num=sample(1:20,10)
traindata_l=traindata[l_num,]
traindata_u=traindata[-l_num,]
head(traindata_u)
traindata_u$V5=rep(0,10)
head(traindata_l)

X <- traindata_l[,1:4]
X_u <- traindata_u[,1:4]
y <- traindata_l[,5]
Xe<-rbind(X,X_u)

library("RSSL")
g_sup <- SVM(X,y,x_center=FALSE,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=10,Cstar=10,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
##step2 tuning(10CV)
#X_u는 고정된채로, X만 바꿔가며..

tuningnum=sample(1:10,10)
traindata_l_cv=traindata_l[tuningnum,]

flower1=function(x){
  
  c1=x
  g_sup_vec=c()
  
  for(i in 1:10){
    
    
    vec=i
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,1:4]
    y=cv_train_data[,5]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,1:4]
    y_cv_test=cv_test_data[,5]
    
    g_sup=SVM(X,y,x_center=FALSE,C=c1,scale=FALSE)
    
    g_sup_vec[i]=mean(y_cv_test==predict(g_sup,X_cv_test))
    
  }
  return(mean(g_sup_vec))
}

flower1(1)

flower2=function(x){
  
  c1=x[1]
  c2=x[2]
  g_constraint_vec=c()
  
  for(i in 1:10){
    
    
    vec=i
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,1:4]
    y=cv_train_data[,5]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,1:4]
    y_cv_test=cv_test_data[,5]
    
    g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                         C=c1,Cstar=c2,balancing_constraint = TRUE,
                         x_center = FALSE,verbose=TRUE)
    
    g_constraint_vec[i]=mean(y_cv_test==predict(g_constraint,X_cv_test))
    
    
    
  }
  return(mean(g_constraint_vec))
}

flower1(1)
flower2(c(1,1))

lis=c(0.001,0.01,0.1,1,10,100)
lis2=c()
for(j in 1:6){
  cc=lis[j]
  lis2[j]=flower1(cc)
}
lis2 #cost 0.1 채택

l=list(lis=lis,lis=lis)
m=do.call(expand.grid,l)
m$lislis=rep(0,36)
for (i in 1:36) {
  tryCatch({
    m[i,3]=flower2(c(m[i,1],m[i,2]))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
m
#cost 0.1, 1 채택




##step3 testdata

g_sup <- SVM(X,y,x_center=FALSE,C=0.1,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=0.1,Cstar=1,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
X_test=testdata[,1:4]
mean(testdata$V5==predict(g_sup,X_test))
mean(testdata$V5==predict(g_constraint,X_test)) #성능이 다소 개선됨

setwd("C://Users//MinSu//Desktop//생물통계(중간발표)")
#3. splice data
set.seed(1234)

##step1 setting
df3=read.table("splice.data",sep=",")
head(df3)
str(df3)
df3=subset(df3,V1!='IE')
df3$V1=factor(df3$V1)


setnum=sample(1:2422,200)
df3=df3[setnum,]
head(df3)
df3[1,]

tp=1:150
testdata=df3[tp,]
traindata=df3[-tp,]

l_num=sample(1:50,20)
traindata_l=traindata[l_num,]
traindata_u=traindata[-l_num,]
head(traindata_u)
traindata_u$V1=rep(0,30)
head(traindata_l)

X <- traindata_l[,2:3]
X_u <- traindata_u[,2:3]
y <- traindata_l[,1]
Xe<-rbind(X,X_u)


library("RSSL")
g_sup <- SVM(X,y,x_center=FALSE,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=10,Cstar=10,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
##step2 tuning(10CV)
#X_u는 고정된채로, X만 바꿔가며..

tuningnum=sample(1:20,20)
traindata_l_cv=traindata_l[tuningnum,]

flower1=function(x){
  
  c1=x
  g_sup_vec=c()
  
  for(i in 1:10){
    a=2*i-1
    b=2*i
    vec=a:b
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,2:3]
    y=cv_train_data[,1]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,2:3]
    y_cv_test=cv_test_data[,1]
    
    g_sup=SVM(X,y,x_center=FALSE,C=c1,scale=FALSE)
    
    g_sup_vec[i]=mean(y_cv_test==predict(g_sup,X_cv_test))
    
  }
  return(mean(g_sup_vec))
}

flower1(0.1)


flower2=function(x){
  
  c1=x[1]
  c2=x[2]
  g_constraint_vec=c()
  
  for(i in 1:10){
    a=2*i-1
    b=2*i
    vec=a:b
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,2:3]
    y=cv_train_data[,1]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,2:3]
    y_cv_test=cv_test_data[,1]
    
    g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                         C=c1,Cstar=c2,balancing_constraint = TRUE,
                         x_center = FALSE,verbose=TRUE)
    
    g_constraint_vec[i]=mean(y_cv_test==predict(g_constraint,X_cv_test))
    
    
    
  }
  return(mean(g_constraint_vec))
}

flower1(10)
flower2(c(1,1))

lis=c(0.001,0.01,0.1,1,10,100)
lis2=c()
for(j in 1:6){
  cc=lis[j]
  lis2[j]=flower1(cc)
}
lis2 #cost 0.001 채택

l=list(lis=lis,lis=lis)
m=do.call(expand.grid,l)
m$lislis=rep(0,36)
for (i in 1:36) {
  tryCatch({
    m[i,3]=flower2(c(m[i,1],m[i,2]))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
m
#cost 1, 0.001 채택



str(X)
#?TSVM
##step3 testdata


g_sup <- SVM(X,y,x_center=FALSE,C=0.001,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=1,Cstar=0.001,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
X_test=testdata[,2:3]
mean(testdata$V1==predict(g_sup,X_test)) #0.6733333
mean(testdata$V1==predict(g_constraint,X_test)) #0.6733333


#4.

set.seed(1234)

##step1 setting
df4=read.table("balance-scale.data",sep=",")
head(df4)
str(df4)
df4=subset(df4,V1!='R')
df4$V1=factor(df4$V1)


setnum=sample(1:337,300)
df4=df4[setnum,]
head(df4)
df4[1,]

tp=1:150
testdata=df4[tp,]
traindata=df4[-tp,]

l_num=sample(1:150,50)
traindata_l=traindata[l_num,]
traindata_u=traindata[-l_num,]
head(traindata_u)
traindata_u$V1=rep(0,100)
head(traindata_l)

X <- traindata_l[,2:5]
X_u <- traindata_u[,2:5]
y <- traindata_l[,1]
Xe<-rbind(X,X_u)


library("RSSL")
g_sup <- SVM(X,y,x_center=FALSE,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=10,Cstar=10,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
##step2 tuning(10CV)
#X_u는 고정된채로, X만 바꿔가며..

tuningnum=sample(1:50,50)
traindata_l_cv=traindata_l[tuningnum,]

flower1=function(x){
  
  c1=x
  g_sup_vec=c()
  
  for(i in 1:10){
    a=5*i-4
    b=5*i
    vec=a:b
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,2:5]
    y=cv_train_data[,1]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,2:5]
    y_cv_test=cv_test_data[,1]
    
    g_sup=SVM(X,y,x_center=FALSE,C=c1,scale=FALSE)
    
    g_sup_vec[i]=mean(y_cv_test==predict(g_sup,X_cv_test))
    
  }
  return(mean(g_sup_vec))
}

flower1(100)


flower2=function(x){
  
  c1=x[1]
  c2=x[2]
  g_constraint_vec=c()
  
  for(i in 1:10){
    a=5*i-4
    b=5*i
    vec=a:b
    cv_train_data=traindata_l_cv[-vec,]
    cv_test_data=traindata_l_cv[vec,]
    X=cv_train_data[,2:5]
    y=cv_train_data[,1]
    Xe=rbind(X,X_u)
    
    X_cv_test=cv_test_data[,2:5]
    y_cv_test=cv_test_data[,1]
    
    g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                         C=c1,Cstar=c2,balancing_constraint = TRUE,
                         x_center = FALSE,verbose=TRUE)
    
    g_constraint_vec[i]=mean(y_cv_test==predict(g_constraint,X_cv_test))
    
    
    
  }
  return(mean(g_constraint_vec))
}

flower1(10)
flower2(c(1,1))

lis=c(0.001,0.01,0.1,1,10,100)
lis2=c()
for(j in 1:6){
  cc=lis[j]
  lis2[j]=flower1(cc)
}
lis2 #cost 0.01 채택

l=list(lis=lis,lis=lis)
m=do.call(expand.grid,l)
m$lislis=rep(0,36)
for (i in 1:36) {
  tryCatch({
    m[i,3]=flower2(c(m[i,1],m[i,2]))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
m
#cost 0.01, 0.01 채택



str(X)
#?TSVM
##step3 testdata


g_sup <- SVM(X,y,x_center=FALSE,C=0.01,scale=FALSE)
g_constraint <- TSVM(X=X,y=y,X_u=X_u,
                     C=0.01,Cstar=0.01,balancing_constraint = TRUE,
                     x_center = FALSE,verbose=TRUE)
X_test=testdata[,2:5]
mean(testdata$V1==predict(g_sup,X_test)) #0.8733333
mean(testdata$V1==predict(g_constraint,X_test)) #0.8733333

