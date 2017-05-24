#data creation
set.seed(1)
x1<-rnorm(2000)
x2<-rnorm(2000)
x3<-rnorm(2000)
x4<-rnorm(2000)
x5<-rnorm(2000)
x6<-rnorm(2000)
x7<-rnorm(2000)
x8<-rnorm(2000)
x9<-rnorm(2000)
x10<-rnorm(2000)

train<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

library(dplyr)

train<-train %>% 
    mutate("y"=ifelse((x1^2+x2^2+x3^2+x4^2+x5^2+x6^2+x7^2+x8^2+x9^2+x10^2>9.34),1,0))

table(train$y)

x1<-rnorm(10000)
x2<-rnorm(10000)
x3<-rnorm(10000)
x4<-rnorm(10000)
x5<-rnorm(10000)
x6<-rnorm(10000)
x7<-rnorm(10000)
x8<-rnorm(10000)
x9<-rnorm(10000)
x10<-rnorm(10000)

test<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

library(dplyr)

test<-test %>% 
    mutate("y"=ifelse((x1^2+x2^2+x3^2+x4^2+x5^2+x6^2+x7^2+x8^2+x9^2+x10^2>9.34),1,0))

table(test$y)

#####tree

library(rpart)

tree_model<-rpart(formula = y~. ,data = train, method = "class")

tree_train_pred<-predict(tree_model, newdata = train[1:10], type = "class")
tree_train_error<-sum(tree_train_pred!=train$y)/2000

tree_test_pred<-predict(tree_model, newdata = test[1:10], type = "class")
tree_test_error<-sum(tree_test_pred!=test$y)/10000

####stump
control<-rpart.control(maxdepth = 2)

tree_model<-rpart(formula = y~. ,data = train, method = "class", control = control)

stump_train_pred<-predict(tree_model, newdata = train[1:10], type = "class")
stump_train_error<-sum(stump_train_pred!=train$y)/2000

stump_test_pred<-predict(tree_model, newdata = test[1:10], type = "class")
stump_test_error<-sum(stump_test_pred!=test$y)/10000


#######BOOSTING

library(xgboost)
dtest <- xgb.DMatrix(data.matrix(test[1:10]))
target <- train$y
dtrain <- xgb.DMatrix(data.matrix(train[-11]), label=target, missing=NA)

result<-data.frame()
for(i in 1:1000){
    param <- list(  objective           = "binary:logistic",
                    booster             = "gbtree",
                    eta                 = 0.05, 
                    max_depth           = 2, 
                    subsample           = 0.8,
                    colsample_bytree    = 0.8,
                    min_child_weight    = 1,
                    maximize            = TRUE
    )
    
    xgb_model <- xgb.train( params              = param, 
                            data                = dtrain,
                            nrounds             = i, 
                            verbose             = 1,
                            print.every.n       = 1
    )
    
    response_xgb<-predict(xgb_model, newdata = dtrain)
    response_xgb<-ifelse(response_xgb>=0.5,1,0)
    train_error<-sum(response_xgb!=train$y)/2000
    
    response_xgb<-predict(xgb_model, newdata = dtest)
    response_xgb<-ifelse(response_xgb>=0.5,1,0)
    test_error<-sum(response_xgb!=test$y)/10000
    
    result<-rbind(result, data.frame("n_tree"=i, "Test_error"=test_error))
    print(i)
}
tail(result)
getinfo(xgb_model)
attr(xgb_model, "handle")

#random forest
library(randomForest)
forest_result<-data.frame()
for(i in 1:1000){
    forest_model<-randomForest(as.factor(y)~., data = train, ntree=i)
    response_forest<-predict(forest_model, newdata = test, type = "class")
    test_error<-sum(response_forest!=test$y)/10000
    forest_result<-rbind(forest_result,data.frame("n_tree"=i,"Test_error"=test_error))
    print(i)
}

tail(forest_result)

#bagging
library(ipred)
bagged_result<-data.frame()
for(i in 1:1000){
    bagged_tree_model<-bagging(as.factor(y)~., data = train, nbagg=i)
    response_bagged_tree<-predict(bagged_tree_model, newdata = test, type = "class")
    test_error<-sum(response_bagged_tree!=test$y)/10000
    bagged_result<-rbind(bagged_result,data.frame("nbagg"=i,"Test_error"=test_error))
    print(i)
}
tail(bagged_result)

read.csv("")
#results - the results might be slightly different each time you run the code
#the code belowe is not parametrised
library(ggplot2)

tree_test_error
tree_label<-paste("Fully trained tree - ",tree_test_error,sep = "")
stump_test_error
stump_label<-paste("Stump - ",stump_test_error,sep = "")
bagged_result$Test_error[1000]
bagged_label<-paste("Bagged Trees - ",bagged_result$Test_error[1000],sep = "")
forest_result$Test_error[1000]
forest_label<-paste("Random Forest - ",forest_result$Test_error[1000],sep = "")
result$Test_error[1000]
result_label<-paste("Boosted Trees - ",result$Test_error[1000],sep = "")

ggplot()+
    xlab("Number of trees")+
    ylab("Test error")+
    geom_hline(aes(yintercept=tree_test_error), linetype="dashed", color="darkgrey")+
    geom_text(aes(y=tree_test_error+0.01, x=800, label=tree_label), col="blue")+
    geom_text(aes(y=bagged_result$Test_error[1000]+0.01, x=800, label=bagged_label), col="blue")+
    geom_line(data=bagged_result ,aes(x=nbagg, y=Test_error), col="blue")

ggplot()+
    xlab("Number of trees")+
    ylab("Test error")+
    geom_hline(aes(yintercept=tree_test_error), linetype="dashed", color="darkgrey")+
    geom_text(aes(y=tree_test_error+0.01, x=800, label=tree_label), col="blue")+
    geom_text(aes(y=bagged_result$Test_error[1000]+0.01, x=800, label=bagged_label), col="blue")+
    geom_line(data=bagged_result ,aes(x=nbagg, y=Test_error), col="blue")+
    geom_line(data = forest_result, aes(x=n_tree, y=Test_error))+
    geom_text(aes(y=forest_result$Test_error[1000]-0.01, x=800, label=forest_label))

ggplot()+
    xlab("Number of trees")+
    ylab("Test error")+
    geom_hline(aes(yintercept=tree_test_error), linetype="dashed", color="darkgrey")+
    geom_text(aes(y=tree_test_error+0.01, x=800, label=tree_label), col="blue")+
    geom_text(aes(y=bagged_result$Test_error[1000]+0.015, x=800, label=bagged_label), col="blue")+
    geom_line(data=bagged_result ,aes(x=nbagg, y=Test_error), col="blue")+
    geom_line(data = forest_result, aes(x=n_tree, y=Test_error))+
    geom_text(aes(y=forest_result$Test_error[1000]-0.01, x=800, label=forest_label))+
    geom_line(data=result ,aes(x=n_tree, y=Test_error), col="red")+
    geom_text(aes(y=result$Test_error[1000]-0.01, x=800, label= result_label), col="red")+
    geom_hline(aes(yintercept=stump_test_error), linetype="dashed", color="darkgrey")+
    geom_text(aes(y=stump_test_error+0.01, x=800, label=stump_label), col="red")
