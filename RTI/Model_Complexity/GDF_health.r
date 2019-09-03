
library(neuralnet)
library(nnet)
library(ROCR)
library(caret)
library(fastDummies)
library(tidyverse)
library(glmnet)
library(pROC)
library(reshape2)

set.seed(100)
#@@@@@@@@@@@@@@@@@@@@@@@@INPUT VALUES@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
input_sizes = c(1,2,3)
input_reps = 2
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
data <- mydata2
dummy_data <- dummy_cols(data, remove_first_dummy = TRUE)
dummy_data <- dummy_data[, !sapply(dummy_data, is.factor)]
y = dummy_data$Ybin

trainInds = createDataPartition(y, p = 0.75, list = F)
train_data = dummy_data[trainInds,]
test_data = dummy_data[-trainInds,]

train_x = model.matrix(Ybin ~ ., data = train_data)
train_y = as.factor(train_data$Ybin)

test_x = model.matrix(Ybin ~ ., data = test_data)
test_y = test_data$Ybin

#----------------Training LASSO GLM ----------------------

cv.out <- cv.glmnet(train_x, train_y, alpha =1, family = "binomial")
lasso_pred = predict(cv.out, newx = test_x, s = cv.out$lambda.min, type = 'response')
lasso_bin = as.factor(ifelse(lasso_pred > 0.1, 1, 0))
table(pred = lasso_bin, true = test_y)
mean(lasso_bin==test_y)

#----------------Training Regular GLM ----------------------

glm_plain <- glm(Ybin ~ ., data = train_data, family = "binomial")
trainpred = glm_plain$fitted.values
testpred = predict(glm_plain, data.frame(test_x), type = "response")
test_res = as.factor(ifelse(testpred > 0.5, 1, 0))
table(pred = test_res, true = test_y)

#----------------Training Neural Network ----------------------

TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

net =  nnet(data.frame(train_x), as.numeric(as.character(train_y)), size = 1, rang = 0.1,
            trControl = TrainingParameters,
     decay = 5e-4, maxit = 200,trace=F)

preds_net = predict(net, data.frame(test_x), type = "raw")


# ------------Training/Tuning Random Forest -----------------------
mtry_grid = c(4, 8, 12, 16)

for (i in mtry_grid){
  rf =randomForest(x = dummy_data %>% select(-Ybin), y = as.factor(y),
               mtry = i)
}

#------ ROC Curves on test set together-----------------
par(mfrow = c(2,2))
roc(test_y, lasso_pred, percent=F,   
    boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, 
    show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    # print.thres = c(0.30,0.35, 0.40, 0.45,0.48, 0.50,0.55, 0.60),#
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, 
    ci.type="bars", print.thres.cex = 0.7, main ="ROC curve using LASSO") 

roc(test_y, testpred, percent=F,   
    boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, 
    show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    # print.thres = c(0.30,0.35, 0.40, 0.45,0.48, 0.50,0.55, 0.60),#
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, 
    ci.type="bars", print.thres.cex = 0.7, main = "ROC Curve using GLM" )

roc(test_y, preds_net, percent=F,   
    boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, 
    show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    # print.thres = c(0.30,0.35, 0.40, 0.45,0.48, 0.50,0.55, 0.60),#
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, 
    ci.type="bars", print.thres.cex = 0.7, main = "ROC curve using Neural Net" )


#-------------------GDF Calculations----------------

ZmodBin <- glm(Ybin ~ ., data = dummy_data, family = "binomial")

summary(ZmodBin)
muhat<-ZmodBin$fitted.values
hist(muhat)
totmean<-mean(muhat)
VarTot<-totmean*(1-totmean)

#Estimate Total Variance
VarEst<-muhat*(1-muhat)
hist(VarEst)
sum(VarEst)

indep = dummy_data %>% select(-Ybin)
y = dummy_data$Ybin
n = nrow(indep)

parts = 50
split_data <- function(n, partitions = 100){
  samp = seq(1,n)
  rand = sample(samp, n, replace = F)
  spl = split(rand, ceiling(seq_along(samp) / (n/partitions)))
  return(spl)
}

VarParts = list()
split = split_data(n, partitions = parts)
for (p in 1:length(split)){
  VarParts[[p]] = VarEst[split[[p]]]
}

pert_nnet_mult <- function(sizes, reps = 1){
  df_list = list()
  for (r in 1:reps){ 
    Omega<-rep(0,parts)
    DFi<-rep(0,parts)
    Omega_df <- matrix(0, nrow = parts, ncol = length(sizes))
    for (k in 1:parts)
    {
      
      Pert1<-y
      Pert2<-y
      inds = split[[k]]
      Pert1[inds]<-1
      Pert2[inds]<-0
      
      for (j in 1:length(sizes)){
        Muhat1<- nnet(indep, Pert1, size = sizes[j], rang = 0.1,
                      decay = 5e-4, maxit = 200,trace=F)$fitted.values
        Muhat2<- nnet(indep, Pert2, size = sizes[j], rang = 0.1,
                      decay = 5e-4, maxit = 200,trace=F)$fitted.values
        Muhat1<-ifelse(Muhat1>0.9999,0.9999,Muhat1)
        Muhat1<-ifelse(Muhat1<0.0001,0.0001,Muhat1)
        Muhat2<-ifelse(Muhat2>0.9999,0.9999,Muhat2)
        Muhat2<-ifelse(Muhat2<0.0001,0.0001,Muhat2)
        
        LambdaEst1<-log(Muhat1/(1-Muhat1))
        LambdaEst2<-log(Muhat2/(1-Muhat2))
        
        Omega_df[k,j]<-sum(VarEst[inds]*(LambdaEst1[inds]-LambdaEst2[inds]))
        #print(Muhat1[k]-Muhat2[k])
      }
      
      print(paste(k, "finished"))
      
      # Should be this, if following logic of equation 2.12, but doesn't work as expected
      #DFi[k]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])/2/VarTot
    }
    
    Omega_df = as.data.frame(Omega_df)
    colnames(Omega_df) = sizes
    df_list[[r]] = Omega_df
  }
  return(df_list)
  
}

# Put in Number of nodes you want to see here
omega_list = pert_nnet_mult(input_sizes, reps = input_reps)

csums = sapply(omega_list, colSums)
print(csums)
rnames = rownames(csums)
df = as.data.frame(csums) %>% tibble::rownames_to_column("num_nodes") %>% 
  melt() %>% rename(repetition = variable, GDF = value)

ggplot(df, aes(num_nodes, GDF, colour = repetition)) +
  geom_point() +
  labs(x = "Num Hidden Nodes", y = "GDF", title = paste("N = ", n))

print(df)
