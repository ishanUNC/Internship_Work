#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@ Based on 2004 Efron's paper @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@ Efron B. (2004) The estimation of prediction error: covariance 
#@@@ penalties and cross-validation, 
#@@@ Journal of the American Statistical Association, Vol. 99(467), 619-632
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(reshape2)
library(tidyverse)

#Create a sample dataset of X
n<-1000
#x1<-sample(1:n,n,replace=T)/n*10
#x2<-sample(1:n,n,replace=T)/n*10
#x3<-x1^2
#x4<-x2^2
#x5<-x1 + x4 
#x6<-sample(1:n,n,replace=T)/n*10

x1<-rnorm(n)
x2<-rnorm(n)*2
x3<-rnorm(n)*3
x4<-x1^2
x5<-x2^2
# x6 is a combination of x1 and x4
x6<-x1 + x4 
x7<-sample(1:n,n,replace=T)/n*10
b<-c(1,1,1,1,1,1,1,1)

#@@@@@ TRUE MODEL @@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@ Create logodds of probabilities

#Empty model
#Model1
#ytoprob<-(b[1])/5

#clean model with independent variables
#Model2
#ytoprob<-(b[1]+b[2]*x1+b[3]*x2+b[4]*x3)/5

#Excessive models
#Model3
ytoprob<-(b[1]+b[2]*x1+b[3]*x2+b[4]*x3+b[5]*x4+b[6]*x5+b[7]*x6+b[8]*x7)/5

#Model4
#ytoprob<-b[1]+b[2]*x1+b[3]*x2+b[4]*x3+b[5]*x4

hist(ytoprob)

#Create TRUE probabilities
Probs<-exp(ytoprob)/(1+exp(ytoprob))
hist(Probs)
mean(Probs)

#@@@@@@@@@@ BINARY OUTCOMES FROM THE MODELS @@@@@@@
#create binary outcomes
Zbin<-rbinom(n,1,Probs)
table(Zbin)
#hist(ytoprob)

#This is the true value of binomial deviance
Lambdai<-log(Probs/(1-Probs))
hist(Lambdai)
sum(Lambdai)

#Run an estimation model
#ZmodBin<-glm(Zbin~x1+x2+x3+x4+x5+x6,family="binomial")
ZmodBin<-glm(Zbin~x1+x2+x3+x4+x5+x6+x7,family="binomial")
#ZmodBin<-glm(Zbin~x1+x2,family="binomial")
#ZmodBin<-lm(Zbin~x1+x2+x6)

summary(ZmodBin)
names(ZmodBin)
muhat<-ZmodBin$fitted.values
hist(muhat)
totmean<-mean(muhat)
VarTot<-totmean*(1-totmean)

#Estimate Total Variance
VarEst<-muhat*(1-muhat)
hist(VarEst)
sum(VarEst)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@ Estimation of DFs by perturbing one observation at a time (conditional covariance) @
#@@@@ Based on equation 3.22 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Discussion in the simulation section shows that df=Omega/2. We have DF=Omega @@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Omega<-rep(0,n)
# DFi<-rep(0,n)
# 
# for (k in 1:n)
# {
#   Pert1<-Zbin
#   Pert2<-Zbin
#   Pert1[k]<-1
#   Pert2[k]<-0
#   Muhat1<-glm(Pert1~x1+x2+x3+x4+x5+x6+x7,family="binomial")$fitted.values
#   Muhat2<-glm(Pert2~x1+x2+x3+x4+x5+x6+x7,family="binomial")$fitted.values
#   #   Muhat1<-glm(Pert1~x1+x2,family="binomial")$fitted.values
#   #   Muhat2<-glm(Pert2~x1+x2,family="binomial")$fitted.values
#   LambdaEst1<-log(Muhat1/(1-Muhat1))
#   LambdaEst2<-log(Muhat2/(1-Muhat2))
#   
#   #Below is not Omega but rather omega/2. Don't need to divide by 2 later.
#   Omega[k]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])
#   # Should be this, if following logic of equation 2.12, but doesn't work as expected
#   DFi[k]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])/2/VarTot
# }
# 
# hist(Omega)
# #hist(DFi)
# #sum(DFi)
# #DFraw seems to be working OK
# Dfraw<-sum(Omega)
# Dfraw
# 
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# #@@@@ Parametric (unconditional) Bootstrap @@@@@@@@@@@@@
# #@@@@ Use the idea of linear regression of y on muhat @@
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 
# mm<-1000
# Simout<-matrix(0,ncol=mm,nrow=n) 
# ZbinSim<-matrix(0,ncol=mm,nrow=n) 
# 
# for (k in 1:mm)
# {
#   ZbinSim[,k]<-rbinom(n,1,muhat)
#   
#   #True values of mu the correct result
#   #ZbinSim[,k]<-rbinom(n,1,Probs)
#   
#   #ZmodBin1<-glm(ZbinSim[,k]~x1+x2+x3+x4+x5+x7,family="binomial")
#   ZmodBin1<-glm(ZbinSim[,k]~x1+x2,family="binomial")
#   #ZmodBin1<-glm(ZbinSim[,k]~x1+x2+x3+x4+x5,family="binomial")
#   Simout[,k]<-ZmodBin1$fitted.values
#   
# }
# 
# 
# CoefReg<-rep(0,n)
# for (k in 1:n)
# {
#   CoefReg[k]<-lm(Simout[k,]~ZbinSim[k,])$coefficients[2]
#   print(CoefReg[k])
#   
#   # if (is.na(CoefReg[k])) {print(Simout[k,])
#   #    plot(ZbinSim[k,],Simout[k,])
#   #    print(summary(lm(Simout[k,]~ZbinSim[k,])))}
# }

#some bootsrtrap samplescan lead to degenerate samples and geenrate NA in regression
#should go away with very large samples that will have enough variability
#Here we ignore the NA runs and rather than directly calculating the sum 
#we calclulate the mean fo non-NA values and multuply by the n
mean(CoefReg,na.rm=T)*n

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@ NEURAL NETWORKS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Parametric (conditional) Bootstrap  @@@@@@@@@@@@@@
#@@@@ Use the calculations in Efron formula 3.17  @@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(nnet)
targets<-Zbin
indep<-cbind(x1,x2,x3,x4,x5,x6,x7)
trainInd <- sample(1:n,2*n/3)
testInd <- (1:n)[-trainInd]
table(targets[trainInd])
NodeList<-1:10
CorVec<-1:length(NodeList)

#@@@@@@@@@@@@@  Estimating which model performs the best @@@@@@
#@@@@@@@@@@@@@ bestNode is the model @@@@@@@@@@@@@@@@@@@@@@@@@@

# for (k in 1:length(NodeList))
# {
#   hidNodes<-NodeList[k]
# NetOut<- nnet(indep[trainInd,], targets[trainInd], size = hidNodes, rang = 0.01,
#             decay = 5e-4, maxit = 200,trace=F)
# 
# #plot(Probs,NetOut$fitted.values)
# print(c(hidNodes,cor(targets[testInd],predict(NetOut,indep[testInd,]))))
# CorVec[k]<-cor(targets[testInd],predict(NetOut,indep[testInd,]))
# }
# CorVec
# bestNode<-NodeList[order(CorVec)[length(CorVec)]]
# bestNode

#@@@@@@@@@@@@@ Calculating the DF using conditional perturbation @@@@@@@@@@@@@@@@@@@@@@

pert_nnet <- function(sizes, maxits, reps = 1){
df_list = list()
iter = 1
  
Omega_df = data.frame(matrix(nrow = n*length(maxits)*length(sizes)*reps, ncol = 5))
colnames(Omega_df) = c("rep", "size", "maxit", "omega_num", "omega")
 for (r in 1:reps){ 
   Omega<-rep(0,n)
   OmegaLarge <- rep(0,n)
   DFi<-rep(0,n)
   
    for (k in 1:n)
    {
      
      Pert1<-Zbin
      Pert2<-Zbin
      Pert1[k]<-1
      Pert2[k]<-0
      
      col = 1
      for (i in 1:length(maxits)){
      for (j in 1:length(sizes)){
        Muhat1<- nnet(indep, Pert1, size = sizes[j], rang = 0.1,
                      decay = 5e-4, maxit = maxits[i],trace=F)$fitted.values
        Muhat2<- nnet(indep, Pert2, size = sizes[j], rang = 0.1,
                      decay = 5e-4, maxit = maxits[i],trace=F)$fitted.values
        Muhat1<-ifelse(Muhat1>0.9999,0.9999,Muhat1)
        Muhat1<-ifelse(Muhat1<0.0001,0.0001,Muhat1)
        Muhat2<-ifelse(Muhat2>0.9999,0.9999,Muhat2)
        Muhat2<-ifelse(Muhat2<0.0001,0.0001,Muhat2)
        
        LambdaEst1<-log(Muhat1/(1-Muhat1))
        LambdaEst2<-log(Muhat2/(1-Muhat2))
        
        
        Omega_df[iter,]<-list(r, sizes[j], maxits[i], k, VarEst[k]*(LambdaEst1[k]-LambdaEst2[k]))
        iter = iter+1
        #print(Muhat1[k]-Muhat2[k])
      }
      }
      
      print(paste(k, "finished"))
      
      # Should be this, if following logic of equation 2.12, but doesn't work as expected
      #DFi[k]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])/2/VarTot
    }
    
 }
  return(Omega_df)
  
}
  
# Put in Number of nodes you want to see here
library(reshape2)
library(dplyr)
omega_df = pert_nnet(c(1,2,3), maxits = c(20, 50, 100, 150, 200, 300, 400, 500), reps = 2)
omega_df$lambda_diff = ifelse(omega_df$omega > 0, "pos", "neg")

omega_sum = omega_df %>% group_by(rep, size, maxit) %>% summarise(GDF = sum(omega)) %>%group_by(size,maxit) %>%
  summarise(mean_over_reps = mean(GDF), sd_over_reps = sd(GDF))
sum_df = spread(omega_df %>% group_by(size, maxit) %>% count(lambda_diff), lambda_diff, n) %>% 
  mutate('prop_neg' = neg/(neg + pos))
sum_df$size = as.factor(sum_df$size)
ggplot(sum_df, aes(maxit, prop_neg, col = size)) +
  geom_point() +
  labs(x = "Max Iterations", y = "Proportion of Negative Omegas", title = paste("N = ", n, ", Partitions = ", n))


omega_df$lambda_diff = ifelse(omega_df$omega > 0, 1, 0)
pos_df = omega_df %>% group_by(size, maxit, omega_num) %>% summarise(prop_pos = sum(lambda_diff), avg_omega = mean(omega))


csums = sapply(omega_list, colSums)
print(csums)
rnames = rownames(csums)
df = as.data.frame(csums) %>% tibble::rownames_to_column("num_nodes") %>% 
  melt() %>% rename(repetition = variable, GDF = value)

ggplot(df, aes(num_nodes, GDF, colour = repetition)) +
  geom_point() +
  labs(x = "Num Hidden Nodes", y = "GDF", title = paste("N = ", n))

# ----------------------MULTIPLE POINT PERTURBATION WITH MAXITS ------------ 

targets<-Zbin
indep<-cbind(x1,x2,x3,x4,x5,x6,x7)
trainInd <- sample(1:n,2*n/3)
testInd <- (1:n)[-trainInd]
table(targets[trainInd])
NodeList<-1:10
CorVec<-1:length(NodeList)

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

pert_nnet_mult <- function(sizes, maxits, reps = 1){
  iter = 1
  df_list = list()
  Omega_df = data.frame(matrix(nrow = parts*length(maxits)*length(sizes)*reps, ncol = 5))
  colnames(Omega_df) = c("rep", "size", "maxit", "partition", "omega")
  for (r in 1:reps){ 
    Omega<-rep(0,parts)
    DFi<-rep(0,parts)
    
    for (k in 1:parts)
    {
      
      Pert1<-Zbin
      Pert2<-Zbin
      inds = split[[k]]
      Pert1[inds]<-1
      Pert2[inds]<-0
      for (i in 1:length(maxits)){
      for (j in 1:length(sizes)){
        Muhat1<- nnet(indep, Pert1, size = sizes[j], rang = 0.1,
                      decay = 5e-4, maxit =maxits[i],trace=F)$fitted.values
        Muhat2<- nnet(indep, Pert2, size = sizes[j], rang = 0.1,
                      decay = 5e-4, maxit =  maxits[i],trace=F)$fitted.values
        Muhat1<-ifelse(Muhat1>0.9999,0.9999,Muhat1)
        Muhat1<-ifelse(Muhat1<0.0001,0.0001,Muhat1)
        Muhat2<-ifelse(Muhat2>0.9999,0.9999,Muhat2)
        Muhat2<-ifelse(Muhat2<0.0001,0.0001,Muhat2)
        
        LambdaEst1<-log(Muhat1/(1-Muhat1))
        LambdaEst2<-log(Muhat2/(1-Muhat2))
        
        Omega_df[iter,]<-list(r, sizes[j], maxits[i], k, sum(VarEst[inds]*(LambdaEst1[inds]-
                                                                             LambdaEst2[inds])))
        iter = iter+1
        #print(Muhat1[k]-Muhat2[k])
      }
      }
      print(paste(k, "finished"))
      
      # Should be this, if following logic of equation 2.12, but doesn't work as expected
      #DFi[k]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])/2/VarTot
    }
    
    Omega_df = as.data.frame(Omega_df)
  }
  return(Omega_df)
  
}

# Put in Number of nodes you want to see here
omega_list = pert_nnet_mult(1, maxits = c(200), reps = 50)

omega_sum = omega_list %>% group_by(rep, size, maxit) %>% summarise(GDF = sum(omega)) %>%group_by(size,maxit) %>%
  summarise(mean_over_reps = mean(GDF), sd_over_reps = sd(GDF))
omega_list$lambda_diff = ifelse(omega_list$omega > 0, "pos", "neg")
sum_df = spread(omega_list %>% group_by(size, maxit) %>% count(lambda_diff), lambda_diff, n) %>% 
  mutate('prop_neg' = neg/(neg + pos))
sum_df$size = as.factor(sum_df$size)
ggplot(sum_df, aes(maxit, prop_neg, col = size)) +
  geom_point() +
  labs(x = "Max Iterations", y = "Proportion of Negative Omegas", title = paste("N = ", n, ", Partitions = ", parts))

by_partition = spread(omega_list %>% select(-lambda_diff), rep, omega)
by_partition = by_partition %>% select(-size, -maxit, -partition)
by_partition = as.data.frame(t(by_partition))

ggplot(data = melt(by_partition), aes(variable, value)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = 'Omega Value', x = 'Partition', title = "1 Hidden Node,  Maxit = 200")

partition_quantiles = as.data.frame(t(apply
                                      (by_partition, 1, quantile, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))))

csums = sapply(omega_list, colSums)
print(csums)
rnames = rownames(csums)
df = as.data.frame(csums) %>% tibble::rownames_to_column("num_nodes") %>% 
  melt() %>% rename(repetition = variable, GDF = value)

ggplot(df, aes(num_nodes, GDF, colour = repetition)) +
  geom_point() +
  labs(x = "Num Hidden Nodes", y = "GDF", title = paste("N = ", n))



# ------------------- Using neuralnet ---------------------------
pert_neuralnet <- function(sizes, reps = 1){
  df_list = list()
  for (r in 1:reps){ 
    Omega<-rep(0,n)
    OmegaLarge <- rep(0,n)
    DFi<-rep(0,n)
    Omega_df <- matrix(0, nrow = n, ncol = length(sizes))
    for (k in 1:n)
    {
      
      Pert1<-Zbin
      Pert2<-Zbin
      Pert1[k]<-1
      Pert2[k]<-0
      AllSet<-data.frame(Pert1, Pert2, x1,x2,x3,x4,x5,x6,x7)
      
      for (j in 1:length(sizes)){
        Muhat1<- neuralnet(Pert1~x1+x2+x3+x4+x5+x6+x7, data=AllSet, stepmax = 200, 
                            hidden= sizes[j], linear.output = FALSE)$net.result[[1]][,1]
        Muhat2<- neuralnet(Pert2~x1+x2+x3+x4+x5+x6+x7, data=AllSet, stepmax = 200,
                            hidden= sizes[j], linear.output = FALSE)$net.result[[1]][,1]
        Muhat1<-ifelse(Muhat1>0.9999,0.9999,Muhat1)
        Muhat1<-ifelse(Muhat1<0.0001,0.0001,Muhat1)
        Muhat2<-ifelse(Muhat2>0.9999,0.9999,Muhat2)
        Muhat2<-ifelse(Muhat2<0.0001,0.0001,Muhat2)
        
        LambdaEst1<-log(Muhat1/(1-Muhat1))
        LambdaEst2<-log(Muhat2/(1-Muhat2))
        
        Omega_df[k,j]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])
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
library(reshape2)
library(dplyr)
omega_list = pert_neuralnet(c(1,2,3), reps = 3)
csums = sapply(omega_list, colSums)
print(csums)
rnames = rownames(csums)
df = as.data.frame(csums) %>% tibble::rownames_to_column("num_nodes") %>% 
  melt() %>% rename(repetition = variable, GDF = value)

ggplot(df, aes(num_nodes, GDF, colour = repetition)) +
  geom_point() +
  labs(x = "Num Hidden Nodes", y = "GDF", title = paste("N = ", n))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ms
#@@@@@@@@@@@@@@ Another package for neural network @@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# library(neuralnet)
# AllSet<-data.frame(Zbin,x1,x2,x3,x4,x5,x6,x7)
# trainInd <- sample(1:n,2*n/3)
# testInd <- (1:n)[-trainInd]
# NodeList<-1:10
# CorVec<-1:length(NodeList)
# 
# #@@@@@@@@@@@@@  Estimating which model performs the best @@@@@@
# #@@@@@@@@@@@@@ bestNode is the model @@@@@@@@@@@@@@@@@@@@@@@@@@
# 
# for (k in 1:length(NodeList))
# {
#   hidNodes<-NodeList[k]
#   NetOut<- neuralnet(Zbin~x1+x2+x3+x4+x5+x6+x7, data=AllSet[trainInd,], hidden= hidNodes, linear.output = FALSE)
#   
#   plot(Probs[trainInd],NetOut$net.result[[1]])
#   print(c(hidNodes,cor(targets[testInd],predict(NetOut,AllSet[testInd,]))))
#   CorVec[k]<-cor(targets[testInd],predict(NetOut,AllSet[testInd,]))
# }
# CorVec
# bestNode<-NodeList[order(CorVec)[length(CorVec)]]
# bestNode
# 
# #@@@@@@@@@@@@@ Calculating the DF using conditional perturbation @@@@@@@@@@@@@@@@@@@@@@
# Omega<-rep(0,n)
# DFi<-rep(0,n)
# 
# for (k in 1:n)
# {
#   
#   AllSet$Pert1<-Zbin
#   AllSet$Pert2<-Zbin
#   AllSet$Pert1[k]<-1
#   AllSet$Pert2[k]<-0
#   
#   # Muhat1<- neuralnet(Pert1~x1+x2+x3+x4+x5+x6+x7, data=AllSet, hidden=bestNode, linear.output = FALSE)$net.result[[1]]
#   # Muhat2<- neuralnet(Pert2~x1+x2+x3+x4+x5+x6+x7, data=AllSet, hidden=bestNode, linear.output = FALSE)$net.result[[1]]
#   Muhat1<- neuralnet(Pert1~x1+x2+x3+x4+x5+x6+x7, data=AllSet, hidden=1, linear.output = FALSE)$net.result[[1]]
#   Muhat2<- neuralnet(Pert2~x1+x2+x3+x4+x5+x6+x7, data=AllSet, hidden=1, linear.output = FALSE)$net.result[[1]]
#   print(Muhat1[k]-Muhat2[k])
#   
#   #Guard against extreme values  
#   Muhat1<-ifelse(Muhat1>0.9999,0.9999,Muhat1)
#   Muhat1<-ifelse(Muhat1<0.0001,0.0001,Muhat1)
#   Muhat2<-ifelse(Muhat2>0.9999,0.9999,Muhat2)
#   Muhat2<-ifelse(Muhat2<0.0001,0.0001,Muhat2)
#   
#   LambdaEst1<-log(Muhat1/(1-Muhat1))
#   LambdaEst2<-log(Muhat2/(1-Muhat2))
#   Omega[k]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])
#   # Should be this, if following logic of equation 2.12, but doesn't work as expected
#   DFi[k]<-VarEst[k]*(LambdaEst1[k]-LambdaEst2[k])/2/VarTot
# }
# 
# hist(Omega)
# hist(DFi)
# sum(DFi)
# 
# #DFraw seems to be working OK
# Dfraw<-sum(Omega)
# mean(Omega,na.rm=T)*n
# 
# Dfraw
# 
# 
# 
# 
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# #@@@@ Parametric (unconditional) Bootstrap @@@@@@@@@@@@@
# #@@@@ Use the calculations in Efron formula 3.17  @@@@@@
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# #@@ Just started, haven't implemented yet
# 
# mm<-1000
# Simout<-matrix(0,ncol=mm,nrow=n) 
# ZbinSim<-matrix(0,ncol=mm,nrow=n) 
# 
# 
# 
# 
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# #@@@@ Trying a new package dglars and gdf function @@@@@@@@@@@@@@@@@
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# #@ dglars is using least angle regress (LARS) which is still a regression model 
# #install.packages("dglars")
# library(dglars)
# fit <- dglars(Zbin~x1+x2+x3+x4+x5+x6+x7, binomial)
# gdf(fit)




