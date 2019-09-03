#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@ Based on 2004 Efron's paper @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@ Efron B. (2004) The estimation of prediction error: covariance 
#@@@ penalties and cross-validation, 
#@@@ Journal of the American Statistical Association, Vol. 99(467), 619-632
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
require(reshape2)
require(tidyverse)
require(nnet)
#Create a sample dataset of X
#@@@@@@@@@@@@@@@@@@@@@@@ Fill in values you want tested here for SINGULAR PERTURBATION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
n = 100 # How big do you want simulated dataset?
input_sizes = c(1, 2, 3) # Range of Node Sizes you want to test
input_maxits = c(20, 50, 100, 250, 500) # Range of Maxit values you want tested
input_reps = 2 #How many times you want this repeated
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 

x1<-rnorm(n)
x2<-rnorm(n)*2
x3<-rnorm(n)*3
x4<-x1^2
x5<-x2^2
x6<-x1 + x4 
x7<-sample(1:n,n,replace=T)/n*10
b<-c(1,1,1,1,1,1,1,1)


ytoprob<-(b[1]+b[2]*x1+b[3]*x2+b[4]*x3+b[5]*x4+b[6]*x5+b[7]*x6+b[8]*x7)/5


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

ZmodBin<-glm(Zbin~x1+x2+x3+x4+x5+x6+x7,family="binomial")

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

mean(CoefReg,na.rm=T)*n

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@ NEURAL NETWORKS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Parametric (conditional) Bootstrap  @@@@@@@@@@@@@@
#@@@@ Use the calculations in Efron formula 3.17  @@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
targets<-Zbin
indep<-cbind(x1,x2,x3,x4,x5,x6,x7)
trainInd <- sample(1:n,2*n/3)
testInd <- (1:n)[-trainInd]
table(targets[trainInd])

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

omega_df = pert_nnet(input_sizes, maxits = input_maxits, reps = input_reps)
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


gdf_table = omega_df %>% group_by(rep, size, maxit) %>% summarise(GDF = sum(omega))
gdf_table$rep = as.factor(gdf_table$rep)

if (length(input_maxits) == 1){
  ggplot(gdf_table, aes(size, GDF, colour = rep)) +
    geom_point() +
    labs(x = "Num Hidden Nodes", y = "GDF", title = paste("N = ", n, " Maxits = ", input_maxits))
}

print(gdf_table)



