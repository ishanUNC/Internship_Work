require(reshape2)
require(tidyverse)
require(nnet)
#Create a sample dataset of X

#@@@@@@@@@@@@@@@@@@@@@@@ Fill in values you want tested here for SINGULAR PERTURBATION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
n = 1000 # How big do you want simulated dataset?
parts = 20 # How many partitions do you want the data split into to find GDF
input_sizes = c(1, 2, 3) # Range of Node Sizes you want to test
input_maxits = c(100, 200) # Range of Maxit values you want tested
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

targets<-Zbin
indep<-cbind(x1,x2,x3,x4,x5,x6,x7)
trainInd <- sample(1:n,2*n/3)
testInd <- (1:n)[-trainInd]
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
omega_list = pert_nnet_mult(input_sizes, maxits = input_maxits, reps = input_reps)
omega_list$lambda_diff = ifelse(omega_list$omega > 0, "pos", "neg")

by_partition = omega_list %>% group_by(partition, rep) %>% 
  summarise(mean_o = mean(omega)) %>% spread(rep, mean_o) %>% 
  ungroup() %>% select(-partition) %>% 
  t() %>% as.data.frame() %>% melt()

ggplot(data = by_partition, aes(variable, value)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = 'Omega Value', x = 'Partition', title = paste("Omega Values over Repetitions = ", input_reps))

omega_sum = omega_list %>% group_by(rep, size, maxit) %>% summarise(GDF = sum(omega)) %>%group_by(size,maxit) %>%
  summarise(mean_over_reps = mean(GDF), sd_over_reps = sd(GDF))
sum_df = spread(omega_list %>% group_by(size, maxit) %>% count(lambda_diff), lambda_diff, n) %>% 
  mutate('prop_neg' = neg/(neg + pos))
sum_df$size = as.factor(sum_df$size)
ggplot(sum_df, aes(maxit, prop_neg, col = size)) +
  geom_point() +
  labs(x = "Max Iterations", y = "Proportion of Negative Omegas", title = paste("N = ", n, ", Partitions = ", parts))


gdf_table = omega_list %>% group_by(rep, size, maxit) %>% summarise(GDF = sum(omega))
gdf_table$rep = as.factor(gdf_table$rep)

if (length(input_maxits) == 1){
  ggplot(gdf_table, aes(size, GDF, colour = rep)) +
    geom_point() +
    labs(x = "Num Hidden Nodes", y = "GDF", title = paste("N = ", n, " Maxits = ", input_maxits))
}

