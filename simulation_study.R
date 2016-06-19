# simulations

# plan: 
# 1. simulate 1000 networks from the mean values previously used as starting values for each model
# 2. combine the 1000 networks into one data set and visualize them ontop of each other.
# 3. repeat for models M1, M2, M3

# Means from previous simulations: 

dist <- read.csv("../NetworksVizInference/Data/simulation-1000-M1-M2-M3.csv")

library(dplyr)
library(tidyr)
dist %>% group_by(Model, parameter) %>% 
  summarise(value = mean(estimate)) %>% 
  spread(Model, value) -> starting

sv_M1 <- as.numeric(na.omit(starting$M1))
sv_M2 <- as.numeric(na.omit(starting$M2))
sv_M3 <- as.numeric(na.omit(starting$M3))

# simulate from each of the models 1000 times 

# load the necessary data
friend.data.w1 <- as.matrix(read.table("../NetworksVizInference/Data/s50_data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("../NetworksVizInference/Data/s50_data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("../NetworksVizInference/Data/s50_data/s50-network3.dat"))
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w2 <- friend.data.w2[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]
# read in covariate data
drink <- as.matrix(read.table("../NetworksVizInference/Data/s50_data/s50-alcohol.dat"))
drink2 <- drink[20:35,]
# siena data
friendData2 <- array( c( fd2.w1, fd2.w2, fd2.w3 ),
                      dim = c( 16, 16, 3 ) )
# and next give this the role of the dependent variable:
friend2 <- sienaDependent(friendData2)
alcohol2 <- varCovar( as.matrix(drink2) )
# create siena data object
mysmalldata <- sienaDataCreate( friend2, alcohol2)
null_model_eff2 <- getEffects(mysmalldata)
myalgorithm2 <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)

sim_frm_m1 <- function(null_eff_struct, rep, my_dat=mysmalldata, ...){
  # browser()
  null_data <- NULL
  n <- rep
  null_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = null_eff_struct, batch=TRUE, verbose = FALSE, silent = TRUE)
  null_eff_struct2 <- null_eff_struct
  null_eff_struct2$initialValue[null_eff_struct2$include] <- sv_M1
  sim_algorithm <- sienaAlgorithmCreate( projname = "sim_model", cond = FALSE,
                                         useStdInits = FALSE, nsub = 0 , simOnly = TRUE)
  for (i in 1:n){
    sim_ans <-  siena07( sim_algorithm, data = my_dat, effects = null_eff_struct2, returnDeps = TRUE)
    net_df <- merge(data.frame(sim_ans$sims[[1000]][[1]][[1]][[1]])[,-3], 
                    data.frame(id = 1:16), by.x = "X1", by.y = "id",
                    all = T)
    for (j in 1:nrow(net_df)){
      if (!(net_df$X1[j] %in% net_df$X2) & is.na(net_df$X2[j])){
        net_df$X2[j] <- net_df$X1[j]
      } else {net_df$X2[j] <- net_df$X2[j]}
    }
    net_df$count <- i 
    print(i)
    null_data <- rbind(null_data, net_df)
  }
  return(null_data)
}  

m1sims1000 <- sim_frm_m1(null_model_eff2, rep = 1000)


