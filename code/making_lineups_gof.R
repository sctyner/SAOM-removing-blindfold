# creating lineups for model goodness of fit. 

load("data/ansnullpaper.rda")
#ansnull$sims

# for model 1 
set.seed(9430852)
M6 <- sample(1000, size = 5, replace = F)
M12 <- sample(1000, size = 11, replace = F)
M16 <- sample(1000, size = 15, replace = F)
M20 <- sample(1000, size = 19, replace = F)

simsM6 <- ansnull$sims[M6]
simsM12 <- ansnull$sims[M12]
simsM16 <- ansnull$sims[M16]
simsM20 <- ansnull$sims[M20]

simsM6[[1]][[1]]$friendship$`1`

# for model2 

#looks like 20:35 are pretty well connected, go with those for now
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w2 <- friend.data.w2[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]
# read in covariate data
drink <- as.matrix(read.table("data/s50_data/s50-alcohol.dat"))
drink2 <- drink[20:35,]
# siena data
library(RSiena)
friendData2 <- array( c( fd2.w1, fd2.w2, fd2.w3 ),
                      dim = c( 16, 16, 3 ) )
# and next give this the role of the dependent variable:
friend2 <- sienaDependent(friendData2)
alcohol2 <- varCovar( as.matrix(drink2) )
# create siena data object
mysmalldata <- sienaDataCreate( friend2, alcohol2)
null_model_eff2 <- getEffects(mysmalldata)
test_model_eff2 <- includeEffects( null_model_eff2, jumpXTransTrip, interaction1 = "alcohol2")
#test_model_eff2 <- includeEffects( null_model_eff2, sameXTransTrip, interaction1 = "alcohol2")
myalgorithm2 <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)
ests_test <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = test_model_eff2, batch=TRUE, verbose = FALSE, silent = TRUE)
#ests_test <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = test_model_eff2, batch=TRUE, verbose = FALSE)

set.seed(7902487)
M6.2 <- sample(1000, size = 5, replace = F)
M12.2 <- sample(1000, size = 11, replace = F)
M16.2 <- sample(1000, size = 15, replace = F)
M20.2 <- sample(1000, size = 19, replace = F)
# ok no matches, proceed.
simsM6.2 <- ests_test$sims[M6.2]
simsM12.2 <- ests_test$sims[M12.2]
simsM16.2 <- ests_test$sims[M16.2]
simsM20.2 <- ests_test$sims[M20.2]

# model 3
test_model_eff3 <- includeEffects( null_model_eff2, nbrDist2twice)
#test_model_eff2 <- includeEffects( null_model_eff2, sameXTransTrip, interaction1 = "alcohol2")
myalgorithm3 <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)
ests_test2 <- siena07( myalgorithm2, data = mysmalldata, returnDeps = TRUE, effects = test_model_eff2, batch=TRUE, verbose = FALSE, silent = TRUE)

set.seed(3983576)
M6.3 <- sample(1000, size = 5, replace = F)
M12.3 <- sample(1000, size = 11, replace = F)
M16.3 <- sample(1000, size = 15, replace = F)
M20.3 <- sample(1000, size = 19, replace = F)
# ok no matches, proceed.
simsM6.3 <- ests_test2$sims[M6.3]
simsM12.3 <- ests_test2$sims[M12.3]
simsM16.3 <- ests_test2$sims[M16.3]
simsM20.3 <- ests_test2$sims[M20.3]

smallfriends <- read_csv("data/smallfriends4Geomnet.csv")
smallfriends2 <- smallfriends[,c(1,2,4)]
names(smallfriends2) <- c("from", "to", "wave")
smallfriends2$sim = "dat"
sims_to_lineup <- function(sims, dat, w, sd, filename){
  dat2 <- filter(dat, wave == w) %>% dplyr::select(from, to, sim)
  sims_df <- sims_to_df(sims = sims)
  sims_df2 <- dplyr::filter(sims_df, wave == w-1) %>% 
    dplyr::select(from, to, sim)
  sims_df2$from <- ifelse(is.na(sims_df2$from), NA, paste0("V", sims_df2$from))
  sims_df2$to <- ifelse(is.na(sims_df2$to), NA, paste0("V", sims_df2$to))
  # sims_for_plot <- fortify(as.edgedf(sims_df2), group = "sim")
  # names(sims_for_plot) <- c("from", "sim", "to")
  # sims_for_plot <- sims_for_plot[,c("from", "to", "sim")]
  dat2$from <- ifelse(is.na(dat2$from), NA, paste0("V", dat2$from))
  dat2$to <- ifelse(is.na(dat2$to), NA, paste0("V", dat2$to))
  
  #create_lineup(sims = sims_df2, dat = dat2, sd = 12345)
  # copy over from create_lineup function. it's not working right
  
  dat_for_lu <- rbind(sims_df2, dat2)
  M <- max(sims_df2$sim) + 1
  if (!is.null(sd)) {
    set.seed(sd)
  }
  dat_for_lu$sim <- as.factor(dat_for_lu$sim)
  dat_for_lu <- arrange(dat_for_lu, sim)
  dat_for_lu$plot_order <- rep(sample(M), as.vector(table(dat_for_lu$sim, 
                                                          useNA = "ifany")))
  ans <- unique(subset(dat_for_lu, sim == "dat")$plot_order)
  require(geomnet)
  p <- ggplot(data = dat_for_lu) + 
    geom_net(color = 'black', directed = T, fiteach = T, 
             aes(from_id = as.factor(from), to_id = as.factor(to)), 
             ecolour = "grey60", arrowgap = 0.02, 
             size = 3, linewidth = 1, arrowsize = 0.5,
             singletons = T) + 
    facet_wrap(~plot_order) + 
    theme_net() + 
    theme(panel.background = element_rect(fill = NA, color = "black") ) 
  ggplot2::ggsave(filename, p, width = unit(11, units = "in"), height = unit(8.5, units = "in"))
  return(list(p = p, dat_plot = ans))
}


sims_to_lineup(sims = simsM6, dat = smallfriends2, w = 2, sd = 12345, filename = "img/Model1_m6.pdf")
sims_to_lineup(sims = simsM6.2, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model2_m6.pdf")
sims_to_lineup(sims = simsM6.3, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model3_m6.pdf")
sims_to_lineup(sims = simsM12, dat = smallfriends2, w = 2, sd = 12345, filename = "img/Model1_m12.pdf")
sims_to_lineup(sims = simsM12.2, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model2_m12.pdf")
sims_to_lineup(sims = simsM12.3, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model3_m12.pdf")
sims_to_lineup(sims = simsM16, dat = smallfriends2, w = 2, sd = 12345, filename = "img/Model1_m16.pdf")
sims_to_lineup(sims = simsM16.2, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model2_m16.pdf")
sims_to_lineup(sims = simsM16.3, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model3_m16.pdf")
sims_to_lineup(sims = simsM20, dat = smallfriends2, w = 2, sd = 12345, filename = "img/Model1_m20.pdf")
sims_to_lineup(sims = simsM20.2, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model2_m20.pdf")
sims_to_lineup(sims = simsM20.3, dat = smallfriends2, w = 2, sd = 12345, filename =  "img/Model3_m20.pdf")
