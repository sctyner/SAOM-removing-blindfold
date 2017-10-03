# distances between wave 1, wave2, and sims
library(RSiena)
library(tidyverse)
load('data/ansnullpaper.rda')

ansnullchainsw1w2 %>% head
summary(ansnullchainsw1w2$rep)

amfull <- matrix(0, 16, 16)

to_adjmat <- function(empty, dat){
  for(i in 1:nrow(dat)){
    row <- dat$from[i]
    coln <- dat$to[i]
    empty[row, coln] <- dat$val[i]
  }
  return(empty)
}

ansnullchainsw1w2 %>% unique() %>% 
  mutate(from = parse_number(from), to = parse_number(to)) %>% 
  arrange(rep, from, to) %>% nest(-rep) %>% 
  mutate(mats = map(data, to_adjmat, empty = amfull)) -> ansnullchainsw1w2nest

diff1 <- function(am, wave1 = w1){
 wave1 - am
}
diff2 <- function(am, wave2 = w2){
  wave2 - am 
}

w1 <- as.matrix(read.table("data/s50_data/s50-network1.dat"))[20:35, 20:35]
colnames(w1) <- paste0("V", 1:16)
w2 <- as.matrix(read.table("data/s50_data/s50-network2.dat"))[20:35, 20:35]
colnames(w2) <- paste0("V", 1:16)
ansnullchainsw1w2nest %>% mutate(diff1 = map(mats, diff1), diff2 = map(mats, diff2)) -> ansnullchainsw1w2nest
ansnullchainsw1w2nest %>% mutate(d1 = map_dbl(diff1, function(x){sum(abs(x))}), 
                                 d2 = map_dbl(diff2, function(x){sum(abs(x))}), 
                                 miss1 = map_dbl(diff1, function(x){length(which(x == 1))}), 
                                 miss2 = map_dbl(diff2, function(x){length(which(x == 1))}),
                                 addl1 = map_dbl(diff1, function(x){length(which(x == -1))}), 
                                 addl2 = map_dbl(diff2, function(x){length(which(x == -1))}), 
                                 eq1 = map_dbl(diff1, function(x){length(which(x == 0))}),
                                 eq2 = map_dbl(diff2, function(x){length(which(x == 0))}), 
                                 dist12 = map_dbl(mats, function(x){sum((w1 != x) & (w2 == x))})) -> ansnullchainsw1w2nest

ggplot(data=ansnullchainsw1w2nest) + 
  geom_density(aes(x = d1), color = 'red', fill = 'red', alpha = .5) + 
  geom_density(aes(x = d2), color = 'blue', fill = 'blue', alpha = .5) + 
  labs(x = "D1 (red) and D2 (blue)")

library(geomnet)
geomnet:::fortify.adjmat(geomnet::as.adjmat(ansnullchainsw1w2nest[which.min(ansnullchainsw1w2nest$d2),]$mats[[1]])) %>% mutate(from = paste0("V", from)) %>% 
  ggplot() + 
  geom_net(aes(from_id = from, to_id = to), arrowgap = .02, size = 10, directed = T, labelon=T, color = 'black', labelcolour = 'white', singletons = T, hjust = .5, vjust = .5) + 
  theme_net()

geomnet:::fortify.adjmat(geomnet::as.adjmat(w2)) %>% 
  ggplot() + 
  geom_net(aes(from_id = from, to_id = to), size = 10, arrowgap = .02, directed = T, labelon=T, color = 'black', labelcolour = 'white', singletons = T, hjust = .5, vjust = .5) + 
  theme_net()

sum(abs(w2-w1))
length(which((w2-w1) == 0))
length(which((w2-w1) == 1))
length(which((w2-w1) == -1))

ggplot(data = ansnullchainsw1w2nest) + 
  geom_jitter(aes(x = d1, y = addl1, size = eq1/240), alpha = .5, color = 'red') + 
  geom_jitter(aes(x = d1, y = -miss1, size = eq1/240), alpha = .5, color = 'red') + 
  geom_jitter(aes(x = d2, y = addl2, size = eq2/240), alpha = .5, color = 'blue') + 
  geom_jitter(aes(x = d2, y = -miss2, size = eq2/240), alpha = .5, color = 'blue') + 
  geom_jitter(aes(x = d1, y = 0), size = .5, alpha = .5, shape = 4, color = "forestgreen") + 
  geom_jitter(aes(x = d2, y = 0), size = .5, alpha = .5, shape = 4, color = "darkorange") + 
  labs(x = "D1 (red) and D2 (blue)", y = "Non-matching edges", title = "Simulation Edge Differences from Data") + 
  scale_size_continuous(name = "Proportion of edges\nidentical to either W1 or W2", range = c(.25, 2))
  geom_point(aes(x = d1, y = eq1/240), color = 'green') + 
  geom_point(aes(x = d2, y = eq2/240), color = 'pink') 
  
ggplot(data = ansnullchainsw1w2nest, aes(x = dist12)) + 
  stat_bin_2d(aes(y = addl2, fill = ..count..)) + 
  stat_bin_2d(aes(y = -miss2, fill = ..count..)) + 
  geom_point(data = data.frame(x = c(0, sum(abs(w2-w1))), y = 0), aes(x = x, y = y), size = 7) + 
  geom_text(data =data.frame(x = c(0, sum(abs(w2-w1))), y = 0, label =c("W1", "W2")), aes(x = x, y = y, label = label) , color = "white") +
  theme(aspect.ratio = 1)

ggplot(data = ansnullchainsw1w2nest, aes(x = dist12)) + 
  geom_jitter(aes(y = addl2)) + 
  geom_jitter(aes(y = -miss2)) + 
  geom_point(data = data.frame(x = c(0, sum(abs(w2-w1))), y = 0), aes(x = x, y = y), size = 7) + 
  geom_text(data =data.frame(x = c(0, sum(abs(w2-w1))), y = 0, label =c("W1", "W2")), aes(x = x, y = y, label = label) , color = "white") + 
  labs(x = "Moving toward wave 2", y = "Non-matching edges (+ = added edges not in w2, - = missing edges in w2)")

  
ggplot(data = ansnullchainsw1w2nest) + 
  geom_jitter(aes(x = eq1, y = eq2))

ggplot(data = ansnullchainsw1w2nest) + 
  geom_jitter(aes(x = miss1, y = miss2))

cor(ansnullchainsw1w2nest$d1, ansnullchainsw1w2nest$d2)

head(ansnullchainsw1w2nest)
