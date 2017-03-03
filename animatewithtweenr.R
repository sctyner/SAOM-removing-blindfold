# animation

library(animation)

#quincunx

# need to take a set of points (x1,y1) and move them to new locations (x2,y2)
 
# extract layout for initial points. then use those to layout first microstep
 
# Step 0: get data in and model it. 
library(RSiena)
setwd("data/s50_data")
friend.data.w1 <- as.matrix(read.table("s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("s50-network3.dat"))
drink <- as.matrix(read.table("s50-alcohol.dat"))
smoke <- as.matrix(read.table("s50-smoke.dat"))
friendship <- sienaDependent(
  array(c(friend.data.w1, friend.data.w2,
          friend.data.w3),
        dim = c(50, 50, 3)))
smoke1 <- coCovar(smoke[ , 1 ])
alcohol <- varCovar(drink)
mydata <- sienaDataCreate(friendship, smoke1, alcohol)
myeffnull <- getEffects(mydata)
myeffalt1 <- includeEffects(myeffnull, transTrip)
myalgorithm <- sienaAlgorithmCreate(projname = 's50_3')
set.seed(823746)
ansnull <- siena07(myalgorithm, data = mydata, effects = myeffnull,
                   returnChains = TRUE, returnDataFrame = TRUE,
                   returnDeps = TRUE, silent = TRUE, verbose = FALSE,
                   batch = TRUE)
get_chain_info <- function(ans){
  L <- length(ans$chain)
  M <- length(ans$chain[[1]][[1]])
  bigres <- NULL
  res <- data.frame()
  for (l in 1:L){
    res <- data.frame()
    for (m in 1:M){
      sub <- data.frame(plyr::ldply(
        lapply(ansnull$chain[[l]][[1]][[m]], unlist),
        rbind), stringsAsFactors = FALSE)
      sub$rep <- l
      sub$period <- m
      res <- rbind(res, sub)
    }
    bigres[[l]] <- res  
  }
  ret <- plyr::rbind.fill(bigres)
  return(ret)
}
ansnullchains <- get_chain_info(ansnull)
library(dplyr)
library(readr)
ansnullchains %>% 
  filter(period == 1) %>% 
  group_by(rep) %>%
  select(rep, from = X4, to = X5) %>% 
  mutate(val = as.numeric(!from == to),
         from = paste0("V", parse_number(from)+1), # make the chains
         to = paste0("V", parse_number(to)+1) # match the data vars 
  ) -> ansnullchainsw1w2

addMicrostep <- function(dat, newedge){
  N <- nrow(dat)
  selfie <- newedge$from == newedge$to
  check <- dplyr::filter(dat, from == newedge$from & to == newedge$to)
  if (nrow(check) == 0 && !selfie){
    dat2 <- tibble::add_row(dat, from = newedge$from, to = newedge$to)
    dat2$col <- FALSE
    dat2$col[N+1] <- TRUE # color the new edge. 
  } else if (nrow(check) == 1 && !selfie){
    dat2 <- dplyr::filter(dat, !(from == newedge$from & to == newedge$to))
    dat2$col <- FALSE
  } else{
    dat2 <- dat
    dat2$col <- FALSE
  }
  return(dat2)
}

listMicrosteps <- function(dat, microsteps){
  iters <- NULL
  iters[[1]] <- dat
  B <- nrow(microsteps)
  for (i in 2:(B+1)){
    iters[[i]] <- addMicrostep(iters[[(i-1)]], microsteps[(i-1),])
  }
  return(iters)
}

# test it on one of the reps from the null model

ms1 <- listMicrosteps(dat = w1, 
                      microsteps = filter(ansnullchainsw1w2, rep == 1))

# Step 1: Layout the initial data
library(sna)
w1.net <- as.network(friend.data.w1)
set.seed(9384750)
# friend.data.w1 is an adjacency matrix. 
layout_init <- gplot.layout.spring(w1.net, layout.par = NULL)
layout_init <- data.frame(layout_init)
names(layout_init) <- c("x", "y")
layout_init$colour <- 'red'
layout_init$id <- as.factor(get.vertex.attribute(w1.net, "vertex.names"))
data1 <- layout_init
library(geomnet)
wave1friends <- fortify(as.adjmat(friend.data.w1))
netwave1 <- merge(merge(wave1friends, data1, by.x = 'from', by.y = 'id'), data1, by.x = 'to', by.y = 'id', suffixes = c(".from", ".to"))
head(netwave1)
# Step 2: get next network data observation
# Microstep friend data wave 1 , step 1
ms_fdw1.1 <- ms1[[2]]
mat <- matrix(0, 50, 50) # generalize to n x n 
mat[apply(ms_fdw1.1[,1:2], 2,readr::parse_number)] <- 1
colnames(mat) <- paste0("V", 1:50)
ms_fdw1.1 <- mat
net_ms_fdw1.1 <- as.network(ms_fdw1.1)
set.seed(9384750)
# layout
layout_ms_fdw1.1 <- gplot.layout.spring(net_ms_fdw1.1, layout.par = NULL)
layout_ms_fdw1.1 <- data.frame(layout_ms_fdw1.1)
names(layout_ms_fdw1.1) <- c("x", "y")
layout_ms_fdw1.1$colour <- 'grey40'
# as.factor() gets rid of that dumb color error. there's an internal function col_classes that makes to big of an assumption about character class columns
layout_ms_fdw1.1$id <- as.factor(get.vertex.attribute(net_ms_fdw1.1, "vertex.names"))
data2 <- layout_ms_fdw1.1

wave1friendsms1 <- fortify(as.adjmat(ms_fdw1.1))
netwave1ms1 <- merge(merge(wave1friendsms1, data2, by.x = 'from', by.y = 'id'), data2, by.x = 'to', by.y = 'id', suffixes = c(".from", ".to"))

library(tweenr)
# TWEEN EM
data <- tween_states(data = list(data1, data2), tweenlength = 3, 1, "linear", 100)
fulldata <- tween_states(data = list(netwave1, netwave1ms1), 3, 1, 'linear', 50)
# doesn't work, need same # of rows. Need to deal with to NAs somehow. 
netwave1 <- arrange(netwave1, to,from)
netwave1ms1 <- arrange(netwave1ms1, to, from)
head(netwave1ms1)
n1 <- nrow(netwave1)
n2 <- nrow(netwave1ms1)
library(ggplot2)
library(gganimate)

#animate em
p <- ggplot(data=data, aes(x=x, y=y)) + 
  geom_point(aes(frame = .frame, colour = colour), size=5) + 
  geom_text(aes(frame = .frame, label=id), alpha = .6) + 
  scale_colour_identity() + 
  theme_bw()
animation::ani.options(interval = 1/15)
gganimate(p, "dancing ball.gif", title_frame = F)

# okay - here's what we need:
  # a data frame of every possible edge
  # from.x and from.y and to.x, to.y
  # rules: from.x and from.y is always the point corresponding to the from node. 
  # if (from,to) is in the network, to.x and to.y is the point corresponding to the to node. 
  # if not, it's from.x and from.y

alledges <- expand.grid(from = paste0("V", 1:50), to = paste0("V", 1:50))
  # get rid of selfies
library(dplyr)
alledges %>% filter(from!=to) -> alledges 

# edges df for initial network state

#big1 <- left_join(alledges, netwave1, by = c("from" = "from", "to" = "to"))
big1 <- left_join(alledges, data1[,-3], by = c("from" = "id"))
head(big1)
names(big1)[3:4] <- c("x.from", "y.from")
big1 <- left_join(big1, data1[,-3], by = c("to"="id")) 
names(big1)[5:6] <- c("x.to", "y.to")
head(big1)

for(i in 1:nrow(big1)){
  edge <- big1[i,c("from","to")]
  dat <- filter(netwave1, from == edge$from & to == edge$to )
  if(nrow(dat) == 0){
    big1[i, c("x.to", "y.to")] <- big1[i, c("x.from", "y.from")]
  }
}

big2 <- left_join(alledges, data2[,-3], by = c("from" = "id"))
head(big2)
names(big2)[3:4] <- c("x.from", "y.from")
big2 <- left_join(big2, data2[,-3], by = c("to"="id")) 
names(big2)[5:6] <- c("x.to", "y.to")
head(big2)

for(i in 1:nrow(big2)){
  edge <- big2[i,c("from","to")]
  dat <- filter(netwave1ms1, from == edge$from & to == edge$to )
  if(nrow(dat) == 0){
    big2[i, c("x.to", "y.to")] <- big2[i, c("x.from", "y.from")]
  }
}
big1$color = "blue"
big2$color = "red"
library(tweenr)
big1$from <- as.factor(big1$from)
big1$to<- as.factor(big1$to)
big2$from <- as.factor(big2$from)
big2$to<- as.factor(big2$to)
big <- tween_states(data = list(big1, big2), 3,1,"linear", nframes = 20)

p <- ggplot() + 
  geom_point(data = unique(big[, c("from", "x.from", "y.from", "color", ".frame")]), 
             aes(x=x.from, y = y.from, frame = .frame, colour = color), size=5) + 
  geom_text(data = unique(big[, c("from", "x.from", "y.from", "color", ".frame")]),
            aes(x=x.from, y = y.from, frame = .frame, label=from), alpha = .5) + 
  geom_segment(data = big, aes(x = x.from, y = y.from, xend = x.to, yend = y.to, frame = .frame)) + 
  scale_colour_identity() + 
  theme_bw()
animation::ani.options(interval = 1/15)
gganimate(p, "networkanimation.gif", title_frame = F)
