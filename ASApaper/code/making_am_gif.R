# making adjmat gif

adjmatall <- matrix(0, nrow = 16, ncol = 16)
rownames(adjmatall) <- paste0("V", 1:16)
colnames(adjmatall) <- paste0("V", 1:16)
dat2 <- sfmsall %>% filter(rep == 1)
adjmatall2 <- NULL
for(i in 0:55){
  MS <- dat2[dat2$ms == i,c("from", "to")]
  myadjmat <- adjmatall
  for(j in 1:nrow(MS)){
    myadjmat[MS[j,"from"][[1]],MS[j,"to"][[1]]] <- 1
  }
  adjmatall2 <- rbind(adjmatall2, myadjmat)
}

adjmatall2 <- cbind(adjmatall2, rep(0:55, each = 16))
adjmatall2 <- data.frame(adjmatall2)
adjmatall2$from <- rep(paste0("V", 1:16), 56)
names(adjmatall2)[17] <- "ms"
adjmatall2 %>% gather(to, val, V1:V16) -> adjmatall3

adjmatall3$to <- factor(adjmatall3$to, levels = order1)
adjmatall3$from <- factor(adjmatall3$from, levels = rev(order1))

adjmatall3$frame <- adjmatall3$ms
#plots <- list()
#for(i in 0:55){
#plots[[i + 1]] <-

#dat3 <- adjmatall3 %>% filter(ms %in% c(0,2,4,6,8,10,12,43,45,47,49,51,53,55))
dat3 <- adjmatall3
dat4 <- dat3 %>% select(-ms) %>% spread(frame, val)
dat5 <- dat4
#p <- Sys.time()
for(i in 4:ncol(dat5)){
  print(i)
  for(j in 1:nrow(dat5)){
    print(j)
    if(dat4[j,i] != dat4[j,i-1]){
      if(dat4[j,i] > dat4[j,i-1]){
        dat5[j,i] <- 2
      } else { dat5[j,i] <- 3}
    }
  }
}
#Sys.time() - p

dat6 <- dat5 %>% gather(frame, val, 3:ncol(dat5)) %>% mutate(frame = parse_number(frame))

p <- ggplot() +
  geom_tile(data = dat6, aes(x = to, y = from, fill = as.factor(val), frame = frame), color = '#969696') +
  scale_fill_manual(values = c("white", "black", "red", "white")) +
  geom_tile(data = dat6 %>% filter(val == 3), aes(x = to, y = from, frame = frame), fill = NA, color = 'red', size = .25) +
  scale_x_discrete(position = "bottom") +
  labs(x = "alter node (j)", y = "ego node (i)") +
  ThemeNoNet +
  theme(legend.position = 'none', axis.text.x.bottom = element_blank(),
        axis.text.y.left = element_blank()) +
  coord_fixed() + 
  labs(title = "Microstep")
animation::ani.options(interval = 1)
gganimate::gganimate(p, 'img/adjmat_ani.gif', title_frame = T, ani.width = 800, ani.height = 800)
gganimate::gganimate(p, 'img/adjmat_ani.mp4', title_frame = T, ani.width = 800, ani.height = 800)
gganimate::gganimate(p, 'img/adjmat_ani.avi', title_frame = T, ani.width = 800, ani.height = 800)
