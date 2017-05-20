# create new "all ms" visualization
library(tidyverse)
library(netvizinf)
library(geomnet)
library(RSiena)
smallfriends <- read_csv("data/smallfriends4Geomnet.csv")
names(smallfriends)[1:2] <- c("from", "to")
ansnullchains <- get_chain_info(ansnull)
chainsms <- ansnullchains %>% 
  group_by(rep, period) %>% 
  mutate(microstep = row_number(), 
         prob = exp(logAlterProb),
         from = paste0("V", as.numeric(as.character(ego))+1),
         to = paste0("V", as.numeric(as.character(alter))+1)) %>%
  ungroup()
wave1friends <- fortify(as.adjmat(fd2.w1))
chainsms_wave2 <- filter(chainsms, period == 1)
chainsms_wave3 <- filter(chainsms, period == 2)
wave2 <- smallfriends %>% filter(Wave == 2) %>% na.omit %>% data.frame()
wave3 <- smallfriends %>% filter(Wave == 3) %>% na.omit %>% data.frame()
## make more generic for wave 3. 
create_microstep_summary <- function(chains, truedat){
  browser()
  R <- max(chains$rep)
  wave <- truedat 
  names(wave) <- c("from", "to", "drink", "Wave")
  all_reps <- list()
  for (i in 1:R){
    msall <- netvizinf::listMicrosteps(dat = wave,
                                       microsteps = filter(chainsms, rep == i))
    msall_df <- plyr::rbind.fill(lapply(X = seq_along(msall), FUN = function(i, x) {
      x[[i]]$ms <- i - 1
      return(x[[i]])
    }, msall))
    msall_df <- msall_df %>% dplyr::select(from, to, ms)
   # msall_df <- msall_df %>% add_row(from = paste0("V",wave2$from),
   #                                  to = paste0("V",wave2$to), 
   #                                  ms = max(msall_df$ms)+1)
    msall_df$rep <- i 
   all_reps[[i]] <- msall_df 
  }
  return(all_reps)
}
testing <- create_microstep_summary(chains = filter(chainsms_wave2, rep %in% 1:5), truedat = wave2)

testingdf<-plyr::rbind.fill(testing)

testingdf %>% 
  group_by(from, to, ms) %>% 
  summarize(numappears = n()) %>% 
  arrange(ms, from, to) 

# okay, it works. do it for all
allms_allreps <- create_microstep_summary(chains = chainsms_wave2, truedat = wave2)
allms_allrepsW3 <- create_microstep_summary(chains = chainsms_wave3, truedat = wave3)

allms_allreps_df <- plyr::rbind.fill(allms_allreps)

summary_allreps <- allms_allreps_df %>%
  group_by(from, to, ms) %>% 
  mutate(numappears = n()) %>% 
  arrange(ms, from, to) %>% 
  filter(ms > 0) 

summary_allreps2 <- summary_allreps %>% ungroup() %>% 
  group_by(rep) %>% 
  mutate(scaled_time = ms / max(ms))

summary_allreps2 %>% ungroup() %>% 
  group_by(rep) %>% summarise(numms = max(ms)) %>% 
  ggplot() + 
  geom_histogram(aes(x = numms), binwidth = 5) 


summary_allreps3 <- allms_allreps_df %>%
  group_by(from, to) %>% 
  summarise(numappears.tot = n()) %>% 
  arrange(numappears.tot) %>% ungroup() %>% 
  mutate(eid = row_number())
   
summary_allreps4 <- left_join(summary_allreps2, summary_allreps3, by = c("from" = "from", "to" = "to"))

summary_allreps5 <- summary_allreps4 %>% 
  group_by(from, to, eid, scaled_time) %>% 
  summarise(appears_time = sum(numappears)) %>% ungroup %>% 
  mutate(logat = log(appears_time),
         height = logat/max(logat) / 2, 
         y = eid - height, yend = eid + height)
# first attempt
ggplot(data = summary_allreps4 %>% filter(eid %in% 1:20)) + 
  geom_line(aes(x = scaled_time, y = eid, group = eid, size = numappears))
# try again
ggplot(data = summary_allreps5) + 
  geom_segment(aes(x = scaled_time, xend = scaled_time, y = y, yend = yend))
# another way 
ggplot(data = summary_allreps5) +
  geom_point(aes(x = scaled_time, y = appears_time))
# density
ggplot(data = summary_allreps5) +
  geom_density(aes(x = appears_time))
ggplot(data = summary_allreps5) +
  geom_density(aes(x = scaled_time))
ggplot(data = summary_allreps5) +
  geom_density2d(aes(x = scaled_time, y = appears_time))




max(summary_allreps$ms)

msall1_df2 <- left_join(msall1_df, edges, by = c("from" = "from", "to" = "to"))

p1 <- ggplot(data = msall1_df2) + 
  geom_tile(aes(x = ms, y = -eid, fill = ms)) + 
  geom_vline(xintercept = c(0.5,81.5)) + 
  scale_fill_continuous(low = "red", high = "blue") + 
  theme(legend.position = 'none', axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title = element_blank()) +
  labs(x = "Microstep")