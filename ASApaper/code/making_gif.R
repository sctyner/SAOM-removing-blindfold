# make gif of a microstep - small friends
setwd("~/Desktop/Dissertation/SAOM-removing-blindfold/")
library(tidyverse)
library(geomnet)
library(RSiena)
library(network)
library(netvizinf)
library(gganimate)
library(ggforce)
library(tweenr)
friend.data.w1 <- as.matrix(read.table("data/s50_data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("data/s50_data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("data/s50_data/s50-network3.dat"))
drink <- as.matrix(read.table("data/s50_data/s50-alcohol.dat"))
fd2.w1 <- friend.data.w1[20:35,20:35]
fd2.w2 <- friend.data.w2[20:35,20:35]
fd2.w3 <- friend.data.w3[20:35,20:35]
friendshipData <- array(c(fd2.w1, fd2.w2,fd2.w3), dim = c(16, 16, 3))

set.seed(823746)
load("data/ansnullpaper.rda")
ansnullchains <- get_chain_info(ansnull)
ansnullchains %>%
  dplyr::filter(period == 1) %>%  #only look at chains from wave 1 to wave 2
  #dplyr::group_by(rep) %>%
  dplyr::select(rep, from = ego, to = alter) %>%
  dplyr::mutate(val = as.numeric(!from == to),
                from = paste0("V", parse_number(from)+1), # make the chains
                to = paste0("V", parse_number(to)+1)) -> ansnullchainsw1w2
colnames(fd2.w1) <- paste0("V", 1:16)
rownames(fd2.w1) <- paste0("V", 1:16)
wave1friends <- fortify(as.adjmat(fd2.w1))
ms1 <- netvizinf::listMicrosteps(dat = wave1friends,
                                 microsteps = dplyr::filter(ansnullchainsw1w2, rep == 1))
# all ms
microsteps_df <- getMicrostepsDF(ms1)
pte <- pretween_edges(microsteps = ms1)
ptv <- pretween_vertices(pte = pte, layoutparams = list(n = 16))
pall <- tween_microsteps(ptv, pte, microsteps_df)

set.seed(34569234)
froms <- pall %>% dplyr::select(id = from, microstep, x = from.x,
                                y = from.y, ms, .frame, size, color) %>% unique
tos <- pall %>% dplyr::select(id = to, microstep, x = to.x, y = to.y,
                              ms, .frame) %>% unique
tos <- dplyr::left_join(tos, froms)
nodedata <- rbind(froms, tos) %>% unique
nodedata <- nodedata %>% tidyr::replace_na(replace = list(size = 1,
                                                          color = "#333333"))
to_ani <- ggplot() +
  geom_curve(data = pall, aes(x = from.x,
                              y = from.y, xend = to.x, yend = to.y, frame = .frame,
                              colour = ecolor, size = esize), curvature = 0.1, arrow = arrow(angle = 20,
                                                                                             length = unit(0.1, "inches"))) +
  geom_point(data = nodedata,
             aes(x = x, y = y, colour = color, size = size, frame = .frame)) +
  scale_colour_identity() + scale_size_identity() + theme_void() +
  coord_fixed()
animation::ani.options(interval = 1/6)
gganimate(to_ani, "vignettes/full-node-link-ani.mp4", title_frame = F, ani.width = 800,
          ani.height = 800)










ms138 <- ms1[1:38]
ms389 <- ms1[38:39]
ms3971 <- ms1[39:71]

ms7182 <- ms1[71:length(ms1)]
microsteps_df7182 <- getMicrostepsDF(ms7182)
pte7182 <- pretween_edges(microsteps = ms7182)
ptv7182 <- pretween_vertices(pte = pte7182, layoutparams = list(n = 16))
p7182 <- tween_microsteps(ptv7182, pte7182, microsteps_df7182)


microsteps_df138 <- getMicrostepsDF(ms138)
microsteps_df389 <- getMicrostepsDF(ms389)
microsteps_df3971 <- getMicrostepsDF(ms3971)

pte138 <- pretween_edges(microsteps = ms138)
pte389 <- pretween_edges(microsteps = ms389)
pte3971 <- pretween_edges(microsteps = ms3971)

ptv138 <- pretween_vertices(pte = pte138, layoutparams = list(n = 16))
ptv389 <- pretween_vertices(pte = pte389, layoutparams = list(n = 16))
ptv3971 <- pretween_vertices(pte = pte3971, layoutparams = list(n = 16))

# step 1 is remove, step 9 is add.
# get tweened data for all microsteps
p138 <- tween_microsteps(ptv138, pte138, microsteps_df138)
p389 <- tween_microsteps(ptv389, pte389, microsteps_df389)
p3971 <- tween_microsteps(ptv3971, pte3971, microsteps_df3971)

#static_pall <- dplyr::filter(pall, .frame %in% c(15,20,22,24,26:28, 71:73, 75, 79, 81, 84))
#static_pall$type <- NA
#static_pall$type[which(static_pall$.frame %in% c(15,20,22,24,26:28))] <- "Remove Edge"
#static_pall$type[which(static_pall$.frame %in% c(71:73, 75, 79, 81, 84))] <- "Add Edge"

set.seed(34569234)
froms <- pall %>% dplyr::select(id = from, microstep, x = from.x,
                                       y = from.y, ms, .frame, size, color) %>% unique
tos <- pall %>% dplyr::select(id = to, microstep, x = to.x, y = to.y,
                                     ms, .frame) %>% unique
tos <- dplyr::left_join(tos, froms)
nodedata <- rbind(froms, tos) %>% unique
nodedata <- nodedata %>% tidyr::replace_na(replace = list(size = 1,
                                                          color = "#333333"))
to_ani <- ggplot() +
  geom_curve(data = pall, aes(x = from.x,
                                     y = from.y, xend = to.x, yend = to.y, frame = .frame,
                                     colour = ecolor, size = esize), curvature = 0.1, arrow = arrow(angle = 20,
                                                                                                    length = unit(0.1, "inches"))) +
  geom_point(data = nodedata,
             aes(x = x, y = y, colour = color, size = size, frame = .frame)) +
  scale_colour_identity() + scale_size_identity() + theme_void() +
  coord_fixed()
to_ani +
  facet_wrap(~.frame, nrow=2, labeller = 'label_both') +
  ThemeNet +
  theme(panel.background = element_rect(color = 'black'))

