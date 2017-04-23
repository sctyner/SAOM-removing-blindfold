# for carson
library(tidyverse)
library(RSiena)
sfmsall <- read_csv("data/smallfriendsmicrosteps.csv")

sfmsall %>% filter(ms == 1) -> one_microstep_nets

sfmsall %>% filter(ms == 0) %>% select(-rep) %>% unique -> initial

load("data/ansnullpaper.rda")
ansnullchains <- get_chain_info(ansnull)

ansnullchains %>% 
  filter(period == 1) %>% 
  group_by(rep) %>%
  mutate(ego = paste0("V", parse_number(ego)+1), # make the chains
         alter = paste0("V", parse_number(alter)+1), 
         ms = row_number(), 
         alterprob = exp(logAlterProb)) %>% 
  filter(ms == 1) -> one_microstep

#join microstep data (full net) with just the one microstep data..

left_join(one_microstep_nets, 
          one_microstep, 
          by = c("from" = "ego", "to" = "alter", "ms" = "ms", "rep" = "rep")) %>% 
  data.frame %>% head(30)

friendNet <- list(wave1 = s501[20:35, 20:35],
                  wave2 = s502[20:35, 20:35],
                  wave3 = s503[20:35, 20:35])
friendAlc <- s50a[20:35,]
colnames(friendAlc) <- paste0("wave",1:3) 
rownames(friendAlc) <- paste0("V", 1:16)
# compute the obj. fn. values of the models
M1_obj_fn <- function(adjmat, egoname, parms){
  idx <- which(rownames(adjmat) == egoname)
  row <- adjmat[idx,]
  col <- adjmat[,idx]
  S_outdeg <- sum(row)
  S_recip <- sum(row*col)
  Ss <- c(S_outdeg, S_recip)
  val <- sum(parms * Ss)
  return(val)
}
M1_obj_fn(adjmat, egoname, pM1)
M2_obj_fn <- function(adjmat, egoname, parms, covar){
  N <- nrow(adjmat)
  idx <- which(rownames(adjmat) == egoname)
  row <- adjmat[idx,]
  col <- adjmat[,idx]
  S_outdeg <- sum(row)
  S_recip <- sum(row*col)
  # compute JTTs 
  vec <- c()  
  for (j in 1:N){
    for(h in setdiff(1:N, j)){
      ind <- as.numeric(covar[idx] == covar[h] && covar[h] != covar[j])
      val <- adjmat[idx,j] * adjmat[idx,h] * adjmat[h,j] * ind
      vec <- c(vec, val)
    }
  }
  S_jtt <- sum(vec)
  Ss <- c(S_outdeg, S_recip, S_jtt)
  val2 <- sum(parms * Ss)
  return(val2)
}
M2_obj_fn(adjmat, egoname, pM2, covar)
M3_obj_fn <- function(adjmat, egoname, parms){
  idx <- which(rownames(adjmat) == egoname)
  row <- adjmat[idx,]
  col <- adjmat[,idx]
  S_outdeg <- sum(row)
  S_recip <- sum(row*col)
  twopaths <- adjmat %*% adjmat
  diag(twopaths) <- 0
  path2 <- sum(twopaths[(twopaths >= 2) & (adjmat == 0)])
  S_dad <- path2
  Ss <- c(S_outdeg, S_recip, S_dad)
  val <- sum(parms * Ss)
  return(val)
}
M3_obj_fn(adjmat, egoname, pM3)

computeDiffs <- function(am, ego, alter, covar, model, parms){
  ridx <- which(rownames(am) == ego)
  cidx <- which(colnames(am) == alter)
  if (ego == alter){
    am2 <- am
  } else{
    am2 <- am
    am2[ridx, cidx] <- 1-am2[ridx,cidx]
  }
  # change tie
  if (model == "M1"){
    return(M1_obj_fn(adjmat = am2, egoname = ego, parms = parms))
  } else if (model == "M2"){
    return(M2_obj_fn(adjmat = am2, egoname = ego, parms = parms, covar = covar))
  } else return(M3_obj_fn(adjmat = am2, egoname = ego, parms = parms))
}

am <- s501[20:35, 20:35]
rownames(am) <- paste0("V", 1:16)
colnames(am) <- paste0("V", 1:16)

# need a data set with the correct model names, parameters, 
# and all the ego, alter node combos.

allEdges <- expand.grid(ego = paste0("V", 1:16), alter = paste0("V", 1:16),
                        model = paste0("M", 1:3), stringsAsFactors = F)
allEdges <- add_column(allEdges, eid = 1:nrow(allEdges), .before = 1)
simu2 <- read.csv("data/simulation-1000-M1-M2-M3.csv")
means <- simu2 %>% group_by(Model, parameter) %>%
  summarize(means = mean(estimate)) %>% 
  spread(parameter, means)

pM1 <- filter(means, Model == "M1") %>% ungroup %>% select(beta1:beta2) %>% as.numeric
pM2 <- filter(means, Model == "M2") %>% ungroup %>% select(beta1:beta3) %>% as.numeric
pM3 <- filter(means, Model == "M3") %>% ungroup %>% select(c(beta1:beta2,beta4)) %>% as.numeric

allEdges2 <- left_join(allEdges, means, by = c("model" = "Model"))
allEdgesNest <- allEdges2 %>% select(-(alpha1:alpha2)) %>% nest(beta1:beta4) %>% nest(-eid)
allEdges3 <- allEdgesNest %>% 
  mutate(objFnVal = map(.x = data, .f = function(x){
    computeDiffs(am = am, ego = x$ego, alter = x$alter, covar = friendAlc, 
                 model = x$model, parms = x$data[[1]] %>% as.numeric %>% na.omit %>% as.vector)
  })
  )

allEdges4 <- allEdges3 %>% unnest %>% select(-data)

write_csv(allEdges4, "allEdgesAllobjfns.csv")

dat <- read_csv("smallfriends4Geomnet.csv")
head(dat)
dat %>% filter(Wave == 1) %>% 
  mutate(ego = ifelse(is.na(X1), NA, paste0("V", X1)),
         alter = ifelse(is.na(X2), NA, paste0("V", X2))) %>% 
  select(ego, alter, drink) -> dat

##### plots start here. 
# network plot
ggplot(dat) + 
  geom_net(aes(from_id = ego, to_id = alter, 
               colour = as.factor(drink)), 
           ecolour = 'grey40', directed = T, labelon = T,
           hjust = .5, vjust = .5, labelcolour= 'black',
           fontsize = 3, size = 5) +
  scale_colour_brewer(palette = "YlOrRd", 
                      name = "Drinking\nBehavior") + 
  theme_net()
# bar chart
allEdges4 %>% filter(ego == "V2", alter %in% c("V1","V2")) %>% 
  ggplot() + geom_bar(aes(x = model, weight = objFnVal, 
                          fill = alter), position = 'dodge') + 
  labs(x = "Model", y = "Objective Function Value for Node V2")


# from carson example
# These examples demonstrate ways to display binned/aggregated selections
library(crosstalk)
library(plotly)

d <- SharedData$new(mtcars)
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("black")) %>%
  layout(dragmode = "select")

# add_histogram() does both continuous _and_ discrete binning in the browser,
# allowing us to perform aggregations on the fly, without 
p <- subplot(
  plot_ly(d, x = ~factor(cyl)) %>% add_histogram(color = I("black")),
  scatterplot
) 

# Crosstalk selections are actually additional traces, and, by default, 
# plotly.js will try to dodge bars placed under the same category
layout(p, barmode = "overlay") %>%
  highlight(selected = attrs_selected(showlegend = FALSE))

# same idea, but now with a boxplot
p <- plot_ly(d, y = ~disp, color = I("black")) %>% add_boxplot(name = "overall")
subplot(p, scatterplot, shareY = TRUE) %>% 
  layout(dragmode = "select") %>%
  highlight(selected = attrs_selected(name = "selection"))


library(plotly)
library(crosstalk)

tx <- SharedData$new(txhousing, ~city)
p1 <- ggplot(tx, aes(date, median, group = city)) + geom_line()
p2 <- plot_ly(tx, x = ~median, color = I("black")) %>% 
  add_histogram(histnorm = "probability density")

subplot(p1, p2) %>% 
  layout(barmode = "overlay") %>%
  highlight(
    "plotly_click", dynamic = TRUE, persistent = TRUE, 
    selected = attrs_selected(opacity = 0.3)
  )


#### 
# my version of above.
allEdges4 <- read_csv("allEdgesAllobjfns.csv")
head(allEdges4)

p1 <- allEdges4 %>% filter(ego == "V2", alter %in% c("V1","V2")) %>% 
  ggplot() + geom_point(aes(x = model, y = objFnVal, 
                          color = alter, shape = alter)) + 
  labs(x = "Model", y = "Objective Function Value for Node V2")
ggplotly(p1)
# 
w1mat <-as.network(s501[20:35, 20:35])
low1 <- gplot.layout.kamadakawai(w1mat, layout.par = NULL)
low1 <- as.data.frame(low1)
names(low1) <- c("x", "y")
low1$id <- paste0("V", 1:16)
str(low1)

join1 <- left_join(low1, allEdges4, by = c("id" = "ego"))
names(join1)[1:3] <- c("ego.x", "ego.y", "ego")
join2 <- left_join(join1, low1, by = c("alter" = "id"))
names(join2)[8:9] <- c("alter.x", "alter.y")
head(join2)
alledges5 <- join2[, c(4,3,6,1,2,8,9,7,5)]

am1 <- data.frame(s501[20:35, 20:35]) 
names(am1) <- paste0("V", 1:16)
am1 %>% 
  mutate(ego = paste0("V", 1:16)) %>% 
  gather(alter, is.edge, -ego) -> am2

alledges6 <- left_join(alledges5, am2, by = c("ego" = "ego", "alter" = "alter"))
head(alledges6)

sd1 <- SharedData$new(alledges6, ~eid)
p1 <- ggplot(data = sd1) + 
  geom_segment(aes(x = ego.x, y = ego.y, xend = alter.x, yend = alter.y, alpha = I(is.edge), group = eid)) + 
  geom_point(aes(x = ego.x, y = ego.y))
p1

p2 <- ggplot(data = sd1) + 
  geom_bar(aes(x = model, weight = objFnVal, fill = alter)) 
ggplotly(p2)
p2 <- plot_ly(sd1, x = ~model, y = ~objFnVal) %>% 
  add_bars()

subplot(p1, ggplotly(p2))

plot_ly(tx, x = ~median, color = I("black")) %>% 
  add_histogram(histnorm = "probability density")
