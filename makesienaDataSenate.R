# senate data to the rsiena data 
library(network)
library(RSiena)
library(tidyverse)
library(lubridate)
# variables - nyear, party, sex

seobama <- read_csv("data/congress/senateobamapres_gsw_25.csv")
legisls <- read_csv("data/congress/legislators_us.csv")
obamayears <- 2009:2016

legisls %>% mutate(yearStart = year(start), yearEnd = year(end)) -> legisls

(legisls %>% 
    filter(mandate == "se" & (yearStart %in% obamayears | yearEnd %in% obamayears)) %>% 
    mutate(name1 = ifelse(is.na(middle), paste(first,last), paste(first,middle,last)),
           name = ifelse(is.na(suffix), name1, paste(name1, suffix))) %>% 
    select(name))[[1]] %>% unique %>% sort -> allsenators
startingsemat <- matrix(0, nrow = length(allsenators), ncol = length(allsenators))
rownames(startingsemat) <- allsenators
colnames(startingsemat) <- allsenators

se111adj <- startingsemat

se111df <- seobama %>% filter(senate == 111 & !is.na(target))

make_se_adjmat <- function(se_no, adjmat=startingsemat, obamaSenates=seobama){
  senatedf <- obamaSenates %>% filter(senate == se_no & !is.na(target))
  N <- nrow(senatedf)
  for(i in 1:N){
    k <- which(rownames(adjmat) == senatedf$source[i])
    l <- which(colnames(adjmat) == senatedf$target[i])
    adjmat[senatedf$source[i], senatedf$target[i]] <- 1
  }
  return(adjmat)
}

se111adj <- make_se_adjmat(se_no = 111)
se112adj <- make_se_adjmat(se_no = 112)
se113adj <- make_se_adjmat(se_no = 113)
se114adj <- make_se_adjmat(se_no = 114)


# okay, that's the networks. now make the covariates
senateCovars <- seobama %>% 
  select(source,senate, nyears, party, sex) %>%
  unique %>% 
  spread(senate, nyears)

load("data/congress/senate111.rda")
load("data/congress/senate112.rda")
load("data/congress/senate113.rda")
load("data/congress/senate114.rda")

net_us_se111_hi <- net_us_se111
delete.edges(net_us_se111_hi, which(net_us_se111_hi %e% "gsw" < .25))
net_us_se112_hi <- net_us_se112
delete.edges(net_us_se112_hi,which(net_us_se112_hi %e% "gsw" < .25))
net_us_se113_hi <- net_us_se113
delete.edges(net_us_se113_hi,which(net_us_se113_hi %e% "gsw" < .25))
net_us_se114_hi <- net_us_se114
delete.edges(net_us_se114_hi,which(net_us_se114_hi %e% "gsw" < .25))



