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

remove_sens <- which(!(colnames(se111adj) %in% senateCovars$source))

se111adj <- se111adj[-remove_sens, -remove_sens]
se112adj <- se112adj[-remove_sens, -remove_sens]
se113adj <- se113adj[-remove_sens, -remove_sens]
se114adj <- se114adj[-remove_sens, -remove_sens]

# okay, that's the networks. now make the covariates
senateCovars <- seobama %>% 
  select(source,senate, n_au, party, sex) %>%
  unique %>% 
  spread(senate, n_au)


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

##### MAKE SIENA DATA #######

senateData <- array( c( se111adj, se112adj, se113adj, se114adj ),
                      dim = c( 156, 156, 4 ) )
senateData2 <- sienaDependent(senateData)
party <- coCovar(val = as.numeric(as.factor(senateCovars$party)))
# party 1 is dem 2 is ind 3 is rep
sex <- coCovar(val = as.numeric(as.factor(senateCovars$sex)))
# sex 1 is F 2 is M
nbills_au <- senateCovars[,4:7]
names(nbills_au) <- paste0("V",1:4) 
nbills_au <- as.matrix(nbills_au)
nbills_au[which(is.na(nbills_au))] <- 0
rownames(nbills_au) <- senateCovars$source
bills <- varCovar(nbills_au)
# rsiena objects are party, sex, bills, senateData2
senateSiena <- sienaDataCreate( senateData2, bills, sex, party )
senatemodeff <- getEffects(senateSiena)
library(netvizinf)
senateSigEffs <- find_sig_effs(dat = senateSiena)
write_csv(x = senateSigEffs, path = "data/congress/senateSignificantEffects.csv")

eff_basic <- RSiena::getEffects(senateSiena)
effectsDocumentation(eff_basic)
RSeffects <- (read_html("eff_basic.html") %>% html_nodes("table") %>% 
                html_table())[[1]]
# only want to test simple rate params and eval parameters
to_fit <- c(1:3, which(RSeffects$type == "eval"))
RSeffects <- RSeffects[to_fit,]
# remove "uspecified interaction" effects 
RSeffects <- RSeffects[-which(RSeffects$shortName == "unspInt"),]
# get rid of effects with short name "simRecipX" for inter1 == "sex" because R session aborted last time I ran it. 
RSeffects <- RSeffects[-which(RSeffects$shortName == "simRecipX" & RSeffects$inter1 == "sex"),]
# get rid of effects with short name from.w.ind (throws error in next line.)
# RSeffects <- RSeffects[-which(RSeffects$shortName == "from.w.ind"),]
RSeffects <- RSeffects %>% 
  nest(shortName:inter1) %>%
  mutate(eff_struct = map(data, .f = get_effects, eff_basic = eff_basic)) %>% 
  mutate(num_eff = map(eff_struct, .f = function(x) length(summary(x)[[2]])))
head(RSeffects)

min_num_eff <- min(unlist(RSeffects$num_eff))
idx2test <- which(RSeffects$num_eff > min_num_eff)
n <- length(idx2test)
N <- 1000
myalgorithm2 <- sienaAlgorithmCreate(projname = Sys.time(), 
                                     n3 = N, firstg = .02)
dat <- senateSiena
test_results <- data.frame(shortName = rep("", n), type = rep("", 
                                                              n), inter1 = rep("", n), estimate = rep(0, n), se = rep(0, 
                                                                                                                      n), Waldpval = rep(0, n), stringsAsFactors = FALSE)
for (i in 1:n) {
  print(i)
  ests_test <- siena07(myalgorithm2, data = dat, effects = RSeffects$eff_struct[[idx2test[i]]], 
                       batch = TRUE, verbose =TRUE, silent = FALSE)
  
  num_notrate <- length(ests_test$theta)
  test_vect <- rep(0, num_notrate)
  addlparm <- which(is.na(match(ests_test$effects$effectName, 
                                c("outdegree (density)", "reciprocity"))))
  test_vect[addlparm] <- 1
  if (is(try(RSienaTest::Wald.RSiena(test_vect, ests_test)), 
         "try-error")) {
    pval <- NA
  }
  else {
    wald_test <- RSienaTest::Wald.RSiena(test_vect, ests_test)
    pval <- wald_test$pvalue
  }
  test_results[i, ] <- c(ests_test$effects$shortName[num_notrate], 
                         ests_test$effects$type[num_notrate], ests_test$effects$interaction1[num_notrate], 
                         ests_test$theta[num_notrate], ifelse(is.null(ests_test$se[num_notrate]), NA, ests_test$se[num_notrate]), 
                         pval)
}
write_csv(test_results, "data/congress/senateModelSigEffects.csv")
