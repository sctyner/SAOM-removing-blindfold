# fit senate data for the same M1, M2, M3 as the small friends

library(RSienaTest)
library(RSiena)
library(netvizinf)

load("data/congress/senateSiena.RDA")


senateM1 <- getEffects(senateSiena)
senateM2 <- includeEffects(senateM1, jumpXTransTrip, include = TRUE, type = "eval", interaction1 = "bills")
senateM3 <- includeEffects(senateM1, nbrDist2twice, include = TRUE, type = "eval", interaction1 = "")
senateM4 <- includeEffects(senateM1, isolateNet, include = TRUE, type = "eval", interaction1 = "")
senateM5 <- includeEffects(senateM1, sameXRecip, include = TRUE, type = "eval", interaction1 = "party")


senateM1ests <- get_effects_dist(dat = senateSiena, struct = senateM1, N = 100)
senateM2ests <- get_effects_dist(dat = senateSiena, struct = senateM2, N = 100)
senateM3ests <- get_effects_dist(dat = senateSiena, struct = senateM3, N = 100)
senateM4ests <- get_effects_dist(dat = senateSiena, struct = senateM4, N = 100)
senateM5ests <- get_effects_dist(dat = senateSiena, struct = senateM5, N = 100)
readr::write_csv(senateM1ests, "data/congress/senateM1ests.csv")
readr::write_csv(senateM2ests, "data/congress/senateM2ests.csv")
readr::write_csv(senateM3ests, "data/congress/senateM3ests.csv")
readr::write_csv(senateM4ests, "data/congress/senateM4ests.csv")
readr::write_csv(senateM5ests, "data/congress/senateM5ests.csv")
