t <- Sys.time()

ani_now <- test$AnimalDurableCode[1]
year_now <- test$year[1]
anis_good <- rep(NA, nrow(test))
k <- 0
good <- TRUE
for (i in 2 : nrow(test)) {
  if (test$AnimalDurableCode[i] == ani_now) {
    if (test$year[i] == year_now + 1) {
      year_now <- year_now + 1
    } else {
      good <- FALSE
    }
  } else {
    if (good) {
      k <- k + 1
      anis_good[k] <- ani_now
      ani_now <- test$AnimalDurableCode[i]
      year_now <- test$year[i]
    } else {
      ani_now <- test$AnimalDurableCode[i]
      year_now <- test$year[i]
      good <- TRUE
    }
  }
}

if (good) {
  k <- k + 1
  anis_good[k] <- ani_now
}
anis_good <- anis_good[1 : k]

print(Sys.time()-t) # 1.59 min
