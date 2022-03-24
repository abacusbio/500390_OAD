# Luna Zhang March 24 2022

library(data.table)
library(tidyverse)

source("utils.R")

fileDir <- "e:/500390_OAD/explore/" # "z:/Luna/500390_OAD/explore/"
rawfileDir <- "e:/OAD Dairy Data/2022021110_EBVs/"
outputDir <- "e:/500390_OAD/output/" # copy to "z:/Luna/500390_OAD/output/" later

# if(!exists(outputDir)) dir.create(outputDir)
stop()

# load ids
animals <- read.csv(paste0(outputDir, "animal_herd.csv"), stringsAsFactors = F)
production <- readRDS(paste0(outputDir, "production_survival_noYoungAnimal.RData"))

# load ebvs
dirs <- dir(rawfileDir, pattern = "-PBLUP") %>% 
  grep(pattern = "Interbull", value = T, invert = T)

ebvs <- lapply(seq(dirs), function(i) {
 
  trait <- strsplit(dirs[i], "-") %>% unlist() %>% head(2) %>% tail(1)
print(trait)
  test <-fread(
    paste0(rawfileDir, dirs[i], "/",
           dir(paste0(rawfileDir, dirs[i], "/"), pattern = "Animal")), 
    sep = ",", select = c("AnimalDurableCode", "EBV")) %>% # no missing data
    filter(AnimalDurableCode %in% animals$AnimalDurableCode)
  
  names(test)[2] <- paste0("EBV_", trait)
  return(test)
})

test <- reduce(ebvs, left_join, by = "AnimalDurableCode")
saveRDS(test, paste0(outputDir, "ebvs.RData"))
ebvs <- test
test <- NULL

df_new <- left_join(production, ebvs) %>% 
  select(-year)
rm(list = ls(pattern = "animals|production|ebvs"))
saveRDS(df_new, paste0(outputDir, 
                     "production_survival_noYoungAnimal_ebv.RData"))
