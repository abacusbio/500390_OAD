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
  col_name <- ifelse(grepl("liveweight", trait, ignore.case = T),
                     "NZ_EBV", "EBV")
  test <-fread(
    paste0(rawfileDir, dirs[i], "/",
           dir(paste0(rawfileDir, dirs[i], "/"), pattern = "AnimalResults$")), 
    sep = ",", select = c("AnimalDurableCode", col_name)) %>% # no missing data
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
# saveRDS(df_new, paste0(outputDir, 
                     # "production_survival_noYoungAnimal_ebv.RData"))

# figure out transition parity #
# df_new <- readRDS(paste0(outputDir, "production_survival_noYoungAnimal_ebv.RData"))
df_sub <- filter(df_new, herd_milk_type=="Transition" 
                 & DairyYear==transition_year) %>% # some cows do not have this year.
  # either died before, or too old?
  select(AnimalDurableCode, HerdDurableKey, AgeParity, 
         event_date, DairyYear, transition_year)

# sanity check
test <- group_by(df_sub, AnimalDurableCode, AgeParity) %>% 
  tally() # check if there's multiple AgeParity of the same transition year
which(duplicated(test$AnimalDurableCode)) # 0

df_sub <- select(df_sub, AnimalDurableCode, HerdDurableKey, AgeParity, 
                 transition_year) %>% 
  distinct()
df_sub$transition_parity <- paste0("s", df_sub$AgeParity-2, df_sub$AgeParity-1)
df_sub$transition_parity[df_sub$AgeParity<3] <- "too_early"
df_sub$transition_parity[df_sub$AgeParity>6] <- "too_late"

df_new <- left_join(df_new, select(df_sub, AnimalDurableCode, HerdDurableKey,
                                   transition_parity))

df_new$new_milk_type <- paste0(df_new$herd_milk_type, df_new$transition_parity)
df_new$new_milk_type <- gsub("NA", "", df_new$new_milk_type)
unique(df_new$new_milk_type) # sanity check
df_new$new_milk_type[df_new$new_milk_type=="Transition"] <- NA # DOESN'T HAVE DAIRYYEAR==TRANSITINO_YEAR

saveRDS(df_new, paste0(outputDir, 
                       "production_survival_noYoungAnimal_ebv.RData"))

df_new <- filter(df_new, !transition_parity %in% c("too_late", "too_early"))
saveRDS(df_new, paste0(outputDir, 
                       "production_survival_noYoungAnimal_ebv_beforeParity5.RData"))


