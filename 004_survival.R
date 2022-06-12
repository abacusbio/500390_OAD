# Luna Zhang March 14 2022

library(data.table)
library(tidyverse)

source("utils.R")

fileDir <- "e:/500390_OAD/explore/" # "z:/Luna/500390_OAD/explore/"
rawfileDir <- "e:/OAD Dairy Data/"
outputDir <- "e:/500390_OAD/output/" # copy to "z:/Luna/500390_OAD/output/" later

# if(!exists(outputDir)) dir.create(outputDir)
stop()
dir(rawfileDir)

##--survival--##

# load
# df <- fread(paste0(rawfileDir, "FunctionalSurvivalExtract.csv"), sep = ",", 
            # na.strings = c("", ".", NA), 
            # colClasses = c(rep("character", 2), rep("integer", 2), "character"))

production <- readRDS(paste0(outputDir, 
                             grep("noOldAnimal", dir(outputDir), value= T)))

# remove herd with only 2 or less herd tests #
test <- select(production, HerdDurableKey, DairyYear, event_date) %>% 
  distinct() %>% 
  group_by(HerdDurableKey, DairyYear) %>% 
  tally() %>% 
  mutate(test_ge_3 = ifelse(n<3, F, T))

test_split <- split(test, f = test$HerdDurableKey)
bad_herds <- lapply(test_split, function(test_sub) {
  if(sum(test_sub$test_ge_3)==0) { # <3 tests all years
    return(test_sub)
  }
})
bad_herds <- do.call(rbind, bad_herds) # 4273 rows 7.6%

df_new <- filter(production, !HerdDurableKey %in% bad_herds$HerdDurableKey)
nrow(df_new) # 27255542
unique(df_new$AnimalDurableCode) %>% length() # 3160161
unique(df_new$HerdDurableKey) %>% length() # 7604
saveRDS(df_new, paste0(outputDir, "production_survival_noYoungAnimal.RData"))
rm(list=ls(pattern = "_slit|test|production"))

# create survival s12 -- s45 #
# AgeParity ranges from 2 to 7, so 2 means 1st parity and s12
# ... 5 means 4th parity and s45
# df_new <- readRDS(paste0(outputDir, "production_survival_noYoungAnimal.RData"))
df_new <- select(df_new, -year) %>% 
  mutate(birthdate = event_date-DaysFromBirthToParturition-
           DaysFromParturitionToHerdTest,
         # lt_june1 = birthdate < as.Date(paste0(year(birthdate), "0601"), 
                                        # "%Y%m%d"  ),
         birthyear = year(birthdate)
         # dairy_birth_year = DairyYear-birthyear,
         # event_birth_date = event_date-birthdate
         )

# sanity check
tmp <- filter(df_new, AgeParity!=dairy_birth_year)
all(tmp$dairy_birth_year > tmp$AgeParity) # F
all(tmp$lt_june1==T) # F
select(df_new, AgeParity, DaysFromBirthToParturition) %>% 
  group_by(AgeParity) %>% 
  summarise(min = min(DaysFromBirthToParturition), 
            max = max(DaysFromBirthToParturition)) # confusing overlaps
# AgeParity   min   max
# <int> <int> <int>
# 1         2   548  1054
# 2         3   789  1353
# 3         4  1123  1722
# 4         5  1480  2057
# 5         6  1773  2420
# 6         7  2178  2739

# create correct age parity 
# (calving date - birth date)/365.25 and round to the nearest integer

test <- select(df_new, AnimalDurableCode, DairyYear, AgeParity
               #, event_date
              #, birthdate
              ) %>% 
  distinct()

# Find the index of 1st occurrence of each herd_season
a <- data.frame(table(test$AnimalDurableCode))
a$var <- as.character(a$Var1)
all(a$var==unique(test$AnimalDurableCode)) # sanity check
a <- a[match(unique(test$AnimalDurableCode), a$var),]
all(a$var==unique(test$AnimalDurableCode)) # sanity check
a$idx <- cumsum(a$Freq)-a$Freq + 1
idx <- a$idx
idx <- c(idx, nrow(test)+1) # for the last set of animal IDs◘
a <- NULL

# sanity check if 2018-2021 is in all animal records
a <- lapply(seq(idx) %>% head(-1), function(i) {
  # return(2021 %in% test$DairyYear[idx[i]:(idx[i+1]-1)] &&
           # !2018 %in% test$DairyYear[idx[i]:(idx[i+1]-1)] )
  
  if(!2 %in% test$AgeParity[idx[i]:(idx[i+1]-1)]) {
    return(T)
  } else (
    return(F)
  )
})
a <- unlist(a)
table(a)
which(a==T) %>% head()
test[idx[632]:(idx[633]-1)]

source("utils.R")
t <- Sys.time()
survival <- mclapply(getMKLthreads()/2, l_env = c("idx", "test"),
  # lapply(
  seq(idx) %>% head(-1), function(i) {
  # the last row is the latest record
  out <- data.frame(AnimalDurableCode = test$AnimalDurableCode[idx[i]],
                    s12 = ifelse(test$AgeParity[(idx[i+1]-1)]>=3, T, F),
                    s23 = ifelse(test$AgeParity[(idx[i+1]-1)]>=4, T, F),
                    s34 = ifelse(test$AgeParity[(idx[i+1]-1)]>=5, T, F),
                    s45 = ifelse(test$AgeParity[(idx[i+1]-1)]>=6, T, F),
                    s56 = ifelse(test$AgeParity[(idx[i+1]-1)]>=7, T, F), # 9June2022
                    s67 = ifelse(test$AgeParity[(idx[i+1]-1)]>=8, T, F)) # not necessary
  
  # fill in missing value for AgeParity 2s
  if(2021 %in% test$DairyYear[idx[i+1]-1]) { # last year is 2021
    if(test$AgeParity[idx[i+1]-1]==2) {      # last year age==2
      out[1,2:7] <- NA                       # survival 12 and later should be unknown
      
    } else if(test$AgeParity[idx[i+1]-1]==3) { # last year age==3
      out[1, 3:7] <- NA                        # survival 23 and later unknown
      
    } else if(test$AgeParity[idx[i+1]-1]==4) {
      out[1,4:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==5) {
      out[1,5:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==6) {
      out[1,6:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==7) {
      out[1,7] <- NA
    } 
  } else if(2020 %in% test$DairyYear[idx[i+1]-1]) {
    if(test$AgeParity[idx[i+1]-1]==2) {
      out[1, 3:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==3) {
      out[1,4:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==4) {
      out[1,5:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==5) {
      out[1,6:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==6) {
      out[1,7] <- NA
    } 
  } else if(2019 %in% test$DairyYear[idx[i+1]-1]) {
    if(test$AgeParity[idx[i+1]-1]==2) {
      out[1,4:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==3) {
      out[1,5:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==4) {
      out[1,6:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==5) {
      out[1,7] <- NA
    } 
  } else if(2018 %in% test$DairyYear[idx[i+1]-1]) {
    if(test$AgeParity[idx[i+1]-1]==2) {
      out[1,5:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==3) {
      out[1,6:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==4) {
      out[1,7] <- NA
    }
  } else if(2017 %in% test$DairyYear[idx[i+1]-1]) {
    if(test$AgeParity[idx[i+1]-1]==2) {
      out[1,6:7] <- NA
      
    } else if(test$AgeParity[idx[i+1]-1]==3) {
      out[1,7] <- NA
    }
  } else if(2016 %in% test$DairyYear[idx[i+1]-1]) {
    if(test$AgeParity[idx[i+1]-1]==2) {
      out[1,7] <- NA
    }
  } 
  return(out)
})
print(Sys.time()-t) # lapply: 14 min, mclapply: 2.5 min
survival_original <- do.call(rbind, survival) # 3160161 animals

# sanity check
which(!test$AnimalDurableCode %in% survival_original$AnimalDurableCode)

# if missing s12, will be missing for all
survival <- filter(survival_original, !is.na(s12)) # 2393502, 93.017%
# if died, then later survival traits should be missing
survival$s23[which(is.na(survival$s12) | survival$s12==F)] <- NA
survival$s34[which(is.na(survival$s23) | survival$s23==F)] <- NA
survival$s45[which(is.na(survival$s34) | survival$s34==F)] <- NA
survival$s56[which(is.na(survival$s45) | survival$s45==F)] <- NA
survival$s67[which(is.na(survival$s56) | survival$s56==F)] <- NA

# don't need s67. all dead or unknown
survival <- survival[,-ncol(survival)]

# summary
summary_table <- sapply(survival[,-1], table, useNA = "always") %>% 
  data.frame()
summary_table <- rbind(summary_table, 
                        summary_table[2,]/apply(summary_table[1:2,], 2, sum))
rownames(summary_table)[4] <- "survival rate"
summary_table
#               s12          s23          s34          s45          s56
# FALSE.        5.941440e+05 4.376110e+05 3.280460e+05 2.776530e+05 2.340230e+05
# TRUE.         2.345358e+06 1.725693e+06 1.256377e+06 8.720460e+05 5.606390e+05
# NA.           0.000000e+00 7.761980e+05 1.355079e+06 1.789803e+06 2.144840e+06
# survival rate 7.978760e-01 7.977117e-01 7.929555e-01 7.584994e-01 7.055062e-01
write.csv(summary_table, paste0(outputDir, "survival_rate.csv"))

df_new <- left_join(df_new, survival) %>% 
  filter(!is.na(s12)) %>%
  select(-starts_with("^birth", ignore.case = F))
nrow(df_new) # 26769650
unique(df_new$AnimalDurableCode) %>% length() # 2939502
unique(df_new$HerdDurableKey) %>% length() # 7579
saveRDS(df_new, paste0(outputDir, "production_survival_noYoungAnimal.RData"))

animals <- select(df_new, AnimalDurableCode, HerdDurableKey) %>% 
  distinct()
write.csv(animals, paste0(outputDir, "animal_herd.csv"), 
          row.names = F, quote = F)

# # CODES BELOW ARE NOT USED #
# # clean
# con_gro <- strsplit(df$ContemporaryGroup, "-") %>% do.call(what = rbind)
# test <- as.data.frame(con_gro)
# 
# for(i in paste0("V", 1:5)) {
#   which(test[,i]==".") %>% length() %>% print()
#   test[,i] <- as.integer(test[,i])
# }
# test <- test[,-4]
# names(test) <- c("HerdDurableKey", "DairySeason", "Season", "AgeParity")
# 
# df <- cbind(df, test) 
# df <- arrange(df, HerdDurableKey, AnimalDurableCode, Age)
# 
# # summary
# nrow(df) # 28466435
# unique(df$HerdDurableKey) %>% length() # 27629
# unique(df$AnimalDurableCode) %>% length() # 14065407
# 
# # sanity check #
# n <- which(!unique(df$AnimalDurableCode) %in% animals$AnimalDurableCode) %>% length() # 11674797
# n/length(unique(df$AnimalDurableCode)) # 0.83
# n <- which(!animals$AnimalDurableCode %in% df$AnimalDurableCode) %>% length() # 1597389
# n/nrow(animals) # 0.4
# 
# test <- filter(production, !AnimalDurableCode %in% df$AnimalDurableCode) %>% 
#   select(AnimalDurableCode, herd_milk_type) %>% 
#   distinct() # 9522 herds, 1597389 animals don't have survival
# head(test)
# which(duplicated(test$AnimalDurableCode)) # no
# table(test$herd_milk_type)
# # OAD        TAD      Transition       
# # 54363    1492431      50595
# table(test$herd_milk_type)/nrow(test)
# # OAD        TAD        Transition        
# # 0.03403241 0.93429403 0.03167356
# 
# #  by = c("AnimalDurableCode", "AgeParity", "HerdDurableKey", "Season")
# test <- left_join(production, df) %>% 
#   select(AnimalDurableCode, HerdDurableKey, herd_milk_type, Observation) %>% 
#   distinct()
# group_by(test, herd_milk_type, Observation) %>% 
#   tally() %>% 
#   mutate(prop = n/sum(n))
# #   herd_milk_type Observation       n   prop
# #   <chr>                <int>   <int>  <dbl>
# # 1 OAD                      0    7267 0.0406
# # 2 OAD                      1   54107 0.302 
# # 3 OAD                     NA  117634 0.657 
# # 4 TAD                      0  188528 0.0363
# # 5 TAD                      1 1597346 0.308 
# # 6 TAD                     NA 3405987 0.656 
# # 7 Transition               0    8108 0.0428
# # 8 Transition               1   62530 0.330 
# # 9 Transition              NA  118726 0.627 
# # end sanity check #
# 
# df <- filter(df, AnimalDurableCode %in% animals$AnimalDurableCode)
# saveRDS(df, paste0(outputDir, "survival.RData"))
# 
# # df <- readRDS(paste0(outputDir, "survival.Rdata"))
# 
# ##--TOP body--##
# df <- fread(paste0(rawfileDir, "TOPBodyExtract.csv"), sep = ",", 
#             na.strings = c("", ".", NA))
# 
# # sanity check
# n <- which(!df$AnimalDurableCode %in% animals$AnimalDurableCode) %>% length() # 1249136
# n/length(unique(df$AnimalDurableCode)) # 0.825
# n <- which(!animals$AnimalDurableCode %in% df$AnimalDurableCode) %>% length() # 3723087
# n/nrow(animals) # 0.93X
# 
