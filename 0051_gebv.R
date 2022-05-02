# Luna Zhang 26 April 2022

library(data.table)
library(tidyverse)

source("utils.R")

fileDir <- "e:/500390_OAD/explore/" # "z:/Luna/500390_OAD/explore/"
rawfileDir <- "e:/OAD Dairy Data/"
outputDir <- "e:/500390_OAD/output/" # copy to "z:/Luna/500390_OAD/output/" later

# if(!exists(outputDir)) dir.create(outputDir)
stop()

# load ids
production <- readRDS(
  paste0(outputDir, "production_survival_noYoungAnimal_ebv_beforeParity5.RData"))

gebv <- fread(paste0(rawfileDir, "20220329_OAD_gBV_Extract.txt"), 
              sep = ',', header = T)

# sanity check
# summary(gebv) # no missing values

# make animal durable code
gebv$AnimalDurableCode <- "a"
idx <- which(gebv$anim_dkey <= 9999999)
gebv$AnimalDurableCode[idx] <- paste0("DK00000", gebv$anim_dkey[idx])
idx <- which(gebv$anim_dkey > 9999999 & gebv$anim_dkey <= 99999999)
gebv$AnimalDurableCode[idx] <- paste0("DK0000", gebv$anim_dkey[idx])

# sanity check
which(gebv$AnimalDurableCode=="a")
idx <- which(is.na(match(production$AnimalDurableCode, gebv$AnimalDurableCode)))
production[idx,]
production$AnimalDurableCode[idx] %>% unique() # 9 animals, all TAD

# remove animals do not have GEBVs
production <- production[-idx,] # 26046302 rows, 2859648 animals, 7576 herds

# change name
test <- rename(gebv, 
               GBV_TotalFat = fat_bv,
               GBV_TotalProtein = prot_bv, 
               GBV_TotalVolume = vol_bv, 
               GBV_Liveweight = lwgt_bv,
               GBV_SomaticCellScore = scc_bv, 
               GBV_Fertility_CR42 = fert_bv,
               GBV_BodyConditionScore = bcs_bv, 
               GBV_FunctionalSurvival = functional_surv_bv,
               GBV_TOP_AdaptabilityToMilking = adaptability_bv, 
               GBV_TOP_ShedTemperament = shed_temperament_bv,
               GBV_TOP_MilkingSpeed = milking_speed_bv, 
               GBV_TOP_OverallOpinion = overall_opinion_bv,
               GBV_TOP_Stature = stature_bv, 
               GBV_TOP_Capacity = capacity_bv,
               GBV_TOP_RumpAngle = rump_angle_bv, 
               GBV_TOP_RumpWidth = rump_width_bv,
               GBV_TOP_Legs = legs_bv, 
               GBV_TOP_UdderSupport = udder_support_bv,
               GBV_TOP_ForeUdder = front_udder_bv, 
               GBV_TOP_RearUdder = rear_udder_bv,
               GBV_TOP_FrontTeat = front_teat_bv, 
               GBV_TOP_RearTeat = rear_teat_bv,
               GBV_TOP_TeatLength = teat_length_bv, 
               GBV_TOP_UdderOverall = udder_overall_bv,
               GBV_TOP_DairyConformation = dairy_conformation_bv
               ) %>% 
  select(matches("AnimalDurableCode|GBV"))

production <- left_join(production, test, by = "AnimalDurableCode")
# saveRDS(production, paste0(outputDir, 
#                        "production_survival_noYoungAnimal_gebv_beforeParity5.RData"))
rm(list = ls(pattern = "test|gebv"))

# load breed percentage information
animal <- fread(
  paste0(rawfileDir, "2022021110_EBVs/2022021110-Common-AnimalInfo"), sep = ",",
  header = T)
animal <- filter(animal, AnimalDurableCode %in% production$AnimalDurableCode) %>% 
  select(AnimalDurableCode, PctHolstein)
production <- left_join(production, animal)
saveRDS(production, paste0(outputDir, 
                           "production_survival_noYoungAnimal_gebv_beforeParity5.RData"))
rm(list = ls(pattern = "animal"))
