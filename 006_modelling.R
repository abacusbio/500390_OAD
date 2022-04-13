# lzhang 3march2020

library(tidyverse)
library(ggpubr)
source("utils.R")

fileDir <- "e:/500390_OAD/output/" # "z:/Luna/500390_OAD/explore/"
# rawfileDir <- "e:/OAD Dairy Data/2022021110_EBVs/"
outputDir <- "e:/500390_OAD/output/modeling/" # copy to "z:/Luna/500390_OAD/output/" later

# if(!exists(outputDir)) dir.create(outputDir)

memory.limit()/1024
memory.limit(size = 191/2*1024^2)
stop()

# load
# # animals <- read.csv(paste0(outputDir, "animal_herd.csv"), stringsAsFactors = F)
# full_df <- readRDS(paste0(fileDir, 
#                           "production_survival_noYoungAnimal_ebv.RData"))
# 
# # new variable
# full_df <- mutate(full_df, 
#                   new_milk_type = paste0(herd_milk_type, "_", transition_parity))
# full_df$new_milk_type[grepl("too", full_df$new_milk_type)] <- NA
# full_df$new_milk_type <- gsub("_NA", "", full_df$new_milk_type)
# full_df$new_milk_type[full_df$new_milk_type=="Transition"] <- NA
# 
# full_df <- filter(full_df, !is.na(new_milk_type))
# nrow(full_df) # 26046340
# unique(full_df$AnimalDurableCode) %>% length() # 2859657
# unique(full_df$HerdDurableKey) %>% length() # 7576
# 
# saveRDS(full_df, paste0(fileDir, "production_survival_noYoungAnimal_ebv_beforeParity5.RData"))
full_df <- readRDS(paste0(fileDir, "production_survival_noYoungAnimal_ebv_beforeParity5.RData"))

# sanity check
filter(full_df, transition_parity=="s12") %>% 
  select(AnimalDurableCode, HerdDurableKey, AgeParity, DairyYear,
         transition_year, transition_parity) %>% head()


## Down sample due to too many herds that uses up all memories

# Down sample TAD herd size to 296. See `006_sumstat.rmd`

idx_leftover <- which(full_df$new_milk_type!="TAD")
idx_tad <- which(full_df$new_milk_type=="TAD")
herd_tad <- full_df$HerdDurableKey[idx_tad] %>% unique()
herd_sample <- sample(herd_tad, 
                      full_df$HerdDurableKey[which(full_df$new_milk_type=="OAD")] %>% 
                        unique() %>% length(), replace = F)
idx <- which(full_df$HerdDurableKey %in% herd_sample)

# sanity check
match(idx, idx_leftover) %>% unique() # NA

set.seed(123)
df_downsample <- full_df[sort(c(idx_leftover, idx)),]

cat("Downsampled dataset has", nrow(df_downsample), "events,",
    unique(df_downsample$AnimalDurableCode) %>% length(), "cows, and",
    unique(df_downsample$HerdDurableKey) %>% length(), "herds.")

df_downsample <- mutate(df_downsample, 
                        cg = paste0(HerdDurableKey, "_", DairyYear))

# The model choice is logistic model, since survival is a binary trait.i.e.
# log odds: $ln(odds) = ln(\frac{p}{1-p}) = a*x_1 + b*x_2 + … + z*x_n$ 
  
  ## Survival s12
  
  # Because each cow has multiple testing record, but their survival doesn't 
# necessarily need call testing record, the data is reduced to one data point per
# cow.

# For s12-s45, filter down to AgeParity==2, 3, 4, or 5. Because at AgeParity==3,
# all existing cows' s12 are 1s.

df_model <- select(df_downsample, AnimalDurableCode, HerdDurableKey, 
                   DairyYear, AgeParity, cg, 
                   new_milk_type, s12, starts_with("EBV_")) %>% 
  filter(AgeParity==2) %>% 
  distinct() %>% 
  mutate(HerdDurableKey = as.factor(HerdDurableKey), 
         DairyYear = as.factor(DairyYear))

which(duplicated(df_model$AnimalDurableCode)) # sanity check

### First, use all the cows to check if milking (regime type) has an effect

# The first factor is CG.
# can't fit Herdurable Key. use more than 500 GB RAM.
# So, fit herd and dairy year separately.
# DairyYear 2021 doesn't have survival data so it is excluded from the model.

# Below is to confirm that Herd and DairyYear has an effect on s12.
# Some herds do not have an effect but all DairyYears have effects.

glm(s12~DairyYear + 0, binomial(link = "logit"), df_model) %>% 
  summary()

glm(s12~HerdDurableKey + 0, binomial(link = "logit"), df_model) %>% 
  summary()

### add milking type to the model

# Fitting CG as combination of herd and year takes too long. Therefore has to
# fit them separately.
# 
# To show milking type effects fully, fit it before herd effect in the model.

glm_s12_on_year_milktype <- glm(s12~DairyYear+new_milk_type+HerdDurableKey+ 0, 
                                # add 2007 into the equation by +0, to make the intercept == OAD
                                binomial(link = "logit"), df_model)
s <- summary(glm_s12_on_year_milktype)
print(s$call)
# cat("deviance residuals:\n"); summary(s$deviance.resid)
coefs <- data.frame(s$coefficients[grepl("new_milk_type", rownames(s$coefficients)),])
coefs$sign <- ifelse(coefs$Pr...z..<0.001, "***", 
                     ifelse(coefs$Pr...z..<0.01, "**",
                            ifelse(coefs$Pr...z..<0.05, "*",
                                   ifelse(coefs$Pr...z..<0.1, ".",""))))
print(coefs)
cat("\nAIC:",s$aic)
cat("\n\n##--ANOVA with Chi-sq test--##\n\n")
anovas <- anova(glm_s12_on_year_milktype, test = "Chisq")
print(anovas)

coefs$survival <- "s12"; anovas$survival <- "s12"
write.csv(coefs, paste0(outputDir, "coef_s12_milk_type.csv"), 
          row.names = F, quote = F)
write.csv(coefs, paste0(outputDir, "anova_s12_milk_type.csv"), 
          row.names = T, quote = F)

# The interception is OAD and HerdDurableKey==145. 
# This is to say when all other effects are at the same level, TAD decreases the 
# odd ratio of S12 1:0 by around 2, meaning they increase the change of mortality.
# Other milking types (transitioning) increases the odd ratio but their effects 
# aren't significant. 

## S23

df_model <- select(full_df, AnimalDurableCode, HerdDurableKey, 
                   DairyYear, AgeParity,
                   new_milk_type, s23, starts_with("EBV_")) %>% 
  filter(AgeParity==3) %>% 
  distinct() %>% 
  # mutate(cg = paste0(HerdDurableKey, "_", DairyYear))
  mutate(HerdDurableKey = as.factor(HerdDurableKey), 
         DairyYear = as.factor(DairyYear))

# which(duplicated(df_model$AnimalDurableCode)) # sanity check

glm_s23_on_year_milktype <- glm(s23~DairyYear+new_milk_type+HerdDurableKey+ 0, 
# add 2007 into the equation by +0, to make the intercept == OAD
                                binomial(link = "logit"), df_model)
s <- summary(glm_s23_on_year_milktype)
print(s$call)
# cat("deviance residuals:\n"); summary(s$deviance.resid)
coefs <- data.frame(s$coefficients[grepl("new_milk_type", rownames(s$coefficients)),])
coefs$sign <- ifelse(coefs$Pr...z..<0.001, "***", 
                     ifelse(coefs$Pr...z..<0.01, "**",
                            ifelse(coefs$Pr...z..<0.05, "*",
                                   ifelse(coefs$Pr...z..<0.1, ".",""))))
print(coefs)
cat("\nAIC:",s$aic)
cat("\n##--ANOVA with Chi-sq test--##\n\n")
anovas <- anova(glm_s23_on_year_milktype, test = "Chisq")
print(anovas)

coefs$survival <- "s23"; anova$survival <- "s23"
write.csv(coefs, paste0(outputDir, "coef_s23_milk_type.csv"), 
          row.names = F, quote = F)
write.csv(coefs, paste0(outputDir, "anova_s23_milk_type.csv"), 
          row.names = T, quote = F)

## S34
df_model <- select(full_df, AnimalDurableCode, HerdDurableKey, 
                   DairyYear, AgeParity,
                   new_milk_type, s34, starts_with("EBV_")) %>% 
  filter(AgeParity==4) %>% 
  distinct() %>% 
  # mutate(cg = paste0(HerdDurableKey, "_", DairyYear))
  mutate(HerdDurableKey = as.factor(HerdDurableKey), 
         DairyYear = as.factor(DairyYear))

# which(duplicated(df_model$AnimalDurableCode)) # sanity check

glm_s34_on_year_milktype <- glm(s34~DairyYear+new_milk_type+HerdDurableKey+ 0, 
                                # add 2007 into the equation by +0, to make the intercept == OAD
                                binomial(link = "logit"), df_model)
s <- summary(glm_s34_on_year_milktype)
print(s$call)
# cat("deviance residuals:\n"); summary(s$deviance.resid)
coefs <- data.frame(s$coefficients[grepl("new_milk_type", rownames(s$coefficients)),])
coefs$sign <- ifelse(coefs$Pr...z..<0.001, "***", 
                     ifelse(coefs$Pr...z..<0.01, "**",
                            ifelse(coefs$Pr...z..<0.05, "*",
                                   ifelse(coefs$Pr...z..<0.1, ".",""))))
print(coefs)
cat("\nAIC:",s$aic)
cat("\n##--ANOVA with Chi-sq test--##\n\n")
anovas <- anova(glm_s34_on_year_milktype, test = "Chisq")
print(anovas)

coefs$survival <- "s34"; anovas$survival <- "s34"
write.csv(coefs, paste0(outputDir, "coef_s34_milk_type.csv"), 
          row.names = F, quote = F)
write.csv(coefs, paste0(outputDir, "anova_s34_milk_type.csv"), 
          row.names = T, quote = F)

## S45

# Transitioning herd at s12, s23 and s34 decreases the log odd ratio of the
# survival. TAD decreases and transitioning at s45 increases the log odd ratio but
# their effects are insignificant.
df_model <- select(full_df, AnimalDurableCode, HerdDurableKey, 
                   DairyYear, AgeParity,
                   new_milk_type, s45, starts_with("EBV_")) %>% 
  filter(AgeParity==5) %>% 
  distinct() %>% 
  # mutate(cg = paste0(HerdDurableKey, "_", DairyYear))
  mutate(HerdDurableKey = as.factor(HerdDurableKey), 
         DairyYear = as.factor(DairyYear))

# which(duplicated(df_model$AnimalDurableCode)) # sanity check

glm_s45_on_year_milktype <- glm(s45~#DairyYear+
                                  cg+new_milk_type + 0, 
                                # add 2007 into the equation by +0, to make the intercept == OAD
                                binomial(link = "logit"), df_model)
s <- summary(glm_s45_on_year_milktype)
print(s$call)
# cat("deviance residuals:\n"); summary(s$deviance.resid)
coefs <- data.frame(s$coefficients[grepl("new_milk_type", rownames(s$coefficients)),])
coefs$sign <- ifelse(coefs$Pr...z..<0.001, "***", 
                     ifelse(coefs$Pr...z..<0.01, "**",
                            ifelse(coefs$Pr...z..<0.05, "*",
                                   ifelse(coefs$Pr...z..<0.1, ".",""))))
print(coefs)
cat("\nAIC:",s$aic)
cat("\n##--ANOVA with Chi-sq test--##\n\n")
anovas <- anova(glm_s45_on_year_milktype, test = "Chisq")
print(anovas)

coefs$survival <- "s45"; anovas$survival <- "s45"
write.csv(coefs, paste0(outputDir, "coef_s45_milk_type.csv"), 
          row.names = F, quote = F)
write.csv(coefs, paste0(outputDir, "anova_s45_milk_type.csv"), 
          row.names = T, quote = F)

```

## Look at TOP EBV within each milk type and their effects on 3 types of survivals

# FUNCTION
stack_within_survival <- function(df_model, survival = "s12", 
                                  milk_type = "OAD") {
  
  t <- Sys.time()
  # out <- lapply(unique(df_model$new_milk_type), function(milk_type) {
  
  by_milk_type <- lapply(
    grep("EBV", names(df_model), value = T), function(trait) {
      cat(milk_type, trait);print(Sys.time()-t)
      fml <- as.formula(paste0(survival, "~DairyYear+HerdDurableKey+",trait))
      glm_s12_model <- glm(fml, binomial(link = "logit"), 
                           filter(df_model, new_milk_type == milk_type))
      
      s <- summary(glm_s12_model)
      coefs <- data.frame(s$coefficients[grepl("EBV", rownames(s$coefficients)),
                                         ,drop = F])
      coefs$sign <- ifelse(coefs$Pr...z..<0.001, "***", 
                           ifelse(coefs$Pr...z..<0.01, "**",
                                  ifelse(coefs$Pr...z..<0.05, "*",
                                         ifelse(coefs$Pr...z..<0.1, ".",""))))
      coefs$milk_type = milk_type
      
      anovas <- anova(glm_s12_model, test = "Chisq")
      anovas$milk_type = milk_type
      
      return(list(coefs=coefs, anova = anovas))
    })
  
  coefs <- do.call(rbind, lapply(by_milk_type, `[[`, "coefs"))
  anovas <- do.call(rbind, lapply(by_milk_type, `[[`, "anova"))
  cat("Finish stack_with_survival. Used"); print(Sys.time()-t)  
  return(list(coef = coefs, anova = anovas))
  # })
} # end function

# lapply will use up memeories and crash
for(i in 2:5) {
  cat(paste0("s", i-1, i), "\n")
  df_model <- select(df_downsample, AnimalDurableCode, HerdDurableKey, 
                     DairyYear, AgeParity,cg, new_milk_type, 
                     matches(paste0("s", i-1, i)), 
                     starts_with("EBV_")) %>% 
    filter(AgeParity==i) %>% 
    distinct() %>% 
    mutate(HerdDurableKey = as.factor(HerdDurableKey), 
           DairyYear = as.factor(DairyYear))
  
  for (j in unique(df_model$new_milk_type)) {
    assign(paste0("glm_s", i-1, i, "_", j),
           stack_within_survival(df_model, paste0("s", i-1, i), j))
    
    l <- get(paste0("glm_s", i-1, i, "_", j))
    write.csv(l$coef, paste0(outputDir, "coef_glm_s", i-1, i, "_", j, ".csv"))
    write.csv(l$anova, paste0(outputDir, "anova_glm_s", i-1, i, "_", j, ".csv"))
    # saveRDS(get(paste0("glm_s", i-1, i, "_", j)), 
    # paste0(outputDir, "glm_s", i-1, i, "_", j, ".RData"))
  }
} # end do loop


cat("Coefficients:\n")
print(coef)
cat("\nAnova with Chi-sq test:\n")
print(anova)

write.csv(coef, paste0(outputDir, "coef_s12_ebv.csv"), row.names = F, quote = F)
write.csv(anovas, paste0(outputDir, "anova_s12_ebv.csv"), 
          row.names = T, quote = F)
