# Luna Zhang Feb 23 2022

library(data.table)
library(tidyverse)

source("utils.R")

fileDir <- "e:/500390_OAD/explore/" # "z:/Luna/500390_OAD/explore/"
rawfileDir <- "e:/OAD Dairy Data/"
outputDir <- "e:/500390_OAD/output/" # copy to "z:/Luna/500390_OAD/output/" later

# if(!exists(outputDir)) dir.create(outputDir)
stop()
dir(fileDir)

df <- fread(paste0(fileDir, "productionextract"), sep = " ")

# Animals with no records earlier than 20070601 (script.sh)
# DairyYear pre-filtered  >=2007 (script.sh)
# Season: 0==before Dec 1st, 1==after Dec 1st
# RegimeType: 1= am&pm, 2=am only, 3=pm only, 4= oad. 1, 2, 3 are TAD (twice a day)

# explore df
str(df)
head(df)
unique(df$RegimeType) # 1, 2, 3, 4
unique(df$AgeParity) # 2, 3, ..7
which(duplicated(df$AnimalDurableCode)) %>% length()
filter(df, AnimalDurableCode==df$AnimalDurableCode[1]) %>% 
  arrange(EventDate)

# count
unique(df$HerdDurableKey) %>% length() # 17982
unique(df$AnimalDurableCode) %>% length() # 14205107

##--delete season = 1--##
df <- filter(df, Season == 0)
nrow(df) # 117423605
unique(df$HerdDurableKey) %>% length() # 17950
unique(df$AnimalDurableCode) %>% length() # 13954233

# convert date column
df <- df %>% 
 # mutate(event_date = as.POSIXct(lubridate::fast_strptime(EventDate, format="%Y%m%d")))
  mutate(event_date = lubridate::ymd(EventDate)) # class == Date

##--delete herd with gap year(s)--##

df <- df %>% mutate(year = year(event_date))
df_split <- split(df, df$HerdDurableKey) # not sorted yet
length(df_split) #  17950 herds

test <- lapply(df_split, function(df_sub) { # check proportion of problematic herds
  # gap <- unique(df_sub$year) %>% sort() %>% diff()
  return(any(
    df_sub$DairyYear %>% unique() %>% sort() %>% diff() > 1))
})
table(unlist(test)) 
# FALSE  TRUE 
# 13892  4058 
table(unlist(test))/length(test)
# FALSE      TRUE 
# 0.7739276 0.2260724

t <- Sys.time()
# test <- mclapply(mc.cores = getMKLthreads()/2, l_env = c("idx", "df"),
test <- lapply(
  df_split, function(df_sub) {
  # gap <- unique(df_sub$year) %>% sort() %>% diff()
  if(any(df_sub$DairyYear %>% unique() %>% sort() %>% diff() > 1)) {
    return(NULL)
  } else {
    return(df_sub %>% 
             arrange(AnimalDurableCode, event_date)) # very long
  }
})
print(Sys.time()-t) # lapply 9.3min mclapply

df_new <- do.call(rbind, test)
nrow(df_new) #  88342185  events
unique(df_new$HerdDurableKey) %>% length() # 13892
unique(df_new$AnimalDurableCode) %>% length() # 9886185

rm(list=ls(pattern = "df_split|df$|test"))
saveRDS(df_new, paste0(outputDir, "production_after2006_noTransferHerd.RData")) # long time

##--delete animals moved around herds--##

# df_new <- readRDS(paste0(outputDir, "productionextract_after2006_noTransferHerd.RData"))
test <- dplyr::select(df_new, AnimalDurableCode, HerdDurableKey, event_date)
# write_excel_csv2(test, paste0(outputDir, "animal_herd_eventDate.csv"), 
                 # delim = ",", quote_escape = F) # 14 min to write

# t <- Sys.time()
# test <- arrange(test, AnimalDurableCode, event_date) # takes forever
# print(Sys.time()-t)
# alternatively run "sort -k1,1 -k3,3 animal_herd_eventDate.csv > tmp" in git.bash
# test <- read.csv("e:/500390_OAD/explore/animal_eventDate_herd_sorted")

animal_herd_date <- test
test <- animal_herd_date[, .(AnimalDurableCode, HerdDurableKey)]
test <- unique(test)

length(unique(test$AnimalDurableCode)) # 9886185 animals
a <- which(duplicated(test$AnimalDurableCode))
dup_id <- unique(test$AnimalDurableCode[a])
length(dup_id) # 1407398 animals transferred herd
a <- NULL

n <- nrow(df_new) # 88342185
test <- df_new[!AnimalDurableCode %in% dup_id]
nrow(test) # 70842190♣ events
unique(test$HerdDurableKey) %>% length() # 13869 herds
unique(test$AnimalDurableCode) %>% length() # 8478787 animals

df_new <- test
t <- Sys.time()
saveRDS(df_new, paste0(outputDir, "production_after2006_noTransferHerd_noGapYear.RData"))
print(Sys.time()-t) # 3.5 min

# animal_herd_date <- animal_herd_date[!AnimalDurableCode %in% dup_id]
# t <- Sys.time()
# write_excel_csv2(animal_herd_date, paste0(outputDir, "animal_herd_eventDate.csv"), 
#                  delim = ",", quote_escape = F) # 14 min to write
# print(Sys.time()-t) # 5.4 min
rm(list=ls(pattern = "animal_herd|test|dup_id"))

##---delete animals having gap years--##

# df_new <- readRDS(paste0(outputDir, "production_after2006_noTransferHerd_noGapYear.RData"))

# Find the index of 1st occurrence of each animal ID
test <- df_new[, .(AnimalDurableCode, DairyYear)]
test <- unique(test)

t <- Sys.time()
a <- data.frame(table(test$AnimalDurableCode))
a$var <- as.character(a$Var1)
all(a$var==unique(test$AnimalDurableCode)) # sanity check
a <- a[match(unique(test$AnimalDurableCode), a$var),]
all(a$var==unique(test$AnimalDurableCode)) # sanity check
a$idx <- cumsum(a$Freq)-a$Freq + 1
idx <- a$idx
idx <- c(idx, nrow(test)+1) # for the last set of animal IDs◘
a <- NULL

# good_ids <- mclapply(mc.cores = getMKLthreads()/2, l_env = c("idx", "test"),
good_ids <- lapply(
                     head(seq(idx), -1), function(i) {
  if(idx[1]==idx[i+1]-1) { # animals only have one record
    return(test$AnimalDurableCode[idx[i]])
  } else if(any(diff(test$DairyYear[idx[i]:(idx[i+1]-1)]) > 1)) { # avoid assignments!
    return(NULL)
  } else {
    return(test$AnimalDurableCode[idx[i]])
  }
})
print(Sys.time()-t) # lapply 2.25 min, mclapply 4.7min

good_ids <- unlist(good_ids)
length(good_ids) # 8006245 animals do not have gap years
length(unique(test$AnimalDurableCode)) # 8478787

test <- df_new[AnimalDurableCode %in% good_ids]
nrow(test) # 65602074 cleaned events
df_new <- test
unique(df_new$HerdDurableKey) %>% length() # 13869
unique(df_new$AnimalDurableCode) %>% length() #  8,006,245 
saveRDS(df_new, paste0(outputDir, "production_after2006_noTransferHerd_noGapYearHerd_noGapYearAnim.RData"))
rm(list = ls(pattern = "good_ids|idx|test"))


##---Define mliking type at event, season, herd level sequentially------------##
##--including herd with animals that have different RegimeType--##
##--Assign milk_type = OAD, TAD, TRANS--------------------------##

# RegimeType: 1= am&pm, 2=am only, 3=pm only, 4= oad. 1, 2, 3 are TAD (twice a day)

#--For each Herd-Season-EventDate, define milking type as 95% of the animals--#
#-- 		○ >=95% are OAD, define as OAD. Same for TAD.-------#
#--     ○ If (5%, 95%) are TAD then define it as unknown. --#
#--     ○ If (5%, 95%) are OAD define it as unknown.--------#
#--The output will be Herd, Season, EventDate, milking_type-#

#  df_new <- readRDS(paste0(outputDir, "production_after2006_noTransferHerd_noGapYearHerd_noGapYearAnim.RData"))
test <- df_new[, .(HerdDurableKey, EventDate, event_date, AnimalDurableCode,
                   RegimeType)]
test <- arrange(test, HerdDurableKey, EventDate)

# saniy check duplicated animal IDs
# which(duplicated(test[,1:3]))
table(df_new$RegimeType, useNA="always")/nrow(df_new) # OAD 16%
# 1          2          3          4       <NA> 
# 0.55341808 0.22561020 0.06186588 0.15910584 0.00000000 

herd_event <- paste0(test$HerdDurableKey, "_", test$EventDate) # long time
test$herd_event <- herd_event
rm(list = ls(pattern = "herd_event"))

# Find the index of 1st occurrence of each herd ID
t <- Sys.time()
a <- data.frame(table(test$herd_event))
a$var <- as.character(a$Var1)
all(a$var==unique(test$herd_event)) # sanity check
a <- a[match(unique(test$herd_event), a$var),]
all(a$var==unique(test$herd_event)) # sanity check
a$idx <- cumsum(a$Freq)-a$Freq + 1
idx <- a$idx
idx <- c(idx, nrow(test)+1) # for the last set of animal IDs◘
a <- NULL

# prepare mclapply in windows from defining clusters
# t <- Sys.time()
# cl <- makeCluster(getMKLthreads()/2)
# this.env <- environment()
# clusterExport(cl,
#               c("idx", "test", "this.env", ".Random.seed"), # long time because test is big
#               envir = globalenv())
# print(Sys.time()-t) # 2.34917 mins

# run mclapply
milk_type <- # mclapply(mc.cores = getMKLthreads()/2, l_env = c("idx", "test"),
  lapply(
    head(seq(idx), -1), function(i) {
      if(sum(test$RegimeType[idx[i]:(idx[i+1]-1)]==4)/(idx[i+1]-idx[i])>=0.95) {
        return("OAD")
      } else if(sum(test$RegimeType[idx[i]:(idx[i+1]-1)]==4)/
                (idx[i+1]-idx[i])<=0.05) {
        return("TAD")
      } else {
        return(NA)
      }
    })
print(Sys.time()-t) # 2.8 sec

milk_type <- unlist(milk_type)
event_milk_type <- cbind(test[head(idx, -1), ], milk_type) %>% 
  select(HerdDurableKey, event_date, event_milk_type = milk_type) %>% 
  mutate(year = year(event_date))  # 309739 herd-season-event

# sanity check
sum(is.na(milk_type)) # 21979
sum(is.na(milk_type))/nrow(event_milk_type) # 0.07
table(event_milk_type$event_milk_type, useNA = "ifany")
# OAD    TAD   <NA> 
# 47611 240152  21976 
table(event_milk_type$event_milk_type, useNA = "ifany")/nrow(event_milk_type)
# OAD     TAD       <NA> 
# 0.15   0.775        0.071 

#--	Define Herd-Season's milking type.-----------------------------------------#
#--Some Herd-Season only has the last test (EventDate) as OAD. It's OK to keep.#
#--Use January 1st as the start of the 2nd season -----------------------------#
#--(June 1 to Dec 1 is the 1st season)-----------------------------------------#
#--  ○ If all TAD before Jan 1st and all OAD after Jan 1st --> TAD, -----------#
#--     PrimaryRegime --> switch ----------------------------------------------#
#--  ○ If OAD happened before Jan 1st --> unclassified, -----------------------#
#--     PrimaryRegime --> unknown----------------------------------------------#
#--  ○ If TAD happened after OAD --> unclassified, PrimaryRegime --> unknown---#
#--  ○ If unknown occurs before Jan 1st --> unclassified, ---------------------#
#--     PrimaryRegime --> unknown----------------------------------------------#

# define season
# event_milk_type <- mutate(event_milk_type, 
#                           herd_year = paste0(HerdDurableKey, "_", year))
season <- lapply(seq(nrow(event_milk_type)), function(i) {
  if(event_milk_type$event_date[i] < 
     as.Date(paste0(event_milk_type$year[i], "0601"), "%Y%m%d")) {
    return(paste0(event_milk_type$year[i]-1, "-", event_milk_type$year[i]))
  } else {
    return(paste0(event_milk_type$year[i], "-", event_milk_type$year[i]+1))
  }
})
season <- unlist(season)
event_milk_type$season <- season
rm(list = (ls(pattern = "^season$")))
event_milk_type$herd_season <- paste0(event_milk_type$HerdDurableKey, "_",
                                      event_milk_type$season)

write_excel_csv2(event_milk_type, paste0(outputDir, "event_milk_type.csv"),
                 delim = ",", quote_escape = "none")

t <- Sys.time()
# Find the index of 1st occurrence of each herd_season
a <- data.frame(table(event_milk_type$herd_season))
a$var <- as.character(a$Var1)
all(a$var==unique(event_milk_type$herd_season)) # sanity check
a <- a[match(unique(event_milk_type$herd_season), a$var),]
all(a$var==unique(event_milk_type$herd_season)) # sanity check
a$idx <- cumsum(a$Freq)-a$Freq + 1
idx <- a$idx
idx <- c(idx, nrow(event_milk_type)+1) # for the last set of animal IDs◘
a <- NULL

milk_type <- lapply(head(seq(idx), -1), function(i) {
  # print(i)
  if(sum(is.na(event_milk_type$event_milk_type[idx[i]:(idx[i+1]-1)]))==0 &&
    all(event_milk_type$event_milk_type[idx[i]:(idx[i+1]-1)]=="OAD")) {
    return(c("OAD", "no"))
    
  } else if(
    sum(is.na(event_milk_type$event_milk_type[idx[i]:(idx[i+1]-1)]))==0 &&
    all(event_milk_type$event_milk_type[idx[i]:(idx[i+1]-1)]=="TAD")) {
    return(c("TAD", "no"))
  } else { # OAD NA, TAD NA, OAD TAD, OAD TAD NA
    
    yearr <- strsplit(event_milk_type$season[idx[i]], "-") %>%
      unlist() %>% tail(1)
    before_jan <- event_milk_type$event_date[idx[i]:(idx[i+1]-1)] <
      as.Date(paste0(yearr, "0101"), "%Y%m%d")
    idx_na <- which(is.na(event_milk_type$event_milk_type[idx[i]:(idx[i+1]-1)]))
    
    if(length(idx_na)>0 && any(before_jan[idx_na])) { # unknown occurs before Jan 1st
      return(c("unclassified", "unknown"))
      
    } else { # OAD..NA, TAD..NA, OAD..TAD, OAD..NA..TAD, OAD..TAD..NA
      
      idx_2 <- which(event_milk_type$event_milk_type[idx[i]:(idx[i+1]-1)]=="TAD")
      idx_1 <- which(event_milk_type$event_milk_type[idx[i]:(idx[i+1]-1)]=="OAD")
      
      if(length(idx_2)>0 && length(idx_1)>0 &&
                any(max(idx_2)>idx_1)) { # TAD occurred after OAD
        return(c("unclassified", "unknown"))
        
      } else if(any(before_jan[idx_1])) { # OAD occurred before Jan 1st
        return(c("unclassified", "unknown"))
        
      } else if(length(idx_1)==0) { # TAD..NA..
        return(c("TAD", "unknown")) # ??? no switch???
        
      } else if(length(idx_2)==0) { # NA..OAD, OAD..NA
        return(c("OAD", "no")) # no NA occurred before Jan 1st. See lines above
        
      } else { # OAD..TAD, OAD..NA..TAD, OAD..TAD..NA
        return(c("TAD", "switch"))
      }
    }
  }
})
print(Sys.time()-t) # 7.3 sec
milk_type <- do.call(rbind, milk_type)
colnames(milk_type) <- c("season_milk_type", "switch_type")

season_milk_type <- data.frame(event_milk_type[head(idx, -1),], milk_type) %>% 
  select(-event_date, -event_milk_type, -year) # 89964 herd-seasons

# saniy check
nrow(season_milk_type)==length(idx)-1
table(season_milk_type$season_milk_type)
# OAD          TAD unclassified 
# 5986        75170         8808
table(season_milk_type$season_milk_type)/nrow(season_milk_type)
#  0.06653773   0.83555644   0.09790583 
table(season_milk_type$switch_type)
# no      switch unknown 
# 56988   17808   15168 
filter(season_milk_type, switch_type=="unknown" & 
         season_milk_type=="TAD") %>% nrow() # 6360
filter(season_milk_type, switch_type=="no" & 
         season_milk_type=="OAD") %>% nrow() # 5986
filter(season_milk_type, switch_type=="switch") %>% head()
test <- filter(season_milk_type, switch_type=="unknown" & 
                 season_milk_type=="TAD") 
test <- filter(season_milk_type, season_milk_type=="unclassified") 
i <- sample(seq(nrow(test)), 1)
filter(event_milk_type, HerdDurableKey==test$HerdDurableKey[i] &
       season==test$season[i])

write_excel_csv2(season_milk_type, paste0(outputDir, "season_milk_type.csv"),
                 delim = ",", quote_escape = "none")

#--Use above milking type to define a Herd's milking type--#
#--  ○ All OAD across seasons --> OAD, same for TAD--------#
#--  ○ TAD to OAD --> transition---------------------------#
#--  ○ TAD after OAD --> delete this herd------------------#
#--  ○ If there is an unclassified, equivalent to a gap year-->delete this herd#

# find gap season and remove the herds
which(duplicated(season_milk_type[,1:2])) # sanity check
dup_ids <- filter(season_milk_type, season_milk_type=="unclassified") %>% 
  select(HerdDurableKey) %>% unlist() %>% unique() # 3925 herds

df_new <- filter(df_new, !HerdDurableKey %in% dup_ids)
nrow(df_new) # 34442119
unique(df_new$HerdDurableKey) %>% length() # 9944
unique(df_new$AnimalDurableCode) %>% length() # 4457465
saveRDS(df_new, paste0(outputDir,
                       "production_after2006_noTransferHerd_noGapYearHerd_noGapYearAnim_noUnclassifiedSeasonMilkingType.RData"))

event_milk_type_unclassified <- filter(event_milk_type, HerdDurableKey %in% dup_ids)
season_milk_type_unclassified <- filter(season_milk_type, HerdDurableKey %in% dup_ids)

event_milk_type <- filter(event_milk_type, !HerdDurableKey %in% dup_ids)
season_milk_type <- filter(season_milk_type, !HerdDurableKey %in% dup_ids)
write.csv(event_milk_type, paste0(outputDir, "event_milk_type.csv"), 
          row.names = F, quote = F)
write.csv(season_milk_type, paste0(outputDir, "season_milk_type.csv"), 
                 row.names = F, quote = F)

# Find the index of 1st occurrence of each herd_season

# df_new <- readRDS(paste0(outputDir, "production_after2006_noTransferHerd_noGapYearHerd_noGapYearAnim_noUnclassifiedSeasonMilkingType.RData"))
# event_milk_type <- read.csv(paste0(outputDir, "event_milk_type.csv"))
# season_milk_type <- read.csv(paste0(outputDir, "season_milk_type.csv"))
# names(event_milk_type)[1] <- "HerdDurableKey"
# names(season_milk_type)[1] <- "HerdDurableKey"
a <- data.frame(table(season_milk_type$HerdDurableKey))
a$var <- as.character(a$Var1)
all(a$var==unique(season_milk_type$HerdDurableKey)) # sanity check
a <- a[match(unique(season_milk_type$HerdDurableKey), a$var),]
all(a$var==unique(season_milk_type$HerdDurableKey)) # sanity check
a$idx <- cumsum(a$Freq)-a$Freq + 1
idx <- a$idx
idx <- c(idx, nrow(season_milk_type)+1) # for the last set of animal IDs◘
a <- NULL

milk_type <- lapply(head(seq(idx), -1), function(i) {
  # print(i)
  if(all(season_milk_type$season_milk_type[idx[i]:(idx[i+1]-1)]=="OAD")) {
    return(c("OAD", NA))
    
  } else if(
    all(season_milk_type$season_milk_type[idx[i]:(idx[i+1]-1)]=="TAD")) {
    return(c("TAD", NA))
    
  } else {
    idx_2 <- which(season_milk_type$season_milk_type[idx[i]:(idx[i+1]-1)]=="TAD")
    idx_1 <- which(season_milk_type$season_milk_type[idx[i]:(idx[i+1]-1)]=="OAD")
    
    if(length(idx_2)>0 && length(idx_1)>0 && # TAD occurs after OAD
       any(max(idx_2) > idx_1)) {  # TAD..OAD, OAD..TAD..OAD
      return(NA) # delete later
      
    } else { # Transition 
      years <- strsplit(season_milk_type$season[idx[i]:(idx[i+1]-1)], "-") %>% 
        sapply(head, 1)
      return(c("Transition", years[min(idx_1)]))
    }
  }
})
milk_type <- data.frame(do.call(rbind, milk_type))
names(milk_type) <- c("herd_milk_type", "transition_year")
milk_type$transition_year <- as.integer(milk_type$transition_year)

herd_milk_type <- cbind(season_milk_type[head(idx, -1),], milk_type) %>% 
  select(HerdDurableKey, herd_milk_type, transition_year) # 9944 herd

# sanity check
table(herd_milk_type$herd_milk_type, useNA = "always")
# OAD        TAD Transition       <NA> 
# 524       8723        283        414 
table(herd_milk_type$herd_milk_type, useNA = "always")/nrow(herd_milk_type)
# OAD               TAD Transition  <NA> 
# 0.05269509 0.87721239 0.02845937 0.04163315 
which(is.na(herd_milk_type$herd_milk_type)) %>% length() # 414 back to TAD herds
which(duplicated(herd_milk_type$HerdDurableKey)) %>% length()

# remove back to TAD herds
herd_milk_type <- filter(herd_milk_type, !is.na(herd_milk_type))
write.csv(herd_milk_type, paste0(outputDir, "herd_milk_type.csv"), 
          row.names = F, quote = F)

df_new <- filter(df_new, HerdDurableKey %in% herd_milk_type$HerdDurableKey)
nrow(df_new) # 32726616
unique(df_new$HerdDurableKey) %>% length() # 9530
unique(df_new$AnimalDurableCode) %>% length() # 4202639

df_new <- left_join(df_new, herd_milk_type)

saveRDS(df_new, paste0(outputDir,
                       "production_after2006_noTransferHerd_noGapYearHerd_",
                       "noGapYearAnim_noUnclassifiedSeasonMilkingType_",
                       "noBackToTADHerd.RData"))
# combine
all_milk_type <- right_join(season_milk_type, herd_milk_type)
all_milk_type <- right_join(event_milk_type, all_milk_type) %>% 
  select(-year)
write.csv(all_milk_type, 
          paste0(outputDir, "herd_season_testday_milk_type.csv"), 
          row.names = F, quote = F)
rm(list=ls(pattern = "^milk_type$|idx|ids"))

# sanity check
test <- left_join(event_milk_type, season_milk_type) %>% 
  left_join(herd_milk_type) %>% 
  filter(!HerdDurableKey %in% all_milk_type)

filter(test, HerdDurableKey==83) %>% head()

unclassified <- left_join(event_milk_type_unclassified, 
                          season_milk_type_unclassified) %>% 
  mutate(herd_milk_type = NA, transition_year = NA)

removed_herd_milk_type <- rbind(test, unclassified)
write.csv(removed_herd_milk_type, 
          paste0(outputDir, "removed_herd_milk_type.csv"), row.names = F,
          quote = F)

##----remove cows in OAD herds entered before 2007-2008 season---##
##--2007 June 1st--##

rm(list=ls(pattern = "milk|test|unclassif"))
# df_new <- readRDS(paste0(outputDir,
# "production_after2006_noTransferHerd_noGapYearHerd_",
# "noGapYearAnim_noUnclassifiedSeasonMilkingType_",
# "noBackToTADHerd.RData"))
df <- fread(paste0(fileDir, "productionextract_original"), sep = " ")
idx <- which(df$EventDate < 20070601)
old_ids <- df$AnimalDurableCode[idx] %>% unique()

# sanity check
length(idx) # 127418367
length(idx)/nrow(df) # 0.51
t <- which(old_ids %in% df_new$AnimalDurableCode) %>% length() # 743071
n <- unique(df_new$AnimalDurableCode) %>% length() # 4731070
t/n # 0.157

df_new <- filter(df_new, !AnimalDurableCode %in% old_ids)
nrow(df_new) # 28199280
unique(df_new$HerdDurableKey) %>% length() # 9490
unique(df_new$AnimalDurableCode) %>% length() # 3517764

saveRDS(df_new, paste0(outputDir,
                       "production_after2006_noTransferHerd_noGapYearHerd_",
                       "noGapYearAnim_noUnclassifiedSeasonMilkingType_",
                       "noBackToTADHerd_noOldAnimal.RData"))

animal_herd <- dplyr::select(df_new, AnimalDurableCode, 
                                   HerdDurableKey) %>% 
  distinct()
write.csv(animal_herd, paste0(outputDir, "animal_herd.csv"),
          quote = F, row.names = F)
