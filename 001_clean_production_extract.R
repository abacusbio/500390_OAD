# Luna Zhang Feb 23 2022

library(data.table)
library(tidyverse)

source("utils.R")

fileDir <- "e:/500390_OAD/explore/" # "z:/Luna/500390_OAD/explore/"
rawfileDir <- "z:/Luna/500390_OAD/OAD Dairy Data/"
outputDir <- "e:/500390_OAD/output/" # copy to "z:/Luna/500390_OAD/output/" later

# if(!exists(outputDir)) dir.create(outputDir)
stop()
dir(fileDir)

df <- fread(paste0(fileDir, "productionextract"), sep = " ")

# DairyYear pre-filtered  >=2006 (script.sh)
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

# convert date column
df <- df %>% 
 # mutate(event_date = as.POSIXct(lubridate::fast_strptime(EventDate, format="%Y%m%d")))
  mutate(event_date = lubridate::ymd(EventDate)) # class == Date

##--delete herd with gap year(s)--##

df <- df %>% mutate(year = year(event_date))
df_split <- split(df, df$HerdDurableKey)
length(df_split) #  18956 herds

test <- lapply(df_split, function(df_sub) { # check proportion of problematic herds
  # gap <- unique(df_sub$year) %>% sort() %>% diff()
  return(any(as.integer(unique(df_sub$year) %>% sort() %>% diff()) > 1))
})
table(unlist(test)) 
# FALSE  TRUE 
# 15791  3165 
table(unlist(test))/length(test)
# FALSE      TRUE 
# 0.8330344 0.1669656 

t <- Sys.time()
# test <- mclapply(mc.cores = getMKLthreads()/2, l_env = "df_split",
test <- lapply(
  df_split, function(df_sub) {
  # gap <- unique(df_sub$year) %>% sort() %>% diff()
  if(any(as.integer(unique(df_sub$year) %>% sort() %>% diff()) > 1)) {
    return(NULL)
  } else {
    return(df_sub %>% arrange(AnimalDurableCode, event_date)) # very long
  }
})
print(Sys.time()-t)

df_new <- do.call(rbind, test)
nrow(df_new) # 105627342 events
nrow(df) # 126900183 events
unique(df_new$HerdDurableKey) %>% length() # 15791
unique(df$HerdDurableKey) %>% length() # 18956♀
unique(df_new$AnimalDurableCode) %>% length() # 11532020F
unique(df$AnimalDurableCode) %>% length() # 14758949

rm(list=ls(pattern = "df_split|df$|test"))
# saveRDS(df_new, paste0(outputDir, "production_after2006_noTransferHerd.RData")) # long time

##--delete animals moved around herds--##

# df_new <- readRDS(paste0(outputDir, "productionextract_after2006_noTransferHerd.RData"))
test <- dplyr::select(df_new, AnimalDurableCode, HerdDurableKey, event_date)
write.csv(test, paste0(outputDir, "animal_herd_eventDate.csv"), quote = F,
          row.names = F) # 14 min to write

t <- Sys.time()
test <- arrange(test, AnimalDurableCode, event_date) # takes forever
print(Sys.time()-t)
# alternatively run "sort -k1,1 -k3,3 animal_herd_eventDate.csv > tmp" in git.bash
# test <- read.csv("e:/500390_OAD/explore/animal_eventDate_herd_sorted")

animal_herd_date <- test
test <- animal_herd_date[, .(AnimalDurableCode, HerdDurableKey)]
test <- unique(test)

length(unique(test$AnimalDurableCode)) # 11532020 animals
a <- which(duplicated(test$AnimalDurableCode))
dup_id <- unique(test$AnimalDurableCode[a])
length(dup_id) # 179910 animals transferred herd
a <- NULL

n <- nrow(df_new) # 105627342
test <- df_new[!AnimalDurableCode %in% dup_id]
nrow(test) # 83258088 events
n <- NULL
unique(test$HerdDurableKey) %>% length() # 15762 herds
unique(test$AnimalDurableCode) %>% length() # 9732920 animals

df_new <- test
t <- Sys.time()
saveRDS(df_new, paste0(outputDir, "production_after2006_noTransferHerd_noGapYear.RData"))
print(Sys.time()-t) # 3.5 min

animal_herd_date <- animal_herd_date[!AnimalDurableCode %in% dup_id]
t <- Sys.time()
write.csv(animal_herd_date, paste0(outputDir, "animal_herd_eventDate.csv"), quote = F,
          row.names = F) # 14 min to write
print(Sys.time()-t) # 3.5 min
rm(list=ls(pattern = "animal_herd|test|dup_id"))

##---delete animals having gap years--##

# df_new <- readRDS(paste0(outputDir, "production_after2006_noTransferHerd_noGapYear.RData"))

# Find the index of 1st occurrence of each animal ID
test <- df_new[, .(AnimalDurableCode, year)]
test <- unique(test)

a <- data.frame(table(test$AnimalDurableCode))
a$var <- as.character(a$Var1)
all(a$var==unique(test$AnimalDurableCode)) # sanity check
a <- a[match(unique(test$AnimalDurableCode), a$var),]
all(a$var==unique(test$AnimalDurableCode)) # sanity check
a$idx <- cumsum(a$Freq)-a$Freq + 1
idx <- a$idx
idx <- c(idx, nrow(test)+1) # for the last set of animal IDs◘
a <- NULL

t <- Sys.time()
# good_ids <- mclapply(mc.cores = getMKLthreads()/2, l_env = c("idx", "test"),
good_ids <- lapply(
                     head(seq(idx), -1), function(i) {
  if(idx[1]==idx[i+1]-1) { # animals only have one record
    return(test$AnimalDurableCode[idx[i]])
  } else if(any(diff(test$year[idx[i]:(idx[i+1]-1)]) > 1)) { # avoid assignments!
    return(NULL)
  } else {
    return(test$AnimalDurableCode[idx[i]])
  }
})
print(Sys.time()-t) # lapply 2.25 min, mclapply 4.7min

good_ids <- unlist(good_ids)
length(good_ids) # 9586263 animals do not have gap years
length(unique(test$AnimalDurableCode)) # 9732920

test <- df_new[AnimalDurableCode %in% good_ids]
nrow(test) # 82054453 cleaned events
df_new <- test
unique(df_new$HerdDurableKey) %>% length() # 15762
saveRDS(df_new, paste0(outputDir, "production_after2006_noTransferHerd_noGapYearHerd_noGapYearAnim.RData"))
rm(list = ls(pattern = "good_ids|idx|test"))


##---delete herds that transit back to TAD----------------------##
##--including herd with animals that have different RegimeType--##
##--Assign milk_type = OAD, TAD, TRANS--------------------------##

# RegimeType: 1= am&pm, 2=am only, 3=pm only, 4= oad. 1, 2, 3 are TAD (twice a day)
# df_new <- readRDS(paste0(outputDir, "production_after2006_noTransferHerd_noGapYearHerd_noGapYearAnim.RData"))

test <- df_new[, .(HerdDurableKey, event_date, RegimeType)]
test <- unique(test)
test <- arrange(test, HerdDurableKey, event_date, RegimeType)

# saniy check
filter(test, HerdDurableKey==109)
filter(df_new, HerdDurableKey==109 & EventDate==20080316) %>% select(RegimeType) %>% table()
# 1   4 
# 129  78 

# Find the index of 1st occurrence of each herd ID
a <- data.frame(table(test$HerdDurableKey))
a$var <- as.character(a$Var1)
all(a$var==unique(test$HerdDurableKey)) # sanity check
a <- a[match(unique(test$HerdDurableKey), a$var),]
all(a$var==unique(test$HerdDurableKey)) # sanity check
a$idx <- cumsum(a$Freq)-a$Freq + 1
idx <- a$idx
idx <- c(idx, nrow(test)+1) # for the last set of animal IDs◘
a <- NULL

# # prepare mclapply in windows from defining clusters
# t <- Sys.time()
# cl <- makeCluster(getMKLthreads()/2)
# this.env <- environment()
# clusterExport(cl,
#               c("idx", "test", "this.env", ".Random.seed"), # long time because test is big
#               envir = globalenv())
# print(Sys.time()-t) # 2.34917 mins

# run mclapply
t <- Sys.time()
n_milking <- #mclapply(mc.cores = getMKLthreads()/2, l_env = c("idx", "test"),
  lapply(
                      head(seq(idx), -1), function(i) {
  
  if(!4 %in% test$RegimeType[idx[i]:(idx[i+1]-1)]) {
    return(c(HerdDurableKey = test$HerdDurableKey[idx[i]], milk_type="OAD"))
    
  } else if(4 %in% test$RegimeType[idx[i]:(idx[i+1]-1)]) { # avoid assignments!
    idxx <- which(test$RegimeType[idx[i]:(idx[i+1]-1)]==4)
    
    if(any(diff(idxx)>1)) { # e.g. RegimeType==1, 1, 4, 1, 4
      return(NULL)
    } else if (max(idxx) < idx[i+1]-idx[i]) { # e.g. RegimeType==4,4,4,1
      return(NULL)
    } else if (any(test$RegimeType[idx[i]:(idx[i+1]-1)]<4)){
      return(c(HerdDurableKey = test$HerdDurableKey[idx[i]], milk_type="TRANS"))
    } else (
      return(c(HerdDurableKey = test$HerdDurableKey[idx[i]], milk_type="TAD"))
    )
  }
    
})
print(Sys.time()-t) # 23 sec with parLapply after setup, 4.2 min with mclapply.hack

n_milking <- data.frame(do.call(rbind, n_milking))
length(unique(n_milking$HerdDuarableKey))<=nrow(n_milking) # sanity check
sum(duplicated(n_milking$HerdDurableKey))==0 # saniy check
nrow(n_milking) # 6233 herds
length(unique(test$HerdDurableKey)) # 15762

df_new <- filter(df_new, HerdDurableKey %in% n_milking$HerdDurableKey) 
nrow(df_new) # 9801851 from  82,054,453 
unique(df_new$AnimalDurableCode) %>% length() # 1536467

# get milk_type
n_milking$HerdDurableKey <- as.integer(n_milking$HerdDurableKey)
df_new <- left_join(df_new, n_milking)

# get transition year
test <- select(df_new, HerdDurableKey, year, RegimeType, milk_type) %>% 
  distinct()
split_test <- split(test, test$HerdDurableKey)

transition_year <- lapply(split_test, function(test_sub) {
  if(test_sub$milk_type[1]!="TRANS") {
    return(data.frame(HerdDurableKey=test_sub$HerdDurableKey[1], 
                      transition_year=NA))
  } else {
    return(data.frame(HerdDurableKey=test_sub$HerdDurableKey[1], 
                      transition_year=
                        test_sub$year[which.min(test_sub$RegimeType==4)]))
    
  }
})
transition_year <- do.call(rbind, transition_year)
df_new <- left_join(df_new, transition_year)

rm(list=ls(pattern = "test|n_milking|transition"))
saveRDS(df_new, paste0(outputDir,
                       "production_after2006_noTransferHerd_noGapYearHerd_noGapYearAnim_noBackToTAD.RData"))


