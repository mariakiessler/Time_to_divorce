################################################################################
#                                                                              #
#                             data preparation                                 #
#                                                                              #
################################################################################
# _______________________________________________                           ####                      
#
# Packages 
# _______________________________________________                           ####

library(haven)
library(tidyverse)
library(psych)
library(openxlsx)
library(corrplot)

#___________________________________________________                        
#
# selecting variables                 
# __________________________________________________                        ####

####  Wave 1  ####
setwd("C:/Users/User/OneDrive/Uni/Master/5. Semester/Masterarbeit/Auswertung/Combined")
data1  <- read_sav("Combined a210c.sav")
# Specify variables needed
var1 <- c("xwaveid","ahhpxid","ahgyob","ahhcompi","ahges","ahglth", "ahgms","ahgsex",
          "ahhd0_4","ahhd5_9","ahhd1014","ahhd1524","ahhad10","ahhec10","ahhed10",
          "ahhpers","ahhsos","ahifisi","ahifnisi","ahifditp","awscei","amvncar",
          "aanbcob","afmagelh","afmeldso","afmfcob","afmfemp","afmfuemp", "afmfo61",
          "afmmo61","afmmcob","afmmemp","afmlwop","afmhsib","afmnsib","afmpdiv",
          "atchad","atcdied","atcnr","atcr","atcyng","aedagels","aedhigh1","afiprosp",
          "aghbp","aghgh","aghmh","aghpf","aghre","aghrp","aghsf","aghvt","alsclub",
          "alsdrink","alshaas","alshacf","alshadt","alshahn","alshals","alshanb",
          "alshrcar","alshrchd","alshrcom","alshrerr","alshrhw","alshrod","alshrvol",
          "alslarea","alsothch","alspact","alsrelpc","alsrelsp","alsrush","alsshare",
          "alssmoke","alssocal","alsstime","alssupac","alssupcd","alssuplf","alssuppi",
          "alssuppv","alssupsh","alssupvl", "ajbmo61","aehtjb","aehto","aehtuj",
          "aatwkhpj","aatwkdnm","aatwkwms","aatwkwfs","aatwkseh","aatwkwrl","aatwkmrl",
          "aatwkmsw","aatwkcdw","aatwkbmw","aatwkadc","aatwkwmr","aatwkwfr","aatwkfhi",
          "aloimphl","aloimpew","aloimpfs","aloimplc","aloimpyh","aloimpla","aloimprl",
          "alosathl","alosatfs","alosatlc","alosatyh","alosatnl","alosatft","amrn",
          "amrcdur","amrplv","amrplvt","amrpend", "amrpsep","amrpdiv","aordfpst",
          "aordfnum","amrcurr","amrsdur", "ahgage", "ancage1","ancage2","ancage3", 
          "ancage4","ancage5","ancage6","ancage7","ancage8","ancage9","ancage10", 
          "ancage11","ancage12","ancage13", "arcage1", "arcage2","arcage3","arcage4",
          "arcage5","arcage6","arcage7","arcage8", "arcage9","arcage10")
data1 <- data1[,var1]


####  Wave 2  ####
data2  <- read_sav("Combined b210c.sav")
var2 <- c("xwaveid","bhhpxid","bhhcompi","bmrpdiv","bmrcms","bmrpend",
          "bmrpsep","bmrcurr","bmrsdur","blesep")
data2 <- data2[,var2]
data_full <- merge(data1, data2, by = "xwaveid", all.x = TRUE) 
rm(data1, data2, var1, var2)

####  Wave 3  ####
data3  <- read_sav("Combined c210c.sav")
var3 <- c("xwaveid","chhpxid","chhcompi","cmrpdiv","cmrcms","cmrpsep",
          "cmschgdv","cmrcurr","cmrsdur","cmschgsp","clesep","cmrpend")
data3 <- data3[,var3]
data_full <- merge(data_full, data3, by = "xwaveid", all.x = TRUE) 
rm(data3, var3)

####  Wave 4  ####
data4  <- read_sav("Combined d210c.sav")
var4 <- c("xwaveid","dhhpxid","dhhcompi","dmrpdiv","dmrcms","dmrpsep",
          "dmschgdv","dmrcurr","dmrsdur","dmschgsp","dlesep","dmrpend")
data4 <- data4[,var4]
data_full <- merge(data_full, data4, by = "xwaveid", all.x = TRUE) 
rm(data4, var4)


####  Wave 5  ####
data5  <- read_sav("Combined e210c.sav")
var5 <- c("xwaveid","ehhpxid","ehhcompi","emrpdiv","emrcms","emrpsep",
          "emschgdv","emrcurr","emrsdur","emschgsp","elesep","emrpend")
data5 <- data5[,var5]
data_full <- merge(data_full, data5, by = "xwaveid", all.x = TRUE) 
rm(data5, var5)


####  Wave 6  ####
data6  <- read_sav("Combined f210c.sav")
var6 <- c("xwaveid","fhhpxid","fhhcompi","fmrpdiv","fmrcms","fmrpsep",
          "fmschgdv","fmrcurr","fmrsdur","fmschgsp","flesep","fmrpend")
data6 <- data6[,var6]
data_full <- merge(data_full, data6, by = "xwaveid", all.x = TRUE) 
rm(data6, var6)


####  Wave 7  ####
data7  <- read_sav("Combined g210c.sav")
var7 <- c("xwaveid","ghhpxid","ghhcompi","gmrpdiv","gmrcms","gmrpsep",
          "gmschgdv","gmrcurr","gmrsdur","gmschgsp","glesep","gmrpend")
data7 <- data7[,var7]
data_full <- merge(data_full, data7, by = "xwaveid", all.x = TRUE) 
rm(data7, var7)


####  Wave 8  ####
data8  <- read_sav("Combined h210c.sav")
var8 <- c("xwaveid","hhhpxid","hhhcompi","hmrpdiv","hmrcms","hmrpsep",
          "hmschgdv","hmrcurr","hmrsdur","hmschgsp","hlesep","hmrpend")
data8 <- data8[,var8]
data_full <- merge(data_full, data8, by = "xwaveid", all.x = TRUE) 
rm(data8, var8)

####  Wave 9  ####
data9  <- read_sav("Combined i210c.sav")
var9 <- c("xwaveid","ihhpxid","ihhcompi","imrpdiv","imrcms","imrpsep",
          "imschgdv","imrcurr","imrsdur","imschgsp","ilesep","imrpend")
data9 <- data9[,var9]
data_full <- merge(data_full, data9, by = "xwaveid", all.x = TRUE) 
rm(data9, var9)


####  Wave 10  ####
data10  <- read_sav("Combined j210c.sav")
var10 <- c("xwaveid","jhhpxid","jhhcompi","jmrpdiv","jmrcms","jmrpsep",
           "jmschgdv","jmrcurr","jmrsdur","jmschgsp","jlesep","jmrpend")
data10 <- data10[,var10]
data_full <- merge(data_full, data10, by = "xwaveid", all.x = TRUE) 
rm(data10, var10)


####  Wave 11  ####
data11  <- read_sav("Combined k210c.sav")
var11 <- c("xwaveid","khhpxid","khhcompi","kmrpdiv","kmrcms","kmrpsep",
           "kmschgdv","kmrcurr","kmrsdur","kmschgsp","klesep","kmrpend")
data11 <- data11[,var11]
data_full <- merge(data_full, data11, by = "xwaveid", all.x = TRUE) 
rm(data11, var11)

gc()
####  Wave 12  ####
data12  <- read_sav("Combined l210c.sav")
var12 <- c("xwaveid","lhhpxid","lhhcompi","lmrpdiv","lmrcms","lmrpsep",
           "lmschgdv","lmrcurr","lmrsdur","lmschgsp","llesep","lmrpend")
data12 <- data12[,var12]
data_full <- merge(data_full, data12, by = "xwaveid", all.x = TRUE) 
rm(data12, var12)


####  Wave 13  ####
data13  <- read_sav("Combined m210c.sav")
var13 <- c("xwaveid","mhhpxid","mhhcompi","mmrpdiv","mmrcms","mmrpsep",
           "mmschgdv","mmrcurr","mmrsdur","mmschgsp","mlesep","mmrpend")
data13 <- data13[,var13]
data_full <- merge(data_full, data13, by = "xwaveid", all.x = TRUE) 
rm(data13, var13)
gc()

####  Wave 14  ####
data14  <- read_sav("Combined n210c.sav")
var14 <- c("xwaveid","nhhpxid","nhhcompi","nmrpdiv","nmrcms","nmrpsep",
           "nmschgdv","nmrcurr","nmrsdur","nmschgsp","nlesep","nmrpend")
data14 <- data14[,var14]
data_full <- merge(data_full, data14, by = "xwaveid", all.x = TRUE) 
rm(data14, var14)


####  Wave 15  ####
data15  <- read_sav("Combined o210c.sav")
var15 <- c("xwaveid","ohhpxid","ohhcompi","omrpdiv","omrcms","omrpsep",
           "omschgdv","omrcurr","omrsdur","omschgsp","olesep","omrpend")
data15 <- data15[,var15]
data_full <- merge(data_full, data15, by = "xwaveid", all.x = TRUE) 
rm(data15, var15)
gc()

####  Wave 16  ####
data16  <- read_sav("Combined p210c.sav")
var16 <- c("xwaveid","phhpxid","phhcompi","pmrpdiv","pmrcms","pmrpsep",
           "pmschgdv","pmrcurr","pmrsdur","pmschgsp","plesep","pmrpend")
data16 <- data16[,var16]
data_full <- merge(data_full, data16, by = "xwaveid", all.x = TRUE) 
rm(data16, var16)


####  Wave 17  ####
data17  <- read_sav("Combined q210c.sav")
var17 <- c("xwaveid","qhhpxid","qhhcompi","qmrpdiv","qmrcms","qmrpsep",
           "qmschgdv","qmrcurr","qmrsdur","qmschgsp","qlesep","qmrpend")
data17 <- data17[,var17]
data_full <- merge(data_full, data17, by = "xwaveid", all.x = TRUE) 
rm(data17, var17)
gc()

####  Wave 18  ####
data18  <- read_sav("Combined r210c.sav")
var18 <- c("xwaveid","rhhpxid","rhhcompi","rmrpdiv","rmrcms","rmrpsep",
           "rmschgdv","rmrcurr","rmrsdur","rmschgsp","rlesep","rmrpend")
data18 <- data18[,var18]
data_full <- merge(data_full, data18, by = "xwaveid", all.x = TRUE) 
rm(data18, var18)


####  Wave 19  ####
data19  <- read_sav("Combined s210c.sav")
var19 <- c("xwaveid","shhpxid","shhcompi","smrpdiv","smrcms","smrpsep",
           "smschgdv","smrcurr","smrsdur","smschgsp","slesep","smrpend")
data19 <- data19[,var19]
data_full <- merge(data_full, data19, by = "xwaveid", all.x = TRUE) 
rm(data19, var19)
gc()

####  Wave 20  ####
data20  <- read_sav("Combined t210c.sav")
var20 <- c("xwaveid","thhpxid","thhcompi","tmrpdiv","tmrcms","tmrpsep",
           "tmschgdv","tmrcurr","tmrsdur","tmschgsp","tlesep","tmrpend")
data20 <- data20[,var20]
data_full <- merge(data_full, data20, by = "xwaveid", all.x = TRUE) 
rm(data20, var20)


####  Wave 21  ####
data21  <- read_sav("Combined u210c.sav")
var21 <- c("xwaveid","uhhpxid","uhhcompi","umrpdiv","umrcms","umrpsep",
           "umschgdv","umrcurr","umrsdur","umschgsp","ulesep","umrpend")
data21 <- data21[,var21]
data_full <- merge(data_full, data21, by = "xwaveid", all.x = TRUE) 
rm(data21, var21)


# _______________________________________________                           ####                            
#
# merging partners
# _______________________________________________                           ####

# A script for adding partner variables was provided by the Melbourne Institute
# https://melbourneinstitute.unimelb.edu.au/hilda/for-data-users/program-library
# Program 8

# removing cases without partners cross wave identifier in wave 1
data_full <- subset(data_full, ahhpxid != "")

# Create data sets with the variables of one partner
partner <- data_full
partner$ahhpxid = NULL # not needed for merging

# Merge partners
data_partner <- merge(partner, data_full, by.x = "xwaveid", by.y = "ahhpxid", 
                      suffixes = c("", "_p"), all.x = TRUE)  # non responding person gets _p
data_partner <- rename(data_partner, xwaveid_r = xwaveid) # respondent identifier

# Gender-specific renaming (there are no same-sex couples after data cleaning)
# getting all respondent variables (no "_p")
vars_r <- setdiff(
  names(data_partner),
  grep("_p$", names(data_partner), value = TRUE)) # extract names from data_partner, ecxluding suffixes "_p"
vars_r <- vars_r[!vars_r %in% c("xwaveid_r")] # does not have to be renamed
for (v in vars_r) {                      # for all responding variables
  vp <- paste0(v, "_p")                  # creating partner variables
  if (vp %in% names(data_partner)) {
    
    data_partner[[paste0(v, "_m")]] <-   # creating male variables
      ifelse(data_partner$ahgsex == 1,
             data_partner[[v]],
             data_partner[[vp]])
    
    data_partner[[paste0(v, "_f")]] <-  # creating female variables
      ifelse(data_partner$ahgsex == 2,
             data_partner[[v]],
             data_partner[[vp]])
  }
}

# create couple's id
data_partner$aa_coupleid <- paste(
  pmin(data_partner$xwaveid_r, data_partner$xwaveid_p),
  pmax(data_partner$xwaveid_r, data_partner$xwaveid_p),
  sep = "_")

# delete variables that do not end with _m or _f
vars_drop <- setdiff(
  names(data_partner),
  grep("_(m|f)$", names(data_partner), value = TRUE))
vars_drop <- setdiff(vars_drop, "aa_coupleid")       # keep couple id
data_partner <- data_partner[, !(names(data_partner) %in% vars_drop)]

# delete duplicate lines
data_partner <- data_partner[!duplicated(data_partner$aa_coupleid),]

# sort column names
data_partner <- data_partner[sort(names(data_partner), decreasing = F)]

# control
any(grepl("_p$", names(data_partner)))    # FALSE
any(!grepl("_(m|f)$", names(data_partner)) &
      names(data_partner) != "aa_coupleid") # FALSE

table(data_partner$ahgsex_m)
table(data_partner$ahgsex_f)
data_partner$same_sex <- ifelse(data_partner$ahgsex_f == data_partner$ahgsex_m, 1, 0)
table(data_partner$same_sex) # same sex couples will no longer be in data after data cleaning
data_partner <- data_partner %>% select(-same_sex)

# Save  data set
setwd("C:/Users/User/OneDrive/Uni/Master/5. Semester/Masterarbeit/Auswertung/data_partner")
save(data_partner, file = "data_partner.Rdata")
                        
setwd("C:/Users/User/OneDrive/Uni/Master/5. Semester/Masterarbeit/Auswertung/data_partner")
data_partner <- get(load("data_partner.Rdata"))

# _______________________________________________                           ####                            
#
# cleaning data
# _______________________________________________                           ####

#### removing couples that are not married in wave 1 ####

# using ahgms (1: married and living together, 7: married and not living together) 
# in data_partner there is only 1,2,7 -> cases with 2 have to be deleted
table(data_partner$ahgms_f)
table(data_partner$ahgms_m)
sum(is.na(data_partner$ahgms_f))
sum(is.na(data_partner$ahgms_m))

str(data_partner$ahgms_f)
data_partner$ahgms_f <- as.factor(data_partner$ahgms_f)
data_partner <- subset(data_partner, ahgms_f != "2")
data_partner$ahgms_m <- as.factor(data_partner$ahgms_m)
data_partner <- subset(data_partner, ahgms_m != "2")


#### removing couples that did not participate in last interview ####
# using uhhcompi (date of last interview)
sum(is.na(data_partner$uhhcompi_f))
sum(is.na(data_partner$uhhcompi_m))
data_partner <- subset(data_partner, !(is.na(uhhcompi_f) & is.na(uhhcompi_m)))

# _______________________________________________                           ####
#
# creating outcome
# _______________________________________________                           ####

#### status indicator ####
# 1: divorced or separated, 0: still married

### mrcms (1: Married; 2: Separated but not divorced; 3: Divorced; 4: Widowed)

# divorced
mrcms <- grep("mrcms", names(data_partner), value = TRUE)
data_partner$status_mrcms_div <- apply(data_partner[, mrcms, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)           # if all NA -> NA
  if(any(x %in% 3)) return(1)            # if one is 2 or 3 -> 1
  return(0)})                            # if 1 or 4 -> 0
table(data_partner$status_mrcms_div)
sum(is.na(data_partner$status_mrcms_div))

# separated
data_partner$status_mrcms_sep <- apply(data_partner[, mrcms, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)           # if all NA -> NA
  if(any(x %in% c(2,3))) return(1)       # if one is 2 or 3 -> 1
  return(0)})                            # if 1 or 4 -> 0
table(data_partner$status_mrcms_sep)
sum(is.na(data_partner$status_mrcms_sep))


### mrpend (1:Married; 2: Separated; 3: Divorced; 4: Widowed)

# divorced
mrpend <- grep("mrpend", names(data_partner), value = TRUE)
data_partner$status_mrpend_div <- apply(data_partner[, mrpend, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)           # if all NA -> NA
  if(any(x %in% 3)) return(1)            # if one is 3 -> 1
  return(0)})                            # else -> 0
table(data_partner$status_mrpend_div)
sum(is.na(data_partner$status_mrpend_div))

# separated
data_partner$status_mrpend_sep <- apply(data_partner[, mrpend, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)           # if all NA -> NA
  if(any(x %in% c(2,3))) return(1)       # if one is 2 or 3 -> 1
  return(0)})                            # else -> 0
table(data_partner$status_mrpend_sep)
sum(is.na(data_partner$status_mrpend_sep))


### mrpsep (year of separation)
mrpsep <- grep("mrpsep", names(data_partner), value = TRUE)
data_partner$status_mrpsep <- apply(data_partner[, mrpsep, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(0)     # only NA -> 0
  return(1) })                    # else → 1
table(data_partner$status_mrpsep)


### mrpdiv (year of divorce)
mrpdiv <- grep("mrpdiv", names(data_partner), value = TRUE)
data_partner$status_mrpdiv <- apply(data_partner[, mrpdiv, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(0)     # only NA -> 0
  return(1) })                    # else → 1
table(data_partner$status_mrpdiv)


### mschgdv (divorced since last interview, binary)
mschgdv <- grep("mschgdv", names(data_partner), value = TRUE)
data_partner$status_mschgdv <- apply(data_partner[, mschgdv, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)               # only NA -> NA
  if(any(x %in% 1)) return(1)                # if one is 1 -> 1
  return(0)})                                # else -> 0
table(data_partner$status_mschgdv)
sum(is.na(data_partner$status_mschgdv))


### mrcurr (1: Legally married; 2: De facto; 3: Separated; 4: Divorced; 5: Widowed;)

# divorced
mrcurr <- grep("mrcurr", names(data_partner), value = TRUE)
data_partner$status_mrcurr_div <- apply(data_partner[, mrcurr, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)           # if all NA -> NA
  if(any(x %in% 4)) return(1)            # if one is 4 -> 1
  return(0)})                            # else -> 0
table(data_partner$status_mrcurr_div)
sum(is.na(data_partner$status_mrcurr_div))

# separated
data_partner$status_mrcurr_sep <- apply(data_partner[, mrcurr, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)           # if all NA -> NA
  if(any(x %in% c(3,4))) return(1)       # if one is 3 or 4 -> 1
  return(0)})                            # else -> 0
table(data_partner$status_mrcurr_sep)
sum(is.na(data_partner$status_mrcurr_sep))

### mrsdur (year since separation / divorce)
mrsdur <- grep("mrsdur", names(data_partner), value = TRUE)
data_partner$status_mrsdur <- apply(data_partner[, mrsdur, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(0)     # only NA -> 0
  return(1) })                    # else → 1
table(data_partner$status_mrsdur)

### mschgsp (separated from marriage, binary)
mschgsp <- grep("mschgsp", names(data_partner), value = TRUE)
data_partner$status_mschgsp <- apply(data_partner[, mschgsp, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)               # only NA -> NA
  if(any(x %in% 1)) return(1)                # if one is 1 -> 1
  return(0)})                                # else -> 0
table(data_partner$status_mschgsp)
sum(is.na(data_partner$status_mschgsp))

### lesep (separated from spouse, binary)
lesep <- grep("lesep", names(data_partner), value = TRUE)
data_partner$status_lesep <- apply(data_partner[, lesep, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)               # only NA -> NA
  if(any(x %in% 2)) return(1)                # if one is 2 -> 1
  return(0)})                                # else -> 0
table(data_partner$status_lesep)
sum(is.na(data_partner$status_lesep))

### creating data frame with status indicators

data_status <- data.frame(data_partner$status_mrcms_div, data_partner$status_mrcms_sep, 
                          data_partner$status_mrpend_div, data_partner$status_mrpend_sep, 
                          data_partner$status_mrpsep, data_partner$status_mrpdiv, 
                          data_partner$status_mschgdv,data_partner$status_mrcurr_div, 
                          data_partner$status_mrcurr_sep, data_partner$status_mrsdur, 
                          data_partner$status_mschgsp, data_partner$status_lesep)

#### creating status 
sep <- grep("data_partner",names(data_status), value = TRUE)
data_status$status <- apply(data_status[, sep, drop = FALSE], 1, function(x) {
  if(all(is.na(x))) return(NA)               # only NA -> NA
  if(any(x %in% 1)) return(1)                # if one is 1 -> 1
  return(0)})                                # else -> 0
table(data_status$status)
sum(is.na(data_status$status))


#### survival time ####

### mrpdiv (year of divorce) 
# there was one person who stated 1992
data_partner$fmrpdiv_m[data_partner$fmrpdiv_m < 2001] <- NA
data_partner$hmrpdiv_m[data_partner$hmrpdiv_m < 2001] <- NA
data_partner$imrpdiv_m[data_partner$imrpdiv_m < 2001] <- NA
data_partner$jmrpdiv_m[data_partner$jmrpdiv_m < 2001] <- NA
data_partner$kmrpdiv_m[data_partner$kmrpdiv_m < 2001] <- NA

mrpdiv_time <- grep("^[a-u]mrpdiv", names(data_partner), value = TRUE)
mrpdiv_var <- data_partner[mrpdiv_time]
mrpdiv_var[] <- lapply(mrpdiv_var, as.numeric)    # numeric
min_year_mrpdiv <- do.call(pmin, c(mrpdiv_var, na.rm = TRUE))
data_partner$year_mrpdiv <- ifelse(is.na(min_year_mrpdiv), 20, min_year_mrpdiv - 2001) # minimum of year, else -> 20
table(data_partner$year_mrpdiv)


### mrpsep (year of separtation)
# same person stated 1991
data_partner$fmrpsep_m[data_partner$fmrpsep_m < 2001] <- NA
data_partner$hmrpsep_m[data_partner$hmrpsep_m < 2001] <- NA
data_partner$imrpsep_m[data_partner$imrpsep_m < 2001] <- NA
data_partner$jmrpsep_m[data_partner$jmrpsep_m < 2001] <- NA
data_partner$kmrpsep_m[data_partner$kmrpsep_m < 2001] <- NA

mrpsep_time <- grep("^[a-u]mrpsep", names(data_partner), value = TRUE)
mrpsep_var <- data_partner[mrpsep_time]
mrpsep_var[] <- lapply(mrpsep_var, as.numeric)    # numeric
min_year_mrpsep <- do.call(pmin, c(mrpsep_var, na.rm = TRUE))
data_partner$year_mrpsep <- ifelse(is.na(min_year_mrpsep), 20, min_year_mrpsep - 2001) # minimum of year, else -> 20
table(data_partner$year_mrpsep)
sum(is.na(min_year_mrpsep))

### mrpend (creating time out of wave)
mrpend_time <- grep("^[a-u]mrpend", names(data_partner), value = TRUE)
years_mrpend <- match(substr(mrpend_time, 1, 1), letters) + 2000  # calculating wave years
data_partner$year_mrpend <- apply(data_partner[mrpend_time], 1, function(x) {
  event <- which(x %in% c(2, 3))              # event (separation if 2 or 3)
  if (length(event) > 0) {
    return(years_mrpend[event[1]] - 2001)}     # if event -> year-2001
  if (all(is.na(x))) return(NA)                # if all NA -> NA
  return(20)})                                 # else -> 20
table(data_partner$year_mrpend)
sum(is.na(data_partner$year_mrpend))


### lesep (creating time out of wave)
lesep_time <- grep("^[a-u]lesep", names(data_partner), value = TRUE)
years_lesep <- match(substr(lesep_time, 1, 1), letters) + 2000  # calculating wave years
data_partner$year_lesep <- apply(data_partner[lesep_time], 1, function(x) {
  event <- which(x %in% 2)                    # event (separation if 2 )
  if (length(event) > 0) {
    return(years_lesep[event[1]] - 2001)}      # if event -> year-2001
  if (all(is.na(x))) return(NA)                # if all NA -> NA
  return(20)})                                 # else -> 20
table(data_partner$year_lesep)
sum(is.na(data_partner$year_lesep))


### creating data frame with survival time
data_time <- data.frame(data_partner$year_mrpdiv, data_partner$year_mrpsep, 
                        data_partner$year_mrpend, data_partner$year_lesep)

### creating survival time
year <- grep("data_partner", names(data_time), value = TRUE)
data_time$time <- apply(data_time[, year, drop = FALSE], 1, function(x) {
  if (all(is.na(x))) return(NA_real_)      # all NA → NA
  x_valid <- x[!is.na(x)]                  # creating time without NA
  if (any(x_valid < 20)) {
    return(min(x_valid[x_valid < 20]))}    # if any <20 -> smallest time
  return(20)})
data_time$time <- data_time$time + 1       # adding 1 because survival analysis cannot deal with time = 0
table(data_time$time) # 1662 haben 20
sum(is.na(data_time$time))


#### data frame of outcomes ####
data_outcome <- data.frame(aa_coupleid = data_partner$aa_coupleid,
                           status = data_status$status,
                           time = data_time$time)

rm(data_full, partner, mrpdiv_var, mrpsep_var)

# _______________________________________________                           ####
#
# creating data
# _______________________________________________                           ####

data <- merge(data_partner, data_outcome, by = "aa_coupleid", all.x = TRUE) 

#### deleting variables that are only needed once ####
data <- data %>% select(-ahhd0_4_m, -ahhd5_9_m, -ahhd1014_m, -ahhd1524_m, -ahhad10_m, 
                        -ahhec10_m, -ahhed10_m, -ahhpers_m, -ahhsos_m, -ahifisi_m,
                        -ahifnisi_m, -ahifditp_m, -amvncar_m, -amrcdur_m)
data <- data %>% 
  rename(
    ahhd0_4x = ahhd0_4_f,
    ahhd5_9x = ahhd5_9_f,
    ahhd1014x = ahhd1014_f,
    ahhd1524x = ahhd1524_f,
    ahhad10x = ahhad10_f,
    ahhec10x = ahhec10_f,
    ahhed10x = ahhed10_f,
    ahhpers = ahhpers_f,
    ahhsos = ahhsos_f,
    ahifisi = ahifisi_f,
    ahifnisi = ahifnisi_f,
    ahifditp = ahifditp_f,
    amvncar = amvncar_f,
    amrcdur = amrcdur_f)

rm(data_partner, data_outcome, data_status, data_time)


#### creating new variables ####

### number of siblings
# creating new categorie (0 = having no siblings)
data$afmsib_f <- data$afmnsib_f
data$afmsib_f[data$afmhsib_f == "2"] <- 0 # if no siblings (afmhsib = 2) -> 0
table(data$afmsib_f)

data$afmsib_m <- data$afmnsib_m
data$afmsib_m[data$afmhsib_m == "2"] <- 0
table(data$afmsib_m)


### premarital cohabitation
# creating new categorie (0 = no premarital cohabitation)
data$amrchb_f <- data$amrplvt_f
data$amrchb_f[data$amrplv_f == "2"] <- 0 # if no cohabitation (amrplv = 2) -> 0

data$amrchb_m <- data$amrplvt_m
data$amrchb_m[data$amrplv_m == "2"] <- 0


### brief country of birth father
table(data$afmfcob_f)
table(data$afmfcob_m)
# 1: Australia -> 1101
# 2: English speaking (United Kingdom, New Zealand, Canada, USA, Ireland and South Africa)
#       -> 1201, 2100, 2201, 8102, 8104, 9225
# 3: Other 
# coding of afmfcob from Release 21 Selected Standard Classifications used in the HILDA Survey
data$afmfbcob_f <- ifelse(data$afmfcob_f == 1101, "1",
                          ifelse(data$afmfcob_f == 1201 | 
                                 data$afmfcob_f == 2100 | 
                                 data$afmfcob_f == 2201 | 
                                 data$afmfcob_f == 8102 | 
                                 data$afmfcob_f == 8104 |
                                 data$afmfcob_f == 9225, "2", "3"))

data$afmfbcob_m <- ifelse(data$afmfcob_m == 1101, "1",
                          ifelse(data$afmfcob_m == 1201 | 
                                 data$afmfcob_m == 2100 | 
                                 data$afmfcob_m == 2201 | 
                                 data$afmfcob_m == 8102 | 
                                 data$afmfcob_m == 8104 |
                                 data$afmfcob_m == 9225, "2", "3"))

### brief country of birth mother
data$afmmbcob_f <- ifelse(data$afmmcob_f == 1101, "1",
                          ifelse(data$afmmcob_f == 1201 | 
                                 data$afmmcob_f == 2100 | 
                                 data$afmmcob_f == 2201 | 
                                 data$afmmcob_f == 8102 | 
                                 data$afmmcob_f == 8104 |
                                 data$afmmcob_f == 9225, "2", "3"))

data$afmmbcob_m <- ifelse(data$afmmcob_m == 1101, "1",
                          ifelse(data$afmmcob_m == 1201 | 
                                 data$afmmcob_m == 2100 | 
                                 data$afmmcob_m == 2201 | 
                                 data$afmmcob_m == 8102 | 
                                 data$afmmcob_m == 8104 |
                                 data$afmmcob_m == 9225, "2", "3"))

### father's occupation
# creating new categorie (0 = 2 in afmfemp and NA in afmfo61) -> not employed
data$afmfo61_f[data$afmfemp_f == "2" & is.na(data$afmfo61_f)] <- 0
table(data$afmfo61_f)

data$afmfo61_m[data$afmfemp_m == "2" & is.na(data$afmfo61_m)] <- 0
table(data$afmfo61_m)


### mother's occupation
# creating new categorie (0 = 2 in afmmemp and NA in afmmo61) -> not employed
data$afmmo61_f[data$afmmemp_f == "2" & is.na(data$afmmo61_f)] <- 0
table(data$afmmo61_f)

data$afmmo61_m[data$afmmemp_m == "2" & is.na(data$afmmo61_m)] <- 0
table(data$afmmo61_m)


### own occupation
# creating new categorie ( 0 = 3,4,5 or 6 in ahges and NA in ajbmo61) -> not employed
data$ajbmo61_f[data$ahges_f == "3" & is.na(data$ajbmo61_f) |
               data$ahges_f == "4" & is.na(data$ajbmo61_f) |
               data$ahges_f == "5" & is.na(data$ajbmo61_f) | 
               data$ahges_f == "6" & is.na(data$ajbmo61_f)] <- 0
table(data$ajbmo61_f)

data$ajbmo61_m[data$ahges_m == "3" & is.na(data$ajbmo61_m) |
               data$ahges_m == "4" & is.na(data$ajbmo61_m) |
               data$ahges_m == "5" & is.na(data$ajbmo61_m) | 
               data$ahges_m == "6" & is.na(data$ajbmo61_m)] <- 0
table(data$ajbmo61_m)


### being the oldest child
# creating new categorie (0 = having no siblings)
table(data$afmeldso_f)
table(data$afmsib_f)
data$afmeldso_f[data$afmsib_f == "0"] <- 0
table(data$afmeldso_f)

data$afmeldso_m[data$afmsib_m == "0"] <- 0
table(data$afmeldso_m)


### age of oldest child
# oldest non resident child
ancage_f <- grep("^ancage[0-9]+_f$", names(data), value = TRUE)
ancage_var_f <- data[ancage_f]
data$ancold_f <- do.call(pmax, c(ancage_var_f, na.rm = TRUE))

ancage_m <- grep("^ancage[0-9]+_m$", names(data), value = TRUE)
ancage_var_m <- data[ancage_m]
data$ancold_m <- do.call(pmax, c(ancage_var_m, na.rm = TRUE))

# oldest resident child
arcage_f <- grep("^arcage[0-9]+_f$", names(data), value = TRUE)
arcage_var_f <- data[arcage_f]
data$arcold_f <- do.call(pmax, c(arcage_var_f, na.rm = TRUE))

arcage_m <- grep("^arcage[0-9]+_m$", names(data), value = TRUE)
arcage_var_m <- data[arcage_m]
data$arcold_m <- do.call(pmax, c(arcage_var_m, na.rm = TRUE))

# oldest child
atcold_var_f <- data.frame(data$arcold_f, data$ancold_f)
data$atcold_f <- do.call(pmax, c(atcold_var_f, na.rm = TRUE))
table(data$atcold_f)

atcold_var_m <- data.frame(data$arcold_m, data$ancold_m)
data$atcold_m <- do.call(pmax, c(atcold_var_m, na.rm = TRUE))
table(data$atcold_m)

rm(ancage_var_f, ancage_var_m, arcage_var_f, arcage_var_m, atcold_var_f, atcold_var_m)


### premarital childbirth
# 1: yes (atcold > amrcdur)
# 2: no (atcold < amrcdur or atchad = 0 (no children) )
data$apmchb_f <- ifelse(data$atcold_f > data$amrcdur, 1, 2)
data$apmchb_f[data$atchad_f == "0"] <- 2

data$apmchb_m <- ifelse(data$atcold_m > data$amrcdur, 1, 2)
data$apmchb_m[data$atchad_m == "0"] <- 2


### age at marriage
# ahgage (age) - amrcdur (length of marriage)
data$amage_f <- data$ahgage_f - data$amrcdur
data$amage_f <- round(data$amage_f, 0)

data$amage_m <- data$ahgage_m - data$amrcdur
data$amage_m <- round(data$amage_m, 0)


### mean and sd of age
mean(data$ahgage_f)
sd(data$ahgage_f)
table(data$ahgage_f)
mean(data$ahgage_m)
sd(data$ahgage_m)
table(data$ahgage_m)


#### deleting not needed variables ####

# variables that are used for creating outcomes or new variables
data <- data %>% select(-status_mrcms_div, -status_lesep, -status_mrcms_sep,
                        -status_mrcurr_div, -status_mrcurr_sep, -status_mrpdiv,
                        -status_mrpend_div, -status_mrpend_sep, -status_mrpsep,
                        -status_mrsdur, -status_mschgdv, -status_mschgsp, -year_lesep, 
                        -year_mrpdiv, -year_mrpend, -year_mrpsep, -bhhpxid_m, 
                        -bhhpxid_f, -chhpxid_m, -chhpxid_f, -dhhpxid_m, -dhhpxid_f, 
                        -ehhpxid_m, -ehhpxid_f, -fhhpxid_m, -fhhpxid_f, -ghhpxid_m, 
                        -ghhpxid_f, -hhhpxid_m, -hhhpxid_f, -ihhpxid_m, -ihhpxid_f, 
                        -jhhpxid_m, -jhhpxid_f, -khhpxid_m, -khhpxid_f, -lhhpxid_m, 
                        -lhhpxid_f, -mhhpxid_m, -mhhpxid_f, -nhhpxid_m, -nhhpxid_f, 
                        -ohhpxid_m, -ohhpxid_f, -phhpxid_m, -phhpxid_f, -qhhpxid_m,
                        -qhhpxid_f, -rhhpxid_m, -rhhpxid_f, -shhpxid_m, -shhpxid_f, 
                        -thhpxid_m, -thhpxid_f, -uhhpxid_m, -uhhpxid_f, -ahhcompi_m, 
                        -ahhcompi_f, -bhhcompi_m, -bhhcompi_f, -chhcompi_m, -chhcompi_f, 
                        -dhhcompi_m, -dhhcompi_f, -ehhcompi_m, -ehhcompi_f, -fhhcompi_m, 
                        -fhhcompi_f, -ghhcompi_m, -ghhcompi_f, -hhhcompi_m, -hhhcompi_f, 
                        -ihhcompi_m, -ihhcompi_f, -jhhcompi_m, -jhhcompi_f, -khhcompi_m, 
                        -khhcompi_f, -lhhcompi_m, -lhhcompi_f, -mhhcompi_m, -mhhcompi_f, 
                        -nhhcompi_m, -nhhcompi_f, -ohhcompi_m, -ohhcompi_f, -phhcompi_m, 
                        -phhcompi_f, -qhhcompi_m, -qhhcompi_f, -rhhcompi_m, -rhhcompi_f, 
                        -shhcompi_m, -shhcompi_f, -thhcompi_m, -thhcompi_f,-uhhcompi_m, 
                        -uhhcompi_f, -ahgms_f, -ahgms_m, -amrpend_m, -amrpend_f, 
                        -bmrpend_m, -bmrpend_f, -cmrpend_m, -cmrpend_f, -dmrpend_m, 
                        -dmrpend_f, -emrpend_m, -emrpend_f, -fmrpend_m, -fmrpend_f, 
                        -gmrpend_m, -gmrpend_f, -hmrpend_m, -hmrpend_f, -imrpend_m, 
                        -imrpend_f, -jmrpend_m, -jmrpend_f, -kmrpend_m, -kmrpend_f, 
                        -lmrpend_m, -lmrpend_f, -mmrpend_m, -mmrpend_f, -nmrpend_m, 
                        -nmrpend_f, -omrpend_m, -omrpend_f, -pmrpend_m, -pmrpend_f, 
                        -qmrpend_m, -qmrpend_f, -rmrpend_m, -rmrpend_f, -smrpend_m, 
                        -smrpend_f, -tmrpend_m, -tmrpend_f, -umrpend_m, -umrpend_f, 
                        -amrpsep_m, -amrpsep_f, -bmrpsep_m, -bmrpsep_f, -cmrpsep_m, 
                        -cmrpsep_f, -dmrpsep_m, -dmrpsep_f, -emrpsep_m, -emrpsep_f, 
                        -fmrpsep_m, -fmrpsep_f, -gmrpsep_m, -gmrpsep_f, -hmrpsep_m, 
                        -hmrpsep_f, -imrpsep_m, -imrpsep_f, -jmrpsep_m, -jmrpsep_f, 
                        -kmrpsep_m, -kmrpsep_f, -lmrpsep_m, -lmrpsep_f, -mmrpsep_m, 
                        -mmrpsep_f, -nmrpsep_m, -nmrpsep_f, -omrpsep_m, -omrpsep_f, 
                        -pmrpsep_m, -pmrpsep_f, -qmrpsep_m, -qmrpsep_f, -rmrpsep_m, 
                        -rmrpsep_f, -smrpsep_m, -smrpsep_f, -tmrpsep_m, -tmrpsep_f, 
                        -umrpsep_m, -umrpsep_f, -amrpdiv_m, -amrpdiv_f, -bmrpdiv_m, 
                        -bmrpdiv_f, -cmrpdiv_m, -cmrpdiv_f, -dmrpdiv_m, -dmrpdiv_f, 
                        -emrpdiv_m, -emrpdiv_f, -fmrpdiv_m, -fmrpdiv_f, -gmrpdiv_m, 
                        -gmrpdiv_f, -hmrpdiv_m, -hmrpdiv_f, -imrpdiv_m, -imrpdiv_f, 
                        -jmrpdiv_m, -jmrpdiv_f, -kmrpdiv_m, -kmrpdiv_f, -lmrpdiv_m, 
                        -lmrpdiv_f, -mmrpdiv_m, -mmrpdiv_f, -nmrpdiv_m, -nmrpdiv_f, 
                        -omrpdiv_m, -omrpdiv_f, -pmrpdiv_m, -pmrpdiv_f, -qmrpdiv_m, 
                        -qmrpdiv_f, -rmrpdiv_m, -rmrpdiv_f, -smrpdiv_m, -smrpdiv_f, 
                        -tmrpdiv_m, -tmrpdiv_f, -umrpdiv_m, -umrpdiv_f, -amrcurr_m, 
                        -amrcurr_f, -bmrcurr_m, -bmrcurr_f, -cmrcurr_m, -cmrcurr_f, 
                        -dmrcurr_m, -dmrcurr_f, -emrcurr_m, -emrcurr_f, -fmrcurr_m, 
                        -fmrcurr_f, -gmrcurr_m, -gmrcurr_f, -hmrcurr_m, -hmrcurr_f, 
                        -imrcurr_m, -imrcurr_f, -jmrcurr_m, -jmrcurr_f, -kmrcurr_m, 
                        -kmrcurr_f, -lmrcurr_m, -lmrcurr_f, -mmrcurr_m, -mmrcurr_f, 
                        -nmrcurr_m, -nmrcurr_f, -omrcurr_m, -omrcurr_f, -pmrcurr_m, 
                        -pmrcurr_f, -qmrcurr_m, -qmrcurr_f, -rmrcurr_m, -rmrcurr_f, 
                        -smrcurr_m, -smrcurr_f, -tmrcurr_m, -tmrcurr_f, -umrcurr_m, 
                        -umrcurr_f, -amrsdur_m, -amrsdur_f, -bmrsdur_m, -bmrsdur_f, 
                        -cmrsdur_m, -cmrsdur_f, -dmrsdur_m, -dmrsdur_f, -emrsdur_m, 
                        -emrsdur_f, -fmrsdur_m, -fmrsdur_f, -gmrsdur_m, -gmrsdur_f, 
                        -hmrsdur_m, -hmrsdur_f, -imrsdur_m, -imrsdur_f, -jmrsdur_m, 
                        -jmrsdur_f, -kmrsdur_m, -kmrsdur_f, -lmrsdur_m, -lmrsdur_f, 
                        -mmrsdur_m, -mmrsdur_f, -nmrsdur_m, -nmrsdur_f, -omrsdur_m, 
                        -omrsdur_f, -pmrsdur_m, -pmrsdur_f, -qmrsdur_m, -qmrsdur_f, 
                        -rmrsdur_m, -rmrsdur_f, -smrsdur_m, -smrsdur_f, -tmrsdur_m, 
                        -tmrsdur_f, -umrsdur_m, -umrsdur_f, -cmschgdv_m, -cmschgdv_f, 
                        -dmschgdv_m, -dmschgdv_f, -emschgdv_m, -emschgdv_f, -fmschgdv_m, 
                        -fmschgdv_f, -gmschgdv_m, -gmschgdv_f, -hmschgdv_m, -hmschgdv_f, 
                        -imschgdv_m, -imschgdv_f, -jmschgdv_m, -jmschgdv_f, -kmschgdv_m, 
                        -kmschgdv_f, -lmschgdv_m, -lmschgdv_f, -mmschgdv_m, -mmschgdv_f, 
                        -nmschgdv_m, -nmschgdv_f, -omschgdv_m, -omschgdv_f, -pmschgdv_m, 
                        -pmschgdv_f, -qmschgdv_m, -qmschgdv_f, -rmschgdv_m, -rmschgdv_f, 
                        -smschgdv_m, -smschgdv_f, -tmschgdv_m, -tmschgdv_f, -umschgdv_m, 
                        -umschgdv_f, -bmrcms_m, -bmrcms_f, -cmrcms_m, -cmrcms_f, -dmrcms_m, 
                        -dmrcms_f, -emrcms_m, -emrcms_f, -fmrcms_m, -fmrcms_f, -gmrcms_m, 
                        -gmrcms_f, -hmrcms_m, -hmrcms_f, -imrcms_m, -imrcms_f, -jmrcms_m, 
                        -jmrcms_f, -kmrcms_m, -kmrcms_f, -lmrcms_m, -lmrcms_f, -mmrcms_m, 
                        -mmrcms_f, -nmrcms_m, -nmrcms_f, -omrcms_m, -omrcms_f, -pmrcms_m, 
                        -pmrcms_f, -qmrcms_m, -qmrcms_f, -rmrcms_m, -rmrcms_f, -smrcms_m, 
                        -smrcms_f, -tmrcms_m, -tmrcms_f, -umrcms_m, -umrcms_f, -cmschgsp_m, 
                        -cmschgsp_f, -dmschgsp_m, -dmschgsp_f, -emschgsp_m, -emschgsp_f, 
                        -fmschgsp_m, -fmschgsp_f, -gmschgsp_m, -gmschgsp_f, -hmschgsp_m, 
                        -hmschgsp_f, -imschgsp_m, -imschgsp_f, -jmschgsp_m, -jmschgsp_f, 
                        -kmschgsp_m,  -kmschgsp_f, -lmschgsp_m, -lmschgsp_f, -mmschgsp_m, 
                        -mmschgsp_f, -nmschgsp_m, -nmschgsp_f, -omschgsp_m, -omschgsp_f, 
                        -pmschgsp_m, -pmschgsp_f, -qmschgsp_m, -qmschgsp_f, -rmschgsp_m, 
                        -rmschgsp_f, -smschgsp_m, -smschgsp_f, -tmschgsp_m, -tmschgsp_f, 
                        -umschgsp_m, -umschgsp_f, -blesep_m, -blesep_f, -clesep_m, -clesep_f, 
                        -dlesep_m, -dlesep_f, -elesep_m, -elesep_f, -flesep_m, -flesep_f, 
                        -glesep_m, -glesep_f, -hlesep_m, -hlesep_f, -ilesep_m, -ilesep_f, 
                        -jlesep_m, -jlesep_f, -klesep_m, -klesep_f, -llesep_m, -llesep_f, 
                        -mlesep_m, -mlesep_f, -nlesep_m, -nlesep_f, -olesep_m, -olesep_f, 
                        -plesep_m, -plesep_f, -qlesep_m, -qlesep_f, -rlesep_m, -rlesep_f, 
                        -slesep_m, -slesep_f, -tlesep_m, -tlesep_f, -ulesep_m, -ulesep_f, 
                        -afmhsib_f, -afmhsib_m, -afmnsib_f, -afmnsib_m, -amrplv_m, -amrplv_f, 
                        -amrplvt_f, -amrplvt_m, -afmfcob_f, -afmfcob_m, -afmmcob_f, -afmmcob_m, 
                        -ahgsex_f, -ahgsex_m, -arcage1_f, -arcage2_f, -arcage3_f, -arcage4_f, 
                        -arcage5_f, -arcage6_f, -arcage7_f, -arcage8_f, -arcage9_f, -arcage10_f, 
                        -arcage1_m, -arcage2_m, -arcage3_m, -arcage4_m, -arcage5_m, -arcage6_m, 
                        -arcage7_m, -arcage8_m, -arcage9_m, -arcage10_m, -ancage1_f, -ancage2_f, 
                        -ancage3_f, -ancage4_f, -ancage5_f, -ancage6_f, -ancage7_f, -ancage8_f, 
                        -ancage9_f, -ancage10_f, -ancage11_f, -ancage12_f, -ancage13_f, -ancage1_m, 
                        -ancage2_m, -ancage3_m, -ancage4_m, -ancage5_m, -ancage6_m, -ancage7_m, 
                        -ancage8_m, -ancage9_m, -ancage10_m, -ancage11_m, -ancage12_m, -ancage13_m,
                        -ahgage_f, -ahgage_m, -arcold_f, -arcold_m, -ancold_f, -ancold_m, -aa_coupleid)

data <- data[sort(names(data), decreasing = F)]

# _______________________________________________                           ####
#
# variables
# _______________________________________________                           ####

#### descriptives ####
setwd("C:/Users/User/OneDrive/Uni/Master/5. Semester/Masterarbeit/Datensatz")
describe <- describe(data)
describe(data,getOption("max.print"))
data_desc <- as.data.frame(describe)
data_desc$variable <- rownames(data_desc)
rownames(data_desc) <- NULL
write.xlsx(data_desc,
          file = "describe_data.xlsx",
          rowNames = FALSE)

# calculating modus (for categorial variables)
get_modus <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])} # extracting the most frequently occurring value
modus <- sapply(data, get_modus) # getting modus for every column
print(modus)
data_modus <- as.data.frame(modus)
data_modus$variable <- rownames(data_modus)
rownames(data_modus) <- NULL
write.xlsx(data_modus,
          file = "modus_data.xlsx",
          rowNames = FALSE)

### correlations
data_cor <- data 
data_cor[] <- sapply(data_cor, as.numeric)
cor_matrix <- cor(data_cor, use = "pairwise.complete.obs")
cor_matrix_filtered <- cor_matrix
cor_matrix_filtered[abs(cor_matrix_filtered) < 0.5] <- NA 
View(cor_matrix_filtered)


#### transformationn of data type (categorial variables) #### 

cat_var <- c("ahges_f", "ahges_m", "ahhad10x", "ahhec10x","ahhed10x", "ahhsos", 
             "aanbcob_f", "aanbcob_m", "afmfbcob_f", "afmfbcob_m", "afmfemp_m", 
             "afmfemp_f", "afmfuemp_f", "afmfuemp_m","afmfo61_f", "afmfo61_m", 
             "afmmo61_f", "afmmo61_m", "afmmbcob_f", "afmmbcob_m", "afmmemp_f", 
             "afmmemp_m", "afmlwop_f", "afmlwop_m", "afmeldso_f", "afmeldso_m", 
             "afmpdiv_f", "afmpdiv_m", "aedhigh1_f", "aedhigh1_m", "afiprosp_f", 
             "afiprosp_m", "ahglth_f", "ahglth_m", "alsclub_f", "alsclub_m", 
             "alsdrink_f", "alsdrink_m", "alshaas_f","alshaas_m", "alshacf_f", 
             "alshacf_m", "alshadt_f", "alshadt_m", "alshahn_f", "alshahn_m", 
             "alshals_f", "alshals_m", "alshanb_f", "alshanb_m", "alslarea_f", 
             "alslarea_m", "alsothch_f", "alsothch_m", "alspact_f", "alspact_m", 
             "alsrush_f", "alsrush_m", "alsshare_f", "alsshare_m", "alssmoke_f", 
             "alssmoke_m", "alssocal_f", "alssocal_m", "alsstime_f", "alsstime_m", 
             "aordfpst_f", "aordfpst_m", "ajbmo61_f", "ajbmo61_m", "apmchb_f", "apmchb_m")
data[cat_var] <- lapply(data[cat_var], as.factor)

data$status <-as.integer(data$status)
str(data$status)

mean(is.na(data))

# Save  data set
setwd("C:/Users/User/OneDrive/Uni/Master/5. Semester/Masterarbeit/Auswertung/data")
save(data, file = "data.Rdata")

rm(data_cor, describe, data_desc, data_modus)

### import data
setwd("C:/Users/User/OneDrive/Uni/Master/5. Semester/Masterarbeit/Auswertung/data")
data <- get(load("data.Rdata"))


### for discussion
cor(data$ahgyob_m, data$aehtjb_m,  use = "pairwise.complete.obs")
cor(data$ahgyob_f, data$aehtjb_f,  use = "pairwise.complete.obs")

table(data$amrchb_f)
table(data$amrchb_m)

