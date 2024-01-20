
# AUTHORS:        Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files from shared REX Google drive Butterfly Surveys L0 folder
# DATA OUTPUT:    Clean L1 data uploaded to Butterfly Surveys L1 folder
# PROJECT:        LTER Butterflies
# DATE:           Dec 2023

# set working directory
setwd("~/Work/MSU Haddad R Directory/haddad-lab")

# read in data
bfly <- read.csv(file.choose(),header=T)

# check column names
colnames(bfly)

# check categories
xtabs(~Transect,data=bfly) # should be strip, walking, NA
xtabs(~Treatment,data=bfly) # should be CLE, T1-T4, T6, T7
xtabs(~CLE.diversity,data=bfly) # should be high, low, NA

# data checked and clean
# export as L1
write.csv(bfly.w, "LTER-butterflies\\LTER_butterflies_2019-2023_L1.csv", row.names=FALSE)
