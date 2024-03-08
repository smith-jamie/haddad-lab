
# AUTHORS:        Jamie Smith, Sean Fitzgerald, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    L2 dragonfly richness data, ready for analysis
# PROJECT:        LTER Dragonflies
# DATE:           March 2024

# set working directory
setwd("~/Work/MSU Haddad R Directory/haddad-lab")

# Load packages
library(dplyr)
library(reshape2)
library(ggplot2)

# read in L1 dragonfly data
dfly <- read.csv(file.choose(),header=T)

# walking transects, 12 min surveys
# exclude strip transects, 8 min surveys

# filter out strip transects
dfly.w <- filter(dfly,Transect=="walking")
xtabs(~Transect,data=dfly.w) # should be just "walking" now

# eliminate any unwanted species/taxa
# mostly for unknowns and higher taxa that couldn't be identified to species level
xtabs(~Species,data=dfly.w)

dfly.w <- filter(dfly.w,Species != "Dragonfly sp")
dfly.w <- filter(dfly.w,Species != "Darner sp")
dfly.w <- filter(dfly.w,Species != "NULL")
dfly.w <- filter(dfly.w,Species != "Pennant sp")

# reshape dataframe so that species counts are additional columns
colnames(dfly.w)
dfly.w <- dcast(dfly.w, Date+Treatment+Treatment_Name+Replicate+DistWater_m ~ Species)

# check format
head(dfly.w)

# make count data presence/absence (1/0) instead of counts per species
# use brackets to exclude non-count columns
# sapply will let us convert any number >0 to 1
colnames(dfly.w)
dfly.w[,6:19] <- sapply(dfly.w[,6:19], function(x) { as.numeric(x > 0) })

# check that counts have been changed to 1s
View(dfly.w)

# add richness column by summing no. species per row
dfly.w$Richness <- rowSums(dfly.w[,6:19])
colnames(dfly.w) # check for new column "Richness"

# species richness data ready for analysis
# export as L2 data
write.csv(dfly.w, "dragonflies\\LTER_dragonfly_richness_L2.csv", row.names=FALSE)
