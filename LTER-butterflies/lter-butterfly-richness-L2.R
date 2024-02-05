# formatting L1 data for analysis
# creating L2 richness data

# set working directory
setwd("~/Work/MSU Haddad R Directory/haddad-lab")

# Load packages
library(dplyr)
library(reshape2)
library(ggplot2)

# load L1 data

bfly <- read.csv(file.choose(),header=T)
colnames(bfly)

# > RICHNESS ----
# walking transect
# T7 included 

# filter out strip transects
bfly.w <- filter(bfly,Transect=="walking")
xtabs(~Transect,data=bfly.w) # should be just "walking" now

# eliminate any unwanted species/taxa
# mostly for unknowns and higher taxa that couldn't be identified to species level
xtabs(~Species.analysis,data=bfly.w)

bfly.w <- filter(bfly.w,Species.analysis != "Blue sp")
bfly.w <- filter(bfly.w,Species.analysis != "Butterfly sp")
bfly.w <- filter(bfly.w,Species.analysis != "Comma sp")
bfly.w <- filter(bfly.w,Species.analysis != "Copper sp")
bfly.w <- filter(bfly.w,Species.analysis != "Fritillary sp")
bfly.w <- filter(bfly.w,Species.analysis != "Hairstreak sp")
bfly.w <- filter(bfly.w,Species.analysis != "Lycaenidae sp")
bfly.w <- filter(bfly.w,Species.analysis != "none")
bfly.w <- filter(bfly.w,Species.analysis != "Swallowtail sp")

# reshape dataframe so that species counts are additional columns
colnames(bfly.w)
bfly.w <- dcast(bfly.w, Date+Treatment+Replicate ~ Species.analysis)

# check format
head(bfly.w)

# make count data presence/absence (1/0) instead of counts per species
# use brackets to exclude non-count columns
# sapply will let us convert any number >0 to 1
colnames(bfly.w)
bfly.w[,4:33] <- sapply(bfly.w[,4:33], function(x) { as.numeric(x > 0) })

# check that counts have been changed to 1s
View(bfly.w)

# add richness column by summing no. species per row
bfly.w$Richness <- rowSums(bfly.w[,4:33])
colnames(bfly.w) # check for new column "Richness"

# species richness data ready for analysis
# export as L2 data
write.csv(bfly.w, "LTER-butterflies\\LTER_butterfly-richness_L2.csv", row.names=FALSE)
