
# AUTHORS:        Annabelle McCarthy, Sean Fitzgerald, Jamie Smith, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Clean L2 butterfly abundance data, ready for analysis
# PROJECT:        LTER Butterflies
# DATE:           March 2024

# set working directory
setwd("~/Work/MSU Haddad R Directory/haddad-lab")

# Load packages
library(tidyverse)
library(dplyr)

# load L1 butterfly data
bfly <- read.csv(file.choose(),header=T)
colnames(bfly)

# make Treatment and Replicate factors
bfly$Treatment <- as.factor(bfly$Treatment)
bfly$Replicate <- as.factor(bfly$Replicate)

# extract Year from Date
bfly$Year <- format(as.Date(bfly$Date, format="%m/%d/%Y"),"%Y")

# filter out strips
bfly.w <- filter(bfly,Transect=="walking")
xtabs(~Transect,data=bfly.w)

# filter out walking
bfly.s <- filter(bfly,Transect=="strip")
xtabs(~Transect,data=bfly.s)

# create dataframes with just Year, Treatment+name, Rep, Transect, and species
colnames(bfly.w)
bfly.w <- bfly.w[,c(2:3,6,22,20)]
bfly.s <- bfly.s[,c(2:3,6,22,20)]

# add a count column to sum
# if a butterfly was observed, "1", if none observed, "0"
bfly.w$count <- with(bfly.w, ifelse(bfly.w$Species.analysis == "none", 0, 1))
bfly.s$count <- with(bfly.s, ifelse(bfly.s$Species.analysis == "none", 0, 1))

# aggregate by plot, summing counts across all weeks
attach(bfly.w)
bfly.w <- aggregate(count~ Treatment+Replicate+Transect+Year,bfly.w,FUN=sum)
# check
bfly.w[1:6,]
detach(bfly.w)

attach(bfly.s)
bfly.s <- aggregate(count~ Treatment+Replicate+Transect+Year,bfly.s,FUN=sum)
# check
bfly.s[1:6,]
detach(bfly.s)

# divide walking by 12 for butterflies/min
colnames(bfly.w)
bfly.w <- bfly.w %>% 
  mutate(bpm = count / 12)
head(bfly.w)

# divide strips by 8 for butterflies/min
bfly.s <- bfly.s %>%
  mutate(bpm = count / 8)
head(bfly.s)

# combine back into one dataframe
bfly.ab <- rbind(bfly.w, bfly.s)
View(bfly.ab)

# abundance data ready for analysis
# export as L2 data
write.csv(bfly.ab, "LTER-butterflies\\LTER_butterfly-abundance-L2.csv", row.names=FALSE)
