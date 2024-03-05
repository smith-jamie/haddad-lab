
# AUTHORS:        Jamie Smith, Sean Fitzgerald, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Clean L1 dragonfly data
# PROJECT:        LTER Dragonflies
# DATE:           March 2024

# set working directory
setwd("~/Work/MSU Haddad R Directory/haddad-lab")

# Load packages
library(tidyverse)
library(dplyr)

# load dragonfly L1 data
dfly <- read.csv(file.choose(),header=T)
colnames(bfly)

# make Treatment and Replicate factors
dfly$Treatment <- as.factor(dfly$Treatment)
dfly$Replicate <- as.factor(dfly$Replicate)

# extract Year from Date
dfly$Year <- format(as.Date(dfly$Date, format="%m/%d/%Y"),"%Y")

# filter out strip transects
dfly.w <- filter(dfly,Transect=="walking")
xtabs(~Transect,data=dfly.w)

# filter out walking transects
dfly.s <- filter(dfly,Transect=="strip")
xtabs(~Transect,data=dfly.s)

# create dataframes with Year, Treatment, Rep, 
# Transect, Distance from Water, and Species
colnames(dfly.w)
dfly.w <- dfly.w[,c(2:6,18,13)]
dfly.s <- dfly.s[,c(2:6,18,13)]

# add a count column to sum
# if a butterfly was observed, "1", if none observed, "0"
dfly.w$count <- with(dfly.w, ifelse(dfly.w$Species == "NULL", 0, 1))
dfly.s$count <- with(dfly.s, ifelse(dfly.s$Species == "NULL", 0, 1))
head(dfly.s)

# aggregate by plot, summing counts across all surveys
attach(dfly.w)
dfly.w <- aggregate(count~ Treatment+Replicate+Transect+Year
                    +DistWater_m,dfly.w,FUN=sum)
# check
dfly.w[1:6,]
detach(dfly.w)

attach(dfly.s)
dfly.s <- aggregate(count~ Treatment+Replicate+Transect+Year
                    +DistWater_m,dfly.s,FUN=sum)
# check
dfly.s[1:6,]
detach(dfly.s)

# divide walking by 12 for dragonflies/min
colnames(dfly.w)
dfly.w <- dfly.w %>% 
  mutate(dpm = count / 12)
head(dfly.w)

# divide strips by 8 for butterflies/min
dfly.s <- dfly.s %>%
  mutate(dpm = count / 8)
head(dfly.s)

# combine back into one dataframe
dfly.ab <- rbind(dfly.w, dfly.s)

# create "treat_trans" column that concatenates treatment with transect
dfly.ab$treat_trans <- paste(dfly.ab$Treatment,dfly.ab$Transect,sep="_")
View(dfly.ab)

# abundance data ready for analysis
# export as L2 data
write.csv(dfly.ab, "dragonflies\\LTER_dragonfly-abundance-L2.csv", row.names=FALSE)
