
# AUTHORS:        Jamie Smith, Sean Fitzgerald, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Clean L1 dragonfly data
# PROJECT:        LTER Dragonflies
# DATE:           March 2024

# set working directory
setwd("~/Work/MSU Haddad R Directory/haddad-lab")

# read in data
dfly <- read.csv(file.choose(),header=T)

# check column names
colnames(dfly)

# check categories
xtabs(~Transect,data=dfly) # should be strip and walking
xtabs(~Treatment,data=dfly) # should be T1, T3, T4, T6, T7
xtabs(~InStrip,data=dfly) # should be 0, 1, or NULL

# check species names
table(dfly$Species)

# data checked and clean
# export as L1
write.csv(dfly, "dragonflies\\LTER_dragonflies_L1.csv", row.names=FALSE)
