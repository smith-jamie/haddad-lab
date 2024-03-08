
# AUTHORS:        Jamie Smith, Sean Fitzgerald, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    L2 dragonfly richness data, ready for analysis
# PROJECT:        LTER Dragonflies
# DATE:           March 2024

# load packages
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(effects)

# load L2 dragonfly richness data

rich <- read.csv(file.choose(),header=T)
colnames(rich)

# check distribution
qqnorm(rich$Richness)
qqline(rich$Richness)
hist(rich$Richness) 
# heavily right-skewed, not surprising
# should use Poisson distribution

# make Treatment and Replicate factors
rich$Treatment <- as.factor(rich$Treatment)
rich$Replicate <- as.factor(rich$Replicate)

# quick glance at richness across treatments
rich %>%
  ggplot(aes(x = Treatment,y = Richness)) +
  geom_boxplot() + labs(x="Treatment",y="Species richness") +
  theme_classic(base_size = 20)

# and across distance from water
rich %>%
  ggplot(aes(x = DistWater_m,y = Richness)) +
  geom_point() + labs(x="Distance from water (m)",y="Species richness") +
  theme_classic(base_size = 20)


# extract year from Date
rich$Year <- format(as.Date(rich$Date, format="%m/%d/%Y"),"%Y")
rich$Year <- as.factor(rich$Year)

# generalized linear mixed model with poisson distribution
glmm <- glmer(Richness ~ Treatment + Year + (1|Replicate), 
              family = "poisson", data = rich)
summary(glmm)
# model has convergence issues when DistWater_m is added
# try a generalized linear model, not mixed

glm <- glm(Richness ~ Treatment + DistWater_m + Year, family = "poisson", data=rich)
summary(glm)
# when replicate is added as a fixed factor, R6 differs from the rest
# this is probably because it's very close to Kettle Pond
# replicate and distance to water might be correlated 

# plot variables
plot(allEffects(glm))

# ag treatment
# effects
plot(effect("Treatment",glm),xlab="Treatment",ylab="Dragonfly species richness",
     main=NULL,colors="#000000",bar.colors="#666666",ci.style="bars",lty=0)
# ggplot
rich %>% 
  ggplot(aes(x=as.factor(Treatment),y=Richness)) + 
  labs(x="Year",y="Dragonfly species richness") +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18))

# year
# effects
plot(effect("Year",glm),xlab="Year",ylab="Dragonfly species richness",
     main=NULL,colors="#000000",bar.colors="#666666",ci.style="bars",lty=0)

# ggplot
rich %>% 
  ggplot(aes(x=as.factor(Year),y=Richness)) + 
  labs(x="Year",y="Dragonfly species richness") +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18))

# distance from water
# effects
plot(effect("DistWater_m",glm),xlab="Distance from water (m)",
     ylab="Dragonfly species richness",main=NULL,colors="#000000")

#ggplot
rich %>% 
  ggplot(aes(x=DistWater_m,y=Richness)) + 
  labs(x="Distance from Water (m)",y="Dragonfly species richness") +
  stat_smooth(method=lm, color="black") +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18))
