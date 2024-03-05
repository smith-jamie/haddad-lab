
# AUTHORS:        Jamie Smith, Sean Fitzgerald, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Clean L1 dragonfly data
# PROJECT:        LTER Dragonflies
# DATE:           March 2024

# load packages
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(effects)
library(scales)

# load L2 dragonfly abundance data

dfly <- read.csv(file.choose(),header=T)
colnames(dfly)

# check distribution
qqnorm(dfly$dpm)
qqline(dfly$dpm)
hist(dfly$dpm) 
# pretty right-skewed

# log transform
dfly$log <- log(dfly$dpm)

# check
qqnorm(dfly$log)
qqline(dfly$log)
hist(dfly$log)
# bit better

# make replicate and year factors
dfly$Replicate <- as.factor(dfly$Replicate)
dfly$Year <- as.factor(dfly$Year)

# quick glance at abundance across treatments
dfly %>%
  ggplot(aes(x = treat_trans,y = log)) +
  geom_boxplot() + labs(x="Treatment",y="log(abundance index)") +
  theme_classic(base_size = 20)

# and across years
dfly %>%
  ggplot(aes(x = as.factor(Year),y = log)) +
  geom_boxplot() + labs(x="Year",y="log(abundance index)") +
  theme_classic(base_size = 20)

# and distance from water/wetlands
dfly %>%
  ggplot(aes(x = DistWater_m,y = log)) +
  geom_point() + labs(x="Distance from water (m)",y="log(abundance index)") +
  theme_classic(base_size = 20)

# add column for descriptive treatment axis titles
dfly <- dfly %>%
  mutate(treat_title = case_when(treat_trans == "1_walking" ~ "Conventional",
                                 treat_trans == "3_walking" ~ "Reduced Input",
                                 treat_trans == "4_walking" ~ "Organic",
                                 treat_trans == "6_walking" ~ "Switchgrass",
                                 treat_trans == "7_walking" ~ "Successional",
                                 treat_trans == "3_strip" ~ "Prairie Strip (reduced input)",
                                 treat_trans == "4_strip" ~ "Prairie Strip (organic)"
  ))
head(dfly)

# linear mixed model
lmm <- lmer(log~treat_title+Year+DistWater_m+(1|Replicate),data=dfly)
summary(lmm)


# plot variables
plot(allEffects(lmm))

# ag treatment
# effects plot
plot(effect("treat_title",lmm),xlab="Treatment",ylab="log(dragonfly abundance index)",
     main=NULL,colors="#000000",bar.colors="#666666",ci.style="bars",lty=0)

# ggplot
dfly %>% 
  mutate(treat_title = factor(treat_title, 
                              levels=c("Conventional","Reduced Input","Organic",
                                       "Switchgrass","Successional",
                                       "Prairie Strip (reduced input)",
                                       "Prairie Strip (organic)"))) %>%
  ggplot(aes(x=treat_title,y=log)) + 
  labs(x="Agricultural Treatment",y="log(dragonfly abundance index)") +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18)) +
  scale_x_discrete(labels = label_wrap(width = 10))

# year
# effects plot
plot(effect("Year",lmm),xlab="Year",ylab="log(dragonfly abundance index)",
     main=NULL,colors="#000000")

# ggplot
dfly %>% 
  ggplot(aes(x=as.factor(Year),y=log)) + 
  labs(x="Year",y="log(dragonfly abundance index)") +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18))

# distance from water
# effects plot
plot(effect("DistWater_m",lmm),xlab="Distance from water (m)",
     ylab="log(dragonfly abundance index)",
     main=NULL,colors="#000000")

# ggplot
dfly %>% 
  ggplot(aes(x=DistWater_m,y=log)) + 
  labs(x="Distance from Water (m)",y="log(dragonfly abundance index)") +
  stat_smooth(method=lm, color="black") +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18))
