
# AUTHORS:        Annabelle McCarthy, Sean Fitzgerald, Jamie Smith, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Butterfly abundance results and figures
# PROJECT:        LTER Butterflies
# DATE:           March 2024

# load packages
library(lme4)
library(lmerTest)
library(ggplot2)
library(effects)

# load L2 abundance data

bfly <- read.csv(file.choose(),header=T)
colnames(bfly)

# check distribution
qqnorm(bfly$bpm)
qqline(bfly$bpm)
hist(bfly$bpm) 
# pretty right-skewed

# log transform
bfly$log <- log10(bfly$bpm)

# check
qqnorm(bfly$log)
qqline(bfly$log)
hist(bfly$log)
# better

# quick glance at abundance across treatments
bfly %>%
  ggplot(aes(x = Treatment,y = log)) +
  geom_boxplot() + labs(x="Treatment",y="log(abundance index)") +
  theme_classic(base_size = 20)

# and across years
bfly %>%
  ggplot(aes(x = as.factor(Year),y = log)) +
  geom_boxplot() + labs(x="Year",y="log(abundance index)") +
  theme_classic(base_size = 20)

# linear mixed model
lmm <- lmer(log~Treatment+Year+(1|Replicate),data=bfly)
summary(lmm)

# plot variables
plot(allEffects(lmm))

# ag treatment
# effects plot
plot(effect("Treatment",lmm),xlab="Treatment",ylab="log(butterfly abundance index)",
     main=NULL,colors="#000000",bar.colors="#666666",ci.style="bars",lty=0)

# ggplot
bfly %>% 
  mutate(Treatment = factor(Treatment, levels=c("1","2","3","4","6","7","CLE"))) %>%
  ggplot(aes(x=Treatment,y=log)) + 
  labs(x="Agricultural Treatment",y="log(butterfly abundance index)") +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18))


# year
# effects plot
plot(effect("Year",lmm),xlab="Year",ylab="log(butterfly abundance index)",
     main=NULL,colors="#000000")

# ggplot
bfly %>% 
  ggplot(aes(x=Year,y=log)) + 
  labs(x="Year",y="log(butterfly abundance index)") +
  stat_smooth(method=lm, color="black") +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18))
