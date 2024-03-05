
# AUTHORS:        Annabelle McCarthy, Sean Fitzgerald, Jamie Smith, Nick Haddad
# COLLABORATORS:  
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Butterfly richness results and figures
# PROJECT:        LTER Butterflies
# DATE:           March 2024

# load packages
library(lme4)
library(lmerTest)
library(ggplot2)
library(effects)

# load L2 data

rich <- read.csv(file.choose(),header=T)
colnames(rich)

# check distribution
qqnorm(rich$Richness)
qqline(rich$Richness)
hist(rich$Richness) # heavily right-skewed, not surprising
# should use Poisson?

# make Treatment and Replicate factors
rich$Treatment <- as.factor(rich$Treatment)
rich$Replicate <- as.factor(rich$Replicate)

# quick glance at richness across treatments
rich %>%
  ggplot(aes(x = Treatment,y = Richness)) +
  geom_boxplot() + labs(x="Treatment",y="Species richness") +
  theme_classic(base_size = 20)

# extract year from Date
rich$Year <- format(as.Date(rich$Date, format="%m/%d/%Y"),"%Y")

# generalized linear mixed model with poisson distribution
glmm <- glmer(Richness ~ Treatment + Year + (1|Replicate), family = "poisson", data = rich)
summary(glmm)


# plot variables
plot(allEffects(glmm))

# ag treatment
plot(effect("Treatment",glmm),xlab="Treatment",ylab="Butterfly species richness",
     main=NULL,colors="#000000",bar.colors="#666666",ci.style="bars",lty=0)

# year
plot(effect("Year",glmm),xlab="Year",ylab="Butterfly species richness",
     main=NULL,colors="#000000",bar.colors="#666666",ci.style="bars",lty=0)