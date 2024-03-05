
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
library(scales)

# load L2 butterfly abundance data

bfly <- read.csv(file.choose(),header=T)
colnames(bfly)

# check distribution
qqnorm(bfly$bpm)
qqline(bfly$bpm)
hist(bfly$bpm) 
# pretty right-skewed

# log transform
bfly$log <- log(bfly$bpm)

# check
qqnorm(bfly$log)
qqline(bfly$log)
hist(bfly$log)
# bit better

# quick glance at abundance across treatments
bfly %>%
  ggplot(aes(x = treat_trans,y = log)) +
  geom_boxplot() + labs(x="Treatment",y="log(abundance index)") +
  theme_classic(base_size = 20)

# and across years
bfly %>%
  ggplot(aes(x = as.factor(Year),y = log)) +
  geom_boxplot() + labs(x="Year",y="log(abundance index)") +
  theme_classic(base_size = 20)

# add column for descriptive treatment axis titles
xtabs(~treat_trans,bfly)
bfly <- bfly %>%
  mutate(treat_title = case_when(treat_trans == "1_walking" ~ "Conventional",
                                 treat_trans == "2_walking" ~ "No Till",
                                 treat_trans == "3_walking" ~ "Reduced Input",
                                 treat_trans == "4_walking" ~ "Organic",
                                 treat_trans == "6_walking" ~ "Switchgrass",
                                 treat_trans == "7_walking" ~ "Successional",
                                 treat_trans == "3_strip" ~ "Prairie Strip (reduced input)",
                                 treat_trans == "4_strip" ~ "Prairie Strip (organic)",
                                 treat_trans == "CLE_strip" ~ "Restored Prairie"
  ))
xtabs(~treat_title,bfly)


# linear mixed model
lmm <- lmer(log~treat_title+Year+(1|Replicate),data=bfly)
summary(lmm)

# plot variables
plot(allEffects(lmm))

# ag treatment
# effects plot
plot(effect("Treatment",lmm),xlab="Treatment",ylab="log(butterfly abundance index)",
     main=NULL,colors="#000000",bar.colors="#666666",ci.style="bars",lty=0)

# ggplot
bfly %>% 
  mutate(treat_title = factor(treat_title, 
                              levels=c("Conventional","No Till",
                                       "Reduced Input","Organic","Switchgrass",
                                       "Successional","Prairie Strip (reduced input)",
                                       "Prairie Strip (organic)","Restored Prairie"))) %>%
  ggplot(aes(x=treat_title,y=log)) + 
  labs(x="Agricultural Treatment",y="log(butterfly abundance index)") +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text=element_text(size=16), 
        axis.title=element_text(size=18)) +
  scale_x_discrete(labels = label_wrap(width = 10))


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
