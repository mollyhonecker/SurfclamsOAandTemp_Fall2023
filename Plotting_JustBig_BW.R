#Three way ANOVA CR-std June-July exp. 
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(ggthemes)
library(wesanderson)
library(forcats) #ordering data for ggplot 
library(ggplot2)
library(ggpubr)
library(rstatix)

setwd("~/Library/CloudStorage/OneDrive-DalhousieUniversity/Rutgers/AIC Work/Spring Summer 2023 AIC Expeirment/Just_Big_Plots")

surfclams <- read.csv("~/Library/CloudStorage/OneDrive-DalhousieUniversity/Rutgers/AIC Work/Spring Summer 2023 AIC Expeirment/Just_Big_Plots/Biodeposition_AllData_ForR_Omits.csv")
attach(surfclams)

surfclams_acute <- surfclams[surfclams$Experiment == "Acute",]
surfclams_chronic <- surfclams[surfclams$Experiment == "Chronic",]
surfclams_acuteandchronic <- surfclams[surfclams$Experiment != "ControlTrial",]
surfclams_ambient <- surfclams[surfclams$Treatment == "Ambient",]
surfclams_medium <- surfclams[surfclams$Treatment == "Medium",]
surfclams_low <- surfclams[surfclams$Treatment == "Low",]
surfclams_big <- surfclams_acuteandchronic[surfclams$SizeClass == "Big",]
surfclams_small <- surfclams_acuteandchronic[surfclams$SizeClass == "Small",]




theme_set(
  theme_bw() +
    theme(legend.position = "right")
)


surfclams_big$Treatment <-factor(surfclams_big$Treatment, levels = c("Ambient", "Medium", "Low"))


#compare_means(CR_Std ~ Experiment, data = surfclams_big, 
              #group.by = "Treatment")


CRbig <- ggplot(surfclams_big, aes(y = CR_Std, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("ClearanceRate\n(L/h)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), CR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  #stat_compare_means(label="p.format")
  #stat_compare_means(aes(group = Treatment), label = "p.format")

CRbig
  

DryW <- ggplot(surfclams_big, aes(y = DryWeight, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Dry Tissue Weight\n(g)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), AR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

DryW
  

ShellT <- ggplot(surfclams_big, aes(y = ShellThickness, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Shell Thickness\n(mm)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), AR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

ShellT


ShellStr <- ggplot(surfclams_big, aes(y = ShellStrength_Std, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Shell Strength\n(N/mm)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), AR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

ShellStr


CI <- ggplot(surfclams_big, aes(y = ConditionIndex, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Condition Index")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), AR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

CI

Egestion <- ggplot(surfclams_big, aes(y = ER_Std, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Egestion Rate\n(mg/h)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), ER_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

Egestion

Rejection <- ggplot(surfclams_big, aes(y = RR_Std, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Rejection Rate\n(mg/h)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), RR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


Rejection 

Filtration <- ggplot(surfclams_big, aes(y = FR_Std, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Filtration Rate\n(mg/h)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  labs(fill = "pH Treatment")+
  labs(colour = "pH Treatment")+
  geom_pwc(
    aes(group = Treatment), RR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


Filtration 


OrganicIR <- ggplot(surfclams_big, aes(y = OIR_Std, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Organic Ingestion Rate\n(mg/h)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  geom_pwc(
    aes(group = Treatment), OIR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

OrganicIR 


AbsorbR <- ggplot(surfclams_big, aes(y = AR_Std, x=Experiment, fill=Treatment, order=Treatment))+
  geom_boxplot()+
  geom_point(aes(colour = Treatment), position = position_jitterdodge())+
  scale_color_grey()+
  scale_fill_grey()+
  scale_x_discrete(labels = c('1 day', '2 weeks', '6 weeks'))+
  theme(axis.ticks.length=unit(.5, "cm"))+
  xlab("")+
  ylab("Absorption Rate\n(mg/h)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2), legend.position= "none")+
  theme(text = element_text(size = 30))+
  geom_pwc(
    aes(group = Treatment), AR_Std = 0,
    method = "t_test", label = "p.adj.format",
    bracket.nudge.y = +0.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

AbsorbR


#Feeding gg
Feeding <- ggarrange (CRbig, Egestion, Rejection, Filtration, OrganicIR, AbsorbR,ncol=3, 
                      nrow=2,labels="AUTO", font.label = list(size = 30, color = "black", face = "bold", family = NULL),
                      common.legend = TRUE)
Feeding

#Growing gg
Growing <- ggarrange (DryW, ShellT, ShellStr, CI, ncol=2, 
                      nrow=2,labels="AUTO", font.label = list(size = 30, color = "black", face = "bold", family = NULL),
                      common.legend = TRUE)
Growing


#Export is 2000 x 1500? 


surfclams %>% sample_n_by(Experiment, Treatment, SizeClass, size=1)

model  <- lm(CR_Std ~ Experiment*Treatment*SizeClass, data = surfclams)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#we are violating normality...but not horribly 

#these looks good 
ggqqplot(surfclams, "CR_Std", ggtheme = theme_bw()) +
  facet_grid(Experiment + Treatment ~ SizeClass, labeller = "label_both")

surfclams %>% levene_test(CR_Std ~ Experiment*Treatment*SizeClass)
#we violate but not like horribly 

#ANOVA
res.aov <- surfclams %>% aov(CR_Std ~ Experiment*Treatment*SizeClass)
summary(res.aov)

TukeyHSD(res.aov)

#Significant three way interaction effect(0.02) so run simple two way interaction
#I am grouping by size, sig. level - 0.025 (for main effects) (0.05/2 because we are splitting two ways)
#Tukey is corrected for bonferroni 

#By splitting into big and small clams we can test the effect of pH over time on each size class 
#This is useful because we aren't really interested in comparing Acute Low pH big clams to Chronic Low pH small clams, you know?

#Big clams only 
big.model <-aov(CR_Std~Experiment*Treatment, data=surfclams_big)
TukeyHSD(big.model)

#Small clams only 
small.model <-aov(CR_Std~Experiment*Treatment, data=surfclams_big)
TukeyHSD(small.model)

#Ok but now for each pH treatment we ARE interested in comparing big and small clams in each treatment
#So we are going to split by treatment 

#Low pH
low.model <-aov(CR_Std~Experiment*SizeClass, data=surfclams_low)
#Because we have significance in our main effects, we can look at post-hoc 
TukeyHSD(low.model)

#Medium pH
medium.model <-aov(CR_Std~Experiment*SizeClass, data=surfclams_medium)
TukeyHSD(medium.model)

#Ambient 
ambient.model <-aov(CR_Std~Experiment*SizeClass, data=surfclams_ambient)
TukeyHSD(ambient.model)