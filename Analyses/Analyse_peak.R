"""
Script made by Celien Iliaens on 9/05/2021
Used to analyse peak frequency and peak amplitude from the individual alpha peak 

"""

library(car)
library(lme4)
library(ggplot2)
library(reshape2)
library(emmeans)
library(tidyr)
library(effects)
library(lattice)
library(effects)
library(sjPlot)
library(jtools)
library(readxl)

setwd("C:\\Users\\celie\\OneDrive - ugentbe\\NOG NODIG\\2de master\\Masterproef\\Datasets\\All_results")

file1 = "Depressed_peak_cleared.csv"
file2 = "LF_peak_cleared.csv"
file3 = "HF_peak_cleared.csv"
file4 = "Health_peak_cleared.csv"

depr = read.csv(file1)
lf = read.csv(file2)
hf = read.csv(file3)
health = read.csv(file4)

data = rbind(depr, lf, hf, health)

#set factors
#> colnames(depr)
#[1] "Subject"       "Group"         "Treatment"     "ROI1"          "frequency"     "power"         "amplitude"     "Bandwith.peak"

data$Subject <- as.factor(data$Subject)
data$Group <- as.factor(data$Group)
data$Treatment <- as.factor(data$Treatment)
data$ROI1 <- as.factor(data$ROI1)

##################################################################################

fit <- lmer(frequency ~ (1|Subject) + Group*ROI1, data)
Anova(fit, type = "III")
# Chisq Df Pr(>Chisq)    
# (Intercept) 327141.506  1     <2e-16 ***
# Group            0.122  3     0.9891    
# ROI1          2904.360 27     <2e-16 ***
# Group:ROI1    2279.329 81     <2e-16 ***

test(emmeans(fit, pairwise ~ Group|ROI1))

#store figure 
tiff("Frequency_Peak.tiff")
plot(predictorEffects(fit, ~ ROI1, xlevels=list(s=c("HF", "Healthy", "Depressed","LF"))),
     axes=list(grid=TRUE,
               y=list(type="response"),
               x=list(rug=FALSE, rotate=30)),
     lines=list(multiline=TRUE),
     confint=list(style="auto"),
     ylab = "Frequency of the peak",
     xlab = "Region",
     main = "Peak frequency in the alpha band (8-12Hz)",
     lattice=list(key.args=list(space="top",
                                columns=4,
                                border=TRUE,
                                fontfamily="serif",
                                cex=1.25,
                                cex.title=1.5)))
dev.off()

##################################################################################"

fit <- lmer(amplitude ~ (1|Subject) + Group*ROI1, data)
Anova(fit, type = "III")
# (Intercept) 589.1651  1     <2e-16 ***
# Group         0.0712  3     0.9951    
# ROI1        162.2685 27     <2e-16 ***
# Group:ROI1  291.3314 81     <2e-16 ***

test(emmeans(fit, pairwise ~ Group|ROI1))


#store figure
plot(effect("Group:ROI1",fit))

tiff("Amplitdue_Peak.tiff")
plot(predictorEffects(fit, ~ ROI1, xlevels=list(s=c("HF", "Healthy", "Depressed","LF"))),
         axes=list(grid=TRUE,
                     y=list(type="response"),
                     x=list(rug=FALSE, rotate=30)),
         lines=list(multiline=TRUE),
         confint=list(style="auto"),
         ylab = "Amplitude of the peak",
         xlab = "Region",
         main = "Peak amplitude in the alpha band (8-12Hz)",
         lattice=list(key.args=list(space="top",
                                columns=4,
                                border=TRUE,
                                fontfamily="serif",
                                cex=1.25,
                                cex.title=1.5)))
dev.off()
