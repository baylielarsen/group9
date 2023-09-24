#setup
anole <- read.csv("anole.dat.csv")
anole.eco <- read.csv("anole.eco.csv")

#load libraries
library(tidyverse) 
install.packages("ape")
install.packages("nlme")
install.packages("geiger")
install.packages("caper")
install.packages("phytools")
install.packages("viridis")
install.packages("MuMIn")

library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)
library(tidyverse)

#establishing the data tibble
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  print()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

print(anole.log)

#using log transformed data to construct two simple linear models
anole.lin1<- lm(SVL ~ ArbPD, data = anole.log)
coef(anole.lin1)

anole.lin2 <- lm(SVL ~ PH, data = anole.log)
coef(anole.lin2)

#plotting residuals
library(modelr)
options(na.action = na.warn)

anole.log$residuals_anole.lin1 <- resid(anole.lin1)

plot1 <- ggplot(anole.log, aes(x = ArbPD, y = residuals_anole.lin1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals vs. Perch Diameter")

plot1

anole.log$residuals_anole.lin2 <- resid(anole.lin2)

plot2 <- ggplot(anole.log, aes(x = PH, y = residuals_anole.lin2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals vs. Perch Height")

plot2

#BM model of trait evolution
anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)
?read.tree

pgls.BM1 <- gls(SVL ~PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.BM2 <- gls(SVL ~ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.BM3 <- gls(SVL ~PH + ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

print(pgls.BM1)
print(pgls.BM2)
print(pgls.BM3)

AICc_pgls.BM1 <- AICc(pgls.BM1)
AICc_pgls.BM2 <- AICc(pgls.BM2)
AICc_pgls.BM3 <- AICc(pgls.BM3)
print(AICc_pgls.BM1)
print(AICc_pgls.BM2)
print(AICc_pgls.BM3)

library(MuMIn)
AICw_pgls.BM1 <- aicw(AICc_pgls.BM1)
AICw_pgls.BM2 <- aicw(AICc_pgls.BM2)
AICw_pgls.BM3 <- aicw(AICc_pgls.BM3)

print(AICw_pgls.BM1)
print(AICw_pgls.BM2)
print(AICw_pgls.BM3)

#make own plot

library(ggplot2)

ggplot(anole.log, aes(x = ArbPD, y = residuals_anole.lin1)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Perch Height vs. Hindlimb Residuals",
    x = "Perch Height",
    y = "Hindlimb Residuals"
  ) +
  theme_minimal()