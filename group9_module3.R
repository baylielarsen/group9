#Combine the code above so that you can establish the anole.log data tibble.

anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole %>% 
  left_join(anole.eco) %>% 
  filter(!Ecomorph %in%c("U", "CH")) %>% 
  na.omit() %>% 
  print()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

#Using the log-transformed data, construct two simple linear models that assess the effect of perch diameter and height by including these as covariates in your models. Be sure to use + notation rather than *, assuming there is no interaction (there isn’t, trust me!).

#Linear Model Assessing Perch Diameter (ArbPD)
Anole.log.ArbPD.lm <- lm(HTotal~SVL + ArbPD, anole.log)

#Linear Model Assessing Perch Height (PH)
Anole.log.PH.lm <- lm(HTotal~SVL + PH, anole.log)

#Explore how both perch diameter and height effect the hindlimb-SVL relationship by plotting the residuals of your simple linear models against these discrete factors. This will require mutating a data tibble to include residuals from both models. Please produce two separate plots.

#Mutating Tibble 
anole.log <- anole.log %>% 
  mutate(res1 = residuals(Anole.log.ArbPD.lm)) %>% 
  mutate(res2 = residuals(Anole.log.PH.lm))

#ArbPD Residuals 
ArbPD.Res.Graph <- anole.log%>%
  ggplot(aes(x=ArbPD,y=res1)) +geom_point()
print(ArbPD.Res.Graph)

#PH Residuals 
PH.Res.Graph <- anole.log%>%
  ggplot(aes(x=PH,y=res2)) +geom_point()
print(PH.Res.Graph)

#Under a BM model of trait evolution and using the tree provided, construct phylogenetic least squares models of the hindlimb-SVL relationships that include the unique combinations of these two covariates, i.e,

#Loading anole.tree
anole.tree <- read.tree("anole.tre")

#A PGLS model with the hinglimb~SVL relationship + no other covariates

Anole.BM1 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#A PGLS model with the hindlimb-SVL relationship + perch diameter
Anole.BM2 <- gls(HTotal ~SVL + ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#A PGLS model with the hindlimb-SVL relationship + perch height

Anole.BM3 <- gls(HTotal ~SVL + PH , correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#A PGSL model with the hindlimb-SVL relationship + perch height + perch diameter

Anole.BM4 <- gls(HTotal ~SVL + PH + ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#Assess the fit of each of these three models using AICc and AICw and comment on (with comments in the script) whether one or both of the covariates is a significant predictor of hindlimb length in a phylogenetic context.

anole.phylo.aic <- AICc(Anole.BM1,Anole.BM2,Anole.BM3,Anole.BM4)
aicw(anole.phylo.aic$AICc)

#Answer:The Model with perch diameter as a covariate has a significantly lower AIC score than the model without covariates, indicating that perch diameter is a significant predictor of hindlimb length in a phylogenetic context. The model with perch height as a covariate has a lower AIC score than the model without covariates indicating that perch height also strengthens the model. The best model (with the lowest AIC score),counts perch height and diameter as covariates. 

#Produce a plot of your own design that concisely visualizes the effect of your covariate(s) and factors (i.e., ecomorph) on the hindlimb residuals of the best fitting PGLS model.

anole.log <- anole.log%>%
  mutate(phylo.res.best=residuals(Anole.BM4)) %>% 
  mutate(phylo.res.no.covariates=residuals(Anole.BM1))


anole.log%>%
  dplyr::select(Ecomorph2,phylo.res.best,phylo.res.no.covariates)%>%
  pivot_longer(cols=c("phylo.res.best", "phylo.res.no.covariates"))%>%
  print %>% 
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")

#Commit your script to your group repository.