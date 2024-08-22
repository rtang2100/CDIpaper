##############################
#childhood disadvantage analyses 

#load pkg
rm(list=ls());
library(psych);
library(lme4);
library(ggpubr);
library(ggplot2);
library(jtools);
library(sjPlot);
library(sjmisc);
library(lmerTest);
library(tidyverse);


in.path <- "C:/Users/cathe/MD/";

#LOAD SIMILARITY TO PRINCIPAL GRADIENT, CDI, AND GCA, FILE
data.tbl<-read.csv(paste0(in.path,"v2v3crossSA477_400parcelSIMIFC.csv"),header=TRUE, stringsAsFactors = FALSE)


################regressions
#CDI to SIM
sim<-lmer(similarity~cdi+MCI_cross+Age+(1|case),data.tbl);
summ(sim, scale=TRUE,confint = TRUE, digits = 4,transform.response = TRUE)

#check assumptions
1 / vif(sim)
hist(resid(sim))
plot(fitted(sim), resid(sim))

#plot CDI & SIM association
cdisim_p<-effect_plot(sim, pred = cdi, interval = TRUE, partial.residuals = TRUE,x.label="CDI", y.label="Cortical Surface Area Profile", colors="red")
cdisim_p

#CDI TO TOTAL SA
sim<-lmer(totSA~cdi+MCI_cross+Age+(1|case),data.tbl);
summ(sim, scale=TRUE,confint = TRUE, digits = 4,transform.response = TRUE)

#check assumptions
1 / vif(sim)
hist(resid(sim))
plot(fitted(sim), resid(sim))


#SIM TO GCA
sim<-lmer(afqt~similarity+MCI_cross+Age+(1|case),data.tbl);
summ(sim, scale=TRUE,confint = TRUE, digits = 4,transform.response = TRUE)

#check assumptions
1 / vif(sim)
hist(resid(sim))
plot(fitted(sim), resid(sim))

#plot SIM to GCA
simgca_P<-effect_plot(sim, pred = similarity, interval = TRUE, partial.residuals = TRUE,x.label="Cortical Surface Area Profile", y.label="GCA", colors="red")

simgca_P

#SA TO GCA
sim<-lmer(afqt~totSA+MCI_cross+Age+(1|case),data.tbl);
summ(sim, scale=TRUE,confint = TRUE, digits = 4,transform.response = TRUE)

#check assumptions
1 / vif(sim)
hist(resid(sim))
plot(fitted(sim), resid(sim))

#CDI TO GCA
sim<-lmer(afqt~cdi+MCI_cross+Age+(1|case),data.tbl);
summ(sim, scale=TRUE,confint = TRUE, digits = 4,transform.response = TRUE)

#check assumptions
1 / vif(sim)
hist(resid(sim))
plot(fitted(sim), resid(sim))

#CDI TO GCA
cdigca_p<-effect_plot(sim, pred = cdi, interval = TRUE, partial.residuals = TRUE,x.label="CDI", y.label="GCA", colors="red")

cdigca_p


#######mediations
test.tbl<-data.tbl

#control for covariates before mediations
library(umx);

df2 		<- NA
df2 		<- umx_residualize(afqt ~ Age + MCI_cross + (1|case), data = test.tbl)
df2 		<- umx_residualize(similarity ~ Age + MCI_cross + (1|case), data = df2)

#standardized
test.tbl<-df2
test.tbl$cdi<-scale(test.tbl$cdi)
test.tbl$afqt<-scale(test.tbl$afqt)
test.tbl$similarity<-scale(test.tbl$similarity)

#cortical SA profile as mediator
process(data = test.tbl, y = "afqt", x = "cdi", m ="similarity", model = 4, effsize =1, total =1, stand =1, boot = 10000 , seed=888888, modelbt = 1)

#####Figures
in.path <- "C:/Users/cathe/MD/";

library(ggplot2)
cohen<-read.csv(paste0(in.path,"v2v3crossSA477_400parcelsadjbetaCDI.csv"),header=FALSE, stringsAsFactors = FALSE)
gradient<-read.csv(paste0(in.path,"fcgradient1.csv"),header=FALSE, stringsAsFactors = FALSE)

df<-cbind(cohen,gradient)
colnames(df)<-c("x","V1")
ggplot(df, aes(x = V1, y = x, fill = x)) + 
  geom_point(size = 3, stroke = NA, pch = 21) +
  scale_fill_gradient2(low= "#5E6ED0", mid = "white", high = "#CE4550", guide = "colourbar", aesthetics = "fill", name = NULL, midpoint = 0, limits = c(-0.143,0.13)) +
  scale_color_manual(values = c("white","black")) +
  labs(x="\nPrincipal Functional Gradient", y="Childhood Disadvantage Effect\n") +
  geom_smooth(method = 'lm', se = TRUE, fill = alpha(c("gray70"),.7), col = "black", linewidth = .5) +
  theme_classic() + 
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12, color = c("black")), axis.title = element_text(size = 12, color = c("black")), axis.line = element_line(linewidth = .22), axis.ticks = element_line(size = .22)) 
  #scale_y_continuous()

ggsave(filename = "C:/Users/cathe/MD/Figure 2.png", dpi = 300, width = 5.5 , height = 5)
