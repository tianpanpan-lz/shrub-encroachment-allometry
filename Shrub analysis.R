library(tidyverse)
library(lme4)
library(lmerTest)
library(sciplot)
library("readxl")
library("openxlsx")
library(metafor)
library(vegan)
library(piecewiseSEM)
library(AICcmodavg)
library(MuMIn)
library(nlme)
library(ggpubr)
library(brms)
library(Matrix)
library(tidybayes)
library(rstan)
library(ggcorrplot)
library(corrplot)
library(ggplot2)
library(performance)
library(gghalves)
library("eoffice")
library(emmeans)
library(cowplot)
library("multcomp")
#library(viridis)
library(ggplot2)
library(tidyverse)

library(agricolae)
library(car)
library(reshape2)

mytheme <- theme_test()+theme(axis.title.x = element_text(size = 20, face = "bold", colour = "black"),
                              axis.title.y = element_text(size = 20, face = "bold", colour = "black"),
                              axis.text.x = element_text(size = 20, colour = "black",hjust = 0.5),
                              axis.text.y = element_text(size = 20, colour = "black"))+
  theme(panel.border = element_rect(fill=NA,color="black", linewidth = 0.5, linetype="solid"))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  theme(plot.title = element_text(size = 25,          #字体大小
                                  hjust = 0.5,          #字体左右的位置
                                  angle = 0))
######  Treatment ######
Treatment <- read_excel("D:\\Haibei_size\\Shrubdata\\Treatment.xlsx",sheet = 1) %>%
  unite(Block_Treatment,Block,Treatment, sep = "_",remove=F)
str(Treatment)

###### 1. Light ######
MaquLight <- read.xlsx("D:\\Haibei_size\\Shrubdata\\MaquShrubPAR_Editor_2.xlsx",sheet = 1) %>%
  dplyr::select(-c(Date,Weather)) %>%
  unite(Block_Treatment, Block, Treatment, sep="_", remove=F)
str(MaquLight)

####
PARData <- MaquLight %>%
  gather(key = Height,value = PAR,-c(Round,Block_Treatment,Block,Treatment,Direction)) %>%
  mutate(Height = plyr::mapvalues(Height, c("Height0","Height10","Height20","Height30","Height40"), c("0","10","20","30","40"))) %>%
  mutate(Height=as.numeric(Height)) %>%
  filter(!is.na(PAR)) %>%
  group_by(Block_Treatment,Round,Height) %>%
  summarise(PAR=mean(PAR)) %>%
  as.data.frame() %>%
  mutate(logPAR=log(PAR)) %>%
  unite(Block_Treatment_Round,Block_Treatment,Round, sep = "_",remove=F) %>%
  merge(Treatment,by='Block_Treatment') %>%
  mutate(Block=as.character(Block)) %>%
  mutate(Treatment=factor(Treatment,levels=c("Control",'Shrub',"RemovedShrub",'ArtificialShrub')))
summary(PARData)
str(PARData)

#### Light asymmetry
#prepare dataframe for result
Block_Treatment_Round <- "A"
PARslope <- 0
PAR.result <- data.frame(Block_Treatment_Round,PARslope)
str(PAR.result)

#cycle
unique(PARData$Block_Treatment_Round)#total 120=40*3 plots*3 days
for (i in 1:120) {
  Block_Treatment_Round_i <- unique(PARData$Block_Treatment_Round)[i]
  PAR_i <- subset(PARData,Block_Treatment_Round==Block_Treatment_Round_i)
  lm_i <- lm(logPAR~Height,PAR_i)
  coef_i <- coef(lm_i)
  PAR.result[i,1] <- Block_Treatment_Round_i
  PAR.result[i,2] <- coef_i[[2]]
}

PAR.result <- separate(PAR.result,'Block_Treatment_Round',into=c('Block','Treatment','Round'),sep = '_',remove = F)

PARslope.mean <- PAR.result %>%
  dplyr::select(-c(Block_Treatment_Round)) %>%
  group_by(Block,Treatment) %>%
  summarise(PARslope=mean(PARslope)) %>%
  merge(Treatment) %>%
  mutate(Block=as.character(Block),
         Treatment=factor(Treatment,levels=c("Control",'Shrub',"RemovedShrub",'ArtificialShrub')))

PARslope.mean <- PARslope.mean %>%
  dplyr::select(Block,Treatment,PARslope)

summary(PARslope.mean)#the average PARslope of 3 times
str(PARslope.mean)
plot(PARslope~Treatment,PARslope.mean)

write.csv(PARslope.mean,"D:\\\\Haibei_size\\Shrubdata\\Result\\MaquLightAsymmetry_0-40.csv")

###
Fig.lnPAR <-
  ggplot(PARData,aes(Height,logPAR))+
  facet_wrap(~Treatment,ncol = 2)+
  geom_smooth(method = "lm",colour='grey',se=FALSE,aes(group = Block_Treatment_Round),size = 0.1)+
  geom_point(size=1.5,colour='#199b26',alpha=0.3)+
  xlab('Height (cm)')+
  ylab(expression(paste('Ln PAR ')))+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 12))+
  theme(axis.text = element_text(size = 10,  color = "black"))+
  theme(strip.text= element_text(size = 12, color = "black"))+
  theme(panel.grid = element_blank())
Fig.lnPAR


Fig.lnPAR <-
  Fig.lnPAR+
  theme(panel.spacing = unit(0.2, "cm"))
Fig.lnPAR
###
########  PAR Data
PARData <- read_excel("D:\\Haibei_size\\Shrubdata\\MaquShrubPAR_Editor_2.xlsx",sheet = 1)
PARData1 <- PARData %>%
  unite(Block_Treatment, Block, Treatment, sep="_", remove=F)
PARData2 <- PARData1 %>%
  dplyr::select(-Date,-Weather,-Treatment,-Block)
str(PARData2)
PARData3 <- PARData2 %>%
  mutate(Direction=as.character(Direction))
str(PARData3)
Pene <- PARData3 %>%
  dplyr::select(-Height10,-Height20,-Height30) %>%
  mutate(Penetration=Height0/Height40) %>%
  group_by(Block_Treatment) %>%
  summarise(Penetration=mean(Penetration)) %>%
  merge(Treatment,by='Block_Treatment') %>%
  mutate(Block=as.character(Block),
         Treatment=factor(Treatment,levels=c("Control","RemovedShrub",'ArtificialShrub','Shrub')))
summary(Pene)
Pene <- Pene %>%
  dplyr::select(c(Block,Treatment,Penetration))

Light <- left_join(PARslope.mean,Pene)
colnames(Light)[3] <- "LightAsymmetry"
colnames(Light)[4] <- "LightPenetration"
write.csv(Light,"D:\\Haibei_size\\Shrubdata\\Result\\Light.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            ~~test Light Asymmetry                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(multcomp)
library(multcompView)

str(Light)
Light <- Light %>% mutate(Treatment=factor(Treatment,levels=c("Control",'Shrub',"RemovedShrub",'ArtificialShrub')))
lmer.LightAsy <- lmer(LightAsymmetry ~ Treatment+(1|Block),Light)
summary(lmer.LightAsy)
anova(lmer.LightAsy)
aov(LightAsymmetry ~ Treatment+Error(Block),Light)
MuMIn::r.squaredGLMM(lmer.LightAsy)

library(emmeans)
emm.LightAsy = emmeans(lmer.LightAsy, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.LightAsy <- emm.LightAsy$emmeans %>%
  as.data.frame()
str(predictMean.emm.LightAsy)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           ~~paint Light Asymmetry                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Fig.LightAsy <- 
  ggplot(predictMean.emm.LightAsy,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  ylab(expression(paste(' Light asymmetry ')))+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(0.038,0.089)


Fig.LightAsy
###### 2.Diversity and composition ######

MaquCommunity <- read.xlsx("D:\\Haibei_Size\\Shrubdata\\Diversity_20250326.xlsx")%>%
  dplyr::select(c(Block,Treatment,SR,Pielou))
str(MaquCommunity)

MaquCommunity$Treatment <- as.factor(MaquCommunity$Treatment)
MaquCommunity <- MaquCommunity 
str(MaquCommunity)


library(multcomp)
library(multcompView)
MaquCommunity <- MaquCommunity %>% mutate(Treatment=factor(Treatment,levels=c("Control",'Shrub',"RemovedShrub",'ArtificialShrub')))

####Richness
lmer.Richness <- lmer(SR ~ Treatment+(1|Block),MaquCommunity)
summary(lmer.Richness)
anova(lmer.Richness)

aov.Richness <- aov(SR ~ Treatment+Error(Block),MaquCommunity)
summary(aov.Richness)
MuMIn::r.squaredGLMM(lmer.Richness)


library(emmeans)
emm.Richness = emmeans(lmer.Richness, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.Richness <- emm.Richness$emmeans %>%
  as.data.frame()
str(predictMean.emm.Richness)
predictMean.emm.Richness$Treatment <- factor(predictMean.emm.Richness$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))


Fig.SR <- 
  ggplot(predictMean.emm.Richness,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  ylab(expression(paste('Species richness',sep=" ")))+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme


Fig.SR

####Pielou
str(MaquCommunity)
MaquCommunity <- MaquCommunity %>% mutate(Treatment=factor(Treatment,levels=c("Control",'Shrub',"RemovedShrub",'ArtificialShrub')))
lmer.Pielou <- lmer(Pielou ~ Treatment+(1|Block),MaquCommunity)
summary(lmer.Pielou)
anova(lmer.Pielou)
aov.Pielou <- aov(Pielou ~ Treatment+Error(Block),MaquCommunity)
summary(aov.Pielou)
MuMIn::r.squaredGLMM(lmer.Pielou)

library(emmeans)
emm.Pielou = emmeans(lmer.Pielou, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.Pielou <- emm.Pielou$emmeans %>%
  as.data.frame()
str(predictMean.emm.Pielou)

predictMean.emm.Pielou$Treatment <- factor(predictMean.emm.Pielou$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))


Fig.Pielou <- 
  ggplot(predictMean.emm.Pielou,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  #ylab('Light asymmetry')+
  ylab(expression(paste('Pielous evenness',sep=" ")))+
  #ylim(0,0.05)+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(0.58,0.75)


Fig.Pielou
###Biomass
####
MaquBiomass2024 <- read.xlsx("D:\\Haibei_size\\Shrubdata\\MaquShrubBiomass2024.xlsx",sheet = 1) %>%
  dplyr::select(c(Block,Treatment,Species,Biomass)) %>%
  unite(Plot,Block,Treatment,sep='_',remove = F) %>%
  unite(Plot_Species,Plot,Species,sep='_',remove = F)
str(MaquBiomass2024)

MaquBiomass2024 <- MaquBiomass2024[!duplicated(MaquBiomass2024$Plot_Species),]
str(MaquBiomass2024)


PlotBiomass2024 <- MaquBiomass2024 %>%
  group_by(Plot) %>%
  summarise(PlotBiomass=sum(Biomass))
str(PlotBiomass2024)
PlotBiomass2024 <-  separate(PlotBiomass2024,'Plot',sep='_',into=c('Block','Treatment'),remove=F)
str(PlotBiomass2024)
summary(PlotBiomass2024)
sd(PlotBiomass2024$PlotBiomass)

PlotBiomass2024$Treatment <- factor(PlotBiomass2024$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))
#### test PlotBiomass
####
lmer.PlotBiomass <- lmer(PlotBiomass ~ Treatment+(1|Block),PlotBiomass2024)
summary(lmer.PlotBiomass)
anova(lmer.PlotBiomass)
aov.PlotBiomass <- aov(PlotBiomass ~ Treatment+Error(Block),PlotBiomass2024)
summary(aov.PlotBiomass)
MuMIn::r.squaredGLMM(lmer.PlotBiomass)


library(emmeans)
emm.PlotBiomass = emmeans(lmer.PlotBiomass, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.PlotBiomass <- emm.PlotBiomass$emmeans %>%
  as.data.frame()
str(predictMean.emm.PlotBiomass)

predictMean.emm.PlotBiomass$Treatment <- factor(predictMean.emm.FDis.In$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))



####
Fig.PlotBiomass <- 
  ggplot(predictMean.emm.PlotBiomass,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  #ylab('Light asymmetry')+
  ylab(expression(paste("Aboveground biomass (g)",sep=" ")))+
  #ylim(0,0.05)+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(12,50)

Fig.PlotBiomass
###### 3.Height scaling exponent and Intercept ######
###### Data preparation ######
MaquTrait1 <- read.xlsx("D:\\Haibei_size\\Shrubdata\\MaquTrait.xlsx",sheet = 1) %>%
  unite(Block_Treatment,Block,Treatment,sep='_',remove = F) 
str(MaquTrait1)
MaquTrait1$Treatment <-as.factor(MaquTrait1$Treatment)

######## Paint: Exponent and Intercept Painting ########
MaquTrait <- MaquTrait1 %>%
  dplyr::select(Block_Treatment,Block,Treatment,Species,ind,Hm,IndividualBiomass,note) %>%
  na.omit

model1 <- lmer(log10(Hm)~log10(IndividualBiomass)*Treatment+(1+log10(IndividualBiomass)|Species)+(1|Block),data=MaquTrait1)
summary(model1)
anova(model1)

library(emmeans)
Intercept.emm <- emmeans(model1, ~ Treatment, at = list(x = 0))
pairs(Intercept.emm) # compare intercept

emm1 = emmeans(model1, specs = pairwise ~ log10(IndividualBiomass)*Treatment, type = 'response', adjust = 'none')
dataMean = emm1$emmeans %>%
  as.data.frame()
dataMean

MaquTrait1$Treatment <- factor(MaquTrait1$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))
                                
MaquTrait1$Treatment <- relevel(MaquTrait1$Treatment,ref='Control')
lmer(log10(Hm)~log10(IndividualBiomass)*Treatment+(1+log10(IndividualBiomass)|Species)+(1|Block_Treatment),data=MaquTrait1)%>%summary()

MaquTrait1$Treatment <- relevel(MaquTrait1$Treatment,ref='RemovedShrub')
lmer(log10(Hm)~log10(IndividualBiomass)*Treatment+(1+log10(IndividualBiomass)|Species)+(1|Block_Treatment),data=MaquTrait1)%>%summary

MaquTrait1$Treatment <- relevel(MaquTrait1$Treatment,ref='ArtificialShrub')
lmer(log10(Hm)~log10(IndividualBiomass)*Treatment+(1+log10(IndividualBiomass)|Species)+(1|Block_Treatment),data=MaquTrait1)%>%summary

MaquTrait1$Treatment <- relevel(MaquTrait1$Treatment,ref='Shrub')
lmer(log10(Hm)~log10(IndividualBiomass)*Treatment+(1+log10(IndividualBiomass)|Species)+(1|Block_Treatment),data=MaquTrait1)%>%summary

MaquTrait1$Treatment <- as.character(MaquTrait1$Treatment)
unique(MaquTrait1$Treatment)

####
MaquTrait1$Treatment <- factor(MaquTrait1$Treatment,levels=c("Control","Shrub","RemovedShrub","ArtificialShrub"))

Fig.MaquSpeciesAllometry <- 
  ggplot(data = MaquTrait1)+
  geom_point(aes(x = log10(IndividualBiomass),y = log10(Hm),color = Treatment),
             size = 1.2,
             alpha=0.1)+
  
  geom_vline(xintercept=0,linetype='dashed',size=0.6,color='gray')+
  geom_hline(yintercept=0,linetype='dashed',size=0.6,color='gray')+
  
  stat_function(fun = function(X)0.2992*X+1.582,linewidth = 0.8,linetype = 1,color = "#E8621B")+
  stat_function(fun = function(X)0.2966*X+1.646,linewidth = 0.8,linetype = 1,color = "#F1AD1E")+
  stat_function(fun = function(X)0.2919*X+1.598,linewidth = 0.8,linetype = 1,color = "#8CC660")+
  stat_function(fun = function(X)0.2806*X+1.613,linewidth = 0.8,linetype = 1,color = "#3188CB")+
  
  theme_bw()+
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 7, color = "black"))+
  theme(panel.grid = element_blank())+
 
  #scale_colour_viridis(discrete=T,name = "Treatment")+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  ylab(expression(paste(Log [10],"(Height)"),sep=""))+
  xlab(expression(paste(Log [10],"(Individual biomass)"),sep=""))+
  theme(legend.background = element_blank())+
  theme(legend.position = c(0.2,0.82),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 12, color = "black"))+
  ylim(0,2)+xlim(-4,1)

Fig.MaquSpeciesAllometry


###############
Trait <- read_excel("D:\\Haibei_size\\Shrubdata\\MaquTrait.xlsx",sheet=1)

str(Trait)

MaquTrait <- Trait %>%
  mutate(Treatment=as.factor(Treatment)) %>%
  mutate(ind = as.character(ind))

str(Trait)

str(MaquTrait)
MaquTrait$ind = as.character(MaquTrait$ind)
summary(MaquTrait)
data <- MaquTrait %>%
  filter(Hm!='NA') %>%
  filter(IndividualBiomass!='NA')

aggdata <- data %>%
  group_by(Treatment,Species) %>%
  summarise(count=n()) %>%
  unite(Species_Treatment,Species,Treatment,sep='_',remove = F) 

summary(aggdata)
dim(aggdata)

#prepare the number >=5 subset of sp in each site
aggdata_Select <- aggdata %>%
  filter(count>=5)

#choose data from prepared >=5 subset
dataSelect <- data %>%
  unite(Species_Treatment,Species,Treatment,sep='_',remove = F) %>%
  subset(Species_Treatment %in% aggdata_Select$Species_Treatment)

str(dataSelect)
unique(dataSelect$Species)
count(dataSelect)
dim(dataSelect)
summary(dataSelect)

#install.packages("smatr")
library("smatr")
SMA <- sma(log10(Hm)~log10(IndividualBiomass)*Species_Treatment,data=dataSelect)
summary(SMA)

#异速生长指数图
plot(SMA)
str(SMA)

pval <- as.data.frame(SMA$pval)%>%
  gather(key = Species_Treatment,value = pval)
number <- as.data.frame(SMA$n)%>%
  gather(key = Species_Treatment,value = number)
r2 <- as.data.frame(SMA$r2)%>%
  gather(key = Species_Treatment,value = r2)
str(pval)
dim(pval)
smaResult <- coef(SMA)

hist(smaResult$elevation)
smaResult$Species_Treatment <- row.names(smaResult)
str(smaResult)
smaResult <-  separate(smaResult,'Species_Treatment',sep='_',into=c('Species','Treatment'),remove=F)
smaResult <- smaResult %>% dplyr::select(c(Species,Treatment,Species_Treatment,slope,elevation))
colnames(smaResult)[4] <- "Exponent"
colnames(smaResult)[5] <- "Intercept"
write.csv(smaResult,"D:\\Haibei_size\\Shrubdata\\Result\\smaResult.csv")

####
smaResult <- read.xlsx("D:\\Haibei_size\\Shrubdata\\smaResult.xlsx") 
str(smaResult)
summary(smaResult)
sd(smaResult$Intercept)


smaResult$Treatment <- factor(smaResult$Treatment,levels=c("Control","Shrub","RemovedShrub","ArtificialShrub"))
####
model.ex <- lmer(Exponent~Treatment+(1|Species),smaResult)
summary(model.ex)
anova(model.ex)
shapiro.test(resid(model.ex))
aov.ex <- aov(Exponent~Treatment+Error(Species),smaResult)
summary(aov.ex)
performance::r2(model.ex)
MuMIn::r.squaredGLMM(model.ex)
smaResult$Treatment <- as.factor(smaResult$Treatment)


emm.ex = emmeans(model.ex, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')
predictMean.emm.ex <- emm.ex$emmeans %>%
  as.data.frame()
str(predictMean.emm.ex)



Fig.Sp.Ex <- 
  ggplot(predictMean.emm.ex,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  #ylab('Light asymmetry')+
  ylab(expression(paste("Height scaling exponent",sep=" ")))+
  #ylim(0,0.05)+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(0.34,0.495)


Fig.Sp.Ex





####
model.in <- lmer(Intercept~Treatment+(1|Species),smaResult)
summary(model.in)
anova(model.in)
shapiro.test(resid(model.in))
aov.in <- aov(Intercept~Treatment+Error(Species),smaResult)
summary(aov.in)
performance::r2(model.in)
MuMIn::r.squaredGLMM(model.in)
smaResult$Treatment <- as.factor(smaResult$Treatment)
library(emmeans)
emm.in = emmeans(model.in, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')
predictMean.emm.in <- emm.in$emmeans %>%
  as.data.frame()
str(predictMean.emm.in)


predictMean.emm.in$Treatment <- factor(predictMean.emm.in$Treatment,levels=c("Control","Shrub","RemovedShrub","ArtificialShrub"))

Fig.Sp.In <- 
  ggplot(predictMean.emm.in,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  ylab(expression(paste("Intercept",sep=" ")))+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c("Control"='#E8621B', "Shrub"='#F1AD1E',"RemovedShrub"= '#8CC660',"ArtificialShrub"=  '#3188CB'))+
  mytheme+
  ylim(1.65,1.98)


Fig.Sp.In
###### Community Allometry ######
CWM_FDis_data <- read.xlsx("D:\\Haibei_size\\Shrubdata\\CWM_FDis_data_20250326.xlsx") %>%
  mutate(Plot=as.character(Plot))
CWM_FDis_data <-  separate(CWM_FDis_data,'Plot',sep='_',into=c('Block','Treatment'),remove=F)


str(CWM_FDis_data)
summary(CWM_FDis_data)

sd(CWM_FDis_data$CWM._Exponent)
sd(CWM_FDis_data$CWM._Intercept)
sd(CWM_FDis_data$FDis._Exponent)
sd(CWM_FDis_data$FDis._Intercept)

CWM_FDis_data$Treatment <- factor(CWM_FDis_data$Treatment,levels=c("Control","Shrub","RemovedShrub","ArtificialShrub"))


#### test CWM Ex
####
lmer.CWM.Ex <- lmer(CWM._Exponent ~ Treatment+(1|Block),CWM_FDis_data)
summary(lmer.CWM.Ex)
anova(lmer.CWM.Ex)
aov.CWM.Ex <- aov(CWM._Exponent ~ Treatment+Error(Block),CWM_FDis_data)
summary(aov.CWM.Ex)
MuMIn::r.squaredGLMM(lmer.CWM.Ex)

library(emmeans)
emm.CWM.Ex = emmeans(lmer.CWM.Ex, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.CWM.Ex <- emm.CWM.Ex$emmeans %>%
  as.data.frame()
str(predictMean.emm.CWM.Ex)

predictMean.emm.CWM.Ex$Treatment <- factor(predictMean.emm.CWM.Ex$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))


####
Fig.CWM.Ex <- 
  ggplot(predictMean.emm.CWM.Ex,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  #ylab('Light asymmetry')+
  ylab(expression(paste("Height sclaing exponentCWM",sep=" ")))+
  #ylim(0,0.05)+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(0.42,0.55)

Fig.CWM.Ex


#### test CWM In
####
str(CWM_FDis_data)
lmer.CWM.In <- lmer(CWM._Intercept ~ Treatment+(1|Block),CWM_FDis_data)
summary(lmer.CWM.In)
anova(lmer.CWM.In)
aov.CWM.Ex <- aov(CWM._Intercept ~ Treatment+Error(Block),CWM_FDis_data)
summary(aov.CWM.Ex)
MuMIn::r.squaredGLMM(lmer.CWM.In)

library(emmeans)
emm.CWM.In = emmeans(lmer.CWM.In, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.CWM.In <- emm.CWM.In$emmeans %>%
  as.data.frame()
str(predictMean.emm.CWM.In)

predictMean.emm.CWM.In$Treatment <- factor(predictMean.emm.CWM.In$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))


####
Fig.CWM.In <- 
  ggplot(predictMean.emm.CWM.In,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  #ylab('Light asymmetry')+
  ylab(expression(paste("InterceptCWM",sep=" ")))+
  #ylim(0,0.05)+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(1.84,2.04)

Fig.CWM.In

#### test FDis Ex
####
str(CWM_FDis_data)
lmer.FDis.Ex <- lmer(FDis._Exponent ~ Treatment+(1|Block),CWM_FDis_data)
summary(lmer.FDis.Ex)
anova(lmer.FDis.Ex)
aov.FDis.Ex <- aov(FDis._Exponent ~ Treatment+Error(Block),CWM_FDis_data)
summary(aov.FDis.Ex)
MuMIn::r.squaredGLMM(lmer.FDis.Ex)

library(emmeans)
emm.FDis.Ex = emmeans(lmer.FDis.Ex, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.FDis.Ex <- emm.FDis.Ex$emmeans %>%
  as.data.frame()
str(predictMean.emm.FDis.Ex)

predictMean.emm.FDis.Ex$Treatment <- factor(predictMean.emm.FDis.Ex$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))


####
Fig.FDis.Ex <- 
  ggplot(predictMean.emm.FDis.Ex,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  ylab(expression(paste("Height sclaing exponentFDis",sep=" ")))+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(0.38,1.15)

Fig.FDis.Ex

#### test FDis In
####
str(CWM_FDis_data)
lmer.FDis.In <- lmer(FDis._Intercept ~ Treatment+(1|Block),CWM_FDis_data)
summary(lmer.FDis.In)
anova(lmer.FDis.In)
aov.FDis.Ex <- aov(FDis._Intercept ~ Treatment+Error(Block),CWM_FDis_data)
summary(aov.FDis.Ex)
MuMIn::r.squaredGLMM(lmer.FDis.In)


library(emmeans)
emm.FDis.In = emmeans(lmer.FDis.In, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.FDis.In <- emm.FDis.In$emmeans %>%
  as.data.frame()
str(predictMean.emm.FDis.In)

predictMean.emm.FDis.In$Treatment <- factor(predictMean.emm.FDis.In$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))


####
Fig.FDis.In <- 
  ggplot(predictMean.emm.FDis.In,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  #ylab('Light asymmetry')+
  ylab(expression(paste("InterceptFDis",sep=" ")))+
  #ylim(0,0.05)+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(0.61,0.83)

Fig.FDis.In

topptx(Fig.FDis.In,'D:/MaquShrub/FigureArticle/Fig.FDis.In_20250327.pptx',width = 5.513,height = 3.80)













####4.cor.CWM####

CWM_FDis_data <- read.xlsx("D:\\Haibei_size\\Shrubdata\\CWM_FDis_data_20250326.xlsx") %>%
  mutate(Plot=as.character(Plot))
str(CWM_FDis_data)



#### Corelation analysis CWMandFDis####
str(CWM_FDis_data)

Cor.Allometry <- CWM_FDis_data %>% dplyr::select(c(CWM._Exponent,CWM._Intercept,FDis._Exponent,FDis._Intercept))
str(Cor.Allometry)
colnames(Cor.Allometry) <- c("ExponentCWM","InterceptCWM","ExponentFDis","InterceptFDis")

#library("corrplot")
pm = cor.mtest(Cor.Allometry,conf.level = 0.95)

env_corrplot <- cor(Cor.Allometry,method = "pearson",use = "complete.obs")

corrplot(env_corrplot,
         tl.col = 'black',
         col=colorRampPalette(c("#AB466C","white","#476BAC"))(200),
         #col = rev(COL2('RdBu', 200)),
         number.cex = .5,
         diag = F)
corrplot(env_corrplot,
         add = T,
         method = "number",
         tl.col = 'black',
         col=colorRampPalette(c("#AB466C","white","#476BAC"))(200),
         #col = rev(COL2('RdBu', 200)),
         diag = F,
         type = 'lower',
         cl.pos = 'n',
         tl.pos = 'n')
corrplot(env_corrplot,
         add = T,
         tl.col = 'black',
         col=colorRampPalette(c("#AB466C","white","#476BAC"))(200),
         #col = rev(COL2('RdBu', 200)),
         method = 'pie', 
         diag = T,
         type = 'upper',
         cl.pos = 'n',
         tl.pos = 'n',
         p.mat = pm$p,pch.col = 'black')







#### 5.Soil ####
MaquSoil <- read.xlsx("D:\\Haibei_size\\Shrubdata\\MaquSoil.xlsx",sheet = 1) %>%
  unite(Block_Treatment,Block,Treatment,sep='_',remove = F) 
str(MaquSoil)

MaquSoil$Treatment <- factor(MaquSoil$Treatment,levels=c("Control","Shrub","RemovedShrub","ArtificialShrub"))

ord <- decorana(MaquSoil[,c('SWC','pH','STC','STN','SAP')])
summary(ord)
dim(MaquSoil)
ord.maqusoil <- rda(MaquSoil[,c('SWC','pH','STC','STN','SAP')], scale = TRUE)
summary(ord.maqusoil)
Soil_axisscores<-scores(ord.maqusoil, choices=c(1,2))
Soil_axisscores
Soil_axisscores$species
Soil_axisscores$sites

biplot(ord.maqusoil)
Soil_PCA <- as.data.frame(Soil_axisscores$sites)
Soil_axisscores$species
Soil_PCA <- cbind(Soil_PCA,MaquSoil$Block_Treatment,MaquSoil$Treatment)
colnames(Soil_PCA) <- c('Soil_PC1','Soil_PC2','Block_Treatment','Treatment')
str(Soil_PCA)
summary(Soil_PCA)
sd(Soil_PCA$SWC)
sd(Soil_PCA$pH)
sd(Soil_PCA$STC)
sd(Soil_PCA$STN)
sd(Soil_PCA$SAP)
sd(Soil_PCA$Soil_PC1)




Soil_PCA$Treatment <- as.factor(Soil_PCA$Treatment)
write.csv(Soil_PCA,"D:\\Haibei_size\\Shrubdata\\Result\\Soil_PCA.csv")

Soil_PCA_var<- as.data.frame(Soil_axisscores$species)
colnames(Soil_PCA_var) <- c('Soil_PC1','Soil_PC2')
Soil_PCA_var$var <- rownames(Soil_PCA_var)
#PCA plot
biplot(ord.maqusoil,
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"))
str(Soil_PCA_var)
class(Soil_PCA_var)
Soil_PCA <- left_join(MaquSoil,Soil_PCA)

Soil_PCA$Treatment <- factor(Soil_PCA$Treatment,levels=c("Control","Shrub","RemovedShrub","ArtificialShrub"))


str(Soil_PCA)
correlation <- cor(Soil_PCA$Soil_PC1, Soil_PCA$SWC, method = "pearson")
print(correlation)
correlation <- cor.test(Soil_PCA$Soil_PC1, Soil_PCA$SWC, method = "pearson")
print(correlation)


correlation <- cor(Soil_PCA$Soil_PC1, Soil_PCA$pH, method = "pearson")
print(correlation)
correlation <- cor.test(Soil_PCA$Soil_PC1, Soil_PCA$pH, method = "pearson")
print(correlation)


correlation <- cor(Soil_PCA$Soil_PC1, Soil_PCA$SAP, method = "pearson")
print(correlation)
correlation <- cor.test(Soil_PCA$Soil_PC1, Soil_PCA$SAP, method = "pearson")
print(correlation)


correlation <- cor(Soil_PCA$Soil_PC1, Soil_PCA$STC, method = "pearson")
print(correlation)
correlation <- cor.test(Soil_PCA$Soil_PC1, Soil_PCA$STC, method = "pearson")
print(correlation)


correlation <- cor(Soil_PCA$Soil_PC1, Soil_PCA$STN, method = "pearson")
print(correlation)
correlation <- cor.test(Soil_PCA$Soil_PC1, Soil_PCA$STN, method = "pearson")
print(correlation)




#### Correlation analysis ####
str(Soil_PCA)
Cor.MQsoil <- Soil_PCA %>% dplyr::select(SWC,pH,SAP,STC,STN,Soil_PC1)
str(Cor.MQsoil)
colnames(Cor.MQsoil) <- c("Soil water content",
                          "Soil pH",
                          "Soil available phosphorus",
                          "Soil total carbon",
                          "Soil total nitrogen",
                          "Soil PC1")

#library("corrplot")
pm = cor.mtest(Cor.MQsoil,conf.level = 0.95)

env_corrplot <- cor(Cor.MQsoil,method = "pearson",use = "complete.obs")

corrplot(env_corrplot,
         tl.col = 'black',
         col=colorRampPalette(c("#AB466C","white","#476BAC"))(200),
         #col = rev(COL2('RdBu', 200)),
         number.cex = .5,
         diag = F)
corrplot(env_corrplot,
         add = T,
         method = "number",
         tl.col = 'black',
         col=colorRampPalette(c("#AB466C","white","#476BAC"))(200),
         #col = rev(COL2('RdBu', 200)),
         diag = F,
         type = 'lower',
         cl.pos = 'n',
         tl.pos = 'n')
corrplot(env_corrplot,
         add = T,
         tl.col = 'black',
         col=colorRampPalette(c("#AB466C","white","#476BAC"))(200),
         #col = rev(COL2('RdBu', 200)),
         method = 'pie', 
         diag = T,
         type = 'upper',
         cl.pos = 'n',
         tl.pos = 'n',
         p.mat = pm$p,pch.col = 'black')

####
Fig.MaquSoilPCA <- 
  ggplot(data = Soil_PCA, aes(x = Soil_PC1, y = Soil_PC2)) +#,group=N, color = N
  geom_point(aes(color = Treatment),
             size=2) +#
  stat_ellipse(aes(color = Treatment),level = 0.95, show.legend = T,size = 0.5) +#stat_ellipse(aes(fill = N), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) 
  scale_colour_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB')) +
  #scale_shape_manual(values = c(17, 14)) +
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB')) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = 'black', 
                                        fill = 'transparent'), 
        legend.key = element_rect(fill = 'transparent'),
        legend.text = element_text(size = 7)) +
  geom_vline(xintercept = 0, color = 'gray', size = 0.5,linetype='dashed') +
  geom_hline(yintercept = 0, color = 'gray', size = 0.5,linetype='dashed') +
  geom_segment(data = Soil_PCA_var, 
               aes(x = 0, y = 0, xend = Soil_PC1, yend = Soil_PC2), 
               arrow = arrow(length = unit(0.2, 'cm')), 
               color = 'black', size = 0.75) +
  geom_text(data = Soil_PCA_var, 
            aes(x = Soil_PC1 * 1.2, y = Soil_PC2 * 1.2, label = var), 
            color = 'black', 
            size = 3) +
  labs(x = 'PC1 (52.12% explained var.)', 
       y = 'PC2 (23.42% explained var.)')+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 12, color = "black"), 
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))+
  theme(legend.position = c(0.2, 0.8),legend.background = element_blank() )

Fig.MaquSoilPCA



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           paint soil                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str(Soil_PCA)

Soil_PCA$Treatment <- factor(Soil_PCA$Treatment,levels=c("Control","Shrub","RemovedShrub","ArtificialShrub"))
summary(Soil_PCA)

lmer.Soil_PC1 <- lmer(Soil_PC1 ~ Treatment+(1|Block),Soil_PCA)
summary(lmer.Soil_PC1)
anova(lmer.Soil_PC1)
library(lmerTest)

aov.Soil_PC1 <- aov(Soil_PC1 ~ Treatment+Error(Block),Soil_PCA)
summary(aov.Soil_PC1)
MuMIn::r.squaredGLMM(lmer.Soil_PC1)

library(emmeans)
emm.Soil_PC1 = emmeans(lmer.Soil_PC1, specs = pairwise ~ Treatment, type = 'response', adjust = 'none')

predictMean.emm.Soil_PC1 <- emm.Soil_PC1$emmeans %>%
  as.data.frame()
str(predictMean.emm.Soil_PC1)

predictMean.emm.Soil_PC1$Treatment <- factor(predictMean.emm.Soil_PC1$Treatment,levels = c("Control","Shrub","RemovedShrub","ArtificialShrub"))


Fig.Soil_PC1 <- 
  ggplot(predictMean.emm.Soil_PC1,
         aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,color=Treatment))+
  geom_pointrange(aes(x=Treatment,y=emmean,ymin=emmean-SE,ymax=emmean+SE,fill=Treatment))+
  #ylab('Light asymmetry')+
  ylab(expression(paste("Soil",sep=" ")))+
  #ylim(0,0.05)+
  theme_bw()+
  theme(text=element_text(family=""))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_blank ())+
  theme(panel.grid = element_blank())+
  theme(legend.position="none")+
  theme(text = element_text(family = ""))+
  scale_fill_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  scale_color_manual(values = c('#E8621B', '#F1AD1E', '#8CC660', '#3188CB'))+
  mytheme+
  ylim(-0.48,0.45)

Fig.Soil_PC1


#####################SEM#######################################
####
Diversity<-read.xlsx("D:\\Haibei_size\\Shrubdata\\Diversity_20250326.xlsx")
CWM_FDis_data<-read.xlsx("D:\\Haibei_size\\Shrubdata\\CWM_FDis_data_20250326.xlsx")
MQcommunity<-read.xlsx("D:\\Haibei_size\\Shrubdata\\MaquCommunity_20240621.xlsx")
Soil_PCA<-read.xlsx("D:\\Haibei_size\\Shrubdata\\Soil_PCA.xlsx")%>%unite(Block_Treatment,Block,Treatment,sep = "-",remove = F)

str(MQcommunity)
MQcommunity_SEM <- MQcommunity %>%
  dplyr::select(c(Block_Treatment,Block,Treatment,AS,S,RS,Soil_PC1,LightAsymmetry))
str(MQcommunity_SEM)
colnames(MQcommunity_SEM)[1] <- "Plot"

str(Diversity)

str(CWM_FDis_data)

SEMdata_new <- merge(MQcommunity_SEM,CWM_FDis_data)
SEMdata_new <- merge(SEMdata_new,Diversity)
str(SEMdata_new)
SEMdata_new<-SEMdata_new%>%rename("CWM.Ex"="CWM._Exponent","CWM.In"="CWM._Intercept",
                                  "FDis.Ex"="FDis._Exponent","FDis.In"="FDis._Intercept")
####
SEMmodel3 <- psem(lmer(SR~CWM.In+FDis.Ex+FDis.In+(1|Block),data = SEMdata_new),
                  lmer(Pielou~CWM.In+FDis.Ex+FDis.In+(1|Block),data = SEMdata_new),
                  
                  #lmer(CWM.Ex~LightAsymmetry+(1|Block),data = SEMdata_new),
                  lmer(CWM.In~LightAsymmetry+Soil_PC1+S+(1|Block),data = SEMdata_new),
                  lmer(FDis.Ex~LightAsymmetry+RS+AS+(1|Block),data = SEMdata_new),
                  lmer(FDis.In~LightAsymmetry+Soil_PC1+(1|Block),data = SEMdata_new),
                  
                  lmer(Soil_PC1~LightAsymmetry+(1|Block),data = SEMdata_new),
                  lmer(LightAsymmetry~S+RS+AS+(1|Block),data = SEMdata_new),
                  
                  FDis.In %~~% CWM.In,
                  FDis.In %~~% FDis.Ex,
                  
                  data=SEMdata_new)
summary(SEMmodel3)
SEMresult <- summary(SEMmodel3)
write.xlsx(SEMresult$coefficients,"D:\\Haibei_size\\Shrubdata\\Result\\Maqu_SEMresult_20250326.xlsx")
