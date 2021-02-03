library("foreign")
library("tidyverse")
library("lmtest")
library("stargazer")
library(ggpubr)
library(moments)

#Chargement des données
data<-read.csv("/Users/Abdoul_Aziz_Berrada/Documents/M1_EcoStat/Cours-M1/M1_EcoStat_EcoSanté/Projet Eco_Santé/insurance.csv")

#Distribution non centree des couts
hist(data$charges, main = "Distribution des couts medicaux")
skewness(data$charges, na.rm = TRUE) # La skewness est de 1.51 (skewness positive donc risque de problème de biais...)

#On met en log charges:
data$charges<-log(data$charges)

#Distribution centrée des coûts
hist(data$charges, main = "Distribution des couts medicaux (log)")

#         Stats descriptives
stargazer(data,column.sep.width = "2pt",font.size ="small")

#Distribution des couts medicaux par sexe
ggplot(data, aes(x = sex, y = charges, color = sex)) +
  geom_boxplot() +
  labs(title = "Couts medicaux par sexes") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

#Selon fumeur ou pas
ggplot(data, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot()+
  labs(title = "Couts medicaux selon fumeur ou pas") +
  theme(plot.title = element_text(hjust = 0.8),
        legend.position = "none")
#   Par nombre d'enfants
describeBy(data$charges,data$children)

ggplot(data,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("Coûts médicaux selon le nombre d'enfants")
#     Par IMC et corpulence
#On créé une variable qui va prendre en compte la corpulence des individus
data <- data %>%
  mutate(corpulence = ifelse(bmi < 18.5, "maigreur", 
                             ifelse( bmi >= 18.5 & bmi < 25, "corpulence normale",
                                     ifelse(bmi >= 25 & bmi < 30, "surpoids",
                                            ifelse(bmi >= 30 & bmi < 40, "obésité", 
                                                   ifelse(bmi >= 40, "obésité morbide", NA))))))


ggplot(data, aes(x = corpulence, y = charges, color = corpulence)) +
  geom_boxplot() +
  labs(title = "Coûts médicaux par corpulence",
       x = "Corpulence") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

#Age et fumeur      
ggplot(data, aes(x = age, y = charges, color = smoker)) +
  geom_point()+
  labs(title = "Couts médicaux par age et fumeur") +
  theme(plot.title = element_text(hjust = 0.5))

#On supprime les outliers de la variable bmi:
Q1 <- quantile(data$bmi, .25)
Q3 <- quantile(data$bmi, .75)
IQR <- IQR(data$bmi)
data_new <- subset(data, data$bmi> (Q1 - 1.5*IQR) & data$bmi< (Q3 + 1.5*IQR))

#Création de la variable "obèse : si bmi>30 la personne est obése 
data_new <- data_new %>%
  mutate(obese = ifelse(bmi >= 30, "yes", "no"))


#         Modèles de regression 

#   Premier modele
M1 <- lm(charges~age+smoker,data_new)
summary(M1)
stargazer(M1,title ="Modèle 1",column.sep.width ="1pt",summary=FALSE,font.size ="small",no.space=TRUE)

#   Deuxieme modele
M2 <- lm(charges~age + sex + children + smoker + obese, data=data_new)
summary(M2)
stargazer(M2,title ="Modèle 2",column.sep.width ="1pt",summary=FALSE,font.size ="small",no.space=TRUE)
#   Troisieme modele
#On se dit que les coûts médicaux ne sont pas une fonction linéaire de l'âge
#On créé age2 qui est le carré de l'âge
data_new$age2 <- data_new$age^2
M3 <- lm(charges ~ age + age2 + children + obese*smoker + sex, data=data_new)
summary(M3)
stargazer(M3,title ="Modèle 3",column.sep.width ="1pt",summary=FALSE,font.size ="small",no.space=TRUE)

#     Autocorrélation ?
#ACF et PACF
acf(residuals(M3))
pacf(residuals(M3))

#     Hétérocédasticité ?
#Breusch Pagan
bptest(M3)

#Quatrième modèle
data_new$resi <- M3$residuals
varfunc.ols <- lm(log(resi^2) ~ charges, data = data_new)
data_new$varfunc <- exp(varfunc.ols$fitted.values)
M4 <- lm(charges ~ age + age2 + children + obese*smoker + sex, weights = 1/sqrt(varfunc), data = data_new)
summary(M4)
stargazer(M4,title ="Modèle 3 corrigé",column.sep.width ="1pt",summary=FALSE,font.size ="small",no.space=TRUE)

