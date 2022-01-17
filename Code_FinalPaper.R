###Statistics Paper 

##Preparations

#Packages
packages <- c("haven", "dplyr", "ggplot2", "MASS", "AER",
              "brant", "summarytools", "sjPlot", "effects")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#Import data

ESS9e03_1_stata <- read_stata("C:/Users/judit/Downloads/ESS9e03_1.stata.zip")
View(ESS9e03_1_stata)

ESS_Data<- dplyr::select(ESS9e03_1_stata, cntry,  name, essround, edition, idno, dweight, 
                           pspwght,  pweight,  anweight, lrscale, freehms, hmsfmlsh, hmsacld,
                           rlgblg, rlgdnm, rlgdgr, rlgatnd,  pray, atcherp, hhmmb, gndr, agea,
                           domicil, edulvlb, eisced, mnactic, isco08, uemp12m, hinctnta)

ESS_Data_predictors <-dplyr::select(ESS_Data, gender, unemployedlastyear, domicil,
                                    education,importanceRel,agegroup, polorientation,
                                    income)

##Prepare variables

#Homophobia
summary(ESS_Data$freehms)
Dist_freehms <- ggplot(ESS_Data, aes(freehms))+
  geom_bar()
Dist_freehms
summary(ESS_Data$hmsacld)
Dist_hmsacld <- ggplot(ESS_Data, aes(hmsacld))+
  geom_bar()
Dist_hmsacld
summary(ESS_Data$hmsfmlsh)

ESS_Data$hmsfmlsh_inv <- dplyr::recode(as.character(ESS_Data$hmsfmlsh), 
                                "1"="5", "2"="4", "3"="3", "4"="2", "5"="1")
ESS_Data$hmsfmlsh_inv <-as.numeric(ESS_Data$hmsfmlsh_inv)
summary(ESS_Data$hmsfmlsh_inv)
Dist_hmsfmlsh_inv <- ggplot(ESS_Data, aes(hmsfmlsh_inv))+
  geom_bar()
Dist_hmsfmlsh_inv

ESS_Data$homophobia <- (ESS_Data$freehms+ESS_Data$hmsfmlsh_inv+ESS_Data$hmsacld)/3
summary(ESS_Data$homophobia)
DistHomophobia <- ggplot(ESS_Data, aes(homophobia))+
  geom_bar()
DistHomophobia

ESS_Data$homophobiaord<- cut(ESS_Data$homophobia, breaks= c(1, 1.5, 2.5,3.5,4.5,5),
                             labels = c("1", "2", "3", "4", "5"))
summary(ESS_Data$homophobiaord) 
Dist_homophobiaord <- ESS_Data%>%
  filter(!is.na(homophobiaord))%>%
  ggplot( aes(homophobiaord))+
  geom_bar()+
  labs(y="Amount",
       x="Homophobia",
       title = "Distribution of answers for dependent variable",
       caption = "Datasource: ESS 2018" )+
  theme_classic()

Dist_homophobiaord

#Age
summary(ESS_Data$agea)
ESS_Data<-ESS_Data%>%
  mutate(agegroup= cut(agea, breaks=c(14,30,45,67,91), labels= c("14-30", "31-45","46-67", "68-91")))
summary(ESS_Data$agegroup)


#Importance of religion
ESS_Data$importanceRel <- (ESS_Data$rlgdgr+ ESS_Data$rlgatnd+ ESS_Data$pray)/3
summary(ESS_Data$importanceRel)
DistImportanceRel<- ggplot(ESS_Data, aes(importanceRel))+
  geom_bar()
DistImportanceRel

#Education
ESS_Data$eisced_factor <-(as.factor(ESS_Data$eisced)) 
summary(ESS_Data$eisced_factor)
ESS_Data$education<- dplyr::recode(ESS_Data$eisced_factor, "1"="low", "2" ="low",
                                "3"="normal", "4"="normal", "5"="normal",
                                "6"="high", "7"="high", .default = NA_character_)

#Income
ESS_Data$income <-(ESS_Data$hinctnta) #es ist eine kategoriale Variabel, evt noch in kleinere Kategorien teilen

#Unemployment 
ESS_Data$uemp12m<- as.factor(ESS_Data$uemp12m)
ESS_Data$unemployedlastyear <- dplyr::recode(ESS_Data$uemp12m, "1"="Yes", "2"="No")
summary(ESS_Data$unemployedlastyear)

#Gender 
ESS_Data$gender <- dplyr::recode(as.factor(ESS_Data$gndr), "2"="Women", "1"="Men")
summary(ESS_Data$gender)

#Political Orientation 
summary(ESS_Data$lrscale)
ESS_Data<-ESS_Data%>%
  mutate(polorientation= cut(lrscale,
                             breaks=c(1, 3, 6, 9),
                             labels= c("left", "center","right")))
summary(ESS_Data$polorientation)


#Urbanization
ESS_Data$domicil <- (as.factor(ESS_Data$domicil))
ESS_Data$domicil<- dplyr::recode(ESS_Data$domicil, "1"="Urban", "2"="Urban",
                                 "3"="Rural", "4"="Rural", "5"="Rural")
summary(ESS_Data$domicil)


##Analysis

#Descriptive statistics

print(dfSummary(ESS_Data_predictors), method = 'browser')

#Multiple step ordinal regression

model1 <- polr(data = ESS_Data,
               homophobiaord~ income+
                 gender+
                 unemployedlastyear,
               Hess = TRUE)
summary(model1)
coeftest(model1)
brant(model1)

model2 <- polr(data = ESS_Data,
               homophobiaord~ gender+
                 unemployedlastyear+
                 domicil+
                 education+
                 income, Hess = TRUE)
summary(model2)
coeftest(model2)
brant(model2) 

model3 <- polr(data = ESS_Data,
               homophobiaord~ gender+
                 unemployedlastyear+
                 domicil+
                 education+
                 importanceRel+
               income, Hess = TRUE)
summary(model3)
coeftest(model3)
brant(model3) 

model4 <- polr(data = ESS_Data,
               homophobiaord~ gender+
                 unemployedlastyear+
                 domicil+
                 education+
                 importanceRel+
                 agegroup+
               income, Hess = TRUE)
summary(model4)
coeftest(model4)
brant(model4) 

model5 <- polr(data = ESS_Data,
               homophobiaord~ gender+
                 unemployedlastyear+
                 domicil+
                 education+
                 importanceRel+
                 agegroup+
                 polorientation+
                 income, Hess = TRUE, na.action = na.omit)
summary(model5)
coeftest(model5)
brant(model5) 

