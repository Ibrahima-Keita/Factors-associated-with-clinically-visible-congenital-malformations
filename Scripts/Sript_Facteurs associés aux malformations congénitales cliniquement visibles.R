## Taille 
 


bdd_cas_tem <- read.csv2("bdd_cas_tem_Rstudio021223.csv", stringsAsFactors = TRUE, fileEncoding = "Latin1")

str(bdd_cas_tem)
summary(bdd_cas_tem)

##
bdd_cas <- read.csv2("bdd_cas_R.csv", header=T, stringsAsFactors = TRUE, check.names = T, fileEncoding = "Latin1")

str(bdd_cas)
summary(bdd_cas)

##
bdd_temoins <- read.csv2("bdd_temoins_R.csv", header=T, stringsAsFactors = TRUE, check.names = T, fileEncoding = "Latin1")

str(bdd_temoins)
summary(bdd_temoins)

#################################### Les statistiques descriptives ############################################
library(prettyR)

describ_gross <- describe(bdd_cas_tem, num.desc = c("mean", "sd", "median", "min", "max", "valid.n"))
describ_gross
capture.output(describ_gross, file = "Dr Maiga Haby")

## cas
describ_cas <- describe(bdd_cas, num.desc = c("mean", "sd", "median", "min", "max", "valid.n"))
describ_cas
capture.output(describ_cas, file = "Dr Maiga Haby")

## Temoins

describ_temoins <- describe(bdd_temoins, num.desc = c("mean", "sd", "median", "min", "max", "valid.n"))
describ_temoins
capture.output(describ_temoins, file = "Dr Maiga Haby")


############################################ Analyses bivarie ##############################################

library(finalfit)
library(tidyverse)
library(dplyr)
library(questionr)

#### Preparation des donnees

dput(names(bdd_cas_tem)) # Noms des variables

ff_glimpse(bdd_cas_tem) # ournit une vue d'ensemble pratique de toutes les données d'un tableau ou d'une base de données. Il est particulièrement important que les facteurs soient correctement spécifiés. C'est pourquoi ff_glimpse() sépare les variables en variables continues et en variables catégorielles. 

missing_glimpse(bdd_cas_tem) # vous souhaitez voir les variables dans l'ordre dans lequel elles apparaissent dans la base de données ou le tableau


# convertion Nb_IVGn en factor

bdd_cas_tem$Nb_IVG <- as.factor(bdd_cas_tem$Nb_IVG) 

# to do interactive recoding of a categorical variable (character or factor)

#irec(bdd_cas_tem, Sexe_enft)

## Regrpoupement et Recodage de bdd_cas_tem$Sexe.enft en bdd_cas_tem$Sexe.enft_rec

# Regrpoupement

bdd_cas_tem$Sexe_enft_rec <- bdd_cas_tem$Sexe_enft %>%
  fct_recode(
    "F" = "Indeter"
  )

table(bdd_cas_tem$Sexe_enft_rec)

# Recoder sexe enfant en 1 et 2
bdd_cas_tem$Sexe_enft_recb<-factor(bdd_cas_tem$Sexe_enft_rec, labels=c(1,2), levels=c("F", "M"))
table(bdd_cas_tem$Sexe_enft_recb)

# Recoder cas_tem en 1 et 0
bdd_cas_tem$Cas_temb<-factor(bdd_cas_tem$Cas_tem, labels=c(1,0), levels=c("Cas", "Temoin"))
table(bdd_cas_tem$Cas_temb)
bdd_cas_tem$Cas_tembr  <- relevel(bdd_cas_tem$Cas_temb, "0")
table(bdd_cas_tem$Cas_tembr)

#irec() # Interface graphique de recodage
## Recodage de bdd_cas_tem$Cas_tem en bdd_cas_tem$Cas_tem_rec
#bdd_cas_tem$Cas_tem_rec <- as.character(bdd_cas_tem$Cas_tem)
#bdd_cas_tem$Cas_tem_rec[bdd_cas_tem$Cas_tem == "Cas"] <- "Oui"
#bdd_cas_tem$Cas_tem_rec[bdd_cas_tem$Cas_tem == "Temoin"] <- "Non"
#bdd_cas_tem$Cas_tem_rec <- factor(bdd_cas_tem$Cas_tem_rec)
#table(bdd_cas_tem$Cas_tem_rec)


#irec(bdd_cas_tem, Taille_nais_Tranche)

## Recodage de Taille_nais_Tranche en Taille_nais_Tranche_rec
table(bdd_cas_tem$Taille_nais_Tranche)
levels(bdd_cas_tem$Taille_nais_Tranche)
bdd_cas_tem$Taille_nais_Tranche_rec <- bdd_cas_tem$Taille_nais_Tranche %>%
  fct_recode(
    "Grand" = "Très GRANT"
  )

table(bdd_cas_tem$Taille_nais_Tranche_rec)

## Recodage de Poids_nais_Tranche en Poids_nais_Tranche_rec
table(bdd_cas_tem$Poids_nais_Tranche)
freq(bdd_cas_tem$Poids_nais_Tranche)
levels(bdd_cas_tem$Poids_nais_Tranche)
bdd_cas_tem$Poids_nais_Tranche_rec <- bdd_cas_tem$Poids_nais_Tranche %>%
  fct_recode(
    ">2,5kg" = "2,5 - 4kg",
    ">2,5kg" = ">4kg"
  )

table(bdd_cas_tem$Poids_nais_Tranche_rec)


# Changer la categorie de reference d'une variable qualitative

## Gestite_cat
table(bdd_cas_tem$Gestite_cat)
levels(bdd_cas_tem$Gestite_cat)
bdd_cas_tem$Gestite_cat2 <- relevel(bdd_cas_tem$Gestite_cat, ref="Primigeste")
table(bdd_cas_tem$Gestite_cat2)
levels(bdd_cas_tem$Gestite_cat2)

#
table(bdd_cas_tem$Gestite_cat2)
levels(bdd_cas_tem$Gestite_cat2)
bdd_cas_tem$Gestite_cat2_rec <- bdd_cas_tem$Gestite_cat2 %>%
  fct_recode(
    "Multigeste" = "Paucigeste",
    "Multigeste" = "Multigeste",
  )

table(bdd_cas_tem$Gestite_cat2_rec)

# Niv_instr_mere
table(bdd_cas_tem$Niv_instr_mere)
levels(bdd_cas_tem$Niv_instr_mere)
bdd_cas_tem$Niv_instr_mere2 <- relevel(bdd_cas_tem$Niv_instr_mere, ref="Superieur")
table(bdd_cas_tem$Niv_instr_mere2)

#
table(bdd_cas_tem$Niv_instr_mere2)
levels(bdd_cas_tem$Niv_instr_mere2)
bdd_cas_tem$Niv_instr_mere2_rec <- bdd_cas_tem$Niv_instr_mere2 %>%
  fct_recode(
    "Secondaire" = "Sec_cycle1",
    "Secondaire" = "Sec_cycle2",
    "Aucun"      = "Alphab",
    "Aucun"      = "Aucun"
    
  )

table(bdd_cas_tem$Niv_instr_mere2_rec)

# Poids_nais_Tranche
table(bdd_cas_tem$Poids_nais_Tranche)

bdd_cas_tem$Poids_nais_Tranche2 <- relevel(bdd_cas_tem$Poids_nais_Tranche, ref="2,5 - 4kg")
table(bdd_cas_tem$Poids_nais_Tranche2)

# Taille_nais_Tranche
table(bdd_cas_tem$Taille_nais_Tranche_rec)

bdd_cas_tem$Taille_nais_Tranche_rec2 <- relevel(bdd_cas_tem$Taille_nais_Tranche_rec, ref= "Moyen")
table(bdd_cas_tem$Taille_nais_Tranche_rec2)

## Recodage de bdd_cas_tem$IMC_app en bdd_cas_tem$IMC_app_rec
table(bdd_cas_tem$IMC_mere_cat)
bdd_cas_tem$IMC_cat_rec <- bdd_cas_tem$IMC_mere_cat %>%
  fct_recode(
    "0" = "Normal",
    "1" = "Maigreur",
    "2" = "Surpoids",
    "3" = "Obesi_mod",
    "4" = "Obesite_severe"
    
  )
table(bdd_cas_tem$IMC_cat_rec)
table(bdd_cas_tem$IMC_cat_rec, bdd_cas_tem$IMC_mere_cat)

## IMC_mere_cat

table(bdd_cas_tem$IMC_mere_cat)
levels(bdd_cas_tem$IMC_mere_cat)
bdd_cas_tem$IMC_mere_cat_rec <- bdd_cas_tem$IMC_mere_cat %>%
  fct_recode(
    "Normal" = "Maigreur",
    "Obesité" = "Obesite_severe",
    "Obesité" = "Obesi_mod"
  )

table(bdd_cas_tem$IMC_mere_cat_rec)

## Age_mere_cat
table(bdd_cas_tem$Age_mere_cat, bdd_cas_tem$Cas_temb)
levels(bdd_cas_tem$Age_mere_cat)
bdd_cas_tem$Age_mere_cat_rec <- bdd_cas_tem$Age_mere_cat %>%
  fct_recode(
    "<= 34ans" = "< 20 ans",
    "<= 34ans"= "20 - 34 ans"
    
  )

table(bdd_cas_tem$Age_mere_cat_rec)


##################################################################################################
######################################## Analyse bivariée ########################################
##################################################################################################

#### chez enfant

dep <- "Cas_tembr"
vars <- c("Sexe_enft_rec", "Poids_nais_Tranche2", "Taille_nais_Tranche_rec2", "PC_nais")

# Analyse bivariee de la variable dependante et les variables explicatives

tab_sum1 <- summary_factorlist(bdd_cas_tem, dep, vars, p = TRUE, na_include = T, add_dependent_label = TRUE, p_cat = "fisher")
tab_sum1

# Analyse bivariee et multivariee avec les OR et IC 

tab_fin1 <- finalfit(bdd_cas_tem, dep, vars)
tab_fin1

######
library(survival)
mod0 <- clogit(formula = as.numeric(Cas_temb) ~ Sexe_enft_rec + Poids_nais_Tranche2 + Taille_nais_Tranche_rec2 + PC_nais + strata(ID_app), data = bdd_cas_tem)
mod0
summary(mod0)

exp(coef(mod0))

exp(cbind(coef(mod0), confint(mod0)))
#########

# Importation des resultats en tableau dans fichier csv

dir.create("Resultats_finaux_25-11-2024")
write.csv2(tab_fin1, here::here("Resultats_finaux_25-11-2024","table_enfant.csv"),row.names = FALSE) 

#### chez parent: ,"Parite"

dep2 <- "Cas_tembr"
vars2<- c("Age_mere_cat_rec", "Milieu_resid_mere", "Niv_instr_mere2_rec","Etat_matr_mere", "IMC_mere_cat_rec" , "Gestite_cat2_rec","fer_acfol", "Expo_rayonX",  
          "cons_medic", "Expo_chim")

# Analyse bivariee de la variable dependante et les variables explicatives

tab_sum2 <- summary_factorlist(bdd_cas_tem, dep2, vars2, p = TRUE, add_dependent_label = TRUE, column = FALSE, p_cat = "fisher")
tab_sum2

# Analyse bivariee et multivariee avec les OR et IC

tab_fin2 <- finalfit(bdd_cas_tem, dep2, vars2)
tab_fin2

# Importation des resultats en tableau dans fichier csv

dir.create("Resultats_finaux_25-11-2024")
write.csv2(tab_fin2, here::here("Resultats_finaux_25-11-2024","table_Parent.csv"),row.names = FALSE ) 

###############################################################################################
############################# Analyse en Composante Principal ################################
##############################################################################################

library(psy) 

bdd_cas_tem$cas<- bdd_cas_tem$Cas_tem=="cas"

response <- "Cas_tem"
explanatory <- c("Age_cat", "Residence", "Etat_matr", "Niv_instr", "Profession", "Gestite_cat", 
                 "Parite","Nb_IVG", "ATCD_Malf", "Tabac", "Alcool", "fer_acfol", "Expo_rayonX",  
                 "cons_medic", "med_trad", "Expo_chim", "Maladie_chron", "Prof_pere")
fpca(data = bdd_cas_tem, y = response, x = explanatory, cx=0.75, pvalues="No", partial="Yes", 
     input="data", contraction="Yes", sample.size=1)

####

response2 <- "Cas_tem"
explanatory2 <- c("Residence", "Niv_instr", "Profession", "Apparten", "Nat_Habit", "Nat_sol", 
                  "Nat_toit", "Type_WC", "Type_douche", "Mod_evac_ord", "Mod_eva.O_us", 
                  "Temps_nor", "Sourcep_eclair", "Sourcep_cuis", "Voiture", "Climatis", 
                  "Congelo", "Moto", "Refriger", "Tele", "Ventil", "Nb_voiture", "Nb_climatis", 
                  "Nb_congelo", "Nb_moto", "Nb_refriger", "Nb_tele", "Nb_ventil", "internet", 
                  "Prof_pere")
fpca(data = bdd_cas_tem, y = response2, x = explanatory2, cx=0.75, pvalues="No", partial="Yes", 
     input="data", contraction="Yes", sample.size=1)


##############################################################################################
############################ Regression logistique conditionnelle ############################ 
##############################################################################################

library(package = "survival")

bdd_cas_tem$Cas_tembr <- as.factor(bdd_cas_tem$Cas_tembr)
bdd_cas_tem$Cas_tembr <- as.numeric(as.factor(bdd_cas_tem$Cas_tembr))
 
#### Regression logistique conditionnelle   : + Niv_instr_mere2_rec +  fer_acfol

mod3 <- clogit(formula = as.numeric(bdd_cas_tem$Cas_tembr)~ Age_mere_cat_rec + Etat_matr_mere + cons_medic  + 
                 Gestite_cat2_rec  +Poids_nais_Tranche_rec + Taille_nais_Tranche_rec + PC_nais + 
                 Expo_rayonX + strata(ID_app), data=bdd_cas_tem)
mod3
summary(mod3)

# verification de la multicollinealite
library(car)
vif(mod3)

# Selection pas a pas ascendente
library(MASS)
backward<-stepAIC(mod3,direction = "backward",trace = FALSE)
backward
backward$anova

# Modele finale pour la regresssion logistique conditionnelle

mod_cond_final <- clogit(formula = as.numeric(bdd_cas_tem$Cas_tembr)~ Age_mere_cat_rec + 
                           Etat_matr_mere + cons_medic + Gestite_cat2_rec + Poids_nais_Tranche_rec + 
                           Expo_rayonX + strata(ID_app),method = "exact",  data=bdd_cas_tem)
summary(mod_cond_final)
capture.output(summary(mod_cond_final), file = "Dr Maiga Haby")
# Obtenir les odds ratios
exp(coef(mod_cond_final))

# Obtenir les intervalles de confiance des odds ratios
exp(confint(mod_cond_final))
exp(cbind(coef(mod_cond_final), confint(mod_cond_final)))

# vérifier la qualité d'ajustement et détecter des valeurs influentes :
# Résidus martingales: Des valeurs éloignées de 0 pourraient indiquer un mauvais ajustement local
residuals(mod_cond_final, type = "martingale")


# Ajouter des prédictions au dataset

bdd_cas_tem$RiskScore <- predict(mod_cond_final, type = "risk")#obtenir les probabilités prédictives 
bdd_cas_tem$RiskScore
#fournit les probabilités estimées pour chaque individu d'être un cas.
#Une probabilité proche de 1 indique une forte probabilité d'être un cas.
#Une probabilité proche de 0 indique une faible probabilité d'être un cas.

# Normalisation pour obtenir des probabilités dans chaque groupe
library(dplyr)
bdd_cas_tem <- bdd_cas_tem %>%
  group_by(ID_app) %>%
  mutate(PredictedProb = RiskScore / sum(RiskScore)) %>%
  ungroup()
bdd_cas_tem$PredictedProb

#install.packages("epiDisplay")
library(epiDisplay)

clogistic.display(mod_cond_final)

#################################################################################################
############################### Appariement par le score de propension ##########################
#################################################################################################

library(MatchIt)
library(optmatch)
library(RItools)
library(epiR)
library(tableone)
library(shiny)

## Table resumant l'ensemble des variables chez l'enfant par rapport a la malformation

propens_tab <- CreateTableOne(vars =c("Age_mere_cat_rec", "Etat_matr_mere", "cons_medic", "fer_acfol",
                       "Poids_nais_Tranche_rec", "Taille_nais_Tranche_rec2", "PC_nais", "Expo_rayonX"), factorVars = "Sexe_enft_rec", strata ="Cas_tembr", 
               data = bdd_cas_tem)
propens_tab



library(purrr)
propens_tab1 <- print(propens_tab, 
                        showAllLevels = TRUE,
                        nonnormal = vars,
                        test=TRUE)
dir.create("resultats")
write.csv2(propens_tab1, here::here("resultats","table_propension.csv"),row.names = TRUE ) 

## Calcul du score de propensions par le modele lineaire globale (glm)

sp <- glm(Cas_tembr~Age_mere_cat_rec + Etat_matr_mere + cons_medic 
          +Poids_nais_Tranche_rec+ Taille_nais_Tranche_rec2 + PC_nais + Expo_rayonX, family = binomial, data = bdd_cas_tem)
sp


# boite a moustache de l'ajustement du score de propension 

boxplot(sp) # indique si on aura ou pas des difficultes pour apparier les individus
            # si les boxplot se recouvrent (s'emboitent) cela signifie qu'il y a pas de difficulté, si non il y a difficutés d'appariement

# Resume du score de propension de tous les individus 
summary(sp$fitted.values) # Moyenne du score de propension est compris entre [0;1]

# Appariement du score de propension
?matchit
vignette("sampling-weights")
vignette("assessing-balance")
vignette("estimating-effects")

mod_mat <- matchit(formula= Cas_tembr~ Poids_nais_Tranche_rec+ Taille_nais_Tranche_rec2 + cons_medic+
                     Age_mere_cat_rec + Etat_matr_mere + strata(ID_app), method= "nearest",
                     ratio=2,  data = bdd_cas_tem) 
mod_mat
summary(mod_mat)



 # numero des lignes des individus apparies

IndApp<- subset(as.data.frame(mod_mat$subclass),!is.na(as.data.frame(mod_mat$subclass)))
head(IndApp) # Afficher les six premiers individus apparies

# Affichage des valeurs des lignes des individus avec head()  

bdd_cas_tem[1,]; bdd_cas_tem[2,]; bdd_cas_tem[3,]; bdd_cas_tem[9,]

# Tous les individus apparies

mod_prop_final <- match.data(mod_mat)
head(mod_prop_final)

# Table resumant l'ensemble des variables par rapport a la variable cas_tem

tab_app <- CreateTableOne(vars =c("Age_mere_cat_rec", "Etat_matr_mere", "cons_medic", 
                       "Poids_nais_Tranche_rec", "Taille_nais_Tranche_rec2", "PC_nais", 
                       "Expo_rayonX"), factorVars = "Sexe_enft_rec", strata ="Cas_temb",
               data = mod_prop_final)
tab_app

summary(tab_app)
#clogit(formula = as.numeric(bdd_cas_tem$Cas_tembr)~ cons_medic + Taille_nais_Tranche_rec2 + Poids_nais_Tranche_rec + fer_acfol +
#Age_mere_cat_rec + Etat_matr_mere + strata(ID_app),method = "exact",  data=bdd_cas_tem)
# Importation des resultats en tableau dans fichier csv

dir.create("resultats_app")
write.csv2(tab_fin2, here::here("resultats_app","table_app.csv"),row.names = FALSE )


mod_prop_final$Sexe_enft_rec <- is.name(x= "M")
a <- mod_prop_final*mod_prop_final$distance

CreateTableOne(vars =c("Age_mere_cat", "Etat_matr_mere", "cons_medic", 
                       "Petit_poids", "Taille_nais", "PC_nais", "Expo_rayonX"), factorVars = "Sexe.enft_rec", strata ="Cas_tem_rec", 
               data = a)
#####################################           ######################################
# Example as a case-control study
## In a different situation where cases could be identified and then matched to controls
## Simulate the match
library("MatchIt")
m.out = matchit(chd ~ age + weight + sbp, data = wcgs,
                method = "nearest", distance = "glm") %>% 
  match.data() %>% 
  mutate(case = ifelse(chd == "Yes", TRUE, FALSE)) # Needed for clogit

m.out %>% 
  summary_factorlist("case", c("smoking"), fit_id = TRUE) %>% 
  ff_merge(
    survival::clogit(case ~ smoking + strata(subclass), m.out) %>% 
      fit2df(estimate_name = "OR (95% CI; case-control)"),
    last_merge = TRUE
  )
label     levels      FALSE       TRUE OR (95% CI; case-control)
smoking Non-smoker 147 (57.2)  98 (38.1)                         -
  Smoker 110 (42.8) 159 (61.9) 2.09 (1.46-2.98, p<0.001)

#########

tab_chi <- table(bdd_cas_tem$Sexe.enft, bdd_cas_tem$Cas_tem, deparse.level = 2)
tab_chi
prop_chi <- prop.table(tab_chi,1)*100
prop_chi
chisq.test(tab_chi)
fisher.test(tab_chi)

############ 
tab_chi <- table(bdd_cas_tem$Gestite_cat, bdd_cas_tem$Cas_tem, deparse.level = 2)
tab_chi
prop_chi <- prop.table(tab_chi,1)*100
prop_chi
chisq.test(tab_chi)
fisher.test(tab_chi)

