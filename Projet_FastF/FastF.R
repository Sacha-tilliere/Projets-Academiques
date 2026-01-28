# -----------------------------------------------------------
# 1. INSTALLATION ET IMPORTATION
# -----------------------------------------------------------
# On installe les packages 
install.packages(c("readxl", "lmtest", "car", "glmx", "pscl", "pROC", "effects", "visreg", "corrplot", "EnvStats"))

library(readxl)
# Importation de la base de données
data_brut <- read_excel("C:/Users/sacha/Downloads/FastF.xlsx")

# -----------------------------------------------------------
# 2. NETTOYAGE ET CRÉATION DES VARIABLES
# -----------------------------------------------------------
# Création de la variable cible Y1 (Fast Fashion vs Reste)
# Si le texte contient "Fast Fashion" -> 1, sinon 0
data_brut$Y_FastFashion <- ifelse(grepl("Fast Fashion", data_brut[[1]], ignore.case = TRUE), 1, 0)

# Suppression des répondants "Non-binaire" (Genre = 2)
FastF <- subset(data_brut, Genre != 2)

# Factorisation du Genre
FastF$Genre <- as.factor(FastF$Genre)

# Conversion de toutes les variables quantitatives en numérique
FastF$Revenu <- as.numeric(FastF$Revenu)
FastF$Conscience_eco <- as.numeric(FastF$Conscience_eco)
FastF$Sensibilité_prix <- as.numeric(FastF$Sensibilité_prix)
FastF$Sensibilité_Tendance <- as.numeric(FastF$Sensibilité_Tendance)
FastF$Accessibilité <- as.numeric(FastF$Accessibilité)
FastF$Influence_FF <- as.numeric(FastF$Influence_FF)
FastF$Influence_durable <- as.numeric(FastF$Influence_durable)
FastF$Norme_subjective <- as.numeric(FastF$Norme_subjective)
FastF$Revente <- as.numeric(FastF$Revente)
# Vérification
str(FastF)

# -----------------------------------------------------------
# 3. TESTS PRÉLIMINAIRES
# -----------------------------------------------------------

# Test des valeurs atypiques (Revenu)
boxplot(FastF$Revenu,
        main = "Distribution des Revenus",
        ylab = "Revenu Mensuel (€)",
        col = "lightblue", border = "darkblue")
library(EnvStats)

# k = 3 signifie qu'on teste les 3 valeurs les plus hautes
test_outliers <- rosnerTest(FastF$Revenu, k = 3)
print(test_outliers$all.stats)
# Tests de corrélation variables qualitatives et quantitatives
t.test(FastF$Revenu ~ FastF$Genre)
t.test(FastF$Conscience_eco ~ FastF$Genre)
t.test(FastF$Sensibilité_prix ~ FastF$Genre)
t.test(FastF$Sensibilité_Tendance ~ FastF$Genre)
t.test(FastF$Accessibilité ~ FastF$Genre)
t.test(FastF$Influence_FF ~ FastF$Genre)
t.test(FastF$Influence_durable ~ FastF$Genre)
t.test(FastF$Norme_subjective ~ FastF$Genre)
t.test(FastF$Revente ~ FastF$Genre)

# Tests de corrélation entre les variables quantitatives
library(corrplot)
vars_quant <- FastF[ , c("Revenu", "Conscience_eco", "Sensibilité_prix", "Sensibilité_Tendance", 
                         "Accessibilité", "Influence_FF", "Influence_durable", "Norme_subjective", "Revente")]
mat_cor <- cor(vars_quant, method = "spearman", use = "complete.obs")
corrplot(mat_cor, method = "number", type = "upper")
print(mat_cor)
# -----------------------------------------------------------
# 4. ESTIMATION DU MODÈLE LOGIT (PRINCIPAL)
# -----------------------------------------------------------
library(lmtest)
library(car)



# Estimation
modele_logit <- glm(Y_FastFashion ~ Genre + Revenu + Conscience_eco + 
                      Sensibilité_prix + Sensibilité_Tendance + Accessibilité + 
                      Influence_FF + Influence_durable + Norme_subjective + Revente,
                    data = FastF,
                    family = "binomial")
summary(modele_logit)

# --- TEST DE VALIDITÉ DU MODÈLE / HÉTÉROSCÉDASTICITÉ—
library(glmx)
modele_het <- hetglm(Y_FastFashion ~ Genre + Revenu + Conscience_eco + 
                       Sensibilité_prix + Sensibilité_Tendance + Accessibilité + 
                       Influence_FF + Influence_durable + Norme_subjective + Revente | 
                       Revenu, 
                     data = FastF, 
                     family = binomial(link = "logit"))
summary(modele_het)
# Test avec HC1 (préféré pour les petits échantillons)
library(lmtest)
library(sandwich)
res_robuste_HC1 <- coeftest(modele_logit, vcov = vcovHC(modele_logit, type = "HC1"))
print(res_robuste_HC1)

# -----------------------------------------------------------
# 5. VALIDATION ET INTERPRÉTATION
# -----------------------------------------------------------
library(pscl) # Pour le hitmiss
library(effects)
library(visreg)
# 1. Test de Multicolinéarité (VIF)
cat("\n--- Test VIF (Multicolinéarité) ---\n")
print(vif(modele_logit))

# 2. Test de Significativité Globale (Khi-2)
cat("\n--- Test de Significativité Globale ---\n")
chi2 <- (modele_logit$null.deviance - modele_logit$deviance)
ddl <- (modele_logit$df.null - modele_logit$df.residual)
pvalue_globale <- pchisq(chi2, ddl, lower.tail = FALSE)
print(paste("P-value du test global (Khi-2) :", pvalue_globale))

# 3. Qualité d'Ajustement (Pseudo R²)
cat("\n--- Qualité d'Ajustement (Pseudo R²) ---\n")
R2_McFadden <- 1 - (modele_logit$deviance / modele_logit$null.deviance)
print(paste("Pseudo R² de McFadden :", R2_McFadden))

# 4. Performances de Prévision
cat("\n--- Matrice de Confusion (hitmiss) ---\n")
print(hitmiss(modele_logit))

# 5. Interprétation des effets significatifs (Odd-Ratios & Effets Marginaux)
cat("\n--- Odd-Ratios (pour les variables qualitatives) ---\n")
print(exp(coef(modele_logit)))



# 1. On récupère les probabilités prédites
prob_pred <- predict(modele_logit, type = "response")

# 2. On génère la courbe ROC
library(pROC)
roc_curve <- roc(modele_logit$y, prob_pred, plot=TRUE, print.auc=TRUE, 
                 main="Courbe ROC - Modèle Fast Fashion", col="blue")
# 3. Afficher l'intervalle de confiance 
ci(roc_curve)
# Effets marginaux
cat("\n--- Effets Marginaux (pour les variables quantitatives) ---\n")
effets_marginaux <- mean(dlogis(predict(modele_logit, type = "response"))) * coef(modele_logit)
print(effets_marginaux)

# -----------------------------------------------------------
# 6. REPRÉSENTATIONS GRAPHIQUES
# -----------------------------------------------------------
# Graphique global
plot(allEffects(mod = modele_logit), type = "response", ylim = c(0, 1))

# Zoom sur 'Influence_durable'
visreg(modele_logit, "Influence_durable", scale = "response", 
       main = "Effet de l'influence durable sur la probabilité de consommer de la FF",
       ylab = "Probabilité de consommer de la FF")
# Zoom sur 'Genre'
visreg(modele_logit, "Genre", scale = "response", 
       main = "Effet du Genre sur la probabilité de consommer de la FF",
       ylab = "Probabilité de consommer de la FF")
# -----------------------------------------------------------
# 7. MODÈLES COMPLÉMENTAIRES
# -----------------------------------------------------------

# --- A. MODÈLE MIROIR (CONSOMMATION RESPONSABLE) ---
# On recharge pour avoir le texte original si besoin, ou on utilise data_brut existant
# Création de Y_Responsable
data_brut$Y_Responsable <- ifelse(grepl("Seconde|éthiques", data_brut[[1]], ignore.case = TRUE), 1, 0)

# Nettoyage (Même procédure)
FastF_Resp <- subset(data_brut, Genre != 2)
FastF_Resp$Genre <- as.factor(FastF_Resp$Genre)
levels(FastF_Resp$Genre) <- c("Homme", "Femme") # Important pour la cohérence
# Conversion numérique (Répétition nécessaire car nouvel objet FastF_Resp)
# Note : Pour être sûr, on réapplique les conversions comme ci-dessus.
FastF_Resp$Sensibilité_prix <- as.numeric(FastF_Resp$Sensibilité_prix)
FastF_Resp$Sensibilité_Tendance <- as.numeric(FastF_Resp$Sensibilité_Tendance)
FastF_Resp$Accessibilité <- as.numeric(FastF_Resp$Accessibilité)
FastF_Resp$Influence_FF <- as.numeric(FastF_Resp$Influence_FF)
FastF_Resp$Influence_durable <- as.numeric(FastF_Resp$Influence_durable)
FastF_Resp$Norme_subjective <- as.numeric(FastF_Resp$Norme_subjective)
FastF_Resp$Revente <- as.numeric(FastF_Resp$Revente)

# Estimation
cat("\n--- RÉSULTATS : MODÈLE RESPONSABLE ---\n")
modele_resp <- glm(Y_Responsable ~ Genre + Revenu + Conscience_eco + 
                     Sensibilité_prix + Sensibilité_Tendance + Accessibilité + 
                     Influence_FF + Influence_durable + Norme_subjective + Revente,
                   data = FastF_Resp, 
                   family = "binomial")
summary(modele_resp)
# --- B. MODÈLE ÉLARGI (FF + MILIEU DE GAMME) ---
# Création de Y_Large
data_brut$Y_Large <- ifelse(grepl("Fast Fashion|milieu de gamme", data_brut[[1]], ignore.case = TRUE), 1, 0)
# Nettoyage
FastF_Large <- subset(data_brut, Genre != 2)
FastF_Large$Genre <- as.factor(FastF_Large$Genre)
levels(FastF_Large$Genre) <- c("Homme", "Femme")

# Conversions (Idem)
FastF_Large$Revenu <- as.numeric(FastF_Large$Revenu)
FastF_Large$Conscience_eco <- as.numeric(FastF_Large$Conscience_eco)
FastF_Large$Sensibilité_prix <- as.numeric(FastF_Large$Sensibilité_prix)
FastF_Large$Sensibilité_Tendance <- as.numeric(FastF_Large$Sensibilité_Tendance)
FastF_Large$Accessibilité <- as.numeric(FastF_Large$Accessibilité)
FastF_Large$Influence_FF <- as.numeric(FastF_Large$Influence_FF)
FastF_Large$Influence_durable <- as.numeric(FastF_Large$Influence_durable)
FastF_Large$Norme_subjective <- as.numeric(FastF_Large$Norme_subjective)
FastF_Large$Revente <- as.numeric(FastF_Large$Revente)
# Estimation
cat("\n--- RÉSULTATS : MODÈLE ÉLARGI ---\n")
modele_large <- glm(Y_Large ~ Genre + Revenu + Conscience_eco + 
                      Sensibilité_prix + Sensibilité_Tendance + Accessibilité + 
                      Influence_FF + Influence_durable + Norme_subjective + Revente,
                    data = FastF_Large, 
                    family = "binomial")
summary(modele_large)

#=================================================================
# Test de robustesse  (Fast Fashion + Milieu de Gamme)
#=================================================================

# Chargement des librairies pour les tests robustes
library(lmtest)
library(sandwich)

# Application de la correction robuste 
res_robuste_large <- coeftest(modele_large, vcov = vcovHC(modele_large, type = "HC1"))
# Affichage des résultats corrigés
print(res_robuste_large)

# Application de la correction robuste 
res_robuste_resp <- coeftest(modele_resp, vcov = vcovHC(modele_resp, type = "HC1"))
# Affichage des résultats corrigés
print(res_robuste_resp)
