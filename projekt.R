#######################################################################
### Title:    R-Code Statistishe Verfahren - Projekt 1 Frischwiesen ###
### Authors:  Max Tiessen, Philip Fuerste, Georg Reinhardt          ###
### Date:     28/04/2020                                            ###
#######################################################################

################
# Datenset laden
################

# Importiere Packages
# Dependencies ------------------------------------------------------------
library("leaps")


# Load --------------------------------------------------------------------
set.seed(22)
data.all <- read.csv("frischwiesen.csv", sep = ";")
data.Ilmtal <- data.all[data.all$Gebiet == "Ilmtal", ]
data.Saaletal <- data.all[data.all$Gebiet == "Saaletal", ]


# Task 1 ------------------------------------------------------------------
####################################################
# 1. Leiten Sie zunaechst getrennt fuer das Saaletal
# und das Ilmtal geeignete lineare Modelle zur 
# Prognose der Biomasseproduktion her.
####################################################

# Untersuche Maße der zentralen Tendenz jeder einzelnen Variable
summary(data.all)
# Visualisieren der Variablen im Verh?ltnis:
plot(data.all)
# Jede Variable gegeneinander Plotten,
# vielversprechende Kombinationen selektieren.

### Ilmtal ###
model_ilm_N <- lm(biom~1+N, data = data.Ilmtal) # biomasse ~ Stickstoff
model_ilm_Corg <- lm(biom~1+Corg, data = data.Ilmtal) # biomasse ~Kohlenstoff
model_ilm_Artenzahl <- lm(biom~1+Artenzahl, data = data.Ilmtal) # biomasse ~ Artenzahl
model_ilm_pH <- lm(biom~1+pH, data = data.Ilmtal) # biomasse ~ pH
model_ilm_K <- lm(biom~1+K, data = data.Ilmtal) # biomasse ~ Kalium
model_ilm_Corg.N <- lm(biom~1+Corg.N, data = data.Ilmtal) # biomasse ~ Kohlenstoff/Stickstoff
model_ilm_Cges <- lm(biom~1+Cges, data = data.Ilmtal) # biomasse ~ Cges
model_ilm_P <- lm(biom~1+P, data = data.Ilmtal) # biomasse ~ P


# biomasse ~ Stickstoff
plot(biom~N, data = data.Ilmtal, col = "red", pch=16)
abline(model_ilm_N, col = "red")

# biomasse ~Kohlenstoff
plot(biom~Corg, data = data.Ilmtal, col = "red", pch=16)
abline(model_ilm_Corg, col = "red")

# biomasse ~ Artenzahl
plot(biom~Artenzahl, data = data.Ilmtal, col = "red", pch=16)
abline(model_ilm_Artenzahl, col = "red")

# biomasse ~ pH
plot(biom~pH, data = data.Ilmtal, col = "red", pch=16)
abline(model_ilm_pH, col = "red")

# biomasse ~ Kalium
plot(biom~K, data = data.Ilmtal, col = "red", pch=16)
abline(model_ilm_K, col = "red")

# biomasse ~ Corg.N
plot(biom~Corg.N, data = data.Ilmtal, col = "red", pch=16)
abline(model_ilm_Corg.N, col = "red")

# biomasse ~ Cges
plot(biom~Cges, data = data.Ilmtal, col = "red", pch=16)
abline(model_ilm_Cges, col = "red")


### Saaletal ###
model_saale_N <- lm(biom~1+N, data = data.Saaletal) # biomasse ~ Stickstoff
model_saale_Corg <- lm(biom~1+Corg, data = data.Saaletal) # biomasse ~Kohlenstoff
model_saale_Artenzahl <- lm(biom~1+Artenzahl, data = data.Saaletal) # biomasse ~ Artenzahl
model_saale_pH <- lm(biom~1+pH, data = data.Saaletal) # biomasse ~ pH
model_saale_K <- lm(biom~1+K, data = data.Saaletal) # biomasse ~ Kalium
model_saale_Corg.N <- lm(biom~1+Corg.N, data = data.Saaletal) # biomasse ~ Kohlenstoff/Stickstoff
model_saale_Cges <-  lm(biom~1+Cges, data = data.Saaletal) # biomasse ~ Cges
model_saale_P <- lm(biom~1+P, data = data.Saaletal) # biomasse ~ Phosphor

# biomasse ~ Stickstoff
plot(biom~N, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_N, col = "darkgreen")

# biomasse ~Kohlenstoff
plot(biom~Corg, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_Corg, col = "darkgreen")

# biomasse ~ Artenzahl
plot(biom~Artenzahl, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_Artenzahl, col = "darkgreen")

# biomasse ~ pH
plot(biom~pH, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_pH, col = "darkgreen")

# biomasse ~ Kalium
plot(biom~K, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_K, col = "darkgreen")

# biomasse ~ Corg.N
plot(biom~Corg.N, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_Corg.N, col = "darkgreen")

# biomasse ~ Cges
plot(biom~Cges, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_Cges, col = "darkgreen")

# biomasse ~ P
plot(biom~P, data = data.Saaletal, col = "darkgreen", pch=16)
abline(model_saale_P, col = "darkgreen")


# Combined Plots ----------------------------------------------------------
#########################
### Kombinierte Plots ###
#########################
ylow <- 0

# biomasse ~ Stickstoff
model_all_N <- lm(biom~1+N, data = data.all)
plot(biom~N, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(0.1,0.6))
par(new=TRUE)
plot(biom~N, data = data.Ilmtal, col = "red", pch=16, ann=FALSE, axes=FALSE, ylim=c(ylow,600), xlim=c(0.1,0.6))
abline(model_saale_N, col = "darkgreen")
abline(model_ilm_N, col = "red")
abline(model_all_N, col = "black", lwd=2)
title(main ="Biomasse ~ Stickstoff (N)")
legend(.45,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")

# biomasse ~ Kohlenstoff
model_all_Corg <- lm(biom~1+Corg, data = data.all)
plot(biom~Corg, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(1,4.5))
par(new=TRUE)
plot(biom~Corg, data = data.Ilmtal, col = "red", pch=16, ann=FALSE, axes=FALSE, ylim=c(ylow,600), xlim=c(1,4.5))
abline(model_ilm_Corg, col = "red")
abline(model_saale_Corg, col = "darkgreen")
abline(model_all_Corg, col = "black", lwd=2)
title(main ="Biomasse ~ Kohlenstoff (Corg)")
legend(3.5,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")

# biomasse ~ Artenzahl
model_all_Artenzahl <- lm(biom~1+Artenzahl, data = data.all)
plot(biom~Artenzahl, data = data.Saaletal, col = "darkgreen", pch=16,  ylim=c(ylow,600), xlim=c(7,31))
par(new=TRUE)
plot(biom~Artenzahl, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(7,31))
abline(model_saale_Artenzahl, col = "darkgreen")
abline(model_ilm_Artenzahl, col = "red")
abline(model_all_Artenzahl, col = "black", lwd=2)
title(main ="Biomasse ~ Artenzahl")
legend(7,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")


# biomasse ~ pH
model_all_pH <- lm(biom~1+pH, data = data.all)
plot(biom~pH, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(7,7.26))
par(new=TRUE)
plot(biom~pH, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(7,7.26))
abline(model_saale_pH, col = "darkgreen")
abline(model_ilm_pH, col = "red")
abline(model_all_pH, col = "black", lwd=2)
title(main ="Biomasse ~ pH")
legend(7,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")


# biomasse ~ Kalium
model_all_K <- lm(biom~1+K, data = data.all)
plot(biom~K, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(5.6, 44.8))
par(new=TRUE)
plot(biom~K, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(5.6, 44.8))
abline(model_ilm_K, col = "red")
abline(model_saale_K, col = "darkgreen")
abline(model_all_K, col = "black", lwd=2)
title(main ="Biomasse ~ Kalium (K)")
legend(33,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")


# biomasse ~ Corg.N
model_all_Corg.N <- lm(biom~1+Corg.N, data = data.all)
plot(biom~Corg.N, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(3.66, 9.62))
par(new=TRUE)
plot(biom~Corg.N, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(3.66, 9.62))
abline(model_ilm_Corg.N, col = "red")
abline(model_saale_Corg.N, col = "darkgreen")
abline(model_all_Corg.N, col = "black", lwd=2)
title(main ="Biomasse ~ Kohlenstoff/Stickstoff (Corg.N)")
legend(3.5,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")


# biomasse ~ Cges
model_all_Cges <- lm(biom~1+Cges, data = data.all)
plot(biom~Cges, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(2.379, 8.576))
par(new=TRUE)
plot(biom~Cges, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(2.379, 8.576))
abline(model_ilm_Cges, col = "red")
abline(model_saale_Cges, col = "darkgreen")
abline(model_all_Cges, col = "black", lwd=2)
title(main ="Biomasse ~ Kohlenstoff-Gesamt (Cges)")
legend(6.6,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")


# biomasse ~ Phosphor
model_all_P <- lm(biom~1+P, data = data.all)
plot(biom~P, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(1.4, 25))
par(new=TRUE)
plot(biom~P, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(1.4, 25))
abline(model_ilm_P, col = "red")
abline(model_saale_P, col = "darkgreen")
abline(model_all_P, col = "black", lwd=2)
title(main ="Biomasse ~ Phosphor (P)")
legend(18,200, legend=c("Ilmtal","Saaletal"), fill = c("red", "darkgreen"), border = FALSE, bty="n")


# Task 2 ------------------------------------------------------------------
###################################################################
# 2. Analysieren Sie dann beide Teildatensaetze gemeinsam
# und untersuchen Sie insbesondere das Vorliegen von 
# Wechselwirkungen, d.h. unterschiedliche quantitative 
# Effekte der Einflussgroessen in den beiden Untersuchungsgebieten.
###################################################################

# Funktion generiert plots, in denen eine Einflussgroesse dichotomisiert wird, 
# um Interaktion mit einer anderen Einflussgroesse zu visualisieren
interactions <- function(area, name) {
  column_names <- c('P','K','pH', 'N', 'Cges', 'Corg', 'Corg.N', 'Artenzahl')
  for(i in column_names) {
    area.sorted <- area[order(area[[i]]),]  #sortieren der jeweiligen Spalte
    for(j in column_names) {
      if(i != j) {
        area.first <- area.sorted[1:ceiling(nrow(area)/2),] #dichotomisieren der Variable
        area.second <- area.sorted[(ceiling((nrow(area)/2))+1):nrow(area),]
        
        xlow <- min(area[[j]]) #X-Achsenabschnitt Grenzen
        xhigh <- max(area[[j]])
        #lineares Modell zusammenfügen
        lmformula <- as.formula(paste("biom~1+",j, sep=""))
        plottitle <- paste(name,": ", i, " against ",j, sep="")
        subtitle <- paste("red = high values, green = low values")
        # visualisieren
        model_first <- lm(lmformula, data = area.first)
        model_second <- lm(lmformula, data = area.second)
        plot(lmformula, data = area.first, 
             col = "darkgreen", 
             pch=16, 
             ylim=c(0,600), 
             xlim=c(xlow, xhigh), 
             main = plottitle, 
             sub = subtitle)
        par(new=TRUE)
        plot(lmformula, data = area.second, 
             col = "red", 
             pch=16, 
             ylim=c(0,600), 
             xlim=c(xlow, xhigh))
        abline(model_first, col = "darkgreen")
        abline(model_second, col = "red")
      }
    }
  }
} #end function

# Funktion für jeweiligen Gebiets- und Gesamtdatensatz aufrufen
interactions(data.Ilmtal, 'Ilmtal')
interactions(data.Saaletal, 'Saaletal')
interactions(data.all, 'Gesamt')


# Correlation Coefficient -------------------------------------------------------
####################################################
# Korrelationskoeffizienten: Variablen 
# untereinander verglichen, in Matrixschreibweise
#################################################### 
vec <- c('P','K','pH', 'N', 'Cges', 'Corg', 'Corg.N', 'Artenzahl', 'biom')
cor(data.Ilmtal[,vec]) # Ilmtal
cor(data.Saaletal[,vec]) # Saaletal


###################################################
# Model-Selection basierend auf Mallows Cp
# Maximales Modell für Gebiete erstellen:
###################################################

# Model Selection Ilmtal --------------------------------------------------
# Wähle Max-Modell anhand der vorausgegangenen visuellen Inspektion der Daten
own_model_Ilm <- lm(biom~1+N+Corg+Cges+Corg.N:pH+(P+K+N):Artenzahl+N:P, data = data.Ilmtal)

# Best Model mit Mallows Cp
require("leaps")
ilm_bss <- regsubsets(biom~1+N+Corg+Cges+Corg.N:pH+(P+K+N):Artenzahl+N:P, data = data.Ilmtal)
summary(ilm_bss)$cp
index <- which.min(summary(ilm_bss)$cp)
summary(ilm_bss)$which[index,]
Cp_Ilm <- lm(biom~1+Cges+Corg.N:pH, data = data.Ilmtal)
summary(Cp_Ilm)

# Model Selection Saaletal ------------------------------------------------
own_model_Saale <- lm(biom~1+P+pH+Cges+Corg.N+Corg+Artenzahl+P:K+P:N+N:Artenzahl+Corg:P, data = data.Saaletal)
summary(own_model_Saale)
#plot(own_model_Saale, which=1)

# Best Model mit Mallows Cp
# K & N rausgeschmissen, da geringster Korrelationskoeffizient; einige interessante Wechselwirkungen reingenommen
saale_bss <- regsubsets(biom~1+P+pH+Cges+Corg.N+Corg+Artenzahl+P:K+P:N+N:Artenzahl+Corg:P, data = data.Saaletal)
summary(saale_bss)$cp
index <- which.min(summary(saale_bss)$cp)
summary(saale_bss)$which[index,]
Cp_Saale <- lm(biom~1+pH+Cges+Corg+Corg.N+P:N, data = data.Saaletal)
summary(Cp_Saale)

# Standardize Regression-coefficients
#library("QuantPsyc")
#lm.beta(own_model_Saale)


# Task 3 ------------------------------------------------------------------
####################################################
# 3. Vergleichen sie die Genauigkeit der Vorhersage 
# fuer Biomasse fuer das Saaletal basierend auf dem 
# separaten und dem gemeinsamem Modell.
# Verwenden sie hierbei auf geeignete Art den SPSE.
####################################################


# Model selection Gesamtdatensatz -----------------------------------------
# Faktorisieren der Einflussgroesse Gebiet
data.all$Gebiet <- as.factor(data.all$Gebiet)
str(data.all)

# Best Model mit Mallows Cp (Gesamter Datensatz)
# Visuelle Inspektion um Maximalmodell zu erstellen
all_bss <- regsubsets(biom~1+(N+Corg+Cges+pH+Artenzahl)*Gebiet+P:Gebiet+Corg.N:Gebiet, data = data.all)
summary(all_bss)$cp
index <- which.min(summary(all_bss)$cp)
summary(all_bss)$which[index,]
Cp_all2 <- lm(biom~1+Cges+P:Gebiet+Corg.N:Gebiet+pH:Gebiet, data=data.all)
summary(Cp_all2)


# SPSE-Saale --------------------------------------------------------------------
max_model_Saale <- lm(biom~1+P:N+pH+Cges+Corg+Corg.N, data = data.Saaletal)
max_RSS_Saale <- sum((data.Saaletal$biom - predict(max_model_Saale, newdata = data.Saaletal))^2)
length = dim(data.Saaletal)[1]  # data entries
sigma2.max_Saale <- max_RSS_Saale/(length - length(coef(max_model_Saale)))  # max.Modell / #entries - #predictor_variables

# SPSE-Gesamt -------------------------------------------------------------
max_model_Gesamt <- lm(biom~1+Cges+P:Gebiet+Corg.N:Gebiet+pH:Gebiet, data=data.all)
max_RSS_Gesamt <- sum((data.Saaletal$biom - predict(max_model_Gesamt, newdata = data.Saaletal))^2)
length = dim(data.all)[1]            # data entries
sigma2.max_Gesamt <- max_RSS_Gesamt/(length - length(coef(max_model_Gesamt)))  # max.Modell / #entries - #predictor_variables


saale_SPSE <- max_RSS_Saale + 2*sigma2.max_Gesamt*length(coef(max_model_Saale))
all_SPSE <- max_RSS_Gesamt + 2*sigma2.max_Gesamt*length(coef(max_model_Gesamt))
cat("SPSE-Saale: ", saale_SPSE, "\nSPSE-Gesamt: ", all_SPSE, "\n")
# Saale-Modell ist besser (SPSE kleiner) zur Vorhersage des Saaletals; 
# Intuition: Ilmtal-Daten bringen keine zusaetzlichen Vorteile zur Vorhersage des Saaletals (keine Testdaten vorhanden)


# SIMULATION --------------------------------------------------------------
###########################################################################
# Wählen sie ein „wahres Modell“ in Anlehnung an die Ergebnisse des ersten
# Teils und eine Designmatrix, die zufällig ausgewählte Zeilen der realen
# Design-Matrix (mit Wiederholung) enthält. Simulieren Sie dann mehrfach 
# Pseudo-Beobachtungen der Zielgröße und führen Sie für die so simulierten
# Pseudo-Datensätze die Modellwahl mit Hilfe von Mallow’s Cp-Kriterium durch.
############################################################################
#Maximales Modell des Gesamtdatensatzes
maxformula <- as.formula(paste("biom~1+(N+Corg+Cges+pH+Artenzahl)*Gebiet+P:Gebiet+Corg.N:Gebiet", sep=""))
#maxformula <- lm(maxformula, data=data.all)

# Best-Model basierend auf Mallow's Cp, welches unser "wahres Modell" stellt
lmformula <- as.formula(paste("biom~1+Cges+P:Gebiet+Corg.N:Gebiet+pH:Gebiet", sep=""))
true_model <- lm(lmformula, data = data.all)


# Liste der Variablen, die im wahren Modell vorkommen
true_list <- c('Cges','GebietIlmtal:Corg.N', 'GebietSaaletal:Corg.N', 'GebietIlmtal:P', 'GebietSaaletal:P', 'pH:GebietSaaletal')
# Liste der verschiedenen Pseudo-Datensatz-Groessen
n <- c(25, 50, 100, 250, 500, 1000, 3000, 6000)
# Erstelle data-frame zum speichern der Haeufigkeiten, mit der die (richtigen) Praediktoren ausgewählt werden
model_selection_matrix <- data.frame(matrix(0, ncol = 16, nrow = length(n)))
rownames(model_selection_matrix) <- n
colnames(model_selection_matrix) <- c('N', 'Corg', 'Cges', 'pH', 'Artenzahl', 'GebietSaaletal',
                                      'N:GebietSaaletal', 'Corg:GebietSaaletal', 'Cges:GebietSaaletal', 'pH:GebietSaaletal', 'Artenzahl:GebietSaaletal',
                                      'GebietIlmtal:P', 'GebietSaaletal:P', 'GebietIlmtal:Corg.N', 'GebietSaaletal:Corg.N', 'true_model_included')
# Erstelle data-frame zum speichern der Anzahl der ausgewaehlten Praediktoren (pro 1000 Simulationen)
model_size_matrix <- data.frame(matrix(0, ncol = 15, nrow = length(n)))
rownames(model_size_matrix) <- n
colnames(model_size_matrix) <- seq(1, 15, 1)

row.index <- 1
for(i in n) {
  bool_model = c()
  model_count <- 0
  for(j in 1:1000) {
    data.prediction <- data.all[sample(1:dim(data.all)[1],i,replace=TRUE),] #ziehe random sample aus Datensatz
    # Simuliere die Zielgroesse basierend auf dem "wahren Modell" plus einem normalverteilten Versatz
    data.prediction$biom <- predict(true_model, newdata = data.prediction) + rnorm(i, mean = 0, sd = sd(data.all$biom))

    minCp <- regsubsets(maxformula, data = data.prediction)
    index <- which.min(summary(minCp)$cp)
    summary(minCp)$which[index,]
    # Wenn alle Praediktor-Variablen, die im wahren Modell vorkommen = true sind,
    # dann erhöhe den Zaehler (model_count) um 1
    bool <- all(c(summary(minCp)$which[index,])[true_list])
    if(bool == TRUE) {
      model_count <- model_count + 1
    }
    #Speichere die Variablen jedes Modells in einer Bool-Matrix (enthalten, nicht enthalten)
    bool_model = rbind(bool_model, c(summary(minCp)$which[index,]))
    #Fuer jedes Vorkommen einer gewissen Anzahl an Praediktor-Variablen im Modell, addiere 1
    model_size_matrix[row.index, index] <-  model_size_matrix[row.index, index] + 1
  }
  model_selection_matrix[row.index,1:15] <- colSums(bool_model[,-1]) #summiere das Vorkommen der Praediktoren
  model_selection_matrix$true_model_included[row.index] <- model_count
  row.index <- row.index+1
}

# Save Table --------------------------------------------------------------
write.csv(model_selection_matrix, file = "model_selection_matrix.csv", row.names=TRUE)
write.csv(model_size_matrix, file = "model_size_matrix.csv", row.names=TRUE)