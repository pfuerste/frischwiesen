################
# Datenset laden
################
set.seed(22)
data.all <- read.csv("frischwiesen.csv", sep = ";")
data.Ilmtal <- data.all[data.all$Gebiet == "Ilmtal", ]
data.Saaletal <- data.all[data.all$Gebiet == "Saaletal", ]

####################################################
# 1. Leiten Sie zuna??chst getrennt fu??r das Saaletal
# und das Ilmtal geeignete lineare Modelle zur 
# Prognose der Biomasseproduktion her.
####################################################

# Visualisieren der Variablen im Verh?ltnis:
plot(data.all)
# Jede Variable gegeneinander Plotten,
# vielversprechende Kombinationen selektieren.

##############
### Ilmtal ###
##############
model_ilm_N <- lm(biom~1+N, data = data.Ilmtal) # biomasse ~ Stickstoff
model_ilm_Corg <- lm(biom~1+Corg, data = data.Ilmtal) # biomasse ~Kohlenstoff
model_ilm_Artenzahl <- lm(biom~1+Artenzahl, data = data.Ilmtal) # biomasse ~ Artenzahl
model_ilm_pH <- lm(biom~1+pH, data = data.Ilmtal) # biomasse ~ pH
model_ilm_K <- lm(biom~1+K, data = data.Ilmtal) # biomasse ~ Kalium
model_ilm_Corg.N <- lm(biom~1+Corg.N, data = data.Ilmtal) # biomasse ~ Kohlenstoff/Stickstoff


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


################
### Saaletal ###
################
model_saale_N <- lm(biom~1+N, data = data.Saaletal) # biomasse ~ Stickstoff
model_saale_Corg <- lm(biom~1+Corg, data = data.Saaletal) # biomasse ~Kohlenstoff
model_saale_Artenzahl <- lm(biom~1+Artenzahl, data = data.Saaletal) # biomasse ~ Artenzahl
model_saale_pH <- lm(biom~1+pH, data = data.Saaletal) # biomasse ~ pH
model_saale_K <- lm(biom~1+K, data = data.Saaletal) # biomasse ~ Kalium
model_saale_Corg.N <- lm(biom~1+Corg.N, data = data.Saaletal) # biomasse ~ Kohlenstoff/Stickstoff

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

# biomasse ~ Kohlenstoff
model_all_Corg <- lm(biom~1+Corg, data = data.all)
plot(biom~Corg, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(1,4.5))
par(new=TRUE)
plot(biom~Corg, data = data.Ilmtal, col = "red", pch=16, ann=FALSE, axes=FALSE, ylim=c(ylow,600), xlim=c(1,4.5))
abline(model_ilm_Corg, col = "red")
abline(model_saale_Corg, col = "darkgreen")
abline(model_all_Corg, col = "black", lwd=2)

# biomasse ~ Artenzahl
model_all_Artenzahl <- lm(biom~1+Artenzahl, data = data.all)
plot(biom~Artenzahl, data = data.Saaletal, col = "darkgreen", pch=16,  ylim=c(ylow,600), xlim=c(7,31))
par(new=TRUE)
plot(biom~Artenzahl, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(7,31))
abline(model_saale_Artenzahl, col = "darkgreen")
abline(model_ilm_Artenzahl, col = "red")
abline(model_all_Artenzahl, col = "black", lwd=2)

# biomasse ~ pH
model_all_pH <- lm(biom~1+pH, data = data.all)
plot(biom~pH, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(7,7.26))
par(new=TRUE)
plot(biom~pH, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(7,7.26))
abline(model_saale_pH, col = "darkgreen")
abline(model_ilm_pH, col = "red")
abline(model_all_pH, col = "black", lwd=2)

# biomasse ~ Kalium
model_all_K <- lm(biom~1+K, data = data.all)
plot(biom~K, data = data.Saaletal, col = "darkgreen", pch=16, ylim=c(ylow,600), xlim=c(5.6, 44.8))
par(new=TRUE)
plot(biom~K, data = data.Ilmtal, col = "red", pch=16, ylim=c(ylow,600), xlim=c(5.6, 44.8))
abline(model_ilm_K, col = "red")
abline(model_saale_K, col = "darkgreen")
abline(model_all_K, col = "black", lwd=2)

#min(data.all$K)
#max(data.all$K)

####################################################
# Korrelationskoeffizienten der einfachen Variablen 
#################################################### 
cor_saale_N <- cor(data.Saaletal$biom, data.Saaletal$N)
cor_ilm_N <- cor(data.Ilmtal$biom, data.Ilmtal$N)
cor_all_N <- cor(data.all$biom, data.all$N)

cor_saale_Corg <- cor(data.Saaletal$biom, data.Saaletal$Corg)
cor_ilm_Corg <- cor(data.Ilmtal$biom, data.Ilmtal$Corg)
cor_all_Corg <- cor(data.all$biom, data.all$Corg)

cor_saale_Artenzahl <- cor(data.Saaletal$biom, data.Saaletal$Artenzahl)
cor_ilm_Artenzahl <- cor(data.Ilmtal$biom, data.Ilmtal$Artenzahl)
cor_all_Artenzahl <- cor(data.all$biom, data.all$Artenzahl)

cor_saale_pH <- cor(data.Saaletal$biom, data.Saaletal$pH)
cor_ilm_pH <- cor(data.Ilmtal$biom, data.Ilmtal$pH)
cor_all_pH <- cor(data.all$biom, data.all$pH)

cor_saale_K <- cor(data.Saaletal$biom, data.Saaletal$K)
cor_ilm_K <- cor(data.Ilmtal$biom, data.Ilmtal$K)
cor_all_K <- cor(data.all$biom, data.all$K)

cor_saale_P <- cor(data.Saaletal$biom, data.Saaletal$P)
cor_ilm_P <- cor(data.Ilmtal$biom, data.Ilmtal$P)
cor_all_P <- cor(data.all$biom, data.all$P)

cor_saale_Cges <- cor(data.Saaletal$biom, data.Saaletal$Cges)
cor_ilm_Cges <- cor(data.Ilmtal$biom, data.Ilmtal$Cges)
cor_all_Cges <- cor(data.all$biom, data.all$Cges)

cor_saale_Corg.N <- cor(data.Saaletal$biom, data.Saaletal$Corg.N)
cor_ilm_Corg.N <- cor(data.Ilmtal$biom, data.Ilmtal$Corg.N)
cor_all_Corg.N <- cor(data.all$biom, data.all$Corg.N)


###################################################
# Model Selection basierend auf Mallows Cp
# Maximales Modell für Gebiete erstellen:
###################################################

### Ilmtal (Alle Variablen über Threshold von 0.3 einbezogen)
own_model_Ilm <- lm(biom~1+N+Corg+Cges+(Corg.N)+pH, data = data.Ilmtal)

# Best Model mit Mallows Cp
require("leaps")
ilm_bss <- regsubsets(biom~1+N+Corg+Cges+(Corg.N)+pH+Artenzahl+P+K, data = data.Ilmtal, nbest = 3)
summary(ilm_bss)$cp
index <- which.min(summary(ilm_bss)$cp)
summary(ilm_bss)$which[index,]
Cp_Ilm <- lm(biom~1+Cges+(Corg.N), data = data.Ilmtal)


### Saaletal (Alle Variablen über Threshold von 0.3 einbezogen)
own_model_Saale <- lm(biom~1+Artenzahl+N+Corg+Cges+(Corg.N)+P+pH, data = data.Saaletal)

# Best Model mit Mallows Cp
saale_bss <- regsubsets(biom~1+N+Corg+Cges+(Corg.N)+pH+Artenzahl+P+K, data = data.Saaletal, nbest = 3)
summary(saale_bss)$cp
index <- which.min(summary(saale_bss)$cp)
summary(saale_bss)$which[index,]
Cp_Saale <- lm(biom~1+Corg+Cges+Corg.N+pH+P+K, data = data.Saaletal)

# Best Model mit MAllows CP (Gesamter Datensatz)
all_bss <- regsubsets(biom~as.factor(Gebiet)+(N+Corg+Cges+(Corg.N)+pH+Artenzahl+P+K)*as.factor(Gebiet), data = data.all, nbest = 3)
summary(all_bss)$cp
index <- which.min(summary(all_bss)$cp)
summary(all_bss)$which[index,]
Cp_all <- lm(biom~1+Corg+Cges+Corg.N+pH+P+K, data = data.all)




#SPSE berechnen
length = dim(data.Ilmtal)[1]            #data entries
RSS <- sum(residuals(own_model_Ilm)^2)  # residual sum squared of maximal model
sigma2.max <- RSS/(length-length(coef(own_model_Ilm)))  # max.Modell / (#entries - #Parameters of maximal model)

SPSE1 <- RSS + 2*sigma2.max*length(coef(own_model_Ilm))
# Compute expected SPSE for the 5 best models (based on mallow's Cp)
summary(ilm_bss)$cp[c(2,3,4)]*sigma2.max + length * sigma2.max

# Saaletal


####################################################
# 2. Analysieren Sie dann beide Teildatensa??tze gemeinsam
# und untersuchen Sie insbesondere das Vorliegen von 
# Wechselwirkungen, d.h. unterschiedliche quantitative 
# Effekte der Einflussgro???en in den beiden Untersuchungsgebieten.
####################################################



