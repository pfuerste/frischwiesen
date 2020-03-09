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

# Visualisieren der Variablen im Verhältnis:
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


################
### Saaletal ###
################
model_saale_N <- lm(biom~1+N, data = data.Saaletal) # biomasse ~ Stickstoff
model_saale_Corg <- lm(biom~1+Corg, data = data.Saaletal) # biomasse ~Kohlenstoff
model_saale_Artenzahl <- lm(biom~1+Artenzahl, data = data.Saaletal) # biomasse ~ Artenzahl
model_saale_pH <- lm(biom~1+pH, data = data.Saaletal) # biomasse ~ pH
model_saale_K <- lm(biom~1+K, data = data.Saaletal) # biomasse ~ Kalium

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
# 2. Analysieren Sie dann beide Teildatensa??tze gemeinsam
# und untersuchen Sie insbesondere das Vorliegen von 
# Wechselwirkungen, d.h. unterschiedliche quantitative 
# Effekte der Einflussgro??ßen in den beiden Untersuchungsgebieten.
####################################################



