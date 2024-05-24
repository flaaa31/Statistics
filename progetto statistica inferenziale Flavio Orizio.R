setwd("C:/Users/orizi/profession_AI/3.statistica inferenziale/progetto/")
#1. importo il dataset
df <- read.csv("neonati.csv", stringsAsFactors = T)
n=nrow(df)
attach(df)

#3. Indaga le variabili effettuando una breve analisi descrittiva
summary(df)
range(Anni.madre)
qual_cols <- df[, c(3,8:10)]
quant_cols <- df[, c(1,2,4:7)]

table(Tipo.parto)/n
table(Fumatrici)/n

plot(density(Peso))
moments::skewness(Peso) #asimmetrica
moments::kurtosis(Peso)-3 # distribuzione leptocurtica
shapiro.test(Peso) #non normale 

#deviazione standard
t(sapply(quant_cols, sd))
#coefficiente di variazione
CV <- function(x){
  return(sd(x)/mean(x)*100)
}
t(sapply(quant_cols, CV)) #n.gravidanze è la variabile con variabilità più elevata

hist(N.gravidanze)
plot(density(N.gravidanze))
abline(v=mean(N.gravidanze),col=2)
library(moments)
t(sapply(quant_cols, skewness))
t(sapply(quant_cols, kurtosis)-3)

#indice di Gini
gini.index <-function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini_norm = gini/((J-1)/J)
  
  return(gini_norm)
}
t(sapply(qual_cols, gini.index))

#4. Saggia l’ipotesi che la media del peso e della lunghezza di questo campione di neonati siano significativamente uguali a quelle della popolazione
#true mu = 3300 grammi
t.test(Peso, mu = 3300) #non posso rifiutare l'ipotesi nulla che peso sia nella media

shapiro.test(Peso)
wilcox.test(Peso, mu=3300)

#true length M = 505 mm 
t.test(subset(df, Sesso == "M")$Lunghezza,
        mu=505,
        conf.level = 0.95,
        alternative = "two.sided")

#true length F = 495 mm 
t.test(subset(df, Sesso == "F")$Lunghezza,
       mu=495,
       conf.level = 0.95,
       alternative = "two.sided")
library(ggplot2)


#grafico di densità per lunghezza femmine
ggplot(subset(df, Sesso == "F"), aes(x = Lunghezza)) +
  geom_density() +
  geom_vline(xintercept = 495, color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(subset(df, Sesso == "F")$Lunghezza), color = "green", linetype = "dashed")+
  xlab("Lunghezza") + ylab("Densità")+
  labs(title = "Grafico di densità della lunghezza per il sesso femminile")+
  annotate("text", x = Inf, y = Inf, label = "Media popolazione", color = "red", hjust = 1.3, vjust = 1) +
  annotate("text", x = Inf, y = Inf, label = "Media campione", color = "green", hjust = 2.8, vjust = 1)+
  theme_minimal()


#grafico di densità per lunghezza maschi
ggplot(subset(df, Sesso == "M"), aes(x = Lunghezza)) +
  geom_density() +
  geom_vline(xintercept = 505, color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(subset(df, Sesso == "M")$Lunghezza), color = "green", linetype = "dashed")+
  xlab("Lunghezza") + ylab("Densità")+
  labs(title = "Grafico di densità della lunghezza per il sesso maschile")+
  annotate("text", x = Inf, y = Inf, label = "Media popolazione", color = "red", hjust = 1.15, vjust = 1) +
  annotate("text", x = Inf, y = Inf, label = "Media campione", color = "green", hjust = 2.6, vjust = 1)+
  theme_minimal()

shapiro.test(Lunghezza)

wilcox.test(subset(df, Sesso == "M")$Lunghezza, mu=505)
wilcox.test(subset(df, Sesso == "F")$Lunghezza, mu=495)

#5.Per le stesse variabili, o per altre per le quali ha senso farlo, verifica differenze significative tra i due sessi

t.test(Lunghezza ~ Sesso, data=df) #significativo
t.test(Peso ~ Sesso, data=df) #significativo
t.test(Cranio ~ Sesso, data=df) #significativo
t.test(Gestazione ~ Sesso, data=df) #significativo
t.test(Anni.madre ~ Sesso, data=df) #no sign


wilcox.test(Lunghezza ~ Sesso, data=df) #significativo
wilcox.test(Peso ~ Sesso, data=df) #significativo
wilcox.test(Cranio ~ Sesso, data=df) #significativo
wilcox.test(Gestazione ~ Sesso, data=df) #significativo
wilcox.test(Anni.madre ~ Sesso, data=df) #no sign


ggplot(df, aes(x = Sesso, y = Gestazione, fill = Sesso)) +
  geom_boxplot() +
  scale_fill_manual(values = c("M" = "lightblue", "F" = "pink"))+
  ylab("Gestazione (settimane)")+
  xlab("Sesso")+
  theme_minimal()

#6. in alcuni ospedali si fanno più parti cesarei?

library(ggpubr)
osp_parto <- table(Tipo.parto, Ospedale)
ggballoonplot(as.data.frame(osp_parto),fill = "blue")+
  theme_minimal()
              
chisq.test(table(Tipo.parto, Ospedale)) #non rifiuto l'ipotesi nulla che si fa lo stesso numero di cesarei


#1) indaga le relazioni a due a due, soprattutto con la variabile risposta   

#relazione Peso-Lunghezza

cor(Peso, Lunghezza) #correlazione positiva
ggplot(df, aes(x=Lunghezza, y=Peso)) +
  geom_point() +
  geom_smooth(method='lm', color="red")+
  xlab("Lunghezza (mm)")+
  ylab("Peso (g)")+
  theme_minimal()

mod_lin <- lm(Peso ~ Lunghezza)
summary(mod_lin)
# per ogni mm di lunghezza il peso aumenta di quasi 16 grammi

#assunzioni sui residui
par(mfrow=c(1,2))
plot(residuals(mod_lin))
abline(h=mean(residuals(mod_lin)), col=2)
plot(density(residuals(mod_lin)))
mtext("Analisi residui correlazione Peso-Lunghezza", side = 3, line = -1.5, cex = 1.5, col = "blue", outer = TRUE)
shapiro.test(residuals(mod_lin)) #non normale
library(lmtest)
bptest(mod_lin) # eteroschedasticità
dwtest(mod_lin) #residui non autocorrelati


#relazione Peso-diametro cranio

cor(Peso, Cranio) #correlazione positiva

ggplot(df, aes(x=Cranio, y=Peso)) +
  geom_point() +
  geom_smooth(method='lm', color="red")+
  xlab("Diametro cranio (mm)")+
  ylab("Peso (g)")+
  theme_minimal()

mod_lin <- lm(Peso ~ Cranio)
summary(mod_lin)
#per ogni millimetro di diametro del cranio, il peso aumenta di 22 grammi
#assunzioni sui residui
par(mfrow=c(1,2))
plot(residuals(mod_lin)) 
abline(h=mean(residuals(mod_lin)), col=2)
plot(density(residuals(mod_lin)))
shapiro.test(residuals(mod_lin)) #non normale
mtext("Analisi residui correlazione Peso-Diametro Cranio", side = 3, line = -1.5, cex = 1.5, col = "blue", outer = TRUE)
bptest(mod_lin) # varianza costante: omoschedasticità
dwtest(mod_lin) #residui non autocorrelati

#correlazione Peso-Sesso

ggplot(df, aes(x = Sesso, y = Peso, fill = Sesso)) +
  geom_boxplot() +
  scale_fill_manual(values = c("M" = "lightblue", "F" = "pink"))+
  ylab("Peso (g)")+
  xlab("Sesso")+
  theme_minimal()

mod_lin <- lm(Peso ~ Sesso)
summary(mod_lin)

#maschi pesano 247 grammi più delle femmine

#assunzioni sui residui
par(mfrow=c(1,2))
plot(residuals(mod_lin))
abline(h=mean(residuals(mod_lin)), col=2)
plot(density(residuals(mod_lin)))
shapiro.test(residuals(mod_lin)) #non normale
bptest(mod_lin) #omoschedasticità
dwtest(mod_lin) #residui autocorrelati

#relazione Peso-Fumatrici

cor(Peso, as.numeric(Fumatrici)) #bassissima correlazione negativa

df$Fumatrici <- factor(df$Fumatrici, levels = c(0, 1), labels = c("Non Fumatrici", "Fumatrici"))
ggplot(df, aes(x=Fumatrici, y=Peso, fill=Fumatrici)) +
  geom_boxplot() +
  scale_fill_manual(values=c("forestgreen", "saddlebrown")) +
  xlab("Fumatrici o non fumatrici") +
  ylab("Peso (g)") +
  theme_minimal()

mod_lin <- lm(Peso ~ Fumatrici)
summary(mod_lin) #variabile non significativa

cor(N.gravidanze,Peso) #bassissima correlazione positiva
mod_lin <- lm(Peso ~ N.gravidanze)
summary(mod_lin) #variabile non significativa

#2) Crea un modello di regressione lineare multipla con tutte le variabili e commenta i coefficienti e il risultato ottenuto

round(cor(quant_cols),2)

#pannello 
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  
  # Imposta il colore del testo in base al valore del coefficiente di correlazione
  col <- ifelse(r > 0.7, "red", ifelse(r > 0.5, "orange", "black"))
  
  text(0.5, 0.5, txt, cex = cex.cor, col = col)
}

x11()
pairs(df, upper.panel = panel.smooth, lower.panel = panel.cor)

boxplot(Peso~Fumatrici)
boxplot(Peso~Tipo.parto)

mod1 <- lm(Peso ~., data=df)
summary(mod1)

mod2 <- update(mod1, ~.-Anni.madre)
summary(mod2)

mod3 <- update(mod2, ~.-Fumatrici)
summary(mod3)

mod4 <- update(mod3, ~.-Ospedale)
summary(mod4)

mod5 <- update(mod4, ~.-Tipo.parto)
summary(mod5)


BIC(mod1, mod2, mod3, mod4, mod5)
AIC(mod1, mod2, mod3, mod4, mod5)

anova(mod4, mod5)
#scelgo il modello 4

#4) Si potrebbero considerare interazioni o effetti non lineari?

#provo con log(Gestazione)
plot(Gestazione,Peso,pch=20)

mod6 <- update(mod4, ~. +I(log(Gestazione)))
summary(mod6) #r2 aumenta di poco
summary(mod4)

#interazione Lunghezza-Cranio?
mod7 <- update(mod4, ~. + Cranio*Lunghezza)
summary(mod7) #aumenta r2 ma di poco

BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)

#ANOVA
anova(mod4, mod6)
anova(mod4, mod7)

#VIF
library(car)
vif(mod4)
vif(mod6)
vif(mod7)

#decido di tenere mod4

#5)diagnostica sui residui

par(mfrow=c(2,2))
plot(mod4) #osservazione 12551 unica problematica
#leverage
lev <- hatvalues(mod4)
plot(lev)
p=sum(lev)
soglia = 2*p/n
abline(h=soglia, col=2)
length(lev[lev>soglia])

#outliers
plot(rstudent(mod4))
abline(h=c(-2,2), col=2)
outlierTest(mod4)

#distanza di cook
cook <- cooks.distance(mod4)
plot(cook)
max(cook)

#test sui residui
bptest(mod4) #eteroschedasticità
dwtest(mod4) #residui non autocorrelati
shapiro.test(residuals(mod4)) # non normali

plot(density(residuals(mod4)))

#Elimino osservazione 1551

new_df <- df[-c(1551),]
new_mod1 <- lm(Peso ~., data=new_df)
summary(new_mod1)

new_mod2 <- update(new_mod1, ~.-Anni.madre)
summary(new_mod2)

new_mod3 <- update(new_mod2, ~.-Fumatrici)
summary(new_mod3)

new_mod4 <- update(new_mod3, ~.-Ospedale)
summary(new_mod4)

new_mod5 <- update(new_mod4, ~.-Tipo.parto)
summary(new_mod5)


BIC(new_mod1, new_mod2, new_mod3, new_mod4, new_mod5) 
AIC(new_mod1, new_mod2, new_mod3, new_mod4, new_mod5) 

#diagnostica sui residui

par(mfrow=c(2,2))
plot(new_mod4)
#leverage
lev <- hatvalues(new_mod4)
plot(lev)
p=sum(lev)
soglia = 2*p/n
abline(h=soglia, col=2)
length(lev[lev>soglia])

#outliers
plot(rstudent(new_mod4))
abline(h=c(-2,2), col=2)
outlierTest(new_mod4)

#distanza di cook
cook <- cooks.distance(new_mod4)
plot(cook)
max(cook)

#test sui residui
bptest(new_mod4) #varianza costante
dwtest(new_mod4) #residui non autocorrelati
shapiro.test(residuals(new_mod4)) # non sono normali

plot(density(residuals(new_mod4)))

# r2 mod4
summary(mod4)$r.squared

# RMSE mod4
sqrt(mean(resid(mod4)^2))

# r2 new_mod4
summary(new_mod4)$r.squared

# RMSE new_mod4
sqrt(mean(resid(new_mod4)^2))

#new_mod4 sembra il miglior modello, cioè mod4 senza l'outlier 12551


#7)predizione bambina, terza gravidanza, 39 settimane 


predict(new_mod4, data.frame(N.gravidanze = 2, Gestazione = 39, Sesso="F", Lunghezza = mean(df$Lunghezza), Cranio = mean(df$Cranio), Tipo.parto = "Nat"))

#8) grafici modello

#numero gravidanze-peso-tipo parto

ggplot(data=df)+
  geom_point(aes(x=N.gravidanze,
                 y=Peso,
                 col=Tipo.parto), position="jitter")+
  geom_smooth(aes(x=N.gravidanze,
                  y=Peso,
                  col=Tipo.parto),se=F, method="lm")+
  geom_smooth(aes(x=N.gravidanze,
                  y=Peso),
              col="black",se=F, method="lm")+
  xlab("Numero di gravidanze")+
  ylab("Peso (g)")+
  scale_color_manual(values=c("red2", "green3"))+
  theme_minimal()

#peso-settimane gestazione-sesso
ggplot(data=df)+
  geom_point(aes(x=Gestazione,
                 y=Peso,
                 col=Sesso), position="jitter")+
  geom_smooth(aes(x=Gestazione,
                  y=Peso,
                  col=Sesso),se=F, method="lm")+
  geom_smooth(aes(x=Gestazione,
                  y=Peso),
              col="black",se=F, method="lm")+
  xlab("tempo gestazione (settimane)")+
  ylab("Peso (g)")+
  scale_color_manual(values=c("hotpink", "#00BFFF"))+
  theme_minimal()

#numero gravidanze-peso-fumatrici
df$Fumatrici <- factor(df$Fumatrici)
ggplot(data=df)+
  geom_point(aes(x=N.gravidanze,
                 y=Peso,
                 col=Fumatrici), position="jitter")+
  geom_smooth(aes(x=N.gravidanze,
                  y=Peso,
                  col=Fumatrici),se=F, method="lm")+
  geom_smooth(aes(x=N.gravidanze,
                  y=Peso),
              col="black",se=F, method="lm")+
  xlab("Numero di gravidanze")+
  ylab("Peso (g)")+
  scale_color_manual(values=c("forestgreen", "saddlebrown"))+
  theme_minimal()

#il grafico prova a dire qualcosa ma nessun test statistico lo supporta


#peso-settimane gestazione-fumo
ggplot(data=df)+
  geom_point(aes(x=Gestazione,
                 y=Peso,
                 col=Fumatrici), position="jitter")+
  geom_smooth(aes(x=Gestazione,
                  y=Peso,
                  col=Fumatrici),se=F, method="lm")+
  geom_smooth(aes(x=Gestazione,
                  y=Peso),
              col="black",se=F, method="lm")+
  xlab("tempo gestazione (settimane)")+
  ylab("Peso (g)")+
  scale_color_manual(values=c("forestgreen", "saddlebrown"))+
  theme_minimal()

#troppo poco per affermare che Fumatrici sia significativo



