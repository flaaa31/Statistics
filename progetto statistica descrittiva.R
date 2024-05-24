#codice progetto statistica descrittiva Flavio Orizio

#1-2. carico dataset e breve descrizione
df = read.csv("realestate_texas.csv")

#3. Indici di posizione, variabilità, forma e distribuzione frequenze

# indici di posizione
summary(df)

#indici di variabilità

#range 
t(sapply(df[, 4:8], range))
#IQR 
t(sapply(df[, 4:8], IQR))
#varianza
t(sapply(df[, 4:8], var))
#deviazione standard
t(sapply(df[, 4:8], sd))
#coefficiente di variazione
CV <- function(x){
  return(sd(x)/mean(x)*100)
}

t(sapply(df[4:8], CV))

#indici di forma
library(moments)
t(sapply(df[, 4:8], skewness))
t(sapply(df[, 4:8], kurtosis))-3

#distribuzione di frequenza
sapply(df[, 1:3], table)

#4. variabile con variabilità più elevata e più asimmetrica

#variabilità più elevata

m = sapply(df[4:8], CV)
max_value = max(m)
max_variable = names(which(m == max_value))
cat("La variabile con variabilità più elevata è:", max_variable, "con un coefficiente di variazione del", round(max_value, digits = 1),"%")
#variabile più asimmetrica
sk = sapply(df[, 4:8], skewness)
max_value = max(sk)
max_skewness = names(which(sk == max_value))
cat("La variabile più asimmetrica è:", max_skewness, "con un valore di skewness di:", max_value)

#5. divido sales in classi, distribuzione di frequenza e indice di Gini

range(df$sales)
breaks <- c(0, 150,300,450)
                   
# Creo una nuova colonna sales_cl utilizzando la funzione cut
df$sales_cl <- cut(df$sales, breaks = breaks)
table(df$sales_cl) 

# calcolo delle frequenze 
N<- dim(df)[1]
ni <- table(df$sales_cl)
fi <- table(df$sales_cl)/N
Ni <- cumsum(ni)
Fi <- Ni/N
sales_distr_freq <- cbind(ni, fi, Ni, Fi)
barplot(Fi*100, #oppure ni, fi, Ni
        main = "Percentuale totale di vendite cumulative",
        xlab = "Vendite", 
        ylab = "Percentuale (%)", 
        col = c("royalblue","royalblue","red3"), 
        border = "black")

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

gini.index(df$sales_cl)

#6. indice di Gini di city
gini.index(df$city)

#7. calcolo probabilità
#probabilità città=Beaumont
probabilita_beaumont <- sum(df$city == "Beaumont") / nrow(df)
library(ggplot2)
prova_city <- sample(df$city, 100000, replace = T)
ggplot() +
  geom_histogram(aes(x = prova_city, y = stat(count / sum(count)), fill = prova_city),
                 stat = "count",
                 col = "black") +
  scale_fill_manual(values = c("Beaumont" = "red")) +
  ggtitle("Probabilità che esca la città Beaumont") +
  xlab("Città") +
  ylab("Probabilità")+
  guides(fill=F)+
  theme_minimal()

#probabilità mese=Luglio
probabilita_luglio <- sum(df$month == 7) / nrow(df)

prova_month <- sample(df$month, 1000000, replace = T)

prova_month <- factor(prova_month, levels = 1:12)

ggplot() +
  geom_histogram(aes(x = prova_month, y = stat(count / sum(count)), fill = prova_month),
                 stat = "count",
                 col = "black") +
  scale_fill_manual(values = c("7" = "red")) +
  ggtitle("Probabilità che esca il mese Luglio") +
  xlab("Mese") +
  ylab("Probabilità")+
  guides(fill=F)+
  theme_minimal()


#probabilità mese=Dicembre2012
probabilita_dicembre2012 <- sum(df$month == 12 & df$year == 2012) / nrow(df)
#creo colonna che unisce mese e anno
df$month_year <- paste(df$month, df$year, sep = "-")

prova_month_year <- sample(df$month_year, 10000000, replace = T)
ggplot() +
  geom_histogram(aes(x = prova_month_year, y = stat(count / sum(count)), fill = prova_month_year),
                 stat = "count",
                 col = "black") +
  scale_fill_manual(values = c("12-2012" = "red")) +
  ggtitle("Probabilità che esca il mese Dicembre 2012") +
  xlab("Mese-Anno") +
  ylab("Probabilità")+
  guides(fill=F)+
  theme_minimal()+
  scale_x_discrete(breaks = c("12-2012"))

#8. aggiungo colonna che calcola il prezzo medio

df$mean_price <- df$volume*1000000 / df$sales

# 9. colonna “efficacia” degli annunci di vendita: listings_effeciency

df['listings_efficacy'] = df['sales'] / df['listings']


# Calcolo l'efficacia media degli annunci di vendita per ogni città
library(dplyr)
city_efficacy <- df %>% group_by(city) %>% summarise(mean_efficacy = mean(listings_efficacy))

# Creo un grafico a barre orizzontale dell'efficacia media degli annunci per città
ggplot(city_efficacy, aes(x = city, y = mean_efficacy)) +
  geom_bar(stat = "identity", fill = c('royalblue','darkorchid', 'forestgreen', 'darkorange')) +
  coord_flip() +
  xlab("Città") +
  ylab("Efficacia media annunci di vendita")+
  theme_minimal()

#10. summary di variabili a scelta

summary(df$sales)

#summary delle vendite per ogni città
city_sales <- df %>%
  group_by(city) %>%
  summarise(
    min_sales = min(sales),
    q1_sales = quantile(sales, 0.25),
    median_sales = median(sales),
    mean_sales = mean(sales),
    q3_sales = quantile(sales, 0.75),
    max_sales = max(sales),
    devstd_sales = sd(sales)
    )
city_sales
#summary del valore delle vendite in milioni di dollari per l'anno 2013, 2011 e totale
city_volume_2013 <- df %>%
  filter(year == 2013) %>%
  group_by(city) %>%
  summarise(
    min_volume = min(volume),
    q1_volume = quantile(volume, 0.25),
    median_volume = median(volume),
    mean_volume = mean(volume),
    q3_volume = quantile(volume, 0.75),
    max_volume = max(volume),
    devstd_volume = sd(volume)
  )

city_volume_2011 <- df %>%
  filter(year == 2011) %>%
  group_by(city) %>%
  summarise(
    min_volume = min(volume),
    q1_volume = quantile(volume, 0.25),
    median_volume = median(volume),
    mean_volume = mean(volume),
    q3_volume = quantile(volume, 0.75),
    max_volume = max(volume),
    devstd_volume = sd(volume)
  )

city_volume_all <- df %>%
  group_by(city) %>%
  summarise(volume_mean_all = mean(volume))


colors <- c('royalblue','darkorchid', 'forestgreen', 'darkorange')

# grafico mean_volume per city nel 2013

p1 <- ggplot(city_volume_2013, aes(x = city, y = mean_volume)) +
  geom_bar(stat = "identity", fill=colors) +
  xlab("Città") +
  ylab("Media del valore totale delle vendite (milioni di $)") +
  ylim(0, 60)+
  theme_minimal()+
  ggtitle("Media del valore totale delle vendite per città - Anno 2013")

# grafico mean_volume per city
p2 <- ggplot(city_volume_all, aes(x = city, y = volume_mean_all)) +
  geom_bar(stat = "identity", fill=colors) +
  xlab("Città") +
  ylab("Media del valore totale delle vendite (milioni di $)") +
  ylim(0, 60)+
  theme_minimal()+
  ggtitle("Media del valore totale delle vendite per città - Tutti gli anni")

# grafico mean_volume per city nel 2011
p3 <- ggplot(city_volume_2011, aes(x = city, y = mean_volume)) +
  geom_bar(stat = "identity", fill=colors) +
  xlab("Città") +
  ylab("Media del valore totale delle vendite (milioni di $)") +
  ylim(0, 60)+
  theme_minimal()+
  ggtitle("Media del valore totale delle vendite per città - Anno 2011")

print(p1)
print(p2)
print(p3)
# Mostra i tre grafici a confronto
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)

# 1) boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città
x_title<- "Città"
y_title<- "Prezzo Mediano"
ggplot(df, aes(x = city, y = median_price, fill=city)) +
  geom_boxplot(outlier.colour = "black", outlier.shape =23, outlier.fill = "red")+
  xlab(x_title)+
  ylab(y_title)+
  ylim(80000, 200000)+
  scale_fill_manual(values =colors)+
  theme_minimal()

x_title<- "Città"
y_title<- "Prezzo Medio"
ggplot(df, aes(x = city, y = mean_price, fill=city)) +
  geom_boxplot(outlier.colour = "black", outlier.shape =23, outlier.fill = "red")+
  xlab(x_title)+
  ylab(y_title)+
  ylim(80000, 200000)+
  scale_fill_manual(values =colors)+
  theme_minimal()



#2) distribuzione del valore totale delle vendite tra le varie città e tra i vari anni
ggplot(df, aes(x = city, y = volume, fill = city)) +
  geom_boxplot(outlier.colour = "black", outlier.fill = "red", outlier.shape = 23) +
  xlab('Città') +
  ylab('Valore totale delle vendite')+
  facet_wrap(~ year)+
  scale_fill_manual(values=colors)+
  theme_minimal() 
#migliore per confronti tra le varie città in quell'anno


# Stessa cosa ma creando un grafico separato per ogni anno con un ciclo for
years <- unique(df$year)
for (year in years) {
  # Filtro i dati per l'anno corrente
  df_year <- df[df$year == year,]
  
  # Creo il boxplot
  p <- ggplot(df_year, aes(x = city, y = volume, fill = city)) +
    geom_boxplot(outlier.colour = "black", outlier.fill = "red", outlier.shape = 23) +
    xlab('Città') +
    ylab('Valore totale delle vendite')+
    ggtitle(paste0("Anno: ", year)) +
    scale_fill_manual(values=colors)+
    theme_minimal()
  
  print(p)
}

# grafico a barre raggruppate
ggplot(df, aes(x = city, y = volume, fill = factor(year))) +
  geom_col(position = 'dodge') +
  xlab('Città') +
  ylab('Valore totale delle vendite')+
  scale_fill_brewer(palette="Set3")+
  theme_minimal()
#migliore per confrontare la stessa città nei diversi anni


#3). grafico a barre sovrapposte per ogni anno, 
#per confrontare il totale delle vendite nei vari mesi

for (i in unique(df$year)) {
  p <- ggplot(df[df$year == i,], aes(fill=city, y=sales, x=month)) +
    geom_bar(position='stack', stat='identity')+
    ggtitle(paste("Anno", i))+
    xlab("Mese")+
    ylab("Totale vendite")+
    scale_x_continuous(breaks = 1:12)+
    scale_fill_manual(values=colors)+
    theme_minimal()
  print(p)
}


#normalizzato

for (i in unique(df$year)) {
  p <- ggplot(df[df$year == i,], aes(fill=city, y=sales, x=month)) +
    geom_bar(position='fill', stat='identity') +
    ggtitle(paste("Anno", i)) +
    scale_x_continuous(breaks = 1:12)+
    xlab("Mese")+
    ylab("Totale vendite")+
    scale_fill_manual(values=colors)+
    theme_minimal()
  print(p)
}

#aggiungo variabile "year" unendola con "month"
ggplot(df, aes(fill=city, y=sales, x=interaction(month, year))) +
  geom_bar(position='stack', stat='identity')+
  xlab("Mese")+
  ylab("Totale vendite")+
  scale_fill_manual(values=colors)+
  theme_minimal()+
  coord_flip()

#4).line chart di una variabile per fare confronti fra città e periodi storici

library(tidyverse)
library(lubridate)


# Trasformo il dataset in formato "long"
data_long <- df %>%
  mutate(date = make_date(year, month)) %>%
  select(city, date, median_price) %>%
  pivot_longer(cols = median_price, names_to = "variable", values_to = "value")

#line chart "median_price"
ggplot(data_long, aes(x = date, y = value, color = city)) +
  geom_line() +
  scale_x_date(limits = c(make_date(2010), make_date(2015)), date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values=colors)+
  theme_minimal()+
  labs(x = "Data", y = "Prezzo mediano di vendita (in dollari)", title = "Confronto del prezzo mediano di vendita tra città")


# Trasformo il dataset in formato "long"
data_long <- df %>%
  mutate(date = make_date(year, month)) %>%
  select(city, date, sales) %>%
  pivot_longer(cols = sales, names_to = "variable", values_to = "value")


# line chart "sales"
ggplot(data_long, aes(x = date, y = value, color = city)) +
  geom_line() +
  scale_x_date(limits = c(make_date(2010), make_date(2015)), date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = colors) +
  theme_minimal()+
  labs(x = "Data", y = "Numero totale di vendite", title = "Confronto del numero totale di vendite tra città")

