#autor: Patryk Bednarski
#Grupowanie: Zadanie zaliczeniowe

library(dbscan)
library(fpc)
library(cluster)
library(mclust)
library(factoextra)
library(dplyr)
library(ggplot2)


set.seed(1234)

# dane: https://www.kaggle.com/datasets/abcsds/pokemon

pokemon <- read.csv("Pokemon.csv")
pokemon2 <- read.csv("Pokemon.csv")

# Punkt 1 Sprawdzanie danych, atrybutów, brakujących wartości

# Sprawdzenie liczby  unikatowych typów pokemonów
table(pokemon$Type.1)
length(unique(pokemon$Type.1)) # 18

# Sprawdzanie liczby legendarnych pokemonów
table(pokemon$Legendary) # 65 legendarnych, 735 zwykłych

# Usunięcie kolumn tekstowych: Name, Type.1, Type.2, Legendary oraz X.
pokemon <- subset(pokemon, select = -X.)
pokemon <- subset(pokemon, select = -Name)
pokemon <- subset(pokemon, select = -Type.1)
pokemon <- subset(pokemon, select = -Type.2)
pokemon <- subset(pokemon, select = -Legendary)

# Usunięcie kolumny generation ze względu na małą przydatność do grupowa
# Kolumna określa w której generacji pokemon został wprowadzony przez twórców do ich uniwersum
pokemon <- subset(pokemon, select = -Generation)

str(pokemon)

# W zbiorze danych występują puste wartości jako "", zostaną zamieniona na NA
for (col in names(pokemon)) {
  pokemon[pokemon[[col]] == "", col] <- NA
}

# Brakujące wartości
brakujace_wartosci <- colSums(is.na(pokemon))
print(brakujace_wartosci) #0

# Badanie danych pod względem zależności ######################################################

# Wykres obrazujący stosunek atrybutów Total i HP z podziałem na Pokemony określone 
# jako Legendarne lub nie.
ggplot(pokemon2, aes(x = Total, y = HP, color = factor(Legendary))) +
  geom_point() +
  labs(x = "Total", y = "HP") +
  scale_color_discrete(name = "Legendary") +
  theme_minimal()
# Wykres wskazuje, że moge istnieć możliwośc grupowania pod względem atrybutów Total i HP,
# Więcej Legendarnych pokemonów znajduje się po prawej stronie na osi X

ggplot(pokemon2, aes(x = Total, y = Attack, color = factor(Legendary))) +
  geom_point() +
  labs(x = "Total", y = "Attack") +
  scale_color_discrete(name = "Legendary") +
  theme_minimal()

ggplot(pokemon2, aes(x = Total, y = Generation, color = factor(Legendary))) +
  geom_point() +
  labs(x = "Total", y = "Generation") +
  scale_color_discrete(name = "Legendary") +
  theme_minimal()
# Pokemony dodane w każdej generacji mają wyraźnie większą wartość w kolumnie Total jeśli są Legendarne


# Punkt 2, Grupowanie algorytmem partycjonującym

#######################################################
#######################################################
# 2a znajdowanie optymalnej liczby grup - metoda 'łokcia'#

# Wyznaczenie losowo wybranych 25%
set.seed(1)
sample_data <- sample_frac(pokemon, 0.25)

# Wykres obrazujący optymalną liczbę grup
wss <- c()
for (i in 1:10) {
  kmeans_model <- kmeans(select(sample_data, -c("Total")), centers = i)
  wss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Liczba grup", ylab = "Suma bledu kwadratowego wewnatrz grup")
# Wykres wskazuje, że optymalna liczba grup to 2 lub 3


#################################
#metoda partycjonująca - kmeans #
#################################

# 2b

# Eksperyment pierwszy - Kmeans z 2 środkami.
# Celem próby jest sprawdzenie, czy algorytm prawidłowo wskaże Pokemony, które są Legendary. 
# (Kolumna Legendary == True)

pokemon.kmeans = kmeans(pokemon,2, iter.max = 20, nstart=20)
print(pokemon.kmeans)
print(pokemon.kmeans$iter)
print(pokemon.kmeans$centers)
print(pokemon.kmeans$cluster)

table(pokemon2$Legendary,pokemon.kmeans$cluster)

#         1   2
# False 372 363
# True    0  65

# Algorytm przyporządkował 65 pokemony legendarne to jednej grupy/
# Pokemony nielegendarne zostały rozdzielone pomiędzy 2 grupy.

# Wykres obrazujący wykryte grupy na wykresie zależności wartości Total i HP
ggplot(pokemon2, aes(x = Total, y = HP, color = factor(pokemon.kmeans$cluster))) +
  geom_point() +
  labs(x = "Total", y = "HP") +
  scale_color_discrete(name = "Grupy") +
  theme_minimal()
# Wyniki wskazują na wyraźny podział na dwie grupy mniej więcej na linii wartości Total = 420 na osi X

# Skalowanie
pokemonScale <- scale(pokemon, center = FALSE)
pokemonScale <- as.data.frame(pokemonScale)

# Próba grupowania danych po skalowaniu
pokemon.kmeansS = kmeans(pokemonScale,2, iter.max = 20,nstart = 20)

table(pokemon2$Legendary,pokemon.kmeansS$cluster)
# Algorytm po skalowaniu prawidłowo przyporządkował wszystkie 65 legendarnych pokemonów do tej samej grupy.

#       1   2
# False 376 359
# True    0  65

# 4 pokemony nielegendarne, które wcześniej byly przyporzadkowane do grupy z legendarnymi zostały przeniesione
# do innej grupy


# Wykres obrazujący wykryte grupy na wykresie zależności wartości Total i HP po skalowaniu
ggplot(pokemon2, aes(x = Total, y = HP, color = factor(pokemon.kmeansS$cluster))) +
  geom_point() +
  labs(x = "Total", y = "HP") +
  scale_color_discrete(name = "Grupy") +
  theme_minimal()
# Na wykresie wyraźnie widać linię podziału na wartości ok 420 na osi X dla kolumny Total.
# Pokemony legendarne rzeczywiście znajdują się na większych wartościach osi X wartości Total, 
# jednak wiele pokemonów nielegendarnych zostało błędnie zakwalifikowanych


# Sprawdzenie dla różnych algorytmów ###############################################
algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
# "Hartigan-Wong"
pokemon.kmeansS_HW = kmeans(pokemonScale,2, iter.max = 20,nstart = 20, algorithm = algorithm[1])
table(pokemon2$Legendary,pokemon.kmeansS_HW$cluster)
#Lloyd
pokemon.kmeansS_Ll = kmeans(pokemonScale,2, iter.max = 20,nstart = 20, algorithm = algorithm[2])
table(pokemon2$Legendary,pokemon.kmeansS_Ll$cluster)
# Forgy
pokemon.kmeansS_F = kmeans(pokemonScale,2, iter.max = 20,nstart = 20, algorithm = algorithm[3])
table(pokemon2$Legendary,pokemon.kmeansS_F$cluster)
# MacQueen
pokemon.kmeansS_MC = kmeans(pokemonScale,2, iter.max = 20,nstart = 20, algorithm = algorithm[4])
table(pokemon2$Legendary,pokemon.kmeansS_MC$cluster)
# najlepsze wyniki w tym przypadku daje algorytm Hartigan-Wong i MacQueen
#####################################################################################

# Przy podziale na dwie grupy algorytm Kmeans potrafi poprawnie wskazać wszystkie Pokemony określane jako
# legendarne, jednak błędnie kwalifikuje dużą część pokemonów nielegendarnych. Podobne wyniki
# są uzyskiwane przez 4 różne algorytmy możliwe do wykorzystania w funkcji kmeans


# 2c
# Ocena jakości grupowania za pomocą indeksu Silhouette
# Nieprzeskalowane
km_alt<-eclust(pokemon, "kmeans", k=2, graph=TRUE)
fviz_silhouette(km_alt, palette="jco")
str(km_alt)
silinfo<-km_alt$silinfo
names(silinfo)
#długości wsk. sylwetkowego dla każdej obserwacji
head(silinfo$widths[,1:3],10)
#długości wsk. sylwetkowego dla każdej grupy
silinfo$clus.avg.widths
#średnia długość wsk. sylw.
silinfo$avg.width
# 0.4870348

  # Im bliżej 1, tym lepsza jest jakość klastrów.
  # Wartości bliskie zeru lub ujemne sugerują, że klastry mogą 
  # być przekształcone lub mają niską jakość.

# Ocena jakości grupowania za pomocą indeksu Silhouette
# Przeskalowane
km_altS<-eclust(pokemonScale, "kmeans", k=2, graph=TRUE)
fviz_silhouette(km_altS, palette="jco")
str(km_altS)
silinfoS<-km_altS$silinfo
#średnia długość wsk. sylw.
silinfoS$avg.width
# 0.306279

# 2d Przypisanie poszczególnych rekordów do grup
pokemon_kmean_2_df <- data.frame(pokemon, Cluster = as.factor(pokemon.kmeansS$cluster))

# 2e
# Znalezienie charakterystycznych elementów w grupach k-średnich
print(pokemon.kmeans$centers)

# Pokemony legedarne zakwalifikowane zostały do grupy drugiej.
# Algorytm wskazał, że środki dla tej grupy są większe dla każdego atrybutu.

# cluster_kmeans    Total       HP   Attack  Defense  Sp..Atk  Sp..Def    Speed
#1              1 328.5027 54.18351 58.34574 56.31383 52.41223 53.63032 53.61702
#2              2 529.6344 82.62736 97.31840 89.38679 90.91745 88.10613 81.2783


# Dane po przeskalowaniu dały lepszy wynik w zestawieniu tabelarycznym, jednak gorszy wskaźnik indeksu Silhouette

#######################################################################
# Eksperyment drugi - próba podziału na 3 grupy

# 2b
#Nieprzeskalowane
pokemon.kmeans3 = kmeans(pokemon,3, iter.max = 20,nstart = 20)

table(pokemon2$Legendary,pokemon.kmeans3$cluster)

#           1   2   3
#   False 288 389  58
#   True    0   0  65

# Skorzystanie z 3 grup pozwoliło zminimalizować ilość pokemonów nielegendarnych przyporządkowanych
# do grupy wraz z legendarnymi.
# Wszystkie 65 pokemonów legendarnych zostało prawidłowo przyporządkowane do wspólnej grupy.
# Do grupy tej przyporządkowano także 58 pokemonów nielegendarnych.

ggplot(pokemon2, aes(x = Total, y = HP, color = factor(pokemon.kmeans3$cluster))) +
  geom_point() +
  labs(x = "Total", y = "HP") +
  scale_color_discrete(name = "Grupy") +
  theme_minimal()

# 2c
# Ocena jakości grupowania za pomocą indeksu Silhouette na 3 grupy
# Nieprzeskalowane
km_alt3<-eclust(pokemon, "kmeans", k=3, graph=TRUE)
fviz_silhouette(km_alt3, palette="jco")
str(km_alt3)
silinfo3<-km_alt3$silinfo
names(silinfo3)
#długości wsk. sylwetkowego dla każdej obserwacji
head(silinfo3$widths[,1:3],10)
#długości wsk. sylwetkowego dla każdej grupy
silinfo3$clus.avg.widths
#średnia długość wsk. sylw.
silinfo3$avg.width
# 0.4335

# Nieznacznie lepsza wartość indeksu została uzyskana przy grupowaniu z 2 środkami na nieprzeskalowanych danych.

#2d Przypisanie poszczególnych rekordów do grup
pokemon_kmean_3_df <- data.frame(pokemon, Cluster = as.factor(pokemon.kmeans3$cluster))

# 2e
# Znalezienie charakterystycznych elementów w grupach k-średnich
print(pokemon.kmeans3$centers)

#       Total       HP    Attack   Defense   Sp..Atk   Sp..Def    Speed
# 1 303.8958  50.14931  53.95486  52.78472  47.85417  49.49306 49.65972
# 2 472.9666  77.19280  85.30077  80.95373  77.54499  79.02057 72.95373
# 3 622.5691  88.91057 117.72358 100.65854 116.33333 101.86179 97.08130

###################################################################################

# Porównanie wyników grupowa przy pomocy algorytmu Kmeans dla 2 grup i 3 grup
# wskaźnik Rand

# Zmiana na oznaczenia liczbowe danych z kolumny Legendary
# Dane w kolumnie były zapisane jako tekst
pokemon2$Legendary <- ifelse(pokemon2$Legendary == "False", FALSE, pokemon2$Legendary)
pokemon2$Legendary <- ifelse(pokemon2$Legendary == "True", TRUE, pokemon2$Legendary)
pokemon2$Legendary <- as.logical(pokemon2$Legendary)
legendary <- as.integer(pokemon2$Legendary)
legendary <- ifelse(legendary == 0, 1, ifelse(legendary == 1, 2, legendary))
# Pokemon legendarny = 2, zwykły = 1

#Przeskalowane, 2 grupy
# W grupowaniu 2 = grupa legendarny, 1 = grupa zwykłych
clust_stats <- cluster.stats(d=dist(pokemon), legendary, pokemon.kmeansS$cluster)
str(clust_stats)
clust_stats$corrected.rand
# Rand = 0.007630934, 2 grupy, przeskalowane


# Nieprzeskalowane, 3 grupy
# Zmieniam oznaczenia grup, jeśli coś było w grupie 3 (głównie pokemony legendarny), grupa zostanie oznaczona
# literą 2, jeśli obiekt był w grupie 1 lub drugiej (same nielegendarne), zostanie oznaczony literą 1
trzy_grupy_grupowanie <- pokemon.kmeans3$cluster
table(trzy_grupy_grupowanie)
trzy_grupy_grupowanie <- ifelse(trzy_grupy_grupowanie == 2, 1, ifelse(trzy_grupy_grupowanie == 3, 2, trzy_grupy_grupowanie))
table(trzy_grupy_grupowanie)

clust_stats3 <- cluster.stats(d=dist(pokemon), legendary, trzy_grupy_grupowanie)
str(clust_stats3)
clust_stats3$corrected.rand
# Rand = 0.5945763, dla grupowania na 3 grupy, po czym połączenia dwóch grup z nielegendarnymi w jedną grupę, nieprzeskalowane


# Dużo lepszy wynik Rand osiągnięto dla wyniku podziału na 3 grupy i po przekształceniu do 2 grup.
# Jest to odzwierciedlenie wyniku w formie tabeli, na której widać, że w przypadku grupowania na 3 grupy,
# tylko 58 pokemonów zostało zakwalifikowanych błędnie jako legendarne, gdzie w przypadku podziału na 2 grupy,
# aż 359 pokemonów zostało błędnie zakwalifikowanych jako legendarne.

################################################################################################
##### DBSCAN ####################################################################################

# Punkt 3

# Epsilon (eps): Określa promień wokół punktu, w którym należy szukać innych punktów. 
# Punkty wewnątrz tego promienia są uważane za sąsiadów.

# minPts: Minimalna liczba punktów wokół danego punktu, 
# które muszą znajdować się wewnątrz promienia eps, aby został on uznany za centralny punkt.

# Wyznaczenie parametru eps dla algorytmu DBSCAN metodą szukania punktu przegięcie z wykorzystaniem 25% losowo 
# wybranych danych – sprawdzenie dla kilku wartości K
set.seed(2)
sample_data <- sample_frac(pokemon, 0.25)

# 3a
dbscan::kNNdistplot(sample_data, k=5)
abline(h=65, lty="dashed")

dbscan::kNNdistplot(sample_data, k=4)
abline(h=70, lty="dashed")

dbscan::kNNdistplot(sample_data, k=6)
abline(h=68, lty="dashed")


pokemon_dbscan <- dbscan::dbscan(pokemon, eps=65, minPts = 5)
table(pokemon2$Legendary, pokemon_dbscan$cluster)

#         0   1
# FALSE   9 726
# TRUE    9  56

# 18 punktów szumu i 782 punkty zakwalifikowane do jednej grupy

pokemon_dbscan <- dbscan::dbscan(pokemon, eps=70, minPts = 4)
table(pokemon2$Legendary, pokemon_dbscan$cluster)

#         0   1   2
# FALSE   8 727   0
# TRUE    2  59   4

# 10 punktów szumu, 786 puntków do jednej grupy, 4 punkty do drugiej grupy

pokemon_dbscan <- dbscan::dbscan(pokemon, eps=68, minPts = 6)
table(pokemon2$Legendary, pokemon_dbscan$cluster)

#         0   1
# FALSE   8 727
# TRUE    8  57

# 16 punktów szumu, 784 punkty przyporządkowane do jednej grupy

pokemon_dbscan <- dbscan::dbscan(pokemon, eps=35, minPts = 5)
table(pokemon2$Legendary, pokemon_dbscan$cluster)

#       0   1   2   3
# FALSE  95 640   0   0
# TRUE   28  26   5   6

# Tworzenie ramki danych z wynikami klastrów
pokemon_clusters_db <- data.frame(pokemon, Cluster = as.factor(pokemon_dbscan$cluster))

# Wykres punktowy przedstawiający klastry
ggplot(pokemon_clusters_db, aes(x = Total, y = HP, color = Cluster)) +
  geom_point() +
  labs(title = "Wyniki algorytmu DBSCAN na danych Pokemon",
       x = "Total", y = "HP") +
  theme_minimal()

# 3b
# Ze wzgledu na trudności w znalezieniu wartości dających dobre wyniki, postanowiłem sprawdzić
# w formie pętli wartości eps od 20 do 80 ze skokiem co 5 i minPts przyjętym na 5:

df <- data.frame(pokemon)

# Pętla zmieniająca wartość eps od 20 do 80 z krokiem 5
for (eps_value in seq(20, 80, by = 5)) {
  # Wykonanie DBSCAN dla aktualnej wartości eps
  pokemon_dbscan <- dbscan::dbscan(pokemon, eps = eps_value, minPts = 5)
  
  # Tworzenie nowej kolumny w ramce danych i zapis wyników
  col_name <- paste("eps_", eps_value, sep = "")
  df[col_name] <- pokemon_dbscan$cluster 
}


for (col in names(df)) {
  if (grepl("^eps_", col)) {  
    cat("Wyniki dla kolumny:", col, "\n")
    print(table(df[[col]]))  
    cat("\n")
  }
}

# Wyniki do poziomu =< 40 dawały wysoki wynik szumu => 123

# Wyniki dla kolumny: eps_45 

# 0   1   2 
# 83 700  17 

# Wyniki dla kolumny: eps_50 

# 0   1   2 
# 54 723  23 

# Najlepsze rezultaty uzyskano dla eps = 50
# Wartość eps => 55 przyporządkowywały większość punktów do jednej grupy

######
######
# Eskperyment powtórzono dla wartości minPts = 7

df2 <- data.frame(pokemon)

# Pętla zmieniająca wartość eps od 20 do 80 z krokiem 5
for (eps_value in seq(20, 80, by = 5)) {
  # Wykonanie DBSCAN dla aktualnej wartości eps
  pokemon_dbscan <- dbscan::dbscan(pokemon, eps = eps_value, minPts = 7)
  
  # Tworzenie nowej kolumny w ramce danych i zapis wyników
  col_name <- paste("eps_", eps_value, sep = "")
  df2[col_name] <- pokemon_dbscan$cluster 
}


for (col in names(df2)) {
  if (grepl("^eps_", col)) {  
    cat("Wyniki dla kolumny:", col, "\n")
    print(table(df2[[col]]))  
    cat("\n")
  }
}

# Wyniki były bardzo podobne do próby z poprzednim parametrem minPts, 
# Najlepsze rezultaty dawała wartość eps >= 45 i <= 50
# Poniżej 45 występował wysoki szum, powyżej 50 grupowanie do jednej grupy.

####
#### Ostatnią próbę podjęto przy minPts = 3

df3 <- data.frame(pokemon)

# Pętla zmieniająca wartość eps od 20 do 80 z krokiem 5
for (eps_value in seq(20, 80, by = 5)) {
  # Wykonanie DBSCAN dla aktualnej wartości eps
  pokemon_dbscan <- dbscan::dbscan(pokemon, eps = eps_value, minPts = 3)
  
  # Tworzenie nowej kolumny w ramce danych i zapis wyników
  col_name <- paste("eps_", eps_value, sep = "")
  df3[col_name] <- pokemon_dbscan$cluster 
}

for (col in names(df3)) {
  if (grepl("^eps_", col)) {  
    cat("Wyniki dla kolumny:", col, "\n")
    print(table(df3[[col]]))  
    cat("\n")
  }
}

# Wszystkie wyniki tej próby wydają się niezadawaljące

# Na podstawie przeprowadzonych eksperyment w mojej ocenie algorytm DBSCAN nie nadaje się do dobrego 
# grupowania pokemonów na Legendarne lub zwykłe.

# Najlepszą wartość uzyskano dla eps = 50 i minpts = 5, więc dalsze punty do potrzeb pracy zaliczeniowej będą
# realizowane na tych parametrach

pokemon_dbscan <- dbscan::dbscan(pokemon, eps=50, minPts = 5)
table(pokemon2$Legendary, pokemon_dbscan$cluster)

#         0   1   2
# FALSE  37 694   4
# TRUE   17  29  19

# Na wykresie widać, że najbliżej pokemonów określonych jako legendarne, 
# znajduje się grupa przyporządkowana prze algorytm do grupy numer 2.


# Wskaźnik Rand
clust_stats_db <- cluster.stats(d=dist(pokemon), legendary, pokemon_dbscan$cluster)
str(clust_stats_db)
clust_stats_db$corrected.rand
# 0.4205733

# 3c. Wskaźnik Silhouette

# Bez usuwania punktów szumu
silhouette_pokemon <- silhouette(pokemon_dbscan$cluster, dist(pokemon))

# Wyświetlenie wskaźnika Silhouette przed usunięciem punktów szumu
print("Wskaźnik Silhouette przed usunięciem punktów szumu:")
summary(silhouette_pokemon)
# średnia 0.2690

# Wykres:
fviz_silhouette(silhouette_pokemon, palette="jco")

# Indeksy punktów oznaczonych jako szum (klaster 0)
noise_points <- which(pokemon_dbscan$cluster == 0)

# Usunięcie punktów szumu
pokemon_filtered <- pokemon[-noise_points, ]
dbscan_cluster_filtered <- pokemon_dbscan$cluster[-noise_points]

#wskaźnika Silhouette po usunięciu punktów szumu
silhouette_pokemon_filtered <- silhouette(dbscan_cluster_filtered, dist(pokemon_filtered))

# Wyświetlenie wskaźnika Silhouette po usunięciu punktów szumu
print("Wskaźnik Silhouette po usunięciu punktów szumu:")
summary(silhouette_pokemon_filtered)
# średnia 0.4180

# Wykres:
fviz_silhouette(silhouette_pokemon_filtered, palette="jco")

# 3d. Przypisanie poszczególnych rekordów do grup
pokemon_clusters_db <- data.frame(pokemon, Cluster = as.factor(pokemon_dbscan$cluster))

# Wykres punktowy przedstawiający klastry
ggplot(pokemon_clusters_db, aes(x = Total, y = HP, color = Cluster)) +
  geom_point() +
  labs(title = "Wyniki algorytmu DBSCAN na danych Pokemon",
       x = "Total", y = "HP") +
  theme_minimal()

# 3e
# Znalezienie charakterystycznych elementów w grupach DBSCAN
db_cluster_centers <- aggregate(pokemon_clusters_db, by = list(pokemon_dbscan$cluster), FUN = mean)
db_cluster_centers <- subset(db_cluster_centers, select=-Cluster)
db_cluster_centers
# Group.1     Total        HP    Attack   Defense   Sp..Atk   Sp..Def    Speed
# 1       0 550.5000  93.14815 101.18519 105.42593  84.14815  96.92593 69.66667
# 2       1 418.5131  66.31535  75.57538  70.34025  70.13278  68.93084 67.21853
# 3       2 685.6522 105.69565 134.60870 109.78261 130.69565 106.56522 98.30435


######################################################################################
######################################################################################

# Punkt 4. Porównanie wyników uzyskanych dwoma metodami grupowania

# Grupowanie Kmeans
# 2 środki
# Dane po przeskalowaniu
# Rand: 0.007630934
# Average Silhoutte width: 0.306279
# Tabela:
#         1   2
# False 376 359
# True    0  65


# Grupowanie Kmeans
# 3 środki
# Dane ez skalowania
# Rand: 0.594576
# Average Silhoutte width: 0.4335
# Tabela:
#           1   2   3
#   False 288 389  58
#   True    0   0  65


# Grupowanie DBSCAN
# eps = 50, minPts = 5
# 3 grupy
# Dane bez skalowania
# Rand: 0.4205733
# Average Silhoutte width:
#   bez usunięcia szumu: 0.2690
#   z usuniętym szumem: 0.4180

# Tabela: 
#         0     1   2
# FALSE  37   694   4
# TRUE   17   29  


# Podsumowując zadanie grupowania, po przeanalizowaniu ww. wyników, w mojej ocenie najlepsze rezultaty uzyskano dla
# grupowania Kmeans z 3 środkami, bez skalowania danych.
# Grupowanie to osiągnęło najlepsze wyniki we wskaźnikach Rand i Średniej szerkości Silhoutte.
# Po połączeniu grup oznaczonych jako 0 i 1, tylko 58 pokemonów nielegendarnych zostaje błęnie zakwalifikowanych do
# niewłaściwej grupy, przy czym wszystkie 65 Pokemonów Legendarnych, zostaje zakwalifikowane do jednej grupy. 

