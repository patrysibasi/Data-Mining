# Autor: Patryk Bednarski
# Zbiór danych: zaproponowany w wytycznych, https://archive.ics.uci.edu/dataset/502/online+retail+ii

# Cel eksperymentów:

# Eksperyment 1
# Celem pierwszego eksprerymentu jest wykrycie produktów często kupowanych razem, 
# gdzie produkty grupowane są ze względu na numer transakcji.

# Eksperyment ten pozwoli sprawdzić, czy przy określonych parametrach instnieją produkty, 
# które często spotykany są razem w zamówieniach.

# Przeprowadzając przygotwanie i analizę danych uznałem, że interesować będą mnie reguły wykryte algorytmem Apriori,
# które mają minimalne wsparcie na poziomie 0.01 oraz zaufanie min = 0.04, minimalna długość = 1.

# W praktyce odkryte w ten sposób reguły mogłby być wykorzystane przy:
# - pozycjonowaniu produktów na stronie internetowej: Jeśli ktos ogląda produkt A, produkty B i C wykryte w ramach
#   interesujących reguł mogłby być proponowane jako interesujące dla klienta
# - w sklepie stacjonarnym produkty A, B, C mogłyby stać na półkach blisko siebie
# - tworzenie promocji, zestawów

# Najlepszą regułę można wybrać porównując jej miary jak wsparcie, zaufanie, lift oraz ilość elementów.
# Im wyższe wymienione wskaźniki, tym lepiej.

# Eksperyment 2

# W ramach drugiego eksperymentu chciałem wykryć reguły asocjacyne grupując produkty nie ze względu na numery transakcji,
# a ze względu na numery klientów.

# Za interesujące reguły uznałem te mające wsparcie >= 0.05, zaufanie >= 0.4, minalną długość transakcji 2.
# Wsparcie w przypadku drugiego eksperymentu jest większe, ponieważ zbiór danych transakcji jest mniejszy.

# Reguły te mogłby w praktyce być użyte do tych samych celów, które zostały wyszczególnione we wstępie dla eksperymentu 1.

# ///////////////////////////////////////////////////////////////////////////////////////////

#wczytanie bibliotek
library(arules)
library(readxl)
library(dplyr)

# ustawienie ziarna
set.seed(1234)

# Wczytanie danych
df <- read_excel("online_retail_II.xlsx")

# //////////////////////// Analiza i przygotowanie danych /////////////////////
summary(df)
colnames(df)

typy_danych <- lapply(df, typeof)
print(typy_danych)

object.size(df) # 35891848 bytes

# kolumna Price wydaje się niepotrzebna do odkrywania reguł asocjacyjnych,
# podobnie kolumna InvoiceDate, ponieważ nie odkrywamy reguł sekwencyjnych
df = subset(df, select = -Price)
df = subset(df, select = -InvoiceDate)

# sprawdzanie brakujących wartości
podsumowanie_brakujacych <- colSums(is.na(df))
print(podsumowanie_brakujacych)

# W kolumnie Description brakuje 2928 rekordów, wiersze te zostaną usunięte
df <- df[!is.na(df$Description),]

# zamiana na typ factor kolumny Invoice
df$Invoice = as.factor(df$Invoice)

# Sprawdzenie ile wartości w kolumnie Quantity jest mniejsze niż 1
length(df$Quantity[which(df$Quantity<1)]) # 10499

# Sprawdzenie ile wartości w kolumnie Quantity jest równe 0
length(df$Quantity[which(df$Quantity==0)]) # 0

table(df$Quantity)

wiersz1 <- df[df$Quantity == -67, ]
print(wiersz1)

# W pierwsze kolejności chciałbym odkyrć reguły asoscjacyjne dla produktów zakupionych w ramach jednej transakcji,
# o których informacje znajdują się w kolumnie Description lub StockCode, zawierającej nazwy zakupionych produktów,
# lub ich identyfikator liczbowy.

# W związku z tym usunę z df te rekordy, gdzie w kolumnie Quantity jest
# wartość mniejsza od 0. Chce wziąć pod uwagę jedynie te rekordy, gdzie
# ilość produktu sprzedanego klientowi ma wartość co najmniej 1, czy produkt
# został zakupiony.

# Przykładowo, do zmiennej "wiersz1" zapisano wiersze z df, gdzie wartość w kolumnie
# Quantity wynosi -67, w kolumnie Description widnieją tam nazwy "demage" lub "missing",
# czyli znaczy, że wpisano zamówienie z towarem uszkodzonym lub brakującym, a
# nie nazwami produktów, które mogą być interesujące do wykrywania reguł asocjacyjnych

df <- df[df$Quantity >= 1, ]

# Kiedy wiem już, że wszystkie wiersze mają wartości dodatnie w kolumnie Quantity,
# mogę usunąć kolumnę Quantity dla potrzeb pierwszego eksperymentu, gdzie chcę poznać
# jakie produkty były często kupowane razem, bez względu na to w jakiej ilości
# były kupowane.

df = subset(df, select = -Quantity)

# Sprawdzanie unikatowych wartości dla kolumn Invoice i Customer ID
length(unique(df$Invoice)) # 21003
length(unique(df$`Customer ID`)) # 4315

# W zbiorze danych znajduje się łącznie 21003 unikatowych numerów faktur w kolumnie Invoice, 
# czyli tyle unikatowych transakcji przeprowadzonych łącznie dla 4315 unikatowych klientów

# Sprawdzenie duplikatów
duplikaty <- which(duplicated(df) == TRUE)
length(duplikaty) # 13227

# Po losowej weryfikacji kilku duplikatów jeszcze w oryginalnym zbiorze danych zauważyć
# można, że podane duplikaty mają taki sam numer w kolumnach Invoice oraz Customer ID, 
# a także taką samą datę i godzinę transakcji.

# Usunięcie duplikatów
df <- unique(df)

# Każdy produkt ma unikatowe ID w kolumnie StockCode. Nie ma więc potrzeby 
# trzymać 2 kolumn z informacją dotyczącą opisu produktu w formie tekstowej i formie
# liczbowej.

# Dla tego celu stworzono nową data frame z listą produktów i przyporządkowanym im ID.
products_id = df[, c("StockCode", "Description")]
products_id <- unique(products_id)

# Teraz można usunąć z głównej df kolumnę Description
df = subset(df, select = -Description)

# Zmiana typu danych w kolumnie StockCode na factor
df$StockCode = as.factor(df$StockCode)

# W pierwszej kolejności chcę wykryć reguły asocjacyjne dla produktów często
# kupowanych razem na jednej transakcji. Do tego celu potrzebuje tylko kolumny
# Invoice i StockCode, czyli numeru transakcji i numeru produktu.

# Mogę więc usunąć kolumnę Customer ID i Country
df_exp1 = df[, c("Invoice", "StockCode")]


# //////////////////// Wyznaczanie reguł asocjacynych i zbiorów częstych ///////////////////////

# Zmiana z df na postać transakcyjną 

# Grupowanie ID produktów wg kolumny Invoice:
transakcje <- split(df_exp1$StockCode, df_exp1$Invoice)

# Przekształcenie do postaci transakcyjnejdla pakietu arules
tr_arules <- as(transakcje, "transactions")


# Sprawdzenie wsparcia elementów
freq = itemFrequency(tr_arules, type = "relative")
str(freq)

summary(freq)
print(freq)

# Sortowanie wg. wsparcia względnego
freq = sort(freq, decreasing= TRUE)


# Wyznaczenie liczby częstych elementów dla wsparcia w przedziale 0.01 do 0.2 ze skokiem co 0.01
sup = seq(0.01, 0.1, 0.01)
nbFSet <- vector(mode= 'integer', length = length(sup))
for(i in 1:length(sup)) {
  nbFSet[i] <- length(freq[freq>= sup[i]])
}

res <- data.frame(sup, nbFSet)

# Po sprawdzeniu elementów dla wsparcia w określonym powyżej zakresie zauważyć można, wyraźny spadek,
# po przekroczeniu wartości wsparcia 0.02
# wsparciem 0.01 dysponuje 561 elementów
# wsparciem 0.02 dysponuje już tylko 178 elementów

#wykres obrazuje powyższą tendencję
library(ggplot2)
qplot(sup, nbFSet, geom=c("point", "line"), xlab="Wsparcie", ylab="Liczba częstych elementów")+theme_bw()

#wykrywanie zbiorów częstych - algorytm apirori
aParam  = new("APparameter", "target" = "frequent itemsets", "support"=0.01, "minlen"= 1, maxtime = 20)
zbiory_czeste <-apriori(tr_arules,aParam)

summary(zbiory_czeste) # wykryto 880 zbiorów częstych o zadanych parametrach

# Wyświetlenie 15 najczęstszych wzorów sortując po wsparciu
inspect(head(sort(zbiory_czeste, by="support"),15))

# Top 5 zbiorów częstych

#       items    support    count
# [1]  {85123A} 0.12677689   3282 
# [2]  {22423}  0.07802843   2020 
# [3]  {85099B} 0.07601978   1968 
# [4]  {21212}  0.07157756   1853 
# [5]  {21232}  0.06334981   1640 

# Najczęstszym elementem w zbiorach jest produkt o numerze 85123A, który posiada wsparcie = 0.127,
# co oznacza, że na wszystkie 25888 transakcje, wystąpił łącznie 3282
# kolejne 3 produkty posiadają wsparcie około 0.07 i wsytępują w zbiorze w przedziale 1853 - 2020 razy


# /////////////////////////////////////////////////////////////////////


# wyznaczenie reguł asocjacyjnych
# Support = 0.01 został wybrany na podstawie wcześniej dokonanych obliczeń, przy wykrywaniu liczby elementów
# częstych dla różnych wartości wsparcia.

# Algorytm Apriori
aParam_r  = new("APparameter", "target" = "rules", "support"=0.01, "confidence" = 0.5, "minlen"= 1, maxtime = 20)

reguly <- apriori(tr_arules, aParam_r)
inspect(sort(reguly, by="support"))

summary(reguly) # 163 reguły
# 110 reguł 2 elementowych
# 53 reguły 3 elementowe

# Top 5

#         lhs                 rhs      support    confidence   coverage     lift      count
# [1]   {21231}          => {21232}  0.03074784   0.7952048  0.03866656  12.552599     796  
# [2]   {21733}          => {85123A} 0.02978214   0.7232645  0.04117738   5.705019     771  
# [3]   {84991}          => {21212}  0.02978214   0.5770958  0.05160692   8.062524     771  
# [4]   {21977}          => {21212}  0.02727132   0.6112554  0.04461527   8.539763     706  
# [5]   {85099F}         => {85099B} 0.02561032   0.6375000  0.04017305   8.385976     663 


# Wykryto 163 reguły dla zadanych parametrów wsparcie = 0.01, zaufanie = 0.5
# Największe wsparcie dla reguły to 0.03, z zaufaniem 0.795
# Najmniejsze wsparcie reguły wyniosło 0.01, z zaufaniem 0.58

# Zmiana wartości zaufania na 0.3

aParam_r2 = new("APparameter", "target" = "rules", "support"=0.01, "confidence" = 0.3, "minlen"= 1, maxtime = 20)

reguly2 <- apriori(tr_arules, aParam_r2)
inspect(sort(reguly2, by="support"))

summary(reguly2) # 417 reguł
# 345 reguł 2 elementowych
# 72 reguły 3 elementowe

# Top 5

#         lhs               rhs      support    confidence    coverage    lift      count
# [1]   {21231}        => {21232}  0.03074784    0.7952048  0.03866656  12.552599    796  
# [2]   {21232}        => {21231}  0.03074784    0.4853659  0.06334981  12.552599    796  
# [3]   {21733}        => {85123A} 0.02978214    0.7232645  0.04117738   5.705019    771  
# [4]   {84991}        => {21212}  0.02978214    0.5770958  0.05160692   8.062524    771  
# [5]   {21212}        => {84991}  0.02978214    0.4160820  0.07157756   8.062524    771

# Zmiana wartości zaufania na 0.4

aParam_r3 = new("APparameter", "target" = "rules", "support"=0.01, "confidence" = 0.4, "minlen"= 1, maxtime = 20)

reguly3 <- apriori(tr_arules, aParam_r3)
inspect(sort(reguly3, by="support"))

summary(reguly3) # 276 reguł
# 207 reguł 2 elementowych
# 69 reguł 3 elementowych

# Top 5

#         lhs               rhs      support    confidence    coverage    lift      count
# [1]   {21231}        => {21232}  0.03074784   0.7952048   0.03866656  12.552599    796  
# [2]   {21232}        => {21231}  0.03074784   0.4853659   0.06334981  12.552599    796  
# [3]   {21733}        => {85123A} 0.02978214   0.7232645   0.04117738   5.705019    771  
# [4]   {84991}        => {21212}  0.02978214   0.5770958   0.05160692   8.062524    771  
# [5]   {21212}        => {84991}  0.02978214   0.4160820   0.07157756   8.062524    771  


# ////////////////// Podsumowanie //////////////////////////
# dla wsparcia minimalnego równego 0.01 i odpowiednio dla:
# zaufania = 0.5, liczba reguł = 163 (0,65% wszystkkich transakcji)
# zaufania = 0.4, liczba reguł = 276 (1,07% wszystkich transakcji)
# zaufania = 0.3, liczba reguł = 417 (1,61% wszystkich transakcji)
# dla wszystkich powyższych reguł określiłem minimalną długość na 1

# Dla powyższego zestawienia zdecydowałem się wybrać parametr zaudania 0.4, ze względu na liczbę reguł,
# na poziomie 276 reguł, co stanowi 1 % procenta wszystkich transakcji i w mojej ocenie było dobrą liczbą
# aby znaleźć najbardziej interesujące reguły

# Zebranie reguł do ramki danych
df_reguly <- DATAFRAME(reguly3, 
                       separate = TRUE, 
                       setStart = '', 
                       itemSep = ', ', 
                       setEnd = '')

summary(df_reguly)

# Top 5 najczęsciej występujących elementów będących w poprzedniku:

# LHS   
# 21975  :  5     
# 82483  :  4    
# 20728  :  4     
# 20726  :  4     
# 22384  :  4     
# 20723  :  4

# Top 5 najczęsciej występujących elementów będących w następniku:

# RHS              
# 85099B : 18     
# 85123A : 17    
# 21212  : 15     
# 84991  : 13     
# 20725  : 13    
# 21977  : 10   

# 20 reguł asocjacyjnych z najwyższą miarą lift 

top20_lift <- df_reguly[order(df_reguly$lift, decreasing = TRUE), ]
top20_lift <- head(top20_lift, 20)
print(top20_lift)

# Top 5

# NP.           LHS    RHS    support   confidence   coverage     lift    count
# 13           22748  22745 0.01058405  0.8011696   0.01321075  65.22226   274
# 14           22745  22748 0.01058405  0.8616352   0.01228368  65.22226   274
# 33           22697  22699 0.01139524  0.7783641   0.01463999  51.01339   295
# 32           22699  22697 0.01139524  0.7468354   0.01525803  51.01339   295
# 6            21124  21122 0.01189740  0.7777778   0.01529666  42.47914   308

# Miara lift pokazuje, jak bardzo zaufanie reguły jest większe od zaufanie wynikającego z
# prawdopodobieństwa występowania w bazie następnika reguły.

# lift > 1 oznacza, że wystąpienie poprzednika reguły w transakcji zwiększa prawdopodobieństwo
# pojawienia się w tej transakcji następnika reguły oraz oznacza silniejszą zależność. 

# Im większa wartość "lift", tym silniejsza zależność między zdarzeniami.

# Na dwóch pierwszych pozycjach występują elementy {22748} i {22745}
# Mają one, zgodnie z definicją, taką samą wartość miary lift.
# Obie reguły wykazują także duże zaufanie, będące powyżej wartości 0.8.
# Oznacza to, że te dwa elementy często występują w zbiorze razem.

# Podobnie sytuacja wygląda dla następnych pozycji.
# Na miejscu 3 i 4 mamy reguły z tych samych elementów:
# {22697} -> {22699} oraz {22699} -> {22697}
# Mają one wysoką wartość lift, czyli są od siebie silnie zależne pozytywnie (powyżej wartości 1)
# Mają także wysokie zaufanie powyżej 0.7.

# Podobnie kształtuje się sytuacja na kolejnych miejscach w zbiorze.

# /////////////////////////////////////////////////////////////////////////////////////////

# 20 reguł asocjacyjnych z największą wartością zaufania

top20_conf <- df_reguly[order(df_reguly$confidence, decreasing = TRUE), ]
top20_conf <- head(top20_conf, 20)
print(top20_conf)
 
# NP.            LHS     RHS      support    confidence    coverage      lift      count
# 14  |         22745   22748   0.01058405   0.8616352   0.01228368   65.222259    274
# 269 | 21231, 85123A   21232   0.01097033   0.8452381   0.01297899   13.342393    284
# 240 |  20723, 22356   20724   0.01162701   0.8292011   0.01402194   20.760501    301
# 13  |         22748   22745   0.01058405   0.8011696   0.01321075   65.222259    274
# 193 |         21231   21232   0.03074784   0.7952048   0.03866656   12.552599    796


# Na pierwszym miejscu znajduje się wymieniona wcześniej reguła o wysokiej wartości lift,
# czyli {22745} -> {22748}. Posiada ona wysokie zaufanie na poziomie 0.86
# Na drugim i trzecim miejscu pojawiają się reguły 3 elementowe, co oznacza, że jeśli klient
# kupił określone 2 produkty, to z wysokim prawdopodobieństwem kupi także konkretny 3 produkt.
# Są to reguły: {21231, 85123A} -> {21232} oraz {20723, 22356} -> {20724}
# Są to ciekawe reguły zawierające już 3 elementy, dysponujące przy tym wysokim zaufaniem na poziomie
# powyżej wartości 0.8

# Wszystkie wyszczególnione top 20 reguł posiadają zaufanie powyżej 0.7, co wskazuje na ich częste występowanie
# razem w transakcjach.

# //////////////////////////////////////////////////////////////////////////////////////////////

# 20 reguł asocjacyjnych o największej wartości wsparcia względnego
# Wsparcie względne określa częstośc występowania reguły w bazie i jest określone w kolumnie support
# jest to liczba transkcji w bazie podzielone przez wszystkie transakcje, czyli liczba z kolumny count podzielona
# przez wszystkie transakcje

top20_sup <- df_reguly[order(df_reguly$support, decreasing = TRUE), ]
top20_sup <- head(top20_sup, 20)
print(top20_sup)

# NP.   LHS      RHS     support    confidence    coverage      lift       count
# 193  21231    21232   0.03074784  0.7952048   0.03866656    12.552599     796
# 194  21232    21231   0.03074784  0.4853659   0.06334981    12.552599     796
# 114  21733    85123A  0.02978214  0.7232645   0.04117738    5.705019      771
# 206  84991    21212   0.02978214  0.5770958   0.05160692    8.062524      771
# 207  21212    84991   0.02978214  0.4160820   0.07157756    8.062524      771

# 2 reguly o największej wartości wsparcia względnego to reguły zaiwerające te same elementy, czyli reguły:
# {21231} -> {21232} oraz {21232} -> {21231}, posiadają one jako jedyne wsparcie względne powyżej 0.03.

# Na liście top 20 transacji o największej wartości wsparcia nie występuje żaden element 3 elementowy,
# co oznacza, że żadna z reguł 3 elementowych nie występuje w zbiorze bardzo często.

# Połączone reguły z top 20 zaufania, wsparcia i wartości lift
combined_df <- rbind(top20_lift, top20_conf, top20_sup)
combined_df <- unique(combined_df)

# Chciałem wyświetlić wszystkie transakcje 3 elemntowe zebrane z 3 wcześniej stworzonych top20 df(conf, sup i lift)
sorted_combined_df <- combined_df[order(combined_df$LHS, decreasing = TRUE), ]
print(head(sorted_combined_df, 10))

# NP.         LHS        RHS      support     confidence   coverage      lift     count
# 271   22355, 22356    20724   0.01263133    0.7500000   0.01684178  18.777563     327
# 269  21231, 85123A    21232   0.01097033    0.8452381   0.01297899  13.342393     284
# 263   21977, 84992    21212   0.01143387    0.7290640   0.01568294  10.185650     296
# 254   20719, 22356    20724   0.01131799    0.7751323   0.01460136  19.406793     293
# 252   21213, 84991    21212   0.01100896    0.7480315   0.01471724  10.450642     285
# 240   20723, 22356    20724   0.01162701    0.8292011   0.01402194  20.760501     301
# 234  22386, 85099F    85099B  0.01189740    0.7350835   0.01618511  9.669635      308
# 225   20726, 22384    20725   0.01050680    0.7351351   0.01429234  12.365938     272
# 214 85099C, 85099F    85099B  0.01073857    0.7658402   0.01402194  10.074223     278
# 210 84997B, 84997D    84997C  0.01027503    0.7800587   0.01317213  32.105180     266

# Wszystkie wymienione transakcje 3 elementowe mają wysokie wskaźniki zaufania (min 0.73) oraz wysokie wartości lift (min 9.6).

# ////////////////////////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////////////////////
# ///////////////////////////////////////////////////////////////////////////////////////////////


# Część druga
# W części drugiej spróbuję odkryć reguły związane z kupowaniem poszczególnych produktów przez konkretnych klientów.
# Do tego celu wykorzystam jedynie kolumny Customer ID oraz StockID z oryginalnej ramki danych.
# Dane będą pogrupowane względem kolumny Customer ID.


names(df)[names(df) == "Customer ID"] <- "CustomerID"

df_exp2 = subset(df, select = -Country)
df_exp2 = subset(df_exp2, select = -Invoice)
df_exp2 = df_exp2[, c('CustomerID', "StockCode")]

# Usunięcie wartości brakujących
sum(is.na(df_exp2$CustomerID))
df_exp2 <- na.omit(df_exp2)

# Przypisanie do wartości transakcyjnej
transakcje_exp2 <- split(df_exp2$StockCode, df_exp2$CustomerID)
tr_arules_exp2 <- as(transakcje_exp2, "transactions")

# Przypisanie parametrów.
aParam_exp2 = new("APparameter", "target" = "rules", "support"=0.05, "confidence" = 0.4, "minlen"= 2, maxtime = 20)

# minlen = 2, ponieważ nie chcę uwzględniać reguł z pustą lewą stroną
# support wybrany metodą prób

options(max.print = 5000)

reguly_exp2_1 <- apriori(tr_arules_exp2, aParam_exp2)
inspect(reguly_exp2_1)

summary(reguly_exp2_1) # 181 reguł
# 157 reguł 2 elementowych
# 24 reguły 3 elementowe
# max support = 0.1
# max conf = 0.85
# min lift > 1, czyli wszystkie elementy są zależne pozytywnie
# wspracie bezwzględne w przedziale 216 - 466

df_reguly_exp2 <- DATAFRAME(reguly_exp2_1, 
                       separate = TRUE, 
                       setStart = '', 
                       itemSep = ', ', 
                       setEnd = '')

# Top 20 reguł asocjacyjnych pod względem miary lift
top20_lift_exp2 <- df_reguly_exp2[order(df_reguly_exp2$lift, decreasing = TRUE), ]
top20_lift_exp2 <- head(top20_lift_exp2, 20)
print(top20_lift_exp2)

# Top 5
# NP.    LHS     RHS      support     confidence   coverage      lift       count
# 5     21124   21122   0.05563282    0.8391608   0.06629578    11.715663     240
# 6     21122   21124   0.05563282    0.7766990   0.07162726    11.715663     240
# 3     82581   82580   0.05146036    0.8538462   0.06026889    11.583309     222
# 4     82580   82581   0.05146036    0.6981132   0.07371349    11.583309     222
# 13    84997D  84997C  0.05980529    0.8431373   0.07093185    11.157344     258

# 6 pierwszych reguł z listy ma wartość lift powyżej 11, reguły te mają także wysoką wartość zaufania > 0.7
# W top 20 pod względem lift pojawiły się takze 4 transakcje 3-elementowe, mające wysokie wartości zaufania, 
# czyli często występujące razem

#/////////////////////////////////////////////

# Top 20 reguł pod względem miary zaufania
top20_conf_exp2 <- df_reguly_exp2[order(df_reguly_exp2$confidence, decreasing = TRUE), ]
top20_conf_exp2 <- head(top20_conf_exp2, 20)
print(top20_conf_exp2)

# Top 5
# NP.   LHS       RHS    support      confidence   coverage      lift        count
# 3     82581   82580   0.05146036    0.8538462   0.06026889    11.583309     222
# 143   21733   85123A  0.10802040    0.8488160   0.12726008    3.209283      466
# 36    21231   21232   0.07904497    0.8482587   0.09318498    6.593492      341
# 13    84997D  84997C  0.05980529    0.8431373   0.07093185    11.157344     258
# 5     21124   21122   0.05563282    0.8391608   0.06629578    11.715663     240

# W top 20 reguł asocjacyjnych pod względem zaufania znalazło się aż 7 zbiorów 3 elementowych - są to reguły warte 
# zanotowania
# Minimalne zaufanie w top 20 wyniosło 0.74, co jest wysoką wartością.

# ///////////////////////////////////////////

# Top 20 reguł pod względem miary wsparcia względnego
top20_sup_exp2 <- df_reguly_exp2[order(df_reguly_exp2$support, decreasing = TRUE), ]
top20_sup_exp2 <- head(top20_sup_exp2, 20)
print(top20_sup_exp2)

# Top 5
# NP.    LHS     RHS        support     confidence   coverage     lift    count
# 143   21733   85123A    0.10802040    0.8488160   0.12726008  3.209283   466
# 144   85123A  21733     0.10802040    0.4084137   0.26448771  3.209283   466
# 154   84991   21212     0.09156236    0.6583333   0.13908206  3.683593   395
# 155   21212   84991     0.09156236    0.5123217   0.17872045  3.683593   395
# 121   21977   21212     0.08437645    0.7027027   0.12007418  3.931854   364

# Reguły w top 20 pod wzglem wsparcia mieszczą się w zakresie wartości dla tej miary między 0.074 a 0.11.
# Znalazly się tu tylko reguły 2 elementowe.
# Wszystkie wreguły w tym zestawieniu posiadają lift > 1.

# Połączona ramka danych ze wszystkich top20(lift, conf, sup)

combined_df_exp2 <- rbind(top20_lift_exp2, top20_conf_exp2, top20_sup_exp2)
combined_df_exp2 <- unique(combined_df_exp2)

# W ramce danych znajduje się 10 zbiorów 3-elementowych powstałych z unikatowych wartości z połączonych 3 ramek danych top20
sorted_combined_df_exp2 <- combined_df_exp2[order(combined_df_exp2$LHS, decreasing = TRUE), ]
print(head(sorted_combined_df_exp2, 10))

# NP.         LHS        RHS      support     confidence    coverage     lift     count
# 179   20727, 22382    20725   0.05308299    0.8006993   0.06629578    5.935080   229
# 176   22382, 22384    20725   0.05493741    0.8061224   0.06815021    5.975279   237
# 173   20727, 22384    20725   0.05030134    0.8188679   0.06142791    6.069753   217
# 170   21977, 84991    21212   0.05563282    0.7523511   0.07394529    4.209653   240
# 164   20728, 22382    20725   0.05099675    0.7971014   0.06397775    5.908412   220
# 162   20725, 20728    22384   0.05006954    0.7105263   0.07046824    7.014212   216
# 161   20728, 22384    20725   0.05006954    0.7883212   0.06351414    5.843329   216
# 160   20725, 22382    22383   0.05540102    0.7113095   0.07788595    7.005912   239
# 178   20725, 22382    22384   0.05493741    0.7053571   0.07788595    6.963182   237
# 159   20725, 22383    22382   0.05540102    0.8356643   0.06629578    6.893032   239


# ////////////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////////////

# Wnioski

# W eksperymencie pierwszym wybrano reguły asocjacyjne metodą Apriori
# Wartość minimalnego wsparcia wybrano na bazie zliczenia potencjalnych zbiorów częstych dla wartości wsparcia od 0.01 do 0.2.
# Na bazie przeprowadzonej analizy wybrano wsparcie = 0.01.
# Mniejsza wartość wsparcia pozwoliłaby na odkrycie większej ilości reguł, większa wartość ograniczyłaby liczbę reguł.

# Wybrano wartość minimalnego zaufania na poziomie 0.4. Również przeprowadzono analizę. Wartość 0.4 pozwoliła na uzyskanie
# wystarczającej liczby reguł asocjacyjnych. Zmniejszenie wartości minimalnego wsparcia pozwalało na odkrycie większej ilości reguł, 
# jednak o mniejszej wartości merytorycznej.

# Do celów dalszej analizy najciekawszych reguł wybrano top 20 reguł pod względem miar: lift, support i confidence,
# w ten sposób powstały 3 ramki danych z top 20 z każdej z tych miar.

# Następnie połączono te 3 ramki danych uzyskując łącznie 51 unikatowych reguł mających najwyższe wartości w jednej z tych 3 miar.
# Z tych 51 reguł, na potrzeby niniejszej pracy zaliczeniowej, szczególną uwagę zwrócono na zbiory 3 elementowe:

#         LHS        RHS       support     confidence    coverage      lift      count
# 22355, 22356      20724   0.01263133    0.7500000   0.01684178  18.777563     327
# 22386, 85099F     85099B  0.01189740    0.7350835   0.01618511  9.669635      308
# 20723, 22356      20724   0.01162701    0.8292011   0.01402194  20.760501     301
# 21977, 84992      21212   0.01143387    0.7290640   0.01568294  10.185650     296
# 20719, 22356      20724   0.01131799    0.7751323   0.01460136  19.406793     293
# 21213, 84991      21212   0.01100896    0.7480315   0.01471724  10.450642     285
# 21231, 85123A     21232   0.01097033    0.8452381   0.01297899  13.342393     284
# 85099C, 85099F    85099B  0.01073857    0.7658402   0.01402194  10.074223     278
# 20726, 22384      20725   0.01050680    0.7351351   0.01429234  12.365938     272
# 84997B, 84997D    84997C  0.01027503    0.7800587   0.01317213  32.105180     266

# Wszystkie wyszczególniene reguły asocjacyjne mają wysokie wzkaźniki miary lift > 9.66, co wskazuje na silną pozytywną zależność,
# Reguły mają także wysoki wskaźnik zaufania (min 0.73), co wskazuje na częste występowanie razem podanych zbiorów.

# Elementy często występuje w zaprezentowanym zbiorze:
# {20724} - 3 razy występuje w następniku
# {22356} - 3 razy w poprzedniku

# Elementy ze zbioru o numerach:
# {85099 X} z końcówkami F, B, C są odmianami tego samego produktu o nazwie "Jumbo Bag"
# {84997 X} z końcówkami B, D, C są odmianami tego samego produktu o nazwie "3 PIECE MINI DOTS CUTLERY SET"

# Eksperyment ten pokazał jakie produkty są często kupowanej razem w ramach jednej transakcji.

# W 327 transakcjach wystąpiła przykładowo reguła, że kiedy kupiono razem produkty {22355}, {22356},
# to kupiono również {20724}.

# W przypadku tej reguły {22355, 22356} -> {20724}, zaufanie na poziomie 0.75 mówi, 
# że w 75% przypadków kiedy kupowano elementy {22355, 22356} kupowano razem elementy {22355, 22356, 20724}.

# Lift dla tej reguły na poziomie 18.77, który jest > 1, pokazuje, że wystąpienie elementów {22355, 22356} w zbiorze transakcji,
# zwiększa prawdopodobieństwo pojawienia się elementu {20724}.

# Analogiczne tłumaczenia móżna by pokazać dla kolejnych reguł z listy. 

# //////////////////////////////////////////////////////////////////////////

# Eksperyment 2

# W eksperymencie drugim wykorzystano również algorytm Apriori.
# Zadano parametry support = 0.05, confidence = 0.4, minlen= 2.
# Wsparcie wybrano na podstawie kilku prób z tą wartością, zmniejszenie progu minimalnego wsparcia wspływało na zwiększenie
# liczby reguł, jednak kosztem ich wartości merytorycznej.
# Podobnie z wartością wsparcia, uznałem, że wartość 40% będzie wystarczająca na odkrycie ciekawych reguł.
# Minimalna wartość 2, aby pominąć zbiory puste w poprzedniku.

# Analogicznie do poprzedniego eksperymentu wyszczególniono top 20 reguł pod względem 3 miar: lift, conf i support,
# a następnie stworzono jedną ramkę danych z  unikatowymi regułami w liczbie 50.

# Ponownie uznałem, że najbardziej warte analizy będą reguły 3-elementowe.

#         LHS      RHS    support       confidence    coverage     lift     count
# 21977, 84991    21212   0.05563282    0.7523511   0.07394529    4.209653   240
# 20725, 22382    22383   0.05540102    0.7113095   0.07788595    7.005912   239
# 20725, 22383    22382   0.05540102    0.8356643   0.06629578    6.893032   239
# 22382, 22384    20725   0.05493741    0.8061224   0.06815021    5.975279   237
# 20725, 22382    22384   0.05493741    0.7053571   0.07788595    6.963182   237
# 20727, 22382    20725   0.05308299    0.8006993   0.06629578    5.935080   229
# 20728, 22382    20725   0.05099675    0.7971014   0.06397775    5.908412   220
# 20727, 22384    20725   0.05030134    0.8188679   0.06142791    6.069753   217
# 20725, 20728    22384   0.05006954    0.7105263   0.07046824    7.014212   216
# 20728, 22384    20725   0.05006954    0.7883212   0.06351414    5.843329   216

# Każda z powyżej wyświetlonych reguł ma wartość podniesienia > 1, oznacza to ich silną, pozytywną zależność.
# Reguły wykazują wysokie zaufanie na poziomie min 0.71, czyli produkty te występują w zbiorze często razem, są więc
# warte uwagi.

# Biorąc pod analizę regułę {21977, 84991} -> {21212}:

# Jej wparcie mówi, że reguła taka pojawiła się w 240 transakcjach, co stanowi 5,5 % wszystkich transakcji w zbiorze.

# Zaufanie mówi, że w 75% przypadków, kiedy kupowano {21977, 84991}, kupiono także {21212}.

# Lift na poziomie 4.2 mówi, że elementy są zależne pozytywne oraz że pojawienie się w zbiorze elementu {21911, 84991}, 
# zwiększa prawdopodobieństwo pojawienia się {21212}. Można to obliczyć ze wzoru ( (PXY) / (PX) ) / P(Y), gdzie
# X = {21977, 84991}, a Y to {21212}.

# Często powtarzającymi się elementami w wymienionych regułach są:
# {20725} - występuje 5 razy w następniku i 4 razy w poprzedniku, jest to też 6 najczęsciej występujący element w zbiorze transakcji.
# {22382} - 1 raz w następniku i 5 razy w poprzedniku, jest to 30 najcześciej występujący zbiór częsty.
# {22384} - 2 razy w następniku i 3 razy w poprzedniku, powyżej 30 pozycji w najczęstych zbiorach częstyc.h

# Zbiory występujące w regułach 3-elementowych tylko raz:
# {21977}, {84991}, {21212}

# Wykryte reguły możnaby użyć do wymienionych na początku pracy celów.

# Dalszej analizie należałoby poddać reguły dwuelementowe oraz wyłonić z nich kandydatów o szczególnie wysokich wartościach
# wsparcia lub zaufania lub miary podniesienia.

# Dziękuję.



