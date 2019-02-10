#Załadowanie pliku z danymi o gminach
gminy <- read.csv("gminy.csv", encoding = "UTF-8")


#Utworzenie nowych zmiennych
gminy$Male.pozary.mieszkaniec <- gminy$Male.pozary / gminy$Ludnosc
gminy$Srednie.pozary.mieszkaniec <- gminy$Srednie.pozary / gminy$Ludnosc
gminy$Duze.pozary.mieszkaniec <- gminy$Duze.pozary / gminy$Ludnosc
gminy$Bardzo.duze.pozary.mieszkaniec <- gminy$Bardzo.duze.pozary / gminy$Ludnosc
gminy$Male.zagrozenia.mieszkaniec <- gminy$Male.zagrozenia / gminy$Ludnosc
gminy$Lokalne.zagrozenia.mieszkaniec <- gminy$Lokalne.zagrozenia / gminy$Ludnosc
gminy$Srednie.zagrozenia.mieszkaniec <- gminy$Srednie.zagrozenia / gminy$Ludnosc
gminy$Duze.zagrozenia.mieszkaniec <- gminy$Duze.zagrozenia / gminy$Ludnosc
gminy$Gigantyczne.kleski.zywiolowe.mieszkaniec <- gminy$Gigantyczne.kleski.zywiolowe / gminy$Ludnosc
gminy$Zlosliwe.alarmy.mieszkaniec <- gminy$Zlosliwe.alarmy / gminy$Ludnosc
gminy$W.dobrej.wierze.mieszkaniec <- gminy$W.dobrej.wierze / gminy$Ludnosc
gminy$Z.urzadzen.mieszkaniec <- gminy$Z.urzadzen / gminy$Ludnosc
gminy$Ofiary.smiertelne.SUMA.mieszkaniec <- gminy$Ofiary.smiertelne.SUMA / gminy$Ludnosc
gminy$Ranni.SUMA.mieszkaniec <- gminy$Ranni.SUMA / gminy$Ludnosc
gminy$Silne.wiatry.mieszkaniec <- gminy$Silne.wiatry / gminy$Ludnosc
gminy$Przybory.wód.mieszkaniec <- gminy$Przybory.wód / gminy$Ludnosc
gminy$Opady.sniegu.mieszkaniec <- gminy$Opady.sniegu / gminy$Ludnosc
gminy$Opady.deszczu.mieszkaniec <- gminy$Opady.deszczu / gminy$Ludnosc
gminy$Chemiczne.mieszkaniec <- gminy$Chemiczne / gminy$Ludnosc
gminy$Ekologiczne.mieszkaniec <- gminy$Ekologiczne / gminy$Ludnosc
gminy$Radiologiczne.mieszkaniec <- gminy$Radiologiczne / gminy$Ludnosc
gminy$Infrastruktury.komunalnej.mieszkaniec <- gminy$Infrastruktury.komunalnej / gminy$Ludnosc
gminy$W.transporcie.drogowym.mieszkaniec <- gminy$W.transporcie.drogowym / gminy$Ludnosc
gminy$W.transporcie.kolejowym.mieszkaniec <- gminy$W.transporcie.kolejowym / gminy$Ludnosc
gminy$W.transporcie.lotniczym.mieszkaniec <- gminy$W.transporcie.lotniczym / gminy$Ludnosc
gminy$Na.obszarach.wodnych.mieszkaniec <- gminy$Na.obszarach.wodnych / gminy$Ludnosc
gminy$Medyczne.mieszkaniec <- gminy$Medyczne / gminy$Ludnosc


#Utworzenie ramki danych do analizy metodą PAM
gminy_pam <- subset(gminy, select = c(Gmina, Województwo, Powiat, Typ.gminy,
                                              Powierzchnia.km.2, Ludnosc,
                                              Male.pozary.mieszkaniec, 
                                              Srednie.pozary.mieszkaniec, 
                                              Male.zagrozenia.mieszkaniec,
                                              Lokalne.zagrozenia.mieszkaniec, 
                                              Srednie.zagrozenia.mieszkaniec,
                                              Zlosliwe.alarmy.mieszkaniec, 
                                              W.dobrej.wierze.mieszkaniec,
                                              Z.urzadzen.mieszkaniec, 
                                              Ofiary.smiertelne.SUMA.mieszkaniec,
                                              Ranni.SUMA.mieszkaniec, 
                                              Silne.wiatry.mieszkaniec, 
                                              Przybory.wód.mieszkaniec, 
                                              Opady.sniegu.mieszkaniec, 
                                              Opady.deszczu.mieszkaniec,
                                              Chemiczne.mieszkaniec, 
                                              Infrastruktury.komunalnej.mieszkaniec, 
                                              W.transporcie.drogowym.mieszkaniec, 
                                              Na.obszarach.wodnych.mieszkaniec, 
                                              Medyczne.mieszkaniec))


#Obliczenie odległości Gowera
gower_dist <- daisy(gminy_pam[2:25],
                    metric = "gower",
                    type = list(logratio = 3))


summary(gower_dist)

gower_mat <- as.matrix(gower_dist)


#Wyłonienia najbardziej i najmniej podobnych do siebie par gmin
gminy_pam[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

gminy_pam[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]



#Obliczenie sylwetek dla 2-10 segmentów
sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}


#Graficzna reprezentacja ogólnych wartości sylwetki
plot(1:10, sil_width,
     xlab = "Liczba segmentow",
     ylab = "Dlugosc sylwetki")
lines(1:10, sil_width)


#Sprawdzenie wartości podstawowych statystyk i medoid dla rozwiązania 3-segmentowego
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- gminy_pam %>%
  dplyr::select(-Gmina) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

gminy_pam[pam_fit$medoids, ]


#Stworzenie wykresu rozkładu obserwacji
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = gminy_pam$Gmina)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))