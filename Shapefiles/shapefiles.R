#Załadowanie pliku z danymi o powiatach
powiaty <- read.csv("powiaty.csv", encoding = "UTF-8")

#
#--- POWIATY - ZBIÓR NIEZNORMALIZOWANY ---
#

#Dodanie nowych zmiennych do ramki danych o powiatach
powiaty$Pozary.mieszkaniec <- powiaty$Pozary.SUMA / powiaty$Ludnosc
powiaty$Zagrozenia.mieszkaniec <- powiaty$Zagrozenia.SUMA / powiaty$Ludnosc
powiaty$Smierc.mieszkaniec <- powiaty$Ofiary.smiertelne.SUMA / powiaty$Ludnosc
powiaty$Ranni.mieszkaniec <- powiaty$Ranni.SUMA / powiaty$Ludnosc
powiaty$Transport.drogowy.mieszkaniec <- powiaty$W.transporcie.drogowym / powiaty$Ludnosc
powiaty$Wiatry.mieszkaniec <- powiaty$Silne.wiatry / powiaty$Ludnosc
powiaty$Uszczerbek.mieszkaniec <- (powiaty$Ranni.SUMA + powiaty$Ofiary.smiertelne.SUMA) / powiaty$Ludnosc

#Wgranie mapy (pliku .shp) z powiatami
pl02 <- readOGR("gadm36_POL_2.shp", encoding = "UTF-8")

#Dodanie danych o powiatach do pliku .shp
pl02@data <- left_join(pl02@data, powiaty, by = c("NAME_2" = "Powiat"))

#Wygenerowanie map
qtm(pl02, "Pozary.mieszkaniec", fill.palette = "Reds")
qtm(pl02, "Zagrozenia.mieszkaniec", fill.palette = "Oranges")
qtm(pl02, "Smierc.mieszkaniec", fill.palette = "Purples")
qtm(pl02, "Ranni.mieszkaniec", fill.palette = "GnBu")
qtm(pl02, "Transport.drogowy.mieszkaniec", fill.palette = "Greens")
qtm(pl02, "Wiatry.mieszkaniec", fill.palette = "Blues")
qtm(pl02, "Uszczerbek.mieszkaniec", fill.palette = "Greys")

#
#--- POWIATY - ZBIÓR Z PRZELICZENIEM NA 10 000 MIESZKAŃCÓW ---
#

#Załadowanie pliku z danymi o powiatach
powiaty2 <- read.csv("powiaty.csv", encoding = "UTF-8")

#Dodanie nowych zmiennych do ramki danych o powiatach
powiaty2$Pozary.mieszkaniec <- (powiaty2$Pozary.SUMA / powiaty2$Ludnosc)*1000
powiaty2$Zagrozenia.mieszkaniec <- (powiaty2$Zagrozenia.SUMA / powiaty2$Ludnosc)*1000
powiaty2$Smierc.mieszkaniec <- (powiaty2$Ofiary.smiertelne.SUMA / powiaty2$Ludnosc)*1000
powiaty2$Ranni.mieszkaniec <- (powiaty2$Ranni.SUMA / powiaty2$Ludnosc)*1000
powiaty2$Transport.drogowy.mieszkaniec <- (powiaty2$W.transporcie.drogowym / powiaty2$Ludnosc)*1000
powiaty2$Wiatry.mieszkaniec <- (powiaty2$Silne.wiatry / powiaty2$Ludnosc)*1000
powiaty2$Uszczerbek.mieszkaniec <- ((powiaty2$Ranni.SUMA + powiaty2$Ofiary.smiertelne.SUMA) / powiaty2$Ludnosc)*1000

#Wgranie mapy (pliku .shp) z powiatami
pl02_2 <- readOGR("gadm36_POL_2.shp", encoding = "UTF-8")

#Dodanie danych o powiatach do pliku .shp
pl02_2@data <- left_join(pl02_2@data, powiaty2, by = c("NAME_2" = "Powiat"))

#Wygenerowanie map
qtm(pl02_2, "Pozary.mieszkaniec", fill.palette = "Reds")
qtm(pl02_2, "Zagrozenia.mieszkaniec", fill.palette = "Oranges")
qtm(pl02_2, "Smierc.mieszkaniec", fill.palette = "Purples")
qtm(pl02_2, "Ranni.mieszkaniec", fill.palette = "GnBu")
qtm(pl02_2, "Transport.drogowy.mieszkaniec", fill.palette = "Greens")
qtm(pl02_2, "Wiatry.mieszkaniec", fill.palette = "Blues")
qtm(pl02_2, "Uszczerbek.mieszkaniec", fill.palette = "Greys")


#
#--- POWIATY - ZBIÓR ZNORMALIZOWANY ---
#

#Utworzenie znormalizowanego zbioru danych
powiaty_opisowe <- powiaty[1:3]
powiaty_norm_values <- normalise(powiaty[4:61], lambda = 3)
powiaty_norm <- cbind(powiaty_opisowe, powiaty_norm_values)

#Dodanie nowych zmiennych do ramki danych o powiatach
powiaty_norm$Pozary.mieszkaniec <- powiaty_norm$Pozary.SUMA / powiaty_norm$Ludnosc
powiaty_norm$Zagrozenia.mieszkaniec <- powiaty_norm$Zagrozenia.SUMA / powiaty_norm$Ludnosc
powiaty_norm$Smierc.mieszkaniec <- powiaty_norm$Ofiary.smiertelne.SUMA / powiaty_norm$Ludnosc
powiaty_norm$Ranni.mieszkaniec <- powiaty_norm$Ranni.SUMA / powiaty_norm$Ludnosc
powiaty_norm$Transport.drogowy.mieszkaniec <- powiaty_norm$W.transporcie.drogowym / powiaty_norm$Ludnosc
powiaty_norm$Wiatry.mieszkaniec <- powiaty_norm$Silne.wiatry / powiaty_norm$Ludnosc
powiaty_norm$Uszczerbek.mieszkaniec <- (powiaty_norm$Ranni.SUMA + powiaty_norm$Ofiary.smiertelne.SUMA) / powiaty_norm$Ludnosc

#Wgranie mapy (pliku .shp) z powiatami
pl02_norm <- readOGR("gadm36_POL_2.shp", encoding = "UTF-8")

#Dodanie danych o powiatach do pliku .shp
pl02_norm@data <- left_join(pl02_norm@data, powiaty_norm, by = c("NAME_2" = "Powiat"))

#Wygenerowanie map
qtm(pl02_norm, "Pozary.mieszkaniec", fill.palette = "Reds")
qtm(pl02_norm, "Zagrozenia.mieszkaniec", fill.palette = "Oranges")
qtm(pl02_norm, "Smierc.mieszkaniec", fill.palette = "Purples")
qtm(pl02_norm, "Ranni.mieszkaniec", fill.palette = "GnBu")
qtm(pl02_norm, "Transport.drogowy.mieszkaniec", fill.palette = "Greens")
qtm(pl02_norm, "Wiatry.mieszkaniec", fill.palette = "Blues")
qtm(pl02_norm, "Uszczerbek.mieszkaniec", fill.palette = "Greys")

