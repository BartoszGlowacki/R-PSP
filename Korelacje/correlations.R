#Załadowanie pliku ze statystykami dla gmin
gminy_corr <- read.csv("gminy.csv", header = TRUE, encoding ="UTF-8")


#Normalizacja danych liczbowych
gminy_corr_norm <- normalise(gminy_corr[6:63])


#Utworzenia pozdbiorów do korelacji
gminy_corr_norm_basic <- gminy_corr_norm[1:4]
gminy_corr_norm_fire <- gminy_corr_norm[5:9]
gminy_corr_norm_acc <- gminy_corr_norm[10:15]
gminy_corr_norm_false <- gminy_corr_norm[16:19]
gminy_corr_norm_acc_type <- gminy_corr_norm[45:58]


#Stworzenie macierzy korelacji
cor_fire <- cor(gminy_corr_norm_basic, gminy_corr_norm_fire)
cor_acc <- cor(gminy_corr_norm_basic, gminy_corr_norm_acc)
cor_false <- cor(gminy_corr_norm_basic, gminy_corr_norm_false)
cor_acc_type <- cor(gminy_corr_norm_basic, gminy_corr_norm_acc_type)


#Stworzenie palet kolorystycznych
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))

col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))

col3 <- colorRampPalette(c("red", "white", "blue")) 

col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))



#Wykres macierzy korelacji dla pożarów
corrplot(cor_fire, 
         method = c("shade"), 
         type = c("full"), 
         add = FALSE,
         col = NULL, 
         bg = "white", 
         title = NULL, 
         is.corr = TRUE, 
         diag = TRUE,
         outline = FALSE, 
         mar = c(0, 0, 0, 0), 
         addgrid.col = "black",
         addCoef.col = "orange", 
         addCoefasPercent = FALSE, 
         order = c("original"), 
         hclust.method = c("complete"),
         addrect = NULL, 
         rect.col = "black", 
         rect.lwd = 2, 
         tl.pos = NULL,
         tl.cex = 1, 
         tl.col = "black", 
         tl.offset = 0.6, 
         tl.srt = 50,
         cl.pos = NULL, 
         cl.lim = NULL, 
         cl.length = NULL, 
         cl.cex = 0.8,
         cl.ratio = 0.4, 
         cl.align.text = "l", 
         cl.offset = 0.5, 
         number.cex = 0.9,
         number.font = 2, 
         number.digits = NULL, 
         addshade = c("negative"), 
         p.mat = NULL,
         sig.level = 0.05, 
         insig = c("pch"),
         pch = 4, 
         pch.col = "yellow", 
         pch.cex = 3, 
         plotCI = c("n"), 
         lowCI.mat = NULL, 
         uppCI.mat = NULL, 
         na.label = "NA",
         na.label.col = "white", 
         win.asp = 1)


#Wykres macierzy korelacji dla skali zagrożeń
corrplot(cor_acc, 
         method = c("circle"), 
         type = c("full"), 
         add = FALSE,
         col = col3(100), 
         bg = "white", 
         title = NULL, 
         is.corr = TRUE, 
         diag = TRUE,
         outline = FALSE, 
         mar = c(0, 0, 0, 0), 
         addgrid.col = "black",
         addCoef.col = "red", 
         addCoefasPercent = FALSE, 
         order = c("original"), 
         hclust.method = c("complete"),
         addrect = NULL, 
         rect.col = "black", 
         rect.lwd = 2, 
         tl.pos = NULL,
         tl.cex = 1, 
         tl.col = "black", 
         tl.offset = 0.6, 
         tl.srt = 50,
         cl.pos = NULL, 
         cl.lim = NULL, 
         cl.length = NULL, 
         cl.cex = 0.8,
         cl.ratio = 0.4, 
         cl.align.text = "l", 
         cl.offset = 0.5, 
         number.cex = 0.9,
         number.font = 2, 
         number.digits = NULL, 
         addshade = c("negative"), 
         p.mat = NULL,
         sig.level = 0.05, 
         insig = c("pch"),
         pch = 4, 
         pch.col = "yellow", 
         pch.cex = 3, 
         plotCI = c("n"), 
         lowCI.mat = NULL, 
         uppCI.mat = NULL, 
         na.label = "NA",
         na.label.col = "red", 
         win.asp = 1)


#Wykres macierzy korelacji dla fałszywych alarmów
corrplot(cor_false, 
         method = c("square"), 
         type = c("full"), 
         add = FALSE,
         col = col4(100), 
         bg = "white", 
         title = NULL, 
         is.corr = TRUE, 
         diag = TRUE,
         outline = FALSE, 
         mar = c(0, 0, 0, 0), 
         addgrid.col = "black",
         addCoef.col = "red", 
         addCoefasPercent = FALSE, 
         order = c("original"), 
         hclust.method = c("complete"),
         addrect = NULL, 
         rect.col = "black", 
         rect.lwd = 2, 
         tl.pos = NULL,
         tl.cex = 1, 
         tl.col = "black", 
         tl.offset = 0.6, 
         tl.srt = 50,
         cl.pos = NULL, 
         cl.lim = NULL, 
         cl.length = NULL, 
         cl.cex = 0.8,
         cl.ratio = 0.4, 
         cl.align.text = "l", 
         cl.offset = 0.5, 
         number.cex = 0.9,
         number.font = 2, 
         number.digits = NULL, 
         addshade = c("negative"), 
         p.mat = NULL,
         sig.level = 0.05, 
         insig = c("pch"),
         pch = 4, 
         pch.col = "yellow", 
         pch.cex = 3, 
         plotCI = c("n"), 
         lowCI.mat = NULL, 
         uppCI.mat = NULL, 
         na.label = "NA",
         na.label.col = "red", 
         win.asp = 1)


#Wykres macierzy korelacji dla rodzajów zagrożeń
corrplot(cor_acc_type, 
         method = c("ellipse"), 
         type = c("full"), 
         add = FALSE,
         col = col1(100), 
         bg = "white", 
         title = NULL, 
         is.corr = TRUE, 
         diag = TRUE,
         outline = FALSE, 
         mar = c(0, 0, 0, 0), 
         addgrid.col = "black",
         addCoef.col = "red", 
         addCoefasPercent = FALSE, 
         order = c("original"), 
         hclust.method = c("complete"),
         addrect = NULL, 
         rect.col = "black", 
         rect.lwd = 2, 
         tl.pos = NULL,
         tl.cex = 1, 
         tl.col = "black", 
         tl.offset = 0.6, 
         tl.srt = 50,
         cl.pos = NULL, 
         cl.lim = NULL, 
         cl.length = NULL, 
         cl.cex = 0.8,
         cl.ratio = 0.2, 
         cl.align.text = "l", 
         cl.offset = 0.5, 
         number.cex = 0.9,
         number.font = 2, 
         number.digits = NULL, 
         addshade = c("negative"), 
         p.mat = NULL,
         sig.level = 0.05, 
         insig = c("pch"),
         pch = 4, 
         pch.col = "yellow", 
         pch.cex = 3, 
         plotCI = c("n"), 
         lowCI.mat = NULL, 
         uppCI.mat = NULL, 
         na.label = "NA",
         na.label.col = "red", 
         win.asp = 1)