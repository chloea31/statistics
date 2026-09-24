###########################################
###########################################
###      Practice work in PCA           ###
###########################################
###########################################


library(ade4)

load(url("https://pbil.univ-lyon1.fr/R/donnees/tdr605/card.Rda"))
card$Topt
couleurs <- ifelse(card$Topt < 50, "blue", "red")
with(card, {
  plot(Tmax, Tmin, col = couleurs, pch = 19, las = 1)
  abline(lm(Tmin~Tmax))
  r2 <- signif(cor(Tmin, Tmax)^2, 3)
  title(main = bquote(r^2 == .(r2)))
  text(Tmax, Tmin, rownames(card), cex = 0.75, pos = 4, xpd = NA)
})

library(ade4)
par(mfrow = c(1, 2))
card.cr <- as.data.frame(scalewt(card)) # centrage et réduction
stripchart(card, main = "Données de départ", las = 1,
           ylab = "Température (°C)", vertical = TRUE, pch = 19, cex = 0.5)
points(1:3, colMeans(card), pch = 21, bg = "purple", cex = 1.5)
stripchart(card.cr, main = "Données centrées-réduites", vertical = TRUE,
           pch = 19, cex = 0.5, ylab = "Nombre d'écart-type", las = 1)
points(1:3, colMeans(card.cr), pch = 21, bg = "purple", cex = 1.5)

library(rgl)
plot3d(card.cr, type = "s", col = couleurs)

plot3d(card.cr, type = "s", col = couleurs)
plot3d(ellipse3d(cor(card.cr)), col = "grey", add = TRUE)



##########################
### 2. PCA with ade4

acp <- dudi.pca(card, center = TRUE, scale = TRUE)
acp$eig
100*acp$eig/sum(acp$eig)
summary(acp)

x <- acp$li[ , 1] ; y <- acp$li[ , 2]
main <- paste("Le premier plan factoriel\nn =", nrow(card), "micro-organismes")
xlab <- paste("F1 :", signif(100*acp$eig[1]/sum(acp$eig), 3), "%")
ylab <- paste("F2 :", signif(100*acp$eig[2]/sum(acp$eig), 3), "%")
plot(x, y, pch = 19, col = couleurs, asp = 1, las = 1, main = main, xlab = xlab, ylab = ylab)
text(x, y, rownames(card), cex = 0.75, pos = 3)

load(url("https://pbil.univ-lyon1.fr/R/donnees/tdr605/cardGenre.Rda"))
x <- acp$li[ , 1] ; y <- acp$li[ , 2]
main <- paste("Le premier plan factoriel\nn =", nrow(card), "micro-organismes")
xlab <- paste("F1 :", signif(100*acp$eig[1]/sum(acp$eig), 3), "%")
ylab <- paste("F2 :", signif(100*acp$eig[2]/sum(acp$eig), 3), "%")
plot(x, y, pch = 19, col = couleurs, asp = 1, las = 1, main = main, xlab = xlab, ylab = ylab)
text(x, y, rownames(card), cex = 0.75, pos = 3)
selection <- names(table(cardGenre))[table(cardGenre) >= 3] # at least three strains
isel <- which(cardGenre %in% selection)
par(bty = "n")
s.class(acp$li[isel, ], cardGenre[isel], clabel = 0.75, add.plot = TRUE, axesell = FALSE)
scatter(acp)
s.corcircle(acp$co)


###########################
### 3. High dimensions (exercise)

load(url("https://pbil.univ-lyon1.fr/R/donnees/tdr605/card9.Rda"))
