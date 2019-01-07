vinos <- read.csv("C:/Users/cabopalos/Documents/UOC/TCVD/PRA2/winequality-red.csv", header=TRUE)

sapply(vinos, function(x) class(x))

str(vinos)

nrow(vinos[duplicated(vinos), ])
vinosDisjuntos <- vinos[!duplicated(vinos), ]
nrow(vinosDisjuntos)

summary(vinos)

par(mfrow=c(3,4))
boxplot(vinos$fixed.acidity, main="fixed.acidity")
boxplot(vinos$volatile.acidity, main="volatile.acidity")
boxplot(vinos$citric.acid, main="citric.acid")
boxplot(vinos$residual.sugar, main="residual.sugar")
boxplot(vinos$chlorides, main="chlorides")
boxplot(vinos$free.sulfur.dioxide, main="free.sulfur.dioxide")
boxplot(vinos$total.sulfur.dioxide, main="total.sulfur.dioxide")
boxplot(vinos$density, main="density")
boxplot(vinos$pH, main="pH")
boxplot(vinos$sulphates, main="sulphates")
boxplot(vinos$alcohol, main="alcohol")
boxplot(vinos$quality, main="quality")

boxplot.stats(vinos$fixed.acidity)$out
boxplot.stats(vinos$volatile.acidity)$out
boxplot.stats(vinos$citric.acid)$out
boxplot.stats(vinos$residual.sugar)$out
boxplot.stats(vinos$chlorides)$out
boxplot.stats(vinos$free.sulfur.dioxide)$out
boxplot.stats(vinos$total.sulfur.dioxide)$out
boxplot.stats(vinos$density)$out
boxplot.stats(vinos$pH)$out
boxplot.stats(vinos$sulphates)$out
boxplot.stats(vinos$alcohol)$out
boxplot.stats(vinos$quality)$out


wss <- (nrow(vinos)-1) * sum(apply(vinos, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(vinos, centers=i,iter.max=30)$withinss)
plot(1:15, wss, type="b", xlab="Nº Segmentos", ylab="within groups sum of squares")

kmvinos <- kmeans(vinos, 4, 15)
print(kmvinos)

aggregate(vinos, by=list(kmvinos$cluster),FUN=mean)
vinosKM <- data.frame(vinos, kmvinos$cluster)
head(vinosKM)

vinosKM$kmvinos.cluster <- factor(vinosKM$kmvinos.cluster)

scatterplotMatrix(~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol | kmvinos.cluster, reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), id.n=0, diagonal= 'density', by.groups=TRUE, data=vinosKM)

library(nortest)
alpha = 0.05
col.names = colnames(vinos)

for (i in 1:ncol(vinos)) {
	if (i == 1) cat("Variables que no siguen una distribución normal:\n")
	if (is.integer(vinos[,i]) | is.numeric(vinos[,i])) {
		p_val = ad.test(vinos[,i])$p.value
		if (p_val < alpha) {
			cat(col.names[i])
			# Format output
			if (i < ncol(vinos) - 1) cat(", ")
			if (i %% 3 == 0) cat("\n")
		}
	}
}

fligner.test(quality ~ alcohol, data = vinos)







corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")

for (i in 1:ncol(vinos) - 1) {
	if (is.integer(vinos[,i]) | is.numeric(vinos[,i])) {
		spearman_test = cor.test(vinos[,i], vinos[,length(vinos)], method = "spearman")
		corr_coef = spearman_test$estimate
		p_val = spearman_test$p.value

		pair = matrix(ncol = 2, nrow = 1)
		pair[1][1] = corr_coef
		pair[2][1] = p_val
		corr_matrix <- rbind(corr_matrix, pair)
		rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(vinos)[i]
	}
}

install.packages("corrplot")
library(corrplot)
corrplot(cor(vinos), method = "circle")

table(vinos$quality)
hist(vinos$quality)

vinos$premium <- ifelse(vinos$quality > 6, 1, 0)
vinos.standard.alcohol <- vinos[vinos$premium == 0,]$alcohol
vinos.premium.alcohol <- vinos[vinos$premium == 1,]$alcohol

t.test(vinos.standard.alcohol, vinos.premium.alcohol, alternative = "less")


modelo <- lm(quality ~  fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = vinos)
summary(modelo)$r.squared

modelo2 <- lm(quality ~ volatile.acidity + citric.acid + total.sulfur.dioxide + sulphates + alcohol, data = vinos)
summary(modelo2)$r.squared

muestra <- data.frame(volatile.acidity = 7.4, citric.acid = 0, total.sulfur.dioxide = 34, sulphates = 0.56, alcohol = 9.4)

muestra2 <- data.frame(volatile.acidity = 0.70, citric.acid = 0, total.sulfur.dioxide = 34, sulphates = 0.56, alcohol = 9.4)
predict(modelo2, muestra2)
