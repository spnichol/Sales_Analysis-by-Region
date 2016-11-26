soup <- read.csv("soup.csv")
library(MASS)
library(psych)
names(soup)
head(soup, lines=10)
summary(soup)

soup["Season"] <- NULL
for (i in 1:nrow(soup)) {
  if (soup$Month[i] >= 10 || soup$Month[i] <=2) {
    soup$Season[i] <- 1
  }
  else {
    soup$Season[i] <- 0
  }
}
soup$Region <- factor(soup$Region)

soup$MidWest<- as.logical(0) 
soup$West<- as.logical(0)  
soup$South<- as.logical(0)  
soup$East<- as.logical(0)  

for (i in 1:nrow(soup)) {
  if (soup$Region[i] == "MidWest") {
    soup$MidWest[i] <- as.logical(1)
    
  }
  else if (soup$Region[i] =="West") {
    soup$West[i] <- as.logical(1)

  }
  else if (soup$Region[i] =="East") {
  
    soup$East[i] <- as.logical(1)
  }
  else {
    soup$South[i] <- as.logical(1)
  }
}
shareWinter <- (sum(soup$Sales.Progresso[soup$Season == 1]) / sum(soup$Category_Sales[soup$Season == 1])) * 100
shareNonWinter <- (sum(soup$Sales.Progresso[soup$Season == 0]) / sum(soup$Category_Sales[soup$Season == 0])) * 100
shareNonWinter
shareWinter
options(scipen=999)

options(error=traceback)
fit <- lm(soup$Sales.Progresso~Price.Campbell+Price.PL+Price.Progresso+South+West+East+MidWest+High_Income+Low_Income+Season, data=soup)
fit
summary(fit)
step <- stepAIC(fit, direction="both")
step$anova
summary(step)

soupEast <- subset(soup, Region=="East")
fitEast <- lm(soupEast$Sales.Progresso~Price.Campbell+Price.PL+Price.Progresso+South+West+East+MidWest+High_Income+Low_Income+Season, data=soupEast)
fitEast
summary(fitEast)
step <- stepAIC(fitEast, direction="both")
step$anova
summary(step)


fitRes <- resid(fit)



lmPP <- (soup$Sales.Progresso ~ soup$Price.Progresso)

plot(soup$Price.PL, fitRes, ylab="Residuals", ann=TRUE, axes=TRUE)
plot(soup$Price.Campbell, residuals(fit), xlab="Price Campbell", ylim=c(-50000, 50000))
plot(soup$Price.Progresso, residuals(fit), xlab="Price Progresso", ylim=c(-50000, 50000))
abline(0,0)
plot(fit)
