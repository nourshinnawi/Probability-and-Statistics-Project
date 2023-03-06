# resistance data

data <- read.csv("C:/yacht.csv", sep=",",header=FALSE)

head(data)

names(data) <- c("longitude","prismatic","displacement",
                 "draught","beam","froude","resistance")

data$resistance
data[1,1]
data[1,2]
data[c(1,2,3,4,5),]

# To find scatter plots
plot(data$longitude,data$resistance)
plot(data$displacement,data$resistance)
plot(data$prismatic,data$resistance)
plot(data$draught,data$resistance)
plot(data$beam,data$resistance)
plot(data$froude,data$resistance)
plot(data$resistance,data$resistance)
plot(data)

# Model #1
fit.disp <- lm(resistance~displacement,data=data)
fit.disp2 <- lm(resistance~displacement+I(displacement^2),data=data)
AIC(fit.disp,fit.disp2)

fit.froude <- lm(resistance~froude,data=data)
fit.froude2 <- lm(resistance~froude+I(froude^2),data=data)
AIC(fit.froude,fit.froude2)

fit.pris <- lm(resistance~prismatic,data=data)
fit.pris2 <- lm(resistance~prismatic+I(prismatic^2),data=data)
AIC(fit.pris,fit.pris2)

fit.beam <- lm(resistance~beam,data=data)
fit.beam2 <- lm(resistance~beam+I(beam^2),data=data)
AIC(fit.beam,fit.beam2)

summary(fit.pris)
summary(fit.pris2)

plot(data$pris, data$resistance)
abline(fit.pris)

x <- data$prismatic
xmesh <- seq(-100, 7000, 0.1)
yhat <- predict(fit.pris2, newdata=data.frame(prismatic=xmesh))

lines(xmesh,yhat,col="red")
legend("topright",c("Linear","Quadratic"),
       lty=c(1,1), col=c("black","red"))

# Model #2:
m2.1 <- lm(resistance~prismatic+I(prismatic^2)+displacement+I(displacement^2)+
             froude+I(froude^2)+beam+I(beam^2)+longitude+draught,data=data)
summary(m2.1)
AIC(m2.1)

m2.2 <- lm(resistance~prismatic+I(prismatic^2)+
             froude+I(froude^2)+beam+I(beam^2)+longitude,data=data)
summary(m2.2)
AIC(m2.2)
