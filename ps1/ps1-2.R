load("ps1-logreg.Rdata")
view(df)
m1<-glm(y1~x,df,family="binomial")
m2<-glm(y2~x,df,family="binomial")
residuals <- residuals(m1, type = "deviance")

# Plotting residuals
plot(residuals ~ df$x, main="Residuals vs. Predictor",
     xlab="X", ylab="Residuals", pch=20, col="red")
abline(h=0, lty=2)


residuals <- residuals(m2, type = "deviance")

# Plotting residuals
plot(residuals ~ df$x, main="Residuals vs. Predictor",
     xlab="X", ylab="Residuals", pch=20, col="red")
abline(h=0, lty=2)

ggplot(x = x, y = y1, data = df) + geom_point()

ggplot(data = df, aes(x = x, y = y1)) +
  geom_point(color = "red") +
  geom_smooth(method = "glm", se = FALSE, color = "blue")

ggplot(data = df, aes(x = x, y = y2)) +
  geom_point(color = "red") +
  geom_smooth(method = "glm", se = FALSE, color = "blue")
