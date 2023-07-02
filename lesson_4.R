x <- seq(1, 100)
e <- rnorm(100, 0, 25)
y <- x*3 + 8 + e
df <- as.data.frame(cbind(y,x))

ggplot()+geom_point(df, mapping = aes(x = x, y=y))+
  geom_smooth(df, mapping = aes(x = x, y=y), method = "lm", se = F)

model1 <- lm(y~x, df)
summary(model1)

plot(model1$residuals)
hist(model1$residuals)
par(mfrow = c(2, 2))
plot(model1)

df$pred <- model1$fitted.values

a <- ggplot(df)+ geom_point(mapping = aes(x,y), color='skyblue3')+
  geom_point(mapping = aes(x,pred), color='orangered')

##### parabola

x <- seq(1, 10, 0.1)
e <- rnorm(100, 0, 9)
y <- 3*x^2 + 8 + e
df <- as.data.frame(cbind(y,x))
ggplot()+geom_point(df, mapping = aes(x = x, y=y))+
  geom_smooth(df, mapping = aes(x = x, y=y), method = "lm", se = F)

model1 <- lm(y~I(x^2), df)
summary(model1)

plot(model1$residuals)
hist(model1$residuals)
par(mfrow = c(2, 2))
plot(model1)

df$pred <- model1$fitted.values

b <- ggplot(df)+ geom_point(mapping = aes(x,y), color='skyblue3')+
  geom_point(mapping = aes(x,pred), color='orangered')+
  geom_smooth(df, mapping = aes(x = x, y=y), method = "lm", se = F)
b

##### influence

x <- seq(1, 100)
e <- rnorm(100, 0, 25)
y <- x*3 + 8 + e
df <- as.data.frame(cbind(y,x))
df <- rbind(df, c(1000, 10))
df <- rbind(df, c(10, 1000))
df <- rbind(df, c(3000, 1000))

ggplot()+geom_point(df, mapping = aes(x = x, y=y))+
  geom_smooth(df, mapping = aes(x = x, y=y), method = "lm", se = F)

model1 <- lm(y~x, df)
summary(model1)

plot(model1$residuals)
hist(model1$residuals)
par(mfrow = c(2, 2))
plot(model1)

df$pred <- model1$fitted.values

ggplot(df[1:100,])+ geom_point(mapping = aes(x,y), color='skyblue3')+
  geom_point(mapping = aes(x,pred), color='orangered')


model1 <- lm(y~x, df[1:101,])
summary(model1)
par(mfrow = c(2, 2))
plot(model1)

df[1:101,]$pred <- model1$fitted.values

ggplot(df[1:100,])+ geom_point(mapping = aes(x,y), color='skyblue3')+
  geom_point(mapping = aes(x,pred), color='orangered')



df <- as.data.frame(cbind(y,x))
df <- rbind(df, c(2000, 1000))
model1 <- lm(y~x, df)
summary(model1)

plot(model1$residuals)
hist(model1$residuals)
par(mfrow = c(2, 2))
plot(model1)

df$pred <- model1$fitted.values

ggplot(df[1:101,])+ geom_point(mapping = aes(x,y), color='skyblue3')+
  geom_point(mapping = aes(x,pred), color='orangered')

ggplot(df)+ geom_point(mapping = aes(x,y), color='skyblue3')+
  geom_point(mapping = aes(x,pred), color='orangered')+
  geom_smooth(df, mapping = aes(x = x, y=y), method = "lm", se = F)
