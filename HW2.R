#Statistical Learning_Homework2
#107064522

library(MASS)
set.seed(100)
n <- 100
x <- matrix(runif(n, -2, 2), nrow = n)
y <- 2+0.75*sin(x)-0.75*cos(x)+rnorm(n, 0, 0.2)

x0 <- seq(-3, 3, len = 1000)
plot(x, y);
lines(x0, 2+0.75*sin(x0)-0.75*cos(x0), col = "black", lwd = 2)

#1.
##training data(n = 50, seed = 100)
set.seed(100)
n <- 50
x <- matrix(runif(n, -2, 2), nrow = n)
y <- 2+0.75*sin(x)-0.75*cos(x)+rnorm(n, 0, 0.2)
tr_data = data.frame(x, y)
plot(tr_data[, 1], tr_data[, 2], main = 'Training Data', xlab = 'X', ylab = 'Y')
lines(x0, 2+0.75*sin(x0)-0.75*cos(x0), col = "black", lwd = 2)

##testing data(n = 50, seed = 2)
set.seed(2)
n <- 50
x <- matrix(runif(n, -2, 2), nrow = n)
y <- 2+0.75*sin(x)-0.75*cos(x)+rnorm(n, 0, 0.2)
te_data = data.frame(x, y)
plot(te_data[, 1], te_data[, 2], main = 'Testing Data', xlab = 'X', ylab = 'Y')
lines(x0, 2+0.75*sin(x0)-0.75*cos(x0), col = "black", lwd = 2)

#2.(a)
linear = lm(y~x, data = tr_data)
plot(tr_data[, 1], tr_data[, 2], main = "Linear Model (with training data)", xlab = "X", ylab = "Y")
abline(linear, lty = 2, lwd = 2)

#2.(b)
#MSE of training data
linear_tr_MSE = sum(linear$residuals^2)/50
#MSE of testing data
linear_te_y <- predict(linear, newdata = te_data) 
linear_te_MSE = sum((te_data[, 2]-linear_te_y)^2)/50

#3.(a)
poly = lm(y~x+I(x^2)+I(x^3), data = tr_data)
plot(tr_data[, 1], tr_data[, 2], main = "Polynomial Model (with training data)", xlab = "X", ylab = "Y")
x3 <- seq(-3, 3, len = 100)
y3 <- predict(poly, newdata = data.frame(x = x3))
lines(x3, y3, lty = 3, lwd = 2)

#3.(b)
#MSE of training data
poly_tr_MSE = sum(poly$residuals^2)/50
#MSE of testing data
poly_te_y <- predict(poly, newdata = te_data)
poly_te_MSE = sum((te_data[, 2]-poly_te_y)^2)/50

#4.(a)
#generate design matrix
nknots <- 12
mat.tpb <- function(x, nknots){
  n <- length(x)
  knots <- seq(-2, 2, len = nknots)
  zb <- cbind(1, x, x^2, x^3)
  zt <- matrix(NA, nrow = n, ncol = nknots)
  for(i in 1:nknots){
    zt[, i] <- ifelse(x > knots[i], (x-knots[i])^3, 0)
  }
  cbind(zb, zt)
}
###z <- mat.tpb(x, nknots)
spline_tpb <- mat.tpb(tr_data[, 1], nknots)
spline <- ginv(t(spline_tpb)%*%spline_tpb)%*%t(spline_tpb)%*%tr_data[, 2] #beta

plot(tr_data[, 1], tr_data[, 2], main = "Spline Model (with training data)", xlab = "X", ylab = "Y")
x4 <- seq(-3, 3, len = 1000)
y4 <- mat.tpb(x4, nknots)%*%spline #y = x*beta
lines(x4, y4, lty = 4, lwd = 2)

#4.(b)
#MSE of training data
spline_tr_y = mat.tpb(tr_data[, 1], nknots)%*%spline
spline_tr_MSE = sum((tr_data[, 2]-spline_tr_y)^2)/50
#MSE of testing data
spline_te_y = mat.tpb(te_data[, 1], nknots)%*%spline
spline_te_MSE = sum((te_data[, 2]-spline_te_y)^2)/50

#5.(a)
#data simulation with training data
plot(tr_data[, 1], tr_data[, 2],
     main = "Linear, Polynomial and Spline Model (with training data)",
     xlab = "X",
     ylab = "Y"
     )
legend("topleft",
       lty = c(2, 3, 4),
       col = c("black", "black", "gray50"),
       legend = c("Linear", "Polynomial", "Spline")
       )
abline(linear, lty = 2, lwd = 2)
lines(x3, y3, lty = 3, lwd = 2)
lines(x4, y4, lty = 4, lwd = 2, col = "gray50")

#MSE of training data and testing data
plot(c(2, 4, 16),
     c(linear_tr_MSE, poly_tr_MSE, spline_tr_MSE),
     main = "MSE of Training and Testing Data",
     xlab = "Flexibility",
     ylab = "Mean Squared Error",
     type = "b",
     lty = 2,
     ylim = c(0, 0.17),
     lwd = 2)
legend("topright",
       lty = c(2, 4),
       lwd = c(2, 2),
       legend = c("MSE of training data", "MSE of testing data")
       )
lines(c(2, 4, 16),
      c(linear_te_MSE, poly_te_MSE, spline_te_MSE),
      type = "b",
      lty = 4,
      lwd = 2
      )
#sigma^2 = 0.04
abline(a = 0.04, b= 0, lty =3)

#6.(a)
y2 = 0 #Record the sum of y_hat of linear model
y3 = 0 #Record the sum of y_hat of polynomial model
y4 = 0 #Record the sum of y_hat of spline model
linear_y = list() #Record y_hat of linear model
poly_y = list() #Record y_hat of polynomial model
spline_y = list() #Record y_hat of spline model

for(i in 1:1000){
  #create 1000 training datasets
  set.seed(i)
  n <- 50
  x <- matrix(runif(n, -2, 2), nrow = n)
  y <- 2+0.75*sin(x)-0.75*cos(x)+rnorm(n, 0, 0.2)
  training_data = data.frame(x, y)
  
  #linear model
  linear = lm(y~x, data = training_data)
  linear_y[[i]] <- predict(linear, newdata = te_data)
  y2 = y2+linear_y[[i]]
  
  #polynomial model
  poly = lm(y~x+I(x^2)+I(x^3), data = training_data)
  poly_y[[i]] <- predict(poly, newdata = te_data)
  y3 = y3+poly_y[[i]]
  
  #spline model
  spline_tpb <- mat.tpb(training_data[, 1], nknots)
  spline <- ginv(t(spline_tpb)%*%spline_tpb)%*%t(spline_tpb)%*%training_data[, 2]
  spline_y[[i]] = mat.tpb(te_data[, 1], nknots)%*%spline
  y4 = y4+spline_y[[i]]
}

f <- function(x){
  2+0.75*sin(x)-0.75*cos(x)
  }
y = f(te_data[, 1])

##### BIAS #####
#bias of linear model
linear_bias = mean((y-y2/1000)^2)
#bias of polynomial model
poly_bias = mean((y-y3/1000)^2)
#bias of spline model
spline_bias = mean((y-y4/1000)^2)


##### VARIANCE #####
#variance of linear model
temp1 = as.matrix(as.data.frame(linear_y))
linear_var_sum = 0
for(i in 1:50){
  linear_var_sum[i] = var(temp1[i, ])
}
linear_var = mean(linear_var_sum)

#variance of polynomial model
temp2 = as.matrix(as.data.frame(poly_y))
poly_var_sum = 0
for(i in 1:50){
  poly_var_sum[i] = var(temp2[i, ])
}
poly_var = mean(poly_var_sum)

#variance of spline model
temp3 = as.matrix(as.data.frame(spline_y))
spline_var_sum = 0
for(i in 1:50){
  spline_var_sum[i] = var(temp3[i, ])
}
spline_var = mean(spline_var_sum)

##### PLOT #####
#plot test MSE
plot(c(2, 4, 16),
     c(linear_te_MSE, poly_te_MSE, spline_te_MSE),
     type = "b",
     ylim = c(0, 0.17),
     xlab = "Flexibility",
     ylab = "",
     lty = 2,
     lwd = 2
     )
#line information
legend("topright",
       lty = c(2, 3, 4),
       lwd = c(2, 2, 2),
       legend = c("Test MSE", "Squared bias", "Variance")
       )
#plot squared bias
lines(c(2, 4, 16),
      c(linear_bias, poly_bias, spline_bias),
      type = "b",
      lty = 3,
      lwd = 2
      )
#plot variance
lines(c(2, 4, 16),
      c(linear_var, poly_var, spline_var),
      type = "b",
      lty = 4,
      lwd = 2
      )
#sigma^2 = 0.04
abline(a = 0.04, b= 0, lty =3)