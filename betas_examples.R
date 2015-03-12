### examples for testing betas package
## load data
load("data/pisa2012che.RData")
data <- pisa2012che

## linear regression models with numerical covariates only
vars <- c("MATH", "ESCS", "USEMATH")
data.v <- data[,vars]
data.na <- na.omit(data.v)
data.s <- data.frame(scale(data.na))

fit0  <- lm(MATH ~ ESCS + USEMATH, data.v, qr = FALSE)

fit1  <- lm(MATH ~ ESCS + USEMATH, data.v)
fit1a <- lm(MATH ~ ESCS + USEMATH, data.na)
fit1b <- lm(MATH ~ ESCS + USEMATH, data.s)

betas.lm(fit1)
betas.lm(fit1a)
summary(fit1b)

## ...and with interaction terms
data.nai <- data.na
data.nai$inter <- data.nai$ESCS * data.nai$USEMATH
data.si <- data.s
data.si$inter <- data.si$ESCS * data.si$USEMATH
data.si$inter.s <- scale(data.si$inter)
data.si$inter.i <- scale(data.nai$inter)

fit1.1 <- lm(MATH ~ ESCS * USEMATH, data.v)
fit1.1a <- lm(MATH ~ ESCS * USEMATH, data.na)
fit1.1b <- lm(MATH ~ ESCS * USEMATH, data.s)

betas.lm(fit1.1)
betas.lm(fit1.1a)
summary(fit1.1b)

fit1.1c <- lm(MATH ~ ESCS + USEMATH + inter, data.nai)
fit1.1d <- lm(MATH ~ ESCS + USEMATH + inter, data.si)
fit1.1e <- lm(MATH ~ ESCS + USEMATH + inter.s, data.si)
fit1.1f <- lm(MATH ~ ESCS + USEMATH + inter.i, data.si)

betas.lm(fit1.1c)
summary(fit1.1d)
summary(fit1.1e)
summary(fit1.1f)

## linear regression models with numerical and factorial covariates
fit2 <- lm(MATH ~ ESCS + USEMATH + ST04Q01 + FAMSTRUC + ST28Q01, data)

## ...and with interaction terms
fit2.1 <- lm(MATH ~ ESCS + USEMATH + ST04Q01 + FAMSTRUC * ST28Q01, data)

## weighted linear regression models
fit3 <- lm(MATH ~ ESCS + USEMATH, data, weights = W_FSTUWT)
fit4 <- lm(MATH ~ ESCS + USEMATH + ST04Q01 + FAMSTRUC + ST28Q01, data, weights = W_FSTUWT)

## ...with interaction terms
fit4.1 <- lm(MATH ~ ESCS + USEMATH + ST04Q01 + FAMSTRUC * ST28Q01, data, weights = W_FSTUWT)

## robust linear regression models
library(robust)
fit5 <- lmRob(MATH ~ ESCS + USEMATH, data)

## robust linear regression models with numerical and factorial covariates
## linear regression models without intercept
## all this models for groups and with or without intercept

#   - linear regression models with numerical covariates only
#    - linear regression models with numerical and factorial covariates
#    - linear regression models without intercept
#    - weighted linear regression models
#    - robust linear regression models
#    - all this models for groups and with or without intercept
#    - for lmer und glmer models
