
options(repr.plot.width=4, repr.plot.height=4)

x <- seq(0, 15, 1)
freq <- dpois(x, lambda=5)
plot(x, freq, type="b", lwd=2)

d <- read.csv("../samplecode/Rで学ぶ統計学入門図版作成用/table10-2.csv")
head(d)

str(d)

with(plot(dose, dead), data = d)

result <- glm(cbind(dead, live) ~ dose, family = binomial(logit), data = d)
summary(result)

coef(result)

names(coef(result))

d.predicted <- data.frame(dose = seq(1, 6, 0.01))
d.predicted$dead <- with(
    1 / (1 + exp(-(coef(result)["(Intercept)"] + coef(result)["dose"] * dose))), 
    data=d.predicted)
summary(d.predicted)

with(data=d, plot(dose, dead))
with(data=d.predicted, lines(dose, dead))

d2 <- read.csv("../samplecode/Rで学ぶ統計学入門図版作成用/table10-3.csv")
head(d2)

str(d2)

summary(d2)

with(plot(wt, flw), data=d2)

result2 <- glm(flw ~ wt, family = poisson, data=d2)
summary(result2)

d2.predicted <- data.frame(wt = seq(20, 40, 0.1))
d2.predicted$flw <- with(exp(coef(result2)["(Intercept)"] + coef(result2)["wt"] * wt), data=d2.predicted)
head(d.predicted)

with(plot(flw ~ wt), data=d2)
with(lines(flw ~ wt), data=d2.predicted)

str(summary(result2))

summary(result2)$deviance / summary(result2)$`df.residual`

y <- c(0:9)
p <- dpois(y, lambda=2)
p

options(digits=4)

print(p)

logL <- function(x, d){sum(dpois(d, x, log=T))}
d <- rpois(50, lambda = 3.1)
lambdas <- seq(2, 5, 0.1)

plot(lambdas, sapply(lambdas, logL, d = d), type="l")

logLik(result)

-2 * logLik(result) + 2 * 2

logLik(result2)

-2 * logLik(result2) + 2 * 2

sum(log(dpois(d2$flw, lambda=d2$flw))) * -2

result2.null <- glm(flw ~ 1, family = poisson, data=d2)
summary(result2.null)

logLik(result2.null) 

-2 * logLik(result2.null)

result2

-2 * logLik(result2)

165.7 - 123.57

d3 <- read.csv("../samplecode/Rで学ぶ統計学入門図版作成用/table10-5.csv")
head(d3)

str(d3)

summary(d3)

result3.A <- glm(cbind(y, 1 - y) ~ dose, family = binomial(logit), data = d3)
summary(result3.A)

result3.B <- glm(cbind(y, 1 - y) ~ dose + sex, family = binomial(logit), data = d3)
summary(result3.B)

result3.C <- glm(cbind(y, 1 - y) ~ dose + sex + dose:sex, family = binomial(logit), data = d3)
summary(result3.C)

library(ggplot2)

library(dplyr)

d3 %>% group_by(dose, sex) %>% summarise(mortality=mean(y))

options(repr.plot.width=5)

d3 %>% group_by(dose, sex) %>% summarise(mortality=mean(y)) %>% ungroup() %>% 
    ggplot(aes(x = dose, y = mortality, group=sex, colour=factor(sex))) + 
        geom_point() + 
        scale_color_discrete(name="Sex", label=c("Male", "Female")) + 
        xlab("Dose") + ylab("Mortality")

d3.pred.A <- 
    data.frame(dose = seq(1, 4, 0.01)) %>% 
    mutate(y = 1 / (1 + exp(-(coef(result3.A)["(Intercept)"] + coef(result3.A)["dose"] * dose))))
head(d3.pred.A)

gp.d3 <- d3 %>% ggplot(aes(x=dose, y=y, group=sex, shape=factor(sex), colour=factor(sex))) + 
    geom_jitter(width=0, height=0.1) + 
    scale_shape_discrete(name = "Sex", labels = c("Male", "Female")) +
    scale_colour_manual(name = "Sex", labels = c("Male", "Female"), values = c("blue", "red")) +
    xlab("Dose")
gp.d3

gp.d3 + 
    geom_line(data=d3.pred.A, aes(x = dose, y = y), inherit.aes = FALSE)

d3.pred.B <- 
    data.frame(dose = seq(1, 4, 0.01)) %>% 
    mutate(yM = 1 / (1 + exp(-(coef(result3.B)["(Intercept)"] + 
                               coef(result3.B)["dose"] * dose + 
                               coef(result3.B)["sex"] * 1))), 
           yF = 1 / (1 + exp(-(coef(result3.B)["(Intercept)"] + 
                               coef(result3.B)["dose"] * dose + 
                               coef(result3.B)["sex"] * 2))))

head(d3.pred.B)

gp.d3 + 
    geom_line(data=d3.pred.B, aes(x = dose, y = yM), inherit.aes = FALSE, colour="blue") + 
    geom_line(data=d3.pred.B, aes(x = dose, y = yF), inherit.aes = FALSE, colour="red") 

d3.pred.C <- 
    data.frame(dose = seq(1, 4, 0.01)) %>% 
    mutate(yM = 1 / (1 + exp(-(coef(result3.C)["(Intercept)"] + 
                               coef(result3.C)["dose"] * dose + 
                               coef(result3.C)["sex"] * 1 + 
                               coef(result3.C)["dose:sex"] * 1 * dose))), 
           yF = 1 / (1 + exp(-(coef(result3.C)["(Intercept)"] + 
                               coef(result3.C)["dose"] * dose + 
                               coef(result3.C)["sex"] * 2 + 
                               coef(result3.C)["dose:sex"] * 2 * dose))))
head(d3.pred.C)

gp.d3 + 
    geom_line(data=d3.pred.C, aes(x = dose, y = yM), inherit.aes = FALSE, colour="blue") + 
    geom_line(data=d3.pred.C, aes(x = dose, y = yF), inherit.aes = FALSE, colour="red") 

d4 <- read.csv("../samplecode/Rで学ぶ統計学入門図版作成用/table10-6.csv")
head(d4)

summary(d4)

str(d4)

result4 <- glm(plants ~ light, offset = log(w_area), family = poisson, data=d4)
summary(result4)

d4.pred <- 
    data.frame(light=seq(0, 4000, 0.1)) %>% 
    mutate(density = exp(coef(result4)["(Intercept)"] + coef(result4)["light"] * light))
head(d4.pred)

ggplot() + 
    geom_point(data=d4, aes(x=light , y=plants / w_area)) + 
    geom_line(data=d4.pred, aes(x=light, y=density)) + 
    xlab("Light (lx)") + ylab(expression("Number of plants (cm"^{-2}*")"))

devtools::session_info()
