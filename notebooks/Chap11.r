
d1 <- readr::read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table11-1.csv")

head(d1)

str(d1)

library(ggplot2)

library(dplyr)

library(tidyr)

options(repr.plot.width = 4, repr.plot.height=4)

ggplot(data = d1, mapping=aes(x, y, group=block, colour=block)) + geom_point()

library("lme4")

res.d1.1 <- glmer(y ~ x + (1 | block), data = d1, family=gaussian(link=identity))

res.d1.2 <- lmer(y ~ x + (1 | block), data=d1)

summary(res.d1.1)

summary(res.d1.2)

d2 <- readr::read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table11-2.csv")

head(d2)

str(d2)

d2 %>% group_by(ID) %>% summarise_at(.vars = vars(y, x), .funs = funs(mean)) %>% 
    ungroup() %>% ggplot(aes(x, y)) + geom_point()

gp.d2 <- d2 %>% ggplot(aes(x, y)) + geom_jitter(width=0, height = 0.05, alpha=0.5) 
gp.d2

library("glmmML")

res.d2.1 <- glmer(y ~ x + (1 | ID), data=d2, family=binomial(link = logit))

summary(res.d2.1)

res.d2.2 <- glmmML(y ~ x, data=d2, cluster=ID, family=binomial(link=logit))

summary(res.d2.2)

coef(res.d2.2)

d2.pred <- data.frame(x = seq(0, 5, 0.01)) %>% 
    mutate(y1 = 1 / (1 + exp(-(coef(summary(res.d2.1))["(Intercept)", "Estimate"] + 
                               coef(summary(res.d2.1))["x", "Estimate"] * x))), 
          y2 = 1 / (1 + exp(-(coef(res.d2.2)["(Intercept)"] + 
                               coef(res.d2.2)["x"] * x)))
          )
head(d2.pred)

gp.d2.pred <- d2.pred %>% gather(func, y, -x) %>% geom_line(data=., mapping=aes(x, y, group=func, linetype=func), alpha=0.5)
gp.d2.pred

options(repr.plot.width=5)

gp.d2 + gp.d2.pred

d3 <- readr::read_csv("../samplecode/Rで学ぶ統計学入門図版作成用//table11-3.csv")

head(d3)

str(d3)

gp.d3 <- ggplot(d3, aes(wt, y)) + geom_point()
gp.d3

res.d3.1 <- glmer(cbind(y, 1-y) ~ wt + (1 | mother), data=d3, family=binomial(link=logit))

summary(res.d3.1)

res.d3.2 <- glmmML(cbind(y, 1-y) ~ wt, data=d3, cluster=mother, family=binomial(link=logit))

summary(res.d3.2)

coef(summary(res.d3.1))

d3.pred <- data.frame(wt = seq(0, 0.6, 0.01)) %>% 
    mutate(y1 = 1 / (1 + exp(-(coef(summary(res.d3.1))["(Intercept)", "Estimate"] + 
                               coef(summary(res.d3.1))["wt", "Estimate"] * wt))), 
          y2 = 1 / (1 + exp(-(coef(res.d3.2)["(Intercept)"] + 
                               coef(res.d3.2)["wt"] * wt)))
          )
head(d3.pred)

gp.d3.pred <- d3.pred %>% gather(func, y, -wt) %>% 
    geom_line(data=., mapping=aes(wt, y, group=func, linetype=func), alpha=0.5)
gp.d3 + gp.d3.pred

matrix(d3$y, nrow=4, ncol=12) %>% print()

res.d3.3 <- glm(cbind(y, 1-y) ~ wt, family=binomial(link=logit), data=d3)

summary(res.d3.3)

d4 <- readr::read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table11-4.csv")

head(d4)

str(d4)

gp.d4 <- ggplot(d4, aes(wt, flw)) + geom_point()
gp.d4

res.d4 <- glm(flw ~ wt, data=d4, family=poisson)

summary(res.d4)

71.071 / 34

res.d4 %>% coef()

d4.pred.1 <- data.frame(wt = seq(20, 60, 0.1)) %>% 
            mutate(flw = exp(coef(res.d4)["(Intercept)"] + coef(res.d4)["wt"] * wt))
head(d4.pred.1)

gp.d4.pred.1 <- geom_line(data=d4.pred.1, mapping=aes(wt, flw))
gp.d4 + gp.d4.pred.1 + scale_y_continuous(limits = c(0, 20))

library(MASS)

res.d4.2 <- glm(flw ~ wt, data=d4, family=negative.binomial(1))

summary(res.d4.2)

coef(res.d4.2)

d4.pred.2 <- data.frame(wt = d4.pred.1$wt) %>% 
        mutate(flw = exp(coef(res.d4.2)["(Intercept)"] + coef(res.d4.2)["wt"] * wt))
head(d4.pred.2)

gp.d4.pred.2 <- geom_line(data=d4.pred.2, mapping=aes(wt, flw))
gp.d4 + gp.d4.pred.2 + scale_y_continuous(limits = c(0, 20))

res.d4.3 <- glm.nb(flw ~ wt, data=d4)

summary(res.d4.3)

d4.pred.3 <- data.frame(wt = d4.pred.1$wt) %>% 
        mutate(flw = exp(coef(res.d4.3)["(Intercept)"] + coef(res.d4.3)["wt"] * wt))
head(d4.pred.3)

gp.d4.pred.3 <- geom_line(data=d4.pred.3, mapping=aes(wt, flw))
gp.d4 + gp.d4.pred.3 + scale_y_continuous(limits = c(0, 20))

d5 <- readr::read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table11-5.csv")

head(d5)

str(d5)

gp.d5 <- ggplot(d5, aes(x, y)) + geom_point()
gp.d5

res.d5 <- glm(y ~ x, data=d5, family=quasipoisson)

summary(res.d5)

d5.pred <- data.frame(x = seq(0, 7, 0.01)) %>% mutate(y = exp(coef(res.d5)["(Intercept)"] + coef(res.d5)["x"] * x))
head(d5.pred)

gp.d5.pred <- geom_line(data=d5.pred, mapping=aes(x, y))
gp.d5 + gp.d5.pred

logLik(res.d5)

res.d5.2 <- glm(y ~ x, data=d5, family=negative.binomial(1))
summary(res.d5.2)

res.d5.3 <- glm.nb(y ~ x, data=d5)
summary(res.d5.3)

d5.pred.2 <- data.frame(x = d5.pred$x) %>% 
    mutate(y = exp(coef(res.d5.2)["(Intercept)"] + coef(res.d5.2)["x"] * x))
head(d5.pred.2)

d5.pred.3 <- data.frame(x = d5.pred$x) %>% 
    mutate(y = exp(coef(res.d5.3)["(Intercept)"] + coef(res.d5.3)["x"] * x))
head(d5.pred.3)

gp.d5.pred.2 <- geom_line(data=d5.pred.2, mapping=aes(x, y), colour="red")
gp.d5.pred.3 <- geom_line(data=d5.pred.3, mapping=aes(x, y), colour="blue")

gp.d5 + list(gp.d5.pred, gp.d5.pred.2, gp.d5.pred.3)

devtools::session_info()
