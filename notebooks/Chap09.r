
library(dplyr)

library(ggplot2)

d1 <- data.frame(
    age = c(19, 23, 27, 35, 44, 51, 59, 66), 
    blood_pressure = c(124, 117, 120, 132, 128, 142, 143, 135)
)

options(repr.plot.width = 4, repr.plot.height=4)

ggplot(d1, aes(age, blood_pressure)) + geom_point() + 
    scale_x_continuous(limits = c(10, 70)) + scale_y_continuous(limits=c(110, 150))

b.d1 <- d1 %>% 
    mutate(x = age - mean(age), y = blood_pressure - mean(blood_pressure)) %>% 
    mutate(xy = x * y) %>% 
    summarise(SP.xy = sum(xy), SS.x = sum(x**2)) %>% 
    summarise(b = SP.xy / SS.x) %>% .[["b"]]
a.d1 <- with(mean(blood_pressure) - b.d1 * mean(age), data=d1)

ggplot(d1, aes(age, blood_pressure)) + geom_point() + 
    scale_x_continuous(limits = c(10, 70)) + scale_y_continuous(limits=c(110, 150)) + 
    geom_abline(slope = b.d1, intercept = a.d1) + 
    annotate("text", x=10, y = 150, hjust=0, vjust=0, label=sprintf("a = %.2f", a.d1)) + 
    annotate("text", x=10, y = 147, hjust=0, vjust=0, label=sprintf("b = %.4f", b.d1))

d2 <- data.frame(
    age = c(19, 23, 27, 35, 44, 51, 59, 66), 
    blood_pressure = c(111, 129, 113, 137, 147, 126, 135, 139)
)

b.d2 <- d2 %>% 
    mutate(x = age - mean(age), y = blood_pressure - mean(blood_pressure)) %>% 
    mutate(xy = x * y) %>% 
    summarise(SP.xy = sum(xy), SS.x = sum(x**2)) %>% 
    summarise(b = SP.xy / SS.x) %>% .[["b"]]
a.d2 <- with(mean(blood_pressure) - b.d2 * mean(age), data=d2)

ggplot(d2, aes(age, blood_pressure)) + geom_point() + 
    scale_x_continuous(limits = c(10, 70)) + scale_y_continuous(limits=c(110, 150)) + 
    geom_abline(slope = b.d2, intercept = a.d2) + 
    annotate("text", x=10, y = 150, hjust=0, vjust=0, label=sprintf("a = %.2f", a.d2)) + 
    annotate("text", x=10, y = 147, hjust=0, vjust=0, label=sprintf("b = %.4f", b.d2))

d3 <- bind_rows(d2, d2)
b.d3 <- d3 %>% 
    mutate(x = age - mean(age), y = blood_pressure - mean(blood_pressure)) %>% 
    mutate(xy = x * y) %>% 
    summarise(SP.xy = sum(xy), SS.x = sum(x**2)) %>% 
    summarise(b = SP.xy / SS.x) %>% .[["b"]]
a.d3 <- with(mean(blood_pressure) - b.d3 * mean(age), data=d3)

ggplot(d3, aes(age, blood_pressure)) + geom_point() + 
    scale_x_continuous(limits = c(10, 70)) + scale_y_continuous(limits=c(110, 150)) + 
    geom_abline(slope = b.d3, intercept = a.d3) + 
    annotate("text", x=10, y = 150, hjust=0, vjust=0, label=sprintf("a = %.2f", a.d3)) + 
    annotate("text", x=10, y = 147, hjust=0, vjust=0, label=sprintf("b = %.4f", b.d3))

lm(blood_pressure ~ age, data = d1) %>% summary()

lm(blood_pressure ~ age, data = d2) %>% summary()

lm(blood_pressure ~ age, data = d3) %>% summary()

d1 %>% 
    mutate(y_hat = age * b.d1 + a.d1) %>% 
    summarise(s = sqrt(sum((blood_pressure - y_hat)**2) / (n() - 2)))

# t
d1 %>% summarise(t = b.d1 * sqrt(sum((age - mean(age))**2)) / 5.83)

# p値
1 - pt(q = 3.62, df = 4)

# total SS
(SS.total <- with(sum((blood_pressure - mean(blood_pressure))**2), data = d1))

# regression SS
(SS.reg <- with(sum(((age * b.d1 + a.d1 - mean(blood_pressure)))**2), data = d1))

# residual SS
(SS.res <- with(sum((blood_pressure - (age * b.d1 + a.d1))**2), data=d1))

identical(all.equal(SS.total, SS.reg + SS.res), TRUE)

# degree of freedom
## total
df.total <- nrow(d1) - 1
## regression
df.reg <- 1
## residual
df.res <- nrow(d1) - 2

# F value
(f_val <- (SS.reg / df.reg) / (SS.res / df.res))

# p value
1 - pf(q = 13.12, df1 = 1, df2 = 6)

# Multiple R-squared
1 - (SS.res / SS.total)

# Adjusted R-squared
1 - (SS.res / df.res) / (SS.total / df.total)

d4 <- data.frame(
    x = c(1,2,4,5,3,2,3,1,5,4,4,2),
    y = c(3,4,4,5,5,3,4,3,6,6,5,5)
)
gp.d4 <- ggplot(d4, aes(x, y)) + geom_point() + xlim(0, 7) + ylim(0, 7) + xlab("Parent (year)") + ylab("Children (year)")
gp.d4

res.d4 <- lm(y ~ x, data=d4)
res.d4 %>% summary()

gp.d4 + geom_abline(slope = coef(res.d4)[["x"]], intercept = coef(res.d4)[["(Intercept)"]])

d5 <- data.frame(
    x = c(3,4,5,6,8,9,10,11,12,14,15,16,17), 
    y = c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5)
)

gp.d5 <- ggplot(d5, aes(x, y)) + geom_point() + xlim(0, 20) + ylim(0, 6) + xlab("Year") + ylab("Wing size (cm)")
gp.d5

res.d5 <- lm(y ~ x, data=d5)
summary(res.d5)

gp.d5 +  geom_abline(slope = coef(res.d5)[["x"]], intercept = coef(res.d5)[["(Intercept)"]])

predict(res.d5, interval="confidence")

d5.conf <- predict(res.d5, interval="confidence") %>% as.data.frame()
d5.conf

ggplot(bind_cols(d5, d5.conf)) + 
    geom_point(mapping =aes(x, y)) + 
    geom_line(mapping=aes(x, upr), linetype="dashed") + 
    geom_line(mapping=aes(x, fit)) + 
    geom_line(mapping=aes(x, lwr), linetype="dashed") + 
    xlim(0, 20) + ylim(0, 6) + xlab("Year") + ylab("Wing size (cm)")

d5.pred <- predict(res.d5, interval="prediction") %>% as.data.frame()
d5.pred

ggplot(bind_cols(d5, d5.pred)) + 
    geom_point(mapping =aes(x, y)) + 
    geom_line(mapping=aes(x, upr), linetype="dashed") + 
    geom_line(mapping=aes(x, fit)) + 
    geom_line(mapping=aes(x, lwr), linetype="dashed") + 
    xlim(0, 20) + ylim(0, 6) + xlab("Year") + ylab("Wing size (cm)")

d.ex <- data.frame(
    nsmog = c(91, 70, 103, 79, 86, 114, 101, 82, 75, 87), 
    n30 = c(14, 5, 28, 17, 15, 19, 20, 7, 10, 9)
)

ggplot(d.ex, aes(n30, nsmog)) + geom_point() + xlim(0, 30) + ylim(50, 150)

res.ex <- lm(nsmog ~ n30, data=d.ex)
summary(res.ex)

d.ex.conf <- predict(res.ex, interval="confidence") %>% as.data.frame()

ggplot(bind_cols(d.ex, d.ex.conf)) + 
    geom_point(aes(n30, nsmog)) + xlim(0, 30) + ylim(50, 150) + 
    geom_line(aes(n30, upr), linetype="dashed") + 
    geom_line(aes(n30, fit)) + 
    geom_line(aes(n30, lwr), linetype="dashed")

devtools::session_info()
