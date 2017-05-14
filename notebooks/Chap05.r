
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(Cairo)

data1 <- data_frame(
    m = c(143.1, 140.9, 147.2, 139.8, 141.3, 150.7, 149.4, 145.6, 146.5, 148.5, 141.2, 136.5, 145.8, 148.1, 144.3), 
    f = c(138.7, 142.8, 150.3, 148.4, 141.7, 149.5, 156.5, 144.6, 144.4, 145.7, 148.3, 140.8, 146.2, 149.9, 144.1)
)
head(data1)

data2 <- data_frame(
    m = c(142.3, 142.5, 145.7, 143.5, 144.2, 145.1, 145.9, 145.2, 146.8, 145.7, 145.4, 144.6, 144.2, 145.9, 142.1),
    f = c(143.5, 144.6, 143.4, 146.6, 145.3, 147.7, 147.2, 147.8, 145.3, 145.7, 147.5, 147.2, 148.8, 147.9, 143.3)
)
head(data2)

data1 %>% summarise_all(.funs = mean)

data2 %>% summarise_all(.funs = mean)

options(repr.plot.height = 4, repr.plot.width = 6)

gp1 <- data1 %>% gather(gender, height) %>% 
    ggplot(aes(x = gender, y = height)) +
        geom_point(alpha = 0.5) + ylim(c(130, 160))
gp2 <- data2 %>% gather(gender, height) %>% 
    ggplot(aes(x = gender, y = height)) + 
        geom_point(alpha = 0.5) + ylim(c(130, 160))
grid.arrange(gp1, gp2, ncol=2)

surgery <- data_frame(new = c(2, 3, 6, 7, 4, 5, 6, 3), 
                     old = c(5, 7, 5, 8, 9, 7, 7, 6))
surgery

surgery %>% {
    t.test(.$new, .$old, var=TRUE)
    }

my.t.test <- function(x, y){
    n.x <- length(x)
    n.y <- length(x)
    
    mean.x <- mean(x)
    mean.y <- mean(y)
    ss.x <- sum((x - mean.x)^2)
    ss.y <- sum((y - mean.y)^2)
    sp2 <- (ss.x + ss.y) / (n.x + n.y - 2)
    se_mean <- sqrt(sp2 * (n.x + n.y) / (n.x * n.y))
    t_val <- (mean.x - mean.y) / se_mean
    return(t_val)
}
surgery %>% {
    my.t.test(.$new, .$old)
    }

qt(0.025, df = 14)

qt(0.975, df = 14)

ggplot(data_frame(x = c(-3, 3)), aes(x)) + stat_function(geom="line", fun = dt, args = list(df = 14)) + 
    geom_vline(xintercept = -2.144, linetype = 2) + 
    geom_vline(xintercept = 2.144, linetype = 2) 

data1 %>% {
    t.test(.$m, .$f, var = TRUE)
}

data2 %>% {
    t.test(.$m, .$f, var=TRUE)
}

growth1 <- data_frame(
    t = c(5.5, 4.2, 3.7, 5.1, 4.4, 4.3), 
    c = c(3.9, 4.1, 3.8, 3.2, 4.5, 3.8)
)
growth1 %>% {
    t.test(.$t, .$c, var=TRUE)
}

growth1 %>% {
    t.test(c(.$t, .$t), c(.$c,.$c),  var=TRUE)
}

foods <- data_frame(
    tomato = c(6, 16, 10, 14, 24, 8), 
    banana = c(1, 10, 5, 15, 20, 3)
)
foods %>% {
    t.test(.$tomato, .$banana, var=TRUE)
}

foods %>% mutate(id = c(1:6), d = tomato - banana) %>% select(id, tomato, banana, d)

?summarise_at

foods %>% mutate(id = c(1:6), d = tomato - banana) %>% select(id, tomato, banana, d) %>% 
    summarise_at(vars(-id), mean)

foods %>% mutate(id = as.character(c(1L:6L))) %>% gather(m, dec, -id) %>% 
    ggplot(aes(x = id, y = dec, group = m, colour = m)) + geom_point() 

?t.test

foods %>% mutate(d = tomato - banana) %>% {
    t.test(.$d, mu = 0)
}

foods %>% {
    t.test(.$tomato, .$banana, paired = TRUE)
}

?shapiro.test

data1$m %>% shapiro.test()

data1$f %>% shapiro.test()

?bartlett.test

?as.factor

data1 %>% gather(sex, score) %>% mutate(sex = factor(sex, levels = c("m", "f"))) %>% str()

data1 %>% gather(sex, score) %>% mutate(sex = factor(sex, levels = c("m", "f"))) %>% {
    bartlett.test(score ~ sex, data = .)
}

ls()

data3 <- data_frame(
    x1 = c(23, 20, 20, 24, 17, 19, 26, 22, 19, 21), 
    x2 = c(17, 23, 25, 34, 25, 28, 20, 31, 26, 36)
)

options(repr.plot.width = 3, repr.plot.height = 3)

data3 %>% gather(vars, vals) %>% ggplot(aes(x = vars, y = vals)) + geom_boxplot()

data3 %>% summarise_all(.funs = funs(var))

data3 %>% gather(group, score) %>% mutate(group = factor(group, levels = c("x1", "x2"))) %>% {
    bartlett.test(score ~ group, data = .)
}

data3 %>% {
    t.test(.$x1, .$x2, var = FALSE)
}

growth2 <- data_frame(
    t = c(25, 15, 19, 17, 22, 20), 
    c = c(16, 18, 17, 11, 19, 14)
)
growth2

growth2 %>% gather(type, growth) %>% ggplot(aes(x = type, y = growth)) + geom_boxplot()

growth2$t %>% shapiro.test()

growth2$c %>% shapiro.test()

growth2 %>% gather(type, growth) %>% mutate(type = factor(type)) %>% {
    bartlett.test(growth ~ type, data = .)
}

growth2 %>% {
    t.test(.$t, .$c, var=TRUE)
}

bind_rows(growth2, growth2) %>% gather(type, growth) %>% ggplot(aes(x = type, y = growth)) + geom_boxplot()

growth2 %>% {
    t.test(rep(.$t, 2), rep(.$c, 2), var=TRUE)
}

mental <- list(
    pos = c(12, 16, 11, 9, 18, 17, 14, 16, 10, 11), 
    neg = c(9, 10, 11, 8, 7, 9, 6, 10, 11, 13, 12)
)
mental

names(mental) %>% sapply(function(x){
    data_frame(type = x, score = mental[[x]])
}, simplify=FALSE) %>% bind_rows() %>% 
    ggplot(aes(x = type, y = score)) + geom_boxplot()

mental$pos %>% shapiro.test()

mental$neg %>% shapiro.test()

names(mental) %>% sapply(function(x){
    data_frame(type = x, score = mental[[x]])
}, simplify=FALSE) %>% bind_rows() %>% {
    bartlett.test(score ~ type, data = .)
}

mental %>% {
    t.test(.$pos, .$neg, var=TRUE)
}

diet <- data_frame(
    id = as.character(c(1L:7L)), 
    abdomen = c(1.2, 2.0, 1.3, 1.7, 2.0, 1.9, 1.3), 
    jogging = c(2.3, 2.6, 1.9, 2.8, 1.5, 1.9, 2.2)
)
diet

diet %>% gather(method, loss, -id) %>% ggplot(aes(x = method, y = loss)) + geom_boxplot()

diet$abdomen %>% shapiro.test()

diet$jogging %>% shapiro.test()

diet %>% gather(method, loss, -id) %>% {
    bartlett.test(loss ~ method, data = .)
}

diet %>% {
    t.test(.$abdomen, .$jogging, var = TRUE, paired = TRUE)
}

diet %>% {
    t.test(.$abdomen, .$jogging, var = TRUE, paired = FALSE)
}

devtools::session_info()
