
data1.m <- c(129.6, 130.5, 130.6, 131.1, 131.5, 132.0, 133.5, 133.9, 135.2, 135.3, 135.4, 136.7, 138.7, 139.3, 141.2)
data1.f <- c(128.5, 128.9, 129.2, 131.5, 131.7, 132.1, 132.1, 132.2, 133.0, 133.4, 133.8, 134.0, 135.2, 137.8, 138.6)
(data1 <- data.frame(male = data1.m, female = data1.f))

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2"), require,character.only=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)



data1 %>>% gather(key = gender, value = height) %>>% 
    ggplot(aes(x = gender, y = height)) + 
        geom_boxplot() + 
        scale_y_continuous(limits = c(120, 160))

data2.m <- c(138.4, 138.9, 142.8, 143.2, 144.0, 144.1, 144.4, 144.6, 144.8, 145.8, 146.2, 146.7, 147.0, 148.6, 149.5)
data2.f <- c(142.3, 142.4, 143.4, 144.0, 144.3, 145.6, 145.9, 147.7, 147.9, 148.6, 149.2, 149.7, 151.6, 151.9, 152.0)
(data2 <- data.frame(male = data2.m, female = data2.f))

data2 %>>% gather(key = gender, value = height) %>>% 
    ggplot(aes(x = gender, y = height)) + 
        geom_boxplot() + 
        scale_y_continuous(limits = c(120, 160))
