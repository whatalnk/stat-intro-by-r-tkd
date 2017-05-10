
sapply(c("dplyr", "ggplot2", "readr", "Cairo"), require, character.only = TRUE)

if (.Platform$OS.type == "windows"){
  windowsFonts(msgothic = windowsFont("MSGothic"))
  jpfont <- element_text(family = "msgothic")
} else if (Sys.info()["sysname"] == "Darwin") {
  jpfont <- element_text(family = "HiraKakuPro-W3")
} else {
  jpfont <- element_text(family = "IPAexGothic")
}

# search()

# detach(package:readr)

c(0:10) %>% sapply(function(r){
    ret <- choose(10, r) * 0.5^r * (1-0.5)^(10-r)
    return(data_frame(r = r, p = round(ret, 3)))
}, simplify = FALSE) %>% bind_rows()

d <- data.frame(r = 0:10, p = round(dbinom(0:10, 10, p=0.5), 3))
d

# options(repr.plot.width = 4, repr.plot.height = 4)
# Cairo(type = "raster")
d %>% ggplot(aes(x = r, y = p)) +
        geom_line() + geom_point() +
        xlab("表の出る回数") + ylab("確率") +
        # theme(axis.title = element_text(family = "IPAexGothic"))
        theme(axis.title = jpfont)
# dev.off()

c(10, 50, 100) %>% 
    sapply(function(x){
        a <- 0.4 * x
        b <- 0.6 * x
        prob <- dbinom(x=c(a:b), size=x, prob=0.5) %>% sum()
        cat(x, "回: ", prob, "\n")
    })

c(200, 500, 1000, 10000) %>% 
    sapply(function(x){
        a <- 0.49 * x
        b <- 0.51 * x
        prob <- dbinom(x=c(a:b), size=x, prob=0.5) %>% sum()
        cat(x, "回: ", prob, "\n")
    })

# data_frame(ID=c(0:99), dw = c(3, 7, 11, 12, 13, 14, 15, 16, 17, 17, 18, 18, 18, 19, 19, 19, 20, 20, 21, 21, 21, 22, 22, 23, 23, 24, 24, 24, 25, 25, 25, 26, 26, 26, 26, 27, 27, 27, 28, 28, 28, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 32, 32, 33, 33, 33, 33, 33, 34, 34, 34, 35, 35, 35, 36, 36, 36, 37, 37, 38, 38, 39, 39, 39, 40, 40, 41, 41, 41, 42, 42, 42, 43, 43, 44, 45, 46, 47, 48, 49, 53, 57)) %>% 
#     write_csv(path="../data/table3-2.csv")

tbl32 <- read_csv("../data/table3-2.csv")
head(tbl32)

# data_frame(ID=c(0:99), dw = c(3, 7, 11, 12, 13, 14, 15, 16, 17, 17, 18, 18, 18, 19, 19, 19, 20, 20, 21, 21, 21, 22, 22, 23, 23, 24, 24, 24, 25, 25, 25, 26, 26, 26, 26, 27, 27, 27, 28, 28, 28, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 32, 32, 33, 33, 33, 33, 33, 34, 34, 34, 35, 35, 35, 36, 36, 36, 37, 37, 38, 38, 39, 39, 39, 40, 40, 41, 41, 41, 42, 42, 42, 43, 43, 44, 45, 46, 47, 48, 49, 53, 57)) %>% 
#     openxlsx::write.xlsx(file = "../data/table3-2.xlsx")
# 
# readxl::read_excel(path="../data/table3-2.xlsx", sheet = 1) %>% head()

str(tbl32)

# options(repr.plot.width = 4, repr.plot.height = 4)

# Cairo(type = "raster")
tbl32 %>% 
    ggplot(aes(dw)) + 
        geom_histogram(binwidth = 5, colour = "black") + 
        xlab("体重増（ポンド）") + ylab("頻度") + 
        # theme(axis.title = element_text(family = "IPAexGothic"))
        theme(axis.title = jpfont)
# dev.off()

# options(repr.plot.width = 6, repr.plot.height = 3)

gp1 <- ggplot(data_frame(x = c(-4, 4)), aes(x)) + 
    stat_function(geom="line", fun = dnorm, args = list(mean = 0, sd = 1))
gp2 <- ggplot(data_frame(x = c(0, 40)), aes(x)) + 
    stat_function(geom="line", fun = dnorm, args = list(mean = 10, sd = 2), colour = "blue") + 
    stat_function(geom="line", fun = dnorm, args = list(mean = 20, sd = 4), colour = "red") 
gridExtra::grid.arrange(gp1, gp2, ncol = 2)

mu <- 10
sigma <- 2
pnorm(mu + sigma, mean=mu, sd = sigma) - pnorm(mu, mean = mu, sd = sigma)

mu <- 20
sigma <- 4
pnorm(mu + sigma, mean=mu, sd = sigma) - pnorm(mu, mean = mu, sd = sigma)

mu <- 10
sigma <- 2
pnorm(mu + 2 * sigma, mean=mu, sd = sigma) - pnorm(mu, mean = mu, sd = sigma)

mu <- 20
sigma <- 4
pnorm(mu + 2 * sigma, mean=mu, sd = sigma) - pnorm(mu, mean = mu, sd = sigma)

f <- function(n, M){
    mean.d <- numeric(M)
    for(k in 1:M){
        d <- runif(n, 0, 10)
        mean.d[k] <- mean(d)
    }
    mean.d
}

f.plt <- function(x){
    gp <- ggplot(data.frame(mean.d = x)) + 
      # theme(axis.title = element_text(family = "IPAexGothic"))
      theme(axis.title = jpfont)
    gp1 <-  gp + geom_histogram(aes(x = mean.d), bins = nclass.Sturges(x), 
                                colour = "black", fill=gray(0.8)) + 
            xlab("標本平均") + ylab("頻度") 
    gp2 <- gp + stat_qq(aes(sample = mean.d)) + xlab("理論変位置") + ylab("標本変位置")
    gridExtra::grid.arrange(gp1, gp2, ncol = 2)
}

# options(repr.plot.width = 6, repr.plot.height = 3)

mean.d1 <- f(3, 100000)

# Cairo(type="raster")
f.plt(mean.d1)
# dev.off()

mean.d2 <- f(10, 100000)

# Cairo(type="raster")
f.plt(mean.d2)
# dev.off()

mean.d3 <- f(1000, 100000)

# Cairo(type="raster")
f.plt(mean.d3)
# dev.off()

# options(repr.plot.width = 3, repr.plot.height = 3)

# f.plt.qq <- function(s){
#     n <- length(s)
#     y <- sort(s)
#     x <- qnorm(ppoints(n))
#     ggplot(data_frame(x, y), aes(x, y)) + geom_point() + 
#         xlim(-5, 5)
# }
# f.plt.qq(mean.d1)

# devtools::session_info()
