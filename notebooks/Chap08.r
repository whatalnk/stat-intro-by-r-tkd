
r <- 0.5
x <- rnorm(1000, mean=0, sd=5)
y <- r*x + sqrt(1 - r**2) * rnorm(1000, mean=0, sd=5)
cor(x, y)

options(repr.plot.width=4, repr.plot.height=4)

plot(x, y)

d <- data.frame(
    X = c(11.7, 11.9, 10.1, 13.6, 12.8, 10.5, 9.8, 10.9, 11.6, 11.8, 13.1, 12.4, 12.2), 
    Y = c(8.2, 8.2, 7.1, 9.2, 8.7, 7.7, 6.5, 7.7, 7.8, 8.0, 9.3, 8.6, 8.3)
)
d

with(data=d, plot(X, Y))

with(data=d, cor.test(x = X, y = Y))

d2 <- data.frame(
    X = c(12.5, 13.1, 18.9, 9.7, 16.4, 8.3, 13.7, 17.5, 11.4, 16.2, 19.3, 15.3), 
    Y = c(10.5, 8.9, 13.6, 6.3, 12.5, 10.3, 10.8, 16.7, 8.3, 9.5, 12.4, 10.1)
)
d2

with(plot(X, Y), data=d2)

with(cor.test(X, Y), data=d2)

my_cor_test <- function(x, y){
    n <- length(x)
    r <- cor(x, y)
    t <- r / (sqrt((1 - r**2) / (n - 2)))
    return(t)
}
with(my_cor_test(X, Y), data=d2)

nrow(d2)

qt(0.05, df=10)

qt(0.95, df=10)

2 * (1 - pt(q=3.1475, df=10))

my_cor <- function(x, y){
    sp.xy <- sum((x - mean(x)) * (y - mean(y)))
    sp.x <- sum((x - mean(x))**2)
    sp.y <- sum((y - mean(y))**2)
    return(sp.xy / sqrt(sp.x * sp.y))
}
with(my_cor(X, Y), data=d2)

my_cor_test <- function(x, y){
    n <- length(x)
    r <- my_cor(x, y)
    cat("r = ", r, "\n")
    t <- r / (sqrt((1 - r**2) / (n - 2)))
    cat("t = ", t, "\n")
    p_val <- 2 * (1 - pt(q=t, df=n - 2))
    cat("p = ", p_val, "\n")
    
}
with(my_cor_test(X, Y), data=d2)

devtools::session_info()
