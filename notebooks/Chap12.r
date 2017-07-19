
binom.test(c(13, 7), p=1/2)

binom.test(c(65, 35), p=1/2)

d2 <- matrix(c(152, 39, 53, 6, 140.625, 46.875, 46.875, 15.625), nrow=2, ncol=4, byrow=TRUE)
d2

sum((d2[1,] - d2[2,])**2 / d2[2,])

qchisq(0.95, df=3)

chisq.test(d2[1,], p= d2[2,], rescale=TRUE)

pchisq(8.9724, df=3, lower.tail = FALSE)

d3 <- matrix(c(0:7, 
               0, 0, 60, 101, 84, 48, 7, 0, 
               9.35, 32.39, 56.20, 65.00, 56.39, 39.14, 22.63, 19.06), nrow=3, ncol=8, byrow=TRUE)
print(d3)

library(dplyr)

d3[c(1:2),] %>% apply(2, function(x){rep(x[1], x[2])}) %>% unlist() %>% 
    hist(br=seq(-0.5, 9.5, 1), plot=FALSE)

options(repr.plot.width=4, repr.plot.height=4)

d3[c(1:2),] %>% apply(2, function(x){rep(x[1], x[2])}) %>% unlist() %>% 
    hist(br=seq(-0.5, 9.5, 1), plot=TRUE)

egg <- d3[c(1:2),] %>% apply(2, function(x){rep(x[1], x[2])}) %>% unlist()
plt.d3 <-  egg %>% hist(br=seq(-0.5, 9.5, 1), plot=TRUE)
egg.expected <- length(egg) * dpois(c(0:9), mean(egg))
lines(plt.d3$mids, egg.expected)

res.d3 <- rbind(plt.d3$counts, egg.expected)
res.d3

rownames(res.d3) <- c("O", "E")
colnames(res.d3) <- plt.d3$mids
res.d3

res.d3[,8]

res.d3[,c(8:10)]

res.d3[, 8] <- apply(res.d3[,c(8:10)], 1, sum)
res.d3

res.d3 <- res.d3[, c(-9, -10)]
res.d3

chisq.test(res.d3["O",], p=res.d3["E",], rescale=TRUE)

pchisq(105.8636, df=6, lower.tail = FALSE)

chisq.test(res.d3["O",], p=res.d3["E",], rescale=TRUE, simulate.p.value=TRUE, B=10000)

d4 <- matrix(c(25, 15, 10, 30), nrow=2, ncol=2, byrow=TRUE)
print(d4)

rowSums(d4)

colSums(d4)

sum(d4)

d4.expected <- matrix(rep(rowSums(d4) *colSums(d4) / sum(d4), 2), nrow=2, ncol=2, byrow=TRUE)
print(d4.expected)

sum((d4 - d4.expected)**2 / d4.expected)

qchisq(0.05, df=1, lower.tail = FALSE)

chisq.test(d4)

chisq.test(d4, correct=FALSE)

hair <- matrix(c(32, 43, 16, 9, 55, 65, 64, 16), nrow=2, ncol=4, byrow=TRUE)
print(hair)

chisq.test(hair)

devtools::dev_mode(TRUE, "Chap12")

library("Deducer")

likelihood.test(hair)

devtools::dev_mode(FALSE)

shellfish.a <- matrix(c(12, 7, 2, 9), 2, 2, TRUE)
shellfish.a

shellfish.b <- matrix(c(13, 6, 1, 10), 2, 2, TRUE)

shellfish.c <- matrix(c(14, 5, 0, 11), 2, 2, TRUE)

fisher.test(shellfish.a, alternative = "greater")

fisher.test(shellfish.b, alternative = "greater")

fisher.test(shellfish.c, alternative = "greater")

fisher.test(hair)

ls()

d5 <- matrix(c(54, 10325, 25, 51790), 2, 2, TRUE)
d5

d5[1] * d5[4] / (d5[2] * d5[3])

log(d5[1]) + log(d5[4]) - log(d5[2]) - log(d5[3])

sqrt(1/d5[1] + 1/d5[2] + 1/d5[3] + 1/d5[4])

qnorm(c(0.025, 0.975))

2.382737 + qnorm(c(0.025, 0.975)) * 0.242145

exp(2.382737 + qnorm(c(0.025, 0.975)) * 0.242145)

fisher.test(d5, or=TRUE, conf.level = 0.95)

devtools::session_info()
