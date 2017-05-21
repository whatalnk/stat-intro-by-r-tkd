
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(Cairo)

d.stron <- data_frame(
    stron = c(28.2, 33.2, 36.4, 34.6, 29.1, 31.0, 
              39.6, 40.8, 37.9, 37.1, 43.6, 42.4, 
              46.3, 42.1, 43.5, 48.8, 43.7, 40.1, 
              41.0, 44.1, 46.4, 40.2, 38.6, 36.3, 
              56.3, 54.1, 59.4, 62.7, 60.0, 57.3), 
    lake = factor(sort(rep(1:5, 6)))
)
head(d.stron)

aov(stron ~ lake, data = d.stron) %>% summary()

(function(){
    m <- d.stron$stron %>% mean()
    
    SS.b <- d.stron %>% group_by(lake) %>% summarise_all(funs(stron = mean, n = n())) %>% 
        mutate(x = n * (stron - m)^2) %>% summarise(SS.b = sum(x)) %>% .[["SS.b"]]
    SS.w <- d.stron %>% group_by(lake) %>% mutate(x = (stron - mean(stron))^2) %>% 
        summarise_all(funs(sum)) %>% summarise(SS.w = sum(x)) %>% .[["SS.w"]]
    
    df_b <- d.stron$lake %>% unique %>% length %>% {. - 1}
    df_w <- d.stron %>% group_by(lake) %>% summarise(n = n()) %>% 
        summarise(df_w = sum(n - 1)) %>% .[["df_w"]]
    
    MS.b <- SS.b / df_b
    MS.w <- SS.w / df_w
    F_val <- MS.b / MS.w
    p_val <- 1 - pf(F_val, df_b, df_w)
    
    return(list("F_val" = F_val, "p_val" = p_val))
})()

d.pig <- data_frame(
    f1 = c(60.8, 57.0, 65.0, 58.6, 61.7),
    f2 = c(68.7, 67.7, 74.0, 66.3, 69.8), 
    f3 = c(102.6, 102.1, 100.2, 96.5, NA), 
    f4 = c(87.9, 84.2, 83.1, 85.7, 90.3)
)
d.pig

d.pig %>% gather(feed, Pig) %>% aov(Pig ~ feed, data = .) %>% summary()

devtools::session_info()
