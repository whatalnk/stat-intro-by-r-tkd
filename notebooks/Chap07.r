
library(dplyr)

library(tidyr)

library(ggplot2)

d.room <- data_frame(
    kaiteki = c(6, 5, 6, 7, 2, 1, 1, 0, 7, 8, 8, 9, 8, 7, 6, 7), 
    size = c(rep("S", 8), rep("L", 8)), 
    student = c(rep(1, 4), rep(2, 4), rep(1, 4), rep(2, 4))
)
head(d.room)

write_csv(d.room, "../data/table7-1.csv")

str(d.room)

d.room %>% aov(kaiteki ~ size + student + size:student, data = .) %>% summary()

options(repr.plot.width = 3, repr.plot.height = 3)

d.room %>% mutate(size=factor(size, levels=c("S", "L"), labels=c("S", "L")), 
                 student=factor(student)) %>% 
    group_by(size, student) %>% summarise(kaiteki = mean(kaiteki)) %>% 
    ggplot(aes(x = size, y = kaiteki, group=student, colour=student)) + geom_line() + geom_point()

d.room %>% mutate(size=factor(size, levels=c("S", "L"), labels=c("S", "L")), 
                 student=factor(student)) %>% 
    group_by(student, size) %>% summarise(kaiteki = mean(kaiteki)) %>% 
    ggplot(aes(x = student, y = kaiteki, group=size, colour=size)) + geom_line() + geom_point()

library(readr)

d.soil <- read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table7-4.csv", col_names = FALSE)
str(d.soil)

d.soil %>% select(plant=X1) %>% mutate(soil = c(rep(1, 10), rep(2, 10), rep(1, 10), rep(2, 10)), 
                  ft = c(rep(1, 20), rep(2, 20)))  -> d.soil
d.soil

write_csv(d.soil, "../data/table7-2.csv")

d.soil %>% aov(plant ~ soil + ft + soil:ft, data = .) %>% summary()

d.soil %>% mutate(soil = factor(soil, levels=c(1, 2), labels = c("natural", "agricultural")), ft = factor(ft, levels = c(1, 2), labels = c("C", "F"))) %>% 
    group_by(soil, ft) %>% summarise(plant=mean(plant)) %>% 
    ggplot(aes(x = soil, y = plant, group=ft, colour=ft)) + geom_line() + geom_point()

d.soil %>% group_by(soil, ft) %>% summarise(plant=mean(plant))

d.soil %>% select(-ft) %>% group_by(soil) %>% summarise(plant=mean(plant))

d.soil %>% select(-soil) %>% group_by(ft) %>% summarise(plant=mean(plant))

d.soil %>% summarise(plant=mean(plant)) 

d.soil %>% group_by(soil, ft) %>% summarise(x = mean(plant), n = n()) %>% 
    mutate(x = n * (x - 32.21)**2) %>% ungroup() %>% summarise(sum(x))

d.soil %>% group_by(soil) %>% summarise(x = mean(plant), n = n()) %>% mutate(x = n * (x - 32.21)**2) %>% summarise(sum(x))

d.soil %>% group_by(ft) %>% summarise(x = mean(plant), n = n()) %>% mutate(x = n * (x - 32.21)**2) %>% summarise(sum(x))

d.soil %>% group_by(soil, ft) %>% mutate(x = (plant - mean(plant))**2 ) %>% summarise(x = sum(x)) %>% ungroup() %>% summarise(x = sum(x))

calc_f_value <- function(df){
    cat("要因", "SS", "df", "MS", "F", "\n")
    n <- 10
    n.soil <- 2
    n.ft <- 2
    mean.soil.ft <- df %>% group_by(soil, ft) %>% summarise(plant=mean(plant)) %>% .[["plant"]]
    mean.soil <- df %>% select(-ft) %>% group_by(soil) %>% summarise(plant=mean(plant)) %>% .[["plant"]]
    mean.ft <- df %>% select(-soil) %>% group_by(ft) %>% summarise(plant=mean(plant)) %>% .[["plant"]]
    mean_ <- df %>% summarise(plant=mean(plant)) %>% .[["plant"]]
    
    Y.SS <- df %>% mutate(x = (plant - mean_)**2) %>% summarise(Y.SS = sum(x)) %>% .[["Y.SS"]]
    df.all <- n.soil * n.ft * n * 1

    among.SS <- df %>% group_by(soil, ft) %>% summarise(x = mean(plant), n = n()) %>% 
        mutate(x = n * (x - mean_)**2) %>% ungroup() %>% summarise(among.SS = sum(x)) %>% .[["among.SS"]]
    df.among <- n.soil * n.ft - 1
    among.MS <- among.SS / df.among
    cat("グループ間", among.SS, df.among, "\n")
    
    soil.SS <- df %>% group_by(soil) %>% summarise(x = mean(plant), n = n()) %>% 
        mutate(x = n * (x - mean_)**2) %>% summarise(soil.SS = sum(x)) %>% .[["soil.SS"]]
    df.soil <- n.soil - 1
    soil.MS <- soil.SS / df.soil
    
    ft.SS <- df %>% group_by(ft) %>% summarise(x = mean(plant), n = n()) %>% 
        mutate(x = n * (x - mean_)**2) %>% summarise(ft.SS = sum(x)) %>% .[["ft.SS"]]
    df.ft <- n.ft - 1
    ft.MS <- ft.SS / df.ft
    
    inter.SS <- among.SS - soil.SS - ft.SS
    df.inter <- (n.soil - 1) * (n.ft - 1)
    inter.MS <- inter.SS / df.inter
    
    within.SS <- df %>% group_by(soil, ft) %>% 
        mutate(x = (plant - mean(plant))**2 ) %>% summarise(x = sum(x)) %>% ungroup() %>% summarise(within.SS = sum(x)) %>% .[["within.SS"]]
    df.within <- n.soil * n.ft * (n - 1)
    within.MS <- within.SS / df.within
    
    F.soil <- soil.MS / within.MS
    cat("土壌効果", soil.SS, df.soil, soil.MS, F.soil, "\n")

    F.ft <- ft.MS / within.MS
    cat("施肥効果", ft.SS, df.ft, ft.MS, F.ft, "\n")

    F.inter <- inter.MS / within.MS
    cat("交互効果", inter.SS, df.inter, inter.MS, F.ft, "\n")
    
    cat("グループ内", within.SS, df.within, within.SS, "\n")
    
    cat("総合計", Y.SS, df.all, "\n")
}
calc_f_value(d.soil) 

d.pig <- read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table7-5.csv")
str(d.pig)

write_csv(d.pig, "../data/table7-5.csv")

head(d.pig)

options(repr.plot.width=6)

summary(d.pig)

d.pig %>% group_by(treat) %>% summarise(mean(wt))

d.pig %>% ggplot(aes(x = number, y = wt, colour=treat)) + geom_point() + 
    geom_hline(yintercept = 111.5, linetype=2, colour=2) + 
    geom_hline(yintercept = 125.06, linetype=2, colour=4) + 
    geom_hline(yintercept = 118.3, linetype=2)

d.pig %>% mutate(block=factor(block), treat=factor(treat)) %>% aov(wt ~ treat + Error(block / treat), data=.) %>% summary()

d.mouse <- data_frame(
    wt = c(55.4, 49.7, 52.1, 49.5, 53.2, 51.4, 54.3, 
          47.2, 49.4, 51.3, 54.5, 48.1, 50.8, 52.7,
          50.5, 48.2, 48.4, 52.1, 51.8, 49.7, 49.2, 
          47.3, 46.2, 48.8, 50.1, 48.2, 47.0, 46.5), 
    feed = c(rep("A", 7*2), rep("B", 7*2)), 
    gender = c(rep("M", 7), rep("F", 7), rep("M", 7), rep("F", 7))
)
head(d.mouse)

write_csv(d.mouse, "../data/table-ex7-2.csv")

options(repr.plot.width=3)

d.mouse %>% ggplot(aes(x = feed, y = wt, group=gender, colour=gender)) + geom_point()

d.mouse %>% mutate(feed=factor(feed), gender=factor(gender)) %>% group_by(feed, gender) %>% summarise(wt=mean(wt)) %>% 
    ggplot(aes(x=feed, y=wt, group=gender, colour=gender)) + geom_line() + geom_point()

d.mouse %>% aov(wt ~ feed + gender + feed:gender, data=.)

d.mouse %>% aov(wt ~ feed + gender + feed:gender, data=.) %>% summary()

d.exam <- data_frame(
    indiv = c(1:36), 
    score = c(78, 72, 81, 71, 74, 72, 
              85, 83, 79, 77, 75, 77, 
              83, 86, 74, 76, 73, 71, 
              66, 71, 72, 60, 58, 55, 
              58, 49, 62, 69, 70, 63, 
              65, 59, 54, 67, 66, 67), 
    juku = c(rep(1, 18), rep(2, 18)), 
    class = rep(c(rep("A", 6), rep("B", 6), rep("C", 6)), 2)
)
head(d.exam)

write_csv(d.exam, "../data/table-ex7-3.csv")

options(repr.plot.width = 6)

d.exam %>% ggplot(aes(x = indiv, y = score, shape=factor(juku), colour=class)) + geom_point()

d.exam %>% mutate(juku=factor(juku), class=factor(class)) %>% 
    aov(score ~ juku + Error(class / juku), data=.)

d.exam %>% mutate(juku=factor(juku), class=factor(class)) %>% 
    aov(score ~ juku + Error(class / juku), data=.) %>% summary()

devtools::session_info()
