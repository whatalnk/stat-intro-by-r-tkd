## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(dpi = 320)

## ----xaringan-themer, include=FALSE--------------------------------------
library(xaringanthemer)
mono_light(
  base_color = "#1c5253",
  # header: family は 源ノ角ゴシック，weight は bold
  header_font_family = "Source Han Sans JP",
  header_font_weight = "Bold", 
  # 本文: family は 源ノ角ゴシック，weight は Regular
  text_font_family = "Source Sans Pro", 
  text_font_family_fallback = "Source Han Sans JP", 
  text_font_size = "18px", 
  # コードは Ricty regular
  code_font_family = "Ricty",
  code_font_size = "0.8em",
  link_color = "#A1C5AB"
)

## ------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)

## ------------------------------------------------------------------------
d.room <- data_frame(
    kaiteki = c(6, 5, 6, 7, 2, 1, 1, 0, 7, 8, 8, 9, 8, 7, 6, 7), 
    size = c(rep("S", 8), rep("L", 8)), 
    student = c(rep(1L, 4), rep(2L, 4), rep(1L, 4), rep(2L, 4))
)
head(d.room)

## ------------------------------------------------------------------------
d.room %>% mutate(student=as.factor(student), size=as.factor(size)) %>% summary()

## ----fig.height=4, fig.width=6-------------------------------------------
d.room %>% mutate(student=as.factor(student), size=as.factor(size)) %>% 
  ggplot(aes(x=size, y=kaiteki, group=size)) + 
  geom_boxplot() + 
  geom_jitter(aes(colour=student), width=0.2)

## ------------------------------------------------------------------------
d.room %>% 
  aov(kaiteki ~ size + student + size:student, data = .) %>% 
  summary()

## ------------------------------------------------------------------------
# 左（図7.2(a), 図7.3）
gp1 <- d.room %>% 
  mutate(size=factor(size, levels=c("S", "L"), labels=c("S", "L")), 
         student=factor(student)) %>% 
  group_by(size, student) %>% 
  summarise(kaiteki = mean(kaiteki)) %>% 
  ggplot(aes(x=size, y=kaiteki, group=student, colour=student)) + 
  geom_line() + geom_point()

# 右（図7.2(b)）
gp2 <- d.room %>% 
  mutate(size=factor(size, levels=c("S", "L"), labels=c("S", "L")), 
         student=factor(student)) %>% 
  group_by(student, size) %>% 
  summarise(kaiteki = mean(kaiteki)) %>% 
  ggplot(aes(x=student, y=kaiteki, group=size, colour=size)) + 
  geom_line() + geom_point()


## ----echo=FALSE, fig.height=4, fig.width=6-------------------------------
grid.arrange(gp1, gp2, ncol=2)

## ----message=FALSE-------------------------------------------------------
d.soil <- read_csv("../../samplecode/Rで学ぶ統計学入門図版作成用（改訂版）/付録/table7-2.csv")
head(d.soil)

d.soil %>% mutate(soil=as.factor(soil), ft=as.factor(ft)) %>% summary()

## ----fig.height=4, fig.width=6-------------------------------------------
d.soil %>% mutate(soil=as.factor(soil), ft=as.factor(ft)) %>% 
  ggplot(aes(x=soil, y=plant, group=soil)) + 
  geom_boxplot() + 
  geom_jitter(aes(colour=ft), width = 0.2)

## ----d-soil-aov----------------------------------------------------------
d.soil %>% 
  aov(plant ~ soil + ft + soil:ft, data = .) %>% 
  summary()

## ----fig.height=4, fig.width=6-------------------------------------------
d.soil %>% 
  mutate(soil = factor(soil, levels=c(1, 2), labels = c("natural", "agricultural")), 
         ft = factor(ft, levels = c(1, 2), labels = c("C", "F"))) %>% 
  group_by(soil, ft) %>% summarise(plant=mean(plant)) %>% 
  ggplot(aes(x = soil, y = plant, group=ft, colour=ft)) + 
  geom_line() + geom_point()

## ------------------------------------------------------------------------
d.soil %>% 
  group_by(soil, ft) %>% 
  summarise(plant=mean(plant)) %>% 
  ungroup()

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
d.soil %>% select(-ft) %>% group_by(soil) %>% summarise(plant=mean(plant))

## ------------------------------------------------------------------------
d.soil %>% select(-soil) %>% group_by(ft) %>% summarise(plant=mean(plant))

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
(mean.Y <- d.soil %>% 
   summarise(mean.Y=mean(plant)) %>% 
   .[["mean.Y"]])

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
(SS.Y <- d.soil %>% 
   mutate(x = (plant - mean.Y)**2) %>% 
   summarise(SS.Y = sum(x)) %>% 
   .[["SS.Y"]])

(df.Y <- 2*2*10-1)

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
(SS.among <- d.soil %>% 
   group_by(soil, ft) %>% 
   summarise(x = mean(plant), n = n()) %>% 
   mutate(x = n * (x - mean.Y)**2) %>% 
   ungroup() %>% 
   summarise(SS.among = sum(x)) %>% 
   .[["SS.among"]])
(df.among <- 2*2-1)

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
(SS.soil <- d.soil %>% 
   group_by(soil) %>% 
   summarise(x = mean(plant), n = n()) %>% 
   mutate(x = n * (x - mean.Y)**2) %>% 
   summarise(SS.soil = sum(x)) %>% 
   .[["SS.soil"]])

(df.soil <- 2 - 1)

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
(SS.ft <- d.soil %>% 
  group_by(ft) %>%
  summarise(x = mean(plant), n = n()) %>% 
  mutate(x = n * (x - mean.Y)**2) %>% 
  summarise(SS.ft = sum(x)) %>% 
  .[["SS.ft"]])
(df.ft <- 2 - 1)

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
(SS.inter <- SS.among - SS.soil - SS.ft)
(df.inter <- (2 - 1) * (2 - 1))

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
(SS.within <- d.soil %>% 
   group_by(soil, ft) %>% 
   mutate(x = (plant - mean(plant))**2 ) %>% 
   summarise(x = sum(x)) %>% ungroup() %>% 
   summarise(SS.within = sum(x)) %>% 
   .[["SS.within"]])
(df.within <- 4 * (10 - 1))

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------

MS.soil <- SS.soil / df.soil
MS.ft <- SS.ft / df.ft
MS.inter <- SS.inter / df.inter
MS.within <- SS.within / df.within

## ------------------------------------------------------------------------
(Fval.soil <- MS.soil / MS.within)
(Fval.ft <- MS.ft / MS.within)
(Fval.inter <- MS.inter / MS.within)

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ------------------------------------------------------------------------
# soil
1 - pf(Fval.soil, df.soil, df.within)
# ft
1 - pf(Fval.ft, df.ft, df.within)
# interaction
1 - pf(Fval.inter, df.inter, df.within)

## ----echo=TRUE, ref.label="d-soil-aov"-----------------------------------


## ----message=FALSE-------------------------------------------------------
d.pig <- read_csv("../../samplecode/Rで学ぶ統計学入門図版作成用（改訂版）/付録/table7-5.csv")
head(d.pig)

## ----fig.height=4, fig.width=6-------------------------------------------
pig.mean <- d.pig$wt %>% mean()
pig.mean.f <- filter(d.pig, treat=="f")$wt %>% mean()
pig.mean.c <- filter(d.pig, treat=="c")$wt %>% mean()
d.pig %>% ggplot(aes(x = number, y = wt, colour=treat)) + geom_point() + 
    geom_hline(yintercept = pig.mean.c, linetype=2, colour=2) + 
    geom_hline(yintercept = pig.mean.f, linetype=2, colour=4) + 
    geom_hline(yintercept = pig.mean, linetype=2)

## ------------------------------------------------------------------------
d.pig %>% 
  mutate(block=as.factor(block), treat=as.factor(treat)) %>% 
  aov(wt ~ treat + Error(block / treat), data=.) %>% 
  summary()

## ------------------------------------------------------------------------
d.mouse <- data_frame(
    wt = c(55.4, 49.7, 52.1, 49.5, 53.2, 51.4, 54.3, 
          47.2, 49.4, 51.3, 54.5, 48.1, 50.8, 52.7,
          50.5, 48.2, 48.4, 52.1, 51.8, 49.7, 49.2, 
          47.3, 46.2, 48.8, 50.1, 48.2, 47.0, 46.5), 
    feed = c(rep("A", 7*2), rep("B", 7*2)), 
    gender = c(rep("M", 7), rep("F", 7), rep("M", 7), rep("F", 7))
)
head(d.mouse)

## ----fig.height=4, fig.width=6-------------------------------------------
d.mouse %>% 
  mutate(feed=factor(feed), gender=factor(gender)) %>% 
  group_by(feed, gender) %>% 
  summarise(wt=mean(wt)) %>% 
  ggplot(aes(x=feed, y=wt, group=gender, colour=gender)) + 
  geom_line() + geom_point()

## ------------------------------------------------------------------------
d.mouse %>% aov(wt ~ feed + gender + feed:gender, data=.) %>% summary()

## ------------------------------------------------------------------------
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

## ----fig.height=4, fig.width=6-------------------------------------------
d.exam %>% mutate(juku=as.factor(juku), class=as.factor(class)) %>% 
  ggplot(aes(x = indiv, y = score, shape=juku, colour=class)) + 
  geom_point()

## ------------------------------------------------------------------------
d.exam %>% mutate(juku=factor(juku), class=factor(class)) %>% 
  aov(score ~ juku + Error(class / juku), data=.) %>% 
  summary()

