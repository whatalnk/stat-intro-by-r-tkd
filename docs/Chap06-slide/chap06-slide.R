## ----setup, include=FALSE, purl=TRUE-------------------------------------
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
  # コードは Ricty regular
  code_font_family = "Ricty",
  code_font_size = "0.8em",
  link_color = "#A1C5AB"
)

## ------------------------------------------------------------------------
# library(multcomp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

## ----echo=FALSE, fig.height=5, fig.width=6-------------------------------
ggplot(data.frame(x = c(0, 15)), aes(x)) +
  stat_function(fun = df, args = list(df1 = 3, df2 = 44)) + 
  scale_x_continuous(limits=c(0, 5), name="F value") + 
  scale_y_continuous(limits=c(0, 0.8), name="prob. density") + 
  geom_vline(xintercept = qf(0.95, 3, 44), linetype="dotted") + 
  annotate(geom="text", label="F>2.816", x=3, y=0.2, hjust=0, vjust=0) + 
  theme_classic()

## ------------------------------------------------------------------------
d.lake <- readr::read_csv("../../samplecode/Rで学ぶ統計学入門図版作成用（改訂版）/付録/table6-3.csv")

glimpse(d.lake)

## ----lake-anova----------------------------------------------------------
d.lake %>% 
  mutate(lake = as.factor(lake)) %>% 
  aov(stron ~ lake, data=.) %>% 
  summary()

## ------------------------------------------------------------------------
# 群数
m <- 5
# 標本サイズ
n <- 6
# 群内平方和
(SS.W <- d.lake %>% 
  group_by(lake) %>% 
  summarise(x = sum((stron - mean(stron))^2)) %>% 
  summarise(SS.W = sum(x)) %>% 
  .[["SS.W"]])
# 群間平方和
(SS.B <- d.lake %>% 
    group_by(lake) %>% 
    summarise(x=mean(stron)) %>% 
    mutate(y = (x - mean(x))^2) %>% 
    summarise(SS.B = n * sum(y)) %>% 
    .[["SS.B"]]
)

## ----echo=TRUE, ref.label="lake-anova"-----------------------------------


## ------------------------------------------------------------------------
# 群内自由度
(df.B <- m - 1)

# 群内分散
(MS.B <- SS.B / df.B)

# 群間自由度
(df.W <- m * (n - 1))

# 群間分散
(MS.W <- SS.W / df.W)



## ----echo=TRUE, ref.label="lake-anova"-----------------------------------


## ------------------------------------------------------------------------
# F値
(F.value <- MS.B / MS.W)

# p値
1 - pf(F.value, df.B, df.W)


## ----echo=TRUE, ref.label="lake-anova"-----------------------------------


## ----message=FALSE-------------------------------------------------------
d.pig <- read_csv("../../samplecode/Rで学ぶ統計学入門図版作成用（改訂版）/付録/table6-4.csv")
glimpse(d.pig)
d.pig %>% filter(is.na(Pig)) 

## ----fig.height=4, fig.width=6-------------------------------------------
d.pig %>% 
  ggplot(aes(x=feed, y=Pig, group=feed)) + 
  geom_boxplot() + 
  theme_bw() + theme(panel.grid=element_blank())

## ------------------------------------------------------------------------
d.pig %>% 
  mutate(feed = as.factor(feed)) %>% 
  aov(Pig ~ feed, data=.) %>% 
  summary()

## ----fig.height=4, fig.width=6-------------------------------------------
d.pig %>% 
  ggplot(aes(x=feed, y=Pig, group=feed)) + 
  geom_boxplot() + 
  theme_bw() + theme(panel.grid=element_blank())

## ------------------------------------------------------------------------
d.lake %>% 
  mutate(lake = as.factor(lake)) %>% 
  aov(stron ~ lake, data=.) %>% 
  TukeyHSD()

## ----echo=FALSE, fig.height=5, fig.width=6-------------------------------
d.lake %>% 
  ggplot(aes(x = lake, y = stron, group=lake)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(
    panel.grid=element_blank()
  ) + 
  annotate(geom="text", label=c("a", "b", "b", "b", "c"), 
           x=c(1, 2, 3, 4, 5)+0.2, 
           y=c(37, 44, 47, 45, 61))

## ------------------------------------------------------------------------
d.lake %>% 
  mutate(lake = as.factor(lake)) %>% 
  aov(stron ~ lake, data=.) %>% 
  TukeyHSD()

## ----message=FALSE-------------------------------------------------------
d_67 <- read_csv("../../samplecode/Rで学ぶ統計学入門図版作成用（改訂版）/付録/table6-7.csv")
glimpse(d_67)

## ------------------------------------------------------------------------
res.d_67 <- d_67 %>% 
  mutate(group = as.factor(group)) %>% aov(score ~ group, data=.)
res.d_67 %>% summary()

## ----fig.height=4, fig.width=6-------------------------------------------
gp.d_67 <- ggplot(d_67, aes(x=group, y=score, group=group)) + 
  geom_boxplot() + 
  theme_bw() + theme(panel.grid=element_blank())
gp.d_67

## ------------------------------------------------------------------------
res.d_67 %>% TukeyHSD()

## ----fig.height=4, fig.width=6-------------------------------------------
gp.d_67 + 
  annotate(geom="text", label=c("ab", "a", "bc", "c"), x=c(1, 2, 3, 4)+0.1, y=23) 

## ----message=FALSE-------------------------------------------------------
d_68 <- read_csv("../../samplecode/Rで学ぶ統計学入門図版作成用（改訂版）/付録/table6-8.csv")
glimpse(d_68)

res.d_68 <- d_68 %>% mutate(group=as.factor(group)) %>% 
  aov(score ~ group, data=.)
res.d_68 %>% summary()

## ----fig.height=4, fig.width=6-------------------------------------------
gp.d_68 <- ggplot(d_68, aes(x=group, y=score, group=group)) +
  geom_boxplot() + 
  theme_bw() + theme(panel.grid=element_blank())
gp.d_68

## ------------------------------------------------------------------------
multcomp::glht(res.d_68, linfct = multcomp::mcp(group="Dunnett")) %>% 
  summary()

## ----fig.height=4, fig.width=6-------------------------------------------
gp.d_68

## ------------------------------------------------------------------------
d_67 %>% 
  {pairwise.t.test(.$score, .$group, p.adj="holm")}

## ----echo=FALSE, fig.height=4, fig.width=6-------------------------------
gp.d_67

## ----message=FALSE-------------------------------------------------------
d_69 <- read_csv("../../samplecode/Rで学ぶ統計学入門図版作成用（改訂版）/付録/table6-9.csv")
glimpse(d_69)

d_69 %>% {pairwise.t.test(.$score, .$group, p.adj="holm")} 

## ----fig.height=4, fig.width=6-------------------------------------------
ggplot(d_69, aes(x=group, y=score, group=group)) + 
  geom_boxplot() + 
  theme_bw() + theme(panel.grid=element_blank()) + 
  annotate(geom="text", label=c("a", "a", "ab", "b"), x=c(1, 2, 3, 4)+0.1, y=88)

## ------------------------------------------------------------------------
d.pig %>% mutate(feed=as.factor(feed)) %>% 
  summary()

## ------------------------------------------------------------------------
d.pig %>% group_by(feed) %>% 
  summarise(mean=mean(Pig, na.rm=TRUE), sd=sd(Pig, na.rm=TRUE))

## ----fig.height=4, fig.width=6-------------------------------------------
d.pig %>% ggplot(aes(x=feed, y=Pig, group=feed)) + 
  geom_boxplot() + 
  theme_bw() + theme(panel.grid=element_blank())

## ------------------------------------------------------------------------
d.pig %>% mutate(feed=as.factor(feed)) %>% 
  aov(Pig ~ feed, data=.) %>% 
  TukeyHSD()

## ------------------------------------------------------------------------
d.pig %>% 
  {pairwise.t.test(.$Pig, .$feed, p.adj= "holm")}

## ------------------------------------------------------------------------
d_69 %>% mutate(group=as.factor(group)) %>%
  summary()

## ------------------------------------------------------------------------
d_69 %>% group_by(group) %>% 
  summarise(mean=mean(score), sd=sd(score))

## ----fig.height=4, fig.width=6-------------------------------------------
d_69 %>% 
  ggplot(aes(x=group, y=score, group=group)) + 
  geom_boxplot() + 
  theme_bw() + theme(panel.grid = element_blank())

## ------------------------------------------------------------------------
d_69 %>% mutate(group=as.factor(group)) %>% 
  aov(score ~ group, data=.) %>% 
  multcomp::glht(linfct = multcomp::mcp(group="Dunnet"))

## ------------------------------------------------------------------------
d_69 %>% 
  {pairwise.t.test(.$score, .$group, p.adj="holm")}

