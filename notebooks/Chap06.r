
# # 6章 一元配置分散分析・多重比較

# In[1]:


library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(Cairo)

# ## 6.1 t検定の繰り返しは誤り

# In[1]:


(1 - 0.05)**3

# ## 6.2 一元配置分散分析: 原理
# 
# 群内分散と群間分散

# ## 6.3 Rで計算

# In[2]:


d.stron <- data_frame(
    stron = c(28.2, 33.2, 36.4, 34.6, 29.1, 31.0, 
              39.6, 40.8, 37.9, 37.1, 43.6, 42.4, 
              46.3, 42.1, 43.5, 48.8, 43.7, 40.1, 
              41.0, 44.1, 46.4, 40.2, 38.6, 36.3, 
              56.3, 54.1, 59.4, 62.7, 60.0, 57.3), 
    lake = factor(sort(rep(1:5, 6)))
)
head(d.stron)

# In[3]:


aov(stron ~ lake, data = d.stron) %>% summary()

# 手動計算
# 
# * `m`: 群の総数
# * `n`: 各群の標本サイズ
# * `SS.b`: 群間平方和
# * `SS.w`: 群内平方和
# * `MS.b`, `MS.w`: 平均平方

# In[4]:


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

# In[5]:


d.pig <- data_frame(
    f1 = c(60.8, 57.0, 65.0, 58.6, 61.7),
    f2 = c(68.7, 67.7, 74.0, 66.3, 69.8), 
    f3 = c(102.6, 102.1, 100.2, 96.5, NA), 
    f4 = c(87.9, 84.2, 83.1, 85.7, 90.3)
)
d.pig

# In[6]:


d.pig %>% gather(feed, Pig) %>% aov(Pig ~ feed, data = .) %>% summary()

# ### Tuket HSD

# In[7]:


aov(stron ~ lake, data = d.stron) %>% summary()

# 標準誤差 SE は，
# 
# $SE = \sqrt{\frac{残差MS}{n}}$

# In[9]:


sqrt(9.8 / 6)

# 統計量 $q$ は，5 と 1とでは，
# 
# $|q| = \frac{\bar{X}_5 - \bar{X}_1}{SE}$

# In[8]:


aov(stron ~ lake, data = d.stron) %>% TukeyHSD()

# * スチューデント化されたq とは?

# In[84]:


d.stron %>% group_by(lake) %>% summarise_each(funs(mean)) %>% 
    {
        l <- list()
        for(i in 1:5){
            l[[.$lake[i]]] = .$stron[i]
        }
        comb <- combn(.$lake, 2)
        for (i in 1:ncol(comb)){
            xy <- comb[,i]
            cat(xy[1], "-", xy[2], ": ", (l[[xy[1]]] - l[[xy[2]]]), "\n")
        }
    }

# ## 多重比較
# 
# * 検定の多重性
#     * 有意水準の調整
# * 事前比較，事後比較
# 
# 種類
# 
# * Tukey-Kramer
# * Dunnett
# * Scheffe
# * Williams

# ### 6.4.1 Tukey-Kramer

# * 表6.7（微妙に値が違う）

# In[64]:


d <- read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table6-5.csv") 
head(d)

# In[65]:


aov(score ~ factor(group), data=d) %>% summary()

# In[66]:


aov(score ~ factor(group), data=d)

# In[67]:


aov(score ~ factor(group), data=d) %>% TukeyHSD()

# g1 と g2 が逆

# In[68]:


d %>% tibble::rownames_to_column() %>% spread(group, score) %>% select(-1) %>% sapply(function(x){
    na.omit(x)
})

# In[73]:


d %>% mutate(group = case_when(.$group == "g1" ~ "g2", 
                              .$group == "g2" ~ "g1", 
                              TRUE ~ .$group)) -> d

# In[74]:


aov(score ~ factor(group), data = d) %>% TukeyHSD()

# In[75]:


options(repr.plot.width = 3, repr.plot.height = 3)

# In[84]:


d %>% ggplot(aes(x = group, y = score)) + geom_boxplot()

# ### 6.4.2 Dunnett

# 表6.8

# In[85]:


d2 <- read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table6-6.csv")
head(d2)

# In[86]:


str(d2)

# In[91]:


d2 %>% tibble::rownames_to_column("id") %>% spread(group, score) %>% select(-id) %>% sapply(function(x){
    na.omit(x)
})

# In[98]:


library(multcomp)

# In[102]:


detach(package:multcomp)

# In[103]:


detach(package:TH.data)

# In[104]:


detach(package:MASS)

# In[105]:


select

# In[106]:


d2 %>% 
    mutate(group = factor(group)) %>% 
    aov(score ~ group, data=.) %>% 
    multcomp::glht(linfct=multcomp::mcp(group="Dunnett")) %>% summary()

# In[108]:


d2 %>% ggplot(aes(x = group, y = score)) + geom_boxplot()

# ### 6.4.3 Bonferroni (Holm)

# 表6.9

# In[109]:


d3 <- read_csv("../samplecode/Rで学ぶ統計学入門図版作成用/table6-7.csv")
head(d3)

# In[110]:


d3 %>% tibble::rownames_to_column("id") %>% spread(group, score) %>% select(-id) %>% sapply(function(x){
    na.omit(x)
})

# In[115]:


d3 %>% ggplot(aes(group, score)) + geom_boxplot()

# 表6.7 のデータ

# In[113]:


d %>% {pairwise.t.test(.$score, .$group, p.adj = "holm")}

# 表6.9 のデータ

# In[114]:


d3 %>% {pairwise.t.test(.$score, .$group, p.adj = "holm")}

# In[116]:


ls()

# ## 演習問題
# ### ex. 6.1

# In[117]:


str(d.pig)

# In[123]:


d.pig %>% gather(feed, weight) %>% ggplot(aes(feed, weight)) + geom_boxplot()

# In[120]:


d.pig %>% gather(feed, weight) %>% aov(weight ~ factor(feed), data=.) %>% TukeyHSD()

# In[121]:


d.pig %>% gather(feed, weight) %>% {pairwise.t.test(.$weight, .$feed, p.adj = "holm")}

# ### ex. 6.2

# In[124]:


str(d3)

# In[126]:


d3 %>% 
    mutate(group = if_else(group == "g1", "C", group)) %>% 
    mutate(group = factor(group)) %>% 
    aov(score ~ group, data=.) %>% 
    multcomp::glht(linfct=multcomp::mcp(group="Dunnett")) %>% summary()

# In[128]:


d3 %>% 
    mutate(group = if_else(group == "g1", "C", group)) %>% 
    {pairwise.t.test(.$score, .$group, p.adj = "holm")}

# In[69]:


devtools::session_info()
