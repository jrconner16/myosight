
library(vroom)
library(dplyr)
library(tidyr)
library(plotrix)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(stringr)
library(gtable)
library(gridExtra)
library(RColorBrewer)

setwd("~/projects/myosight/lamaninTest/")
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = FALSE) 
#list_of_files


df<- vroom(list_of_files, id = "fileName")

df1<-df$fileName 
df1<- as.data.frame(df1)

df2<- df1 %>% mutate(genotype=str_extract(df1, "Jag1mdx")) %>% na.omit()
df3<- df1 %>% mutate(genotype=str_extract(df1, "mdx5cv")) %>% na.omit()

df_genotype<- rbind(df2, df3)
genotype<- df_genotype %>% select(genotype)

df<- df %>% select(MinFeret) %>% cbind(genotype) %>% select(genotype, MinFeret) %>% group_by(genotype)


class(df)
df_stat<- df %>%  mutate(mean=mean(MinFeret), se=std.error(MinFeret), sd=sd(MinFeret)) %>% 
          select(genotype, mean, se, sd)
unique(df_stat)
df
brewer.pal(2, name= "Paired")

p<- ggplot(df, aes(x=genotype, y=MinFeret, fill=genotype))+
  geom_boxplot()+
  stat_compare_means(method="t.test")+
  ggtitle("1 year minFeret")
  
p

