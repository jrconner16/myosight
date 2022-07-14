####
####
#### packages
####
library(tidyverse)
library(vroom)
library(plotrix)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggpubr)

my_pallete<- brewer.pal(4, "GnBu")
brewer.pal(4, "GnBu")

list_of_files <- list.files(path = "./copy/", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE) 



df <- vroom(list_of_files, id="fileid")   #creats DF with folder path as column

df<- df %>% 
            mutate(id= str_sub(fileid, start= 16, end= 21)) %>% 
            mutate(group= str_extract(fileid, "AAV")) %>% 
            select(-Perinuclei)
df[is.na(df)] = "ctrl"

df<- df %>% 
          mutate(section= str_sub(fileid, -21, -21)) %>% 
          select(id, group, section, MinFeret) %>% 
          mutate_at(vars(id, group, section), factor) %>% 
          group_by(id, group) %>% 
          filter(MinFeret>800 & MinFeret <9000)




df_stat<- df %>% 
  mutate(mean=mean(MinFeret), se=std.error(MinFeret), sd=sd(MinFeret)) 


  

p_minFer_bygroup<-ggplot(df_stat, aes(x=df_stat$group, y=df_stat$mean, fill=id)) +
  scale_fill_manual(values= c("blue", "red", "grey", "green", "yellow", "orance"))+
  labs(title= "minimum Feret diameter", x="AAV v control", y='min Ferret')+
  geom_boxplot()+
  stat_compare_means(method="t.test")
p_minFer_bygroup

p_minFer_byMouse<-ggplot(df_stat, aes(x=df_stat$group, y=df_stat$MinFeret, fill=id)) +
  scale_fill_manual(values= c("blue", "red", "grey", "green", "yellow", "orance"))+
  labs(title= "minimum Feret diameter", x="AAV v control", y='min Ferret')+
  geom_()+
  stat_compare_means(method="t.test")
p_minFer_byMouse
 









df_bar<- df_stat %>% select( -MinFeret, -section) %>% unique() %>% mutate(mean2=mean/100, se2=se/100)
df_bar      



ggplot(df_bar, aes(x=group, y=mean2, label=id))+
  geom_boxplot(aes(fill=group)) +
  geom_point(position=position_jitter(width=0.5))+
  stat_compare_means(method="t.test",
                     label.x=1.8)+
  labs(title= "minimum Feret diameter", x=" ", y='minFeret uM')+
  
  scale_fill_manual(values=c("#ed4227", "#0f72e1"))+
  geom_text(check_overlap = TRUE,
            position=position_jitter(width=0.4))


pp<- ggplot(df_bar, aes(color=id))+
  geom_point(aes(x=group, y=mean2, fill=id))

pp

df_bar



p2<- ggplot(df_bar, aes(x=group, y=mean, fill=id))+
  geom_boxplot(stat="identity",
           color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) 
  
p2



