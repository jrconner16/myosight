######TPM3 
####goals are minferret, CSA, fiber type %

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

my_pallete<- brewer.pal(4, "GnBu")
brewer.pal(4, "GnBu")

list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE) 



df <- vroom(list_of_files, id="file")   #creats DF with folder path as column

#################3creates a genotype or in this blinded case a color column#####
df_black<- df %>% 
    mutate(Color= str_extract(df$file, "BLACK")) %>%   ####cleans file path column
    filter(Color=="BLACK") %>% 
    select("file", "Color", "Label", "Area", "MinFeret")
df_red<- df %>% 
  mutate(Color= str_extract(df$file, "RED")) %>% 
  filter(Color=="RED") %>% 
  select("file","Color", "Label", "Area", "MinFeret")
df<- rbind(df_black, df_red)

df$id <- cumsum(!duplicated(df[1])) ######creates ID column based on unique file names
View(df)
###############################################################
##########################PLOT#################################
###############################################################

######factor for plots###############
df<-df %>%
  mutate_at(vars(id, Color), factor)

df_grouped<- group_by(df, id, Color)

minFeret_theme<- function(){
}
  
####minFeret 
      ######stats
      df_stat<- df_grouped %>% 
                mutate(mean=mean(MinFeret), se=std.error(MinFeret), sd=sd(MinFeret)) 

    

p_minFer_byLabel<-ggplot(df_stat, aes(x=df_stat$Color, y=df_stat$MinFeret, fill=Label)) +
          geom_boxplot()+
          labs(title= "minimum Feret diameter", x="genotypes", y='minFere(uM')+
          scale_fill_discrete(name = "Fiber Type")
p_minFer_byLabel


p_minFer_byColor<-ggplot(df_stat, aes(x=df_stat$Color, y=df_stat$MinFeret, fill=Color)) +
  scale_fill_manual(values= c("grey", "red"))+
  labs(title= "minimum Feret diameter", x="genotypes", y='minFere(uM')+
  geom_boxplot()+
 
  minFeret_theme()
p_minFer_byColor

grid.arrange(p_minFer_byColor, p_minFer_byLabel)

