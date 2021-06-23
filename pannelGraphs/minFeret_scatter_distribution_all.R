
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

setwd("~/projects/myosight/")
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = FALSE) 
list_of_files

df<- vroom(list_of_files, id = "fileName")
View(df)

#########filters so each can be substringed differently

year<- df %>% 
    filter(str_detect(fileName, "1yr"))
View(year)

oneMonth<- df %>% 
  filter(str_detect(fileName, "1month"))
View(oneMonth)

fourMonth<- df %>% 
  filter(str_detect(fileName, "4month"))
View(fourMonth)


#######year substring and relabel#######
year_sub<- str_sub(year$fileName, 22, 32)
View(year_sub)
year_sub <- as.data.frame(year_sub)
year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #1", "mdx 1yr")
year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #2", "mdx 1yr")
year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #3", "mdx 1yr")
year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #4", "mdx 1yr")
year_sub <- as.data.frame(year_sub)
year_sub<- str_replace_all(year_sub$year_sub, "mdxJAG1 1yr", "mdxJAG 1yr")
View(year_sub)

year<- cbind(year, year_sub)
View(year)
year<- select(year, year_sub, Label:Perinuclei)

names(year)<- c("genotype_age","Label","Area","Feret","FeretX","FeretY","FeretAngle","MinFeret","CentralNuclei","TotalMyonuclei","Perinuclei")   
#################################################################################################################### year is labeled correctly ###########

##############oneMonth substring and relablel########################
View(oneMonth)
oneMonth_sub<- str_sub(oneMonth$fileName, 19, 29)
oneMonth_sub<- as.data.frame(oneMonth_sub)
oneMonth_sub<- str_replace_all(oneMonth_sub$oneMonth_sub, "1month mdxJ", "mdxJAG 1month")
oneMonth_sub<- as.data.frame(oneMonth_sub)
oneMonth_sub<- str_replace_all(oneMonth_sub$oneMonth_sub, "1month mdx", "mdx 1month")

oneMonth<- cbind(oneMonth, oneMonth_sub)

oneMonth<- select(oneMonth, oneMonth_sub, Label:Perinuclei)

names(oneMonth)<- c("genotype_age","Label","Area","Feret","FeretX","FeretY","FeretAngle","MinFeret","CentralNuclei","TotalMyonuclei","Perinuclei")   
View(oneMonth)
########################################################################################1 month is labeled correctly#######

####################################### fixing 4 month to be labeled correctly #########
View(fourMonth)
fourMonth_sub<- str_sub(fourMonth$fileName, 12, 22)
fourMonth_sub<- as.data.frame(fourMonth_sub)
fourMonth_sub<- str_replace_all(fourMonth_sub$fourMonth_sub, "4month mdxJ", "mdxJAG 4month")
fourMonth_sub<- as.data.frame(fourMonth_sub)
fourMonth_sub<- str_replace_all(fourMonth_sub$fourMonth_sub, "4month mdx", "mdx 4month")


fourMonth<- cbind(fourMonth, fourMonth_sub)

fourMonth<- select(fourMonth, fourMonth_sub, Label:Perinuclei)

names(fourMonth)<- c("genotype_age","Label","Area","Feret","FeretX","FeretY","FeretAngle","MinFeret","CentralNuclei","TotalMyonuclei","Perinuclei")   
View(fourMonth)
#############################################4 month labeled correctly ##########################################################################


################recombineds all and makes seperate colomns for genotype and age to make graphing possible ##################3
df<- rbind(oneMonth, fourMonth, year) 

df <-  separate(data= df, into=c("genotype", "age"), col = genotype_age, sep= " ")
View(df)
################################################################################################################################3
#####make into factors######

df$age<- factor(df$age)
df$genotype <- factor(df$genotype)

View(df)

##############################GRAPHING##################
##########GRAPHING#############GRAPHING#############
##########GRAPHING#############GRAPHING############
##########GRAPHING#############GRAPHING#############
##########GRAPHING#############GRAPHING#############

########### minFerret pannel by age ###############
pannel_minFeret <- ggboxplot(df, x = "genotype", y = "MinFeret",
                     color = "genotype", palette = c("red", "blue"), add= "jitter", )

pannel_minFeret<-pannel_minFeret + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")) )+ 
  stat_compare_means(method="t.test") +
  ggtitle("Minimum Ferret Diameter")+
  xlab("")+
  ylab("microns")
pannel_minFeret
############################################################
############minFerret disribution pannel by age ##########

his_minFeret<- ggplot(df, aes(x=MinFeret))+
  geom_histogram(aes(color=genotype),
                 position= "identity", bins=30, alpha=0.3)+
  theme(legend.position = c(.4,.8))+
  #xlab("xlab")+
  #ylab("ylab")+
  ggtitle(" Distribution of Minimum Ferret Diameter")+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  scale_color_manual(values=c("red", "blue"))+
  scale_fill_manual(values=c("red", "blue"))

his_minFeret
################################################################################
#################### COMBINED the two into super pannel#########################
################################################################################
grid.arrange(pannel_minFeret, his_minFeret, nrow=2 )
####################################################################################
################################################################################
################################################################################





