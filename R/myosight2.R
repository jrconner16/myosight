
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


########################percent binning instead of total binning)

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
  ggtitle("Minimum Feret Diamter")+
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
  ggtitle(" Distribution of Minimum Feret Diameter")+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  scale_color_manual(values=c("red", "blue"))+
  scale_fill_manual(values=c("red", "blue"))

his_minFeret


########333########3###################
##########################################3
#####histogram minFeret normalized to count total 
df_nucleiZero_groupBy_genotype_age<- group_by(df, genotype, age) %>% 
  mutate(n=n())
View(df_nucleiZero_groupBy_genotype_age)

minFeret_pannel_norm<-
  ggplot(df_nucleiZero_groupBy_genotype_age,aes(x=MinFeret,fill=genotype))+
  geom_histogram(aes(y=0.5*..density..*10),
                 alpha=0.3,position='identity', bins=20)+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+ 
  scale_fill_manual(values=c("red", "blue"))+
  ylab("percent")+
  theme(legend.position = c(.9,.85))+
  ggtitle("Distribution of minimum feret diameter")


minFeret_pannel_norm
#####################################
#####################################
################################################################################
#################### COMBINED the two into super pannel#########################
################################################################################
grid.arrange(pannel_minFeret, his_minFeret, nrow=2 )
####################################################################################
################################################################################
################################################################################


#CENTRAL NUCLEI GRAPH#alll fiberes
################################################################################
################################################################################
pannel_CentralNuclei <- ggboxplot(df, x="genotype", y="CentralNuclei",
                                  color = "genotype", palette = "jco", add="jitter" )+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  ylim(0,4.5)+
  stat_compare_means(method="t.test",
                     label.x= 1.6,
                     label.y= 4.5) +
  ggtitle("Central Nuclei (all fibers)")+
  xlab("")+
  scale_color_manual(values= c("red","blue"))
pannel_CentralNuclei




###############percent with full data set########################
###############################################################
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}
percentage<-
  df%>%
  group_by(genotype, age) %>%
  mutate(nn = sum(CentralNuclei != 0), n=n(), `percent`= nn/n) %>% 
  select(genotype, age,`percent`) %>% distinct()
percentage<- round_df(percentage, 2)
#########
View(percentage)
percentage<- as.data.frame(percentage)
class(percentage)
View(percentage)
percent_p<- ggplot(percentage, aes(x=genotype, y=`percent`, fill=genotype)) +
  geom_bar(stat = "identity") +
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  xlab(" ")+
  ggtitle(" % of Fibers containing central nuclei (all fibers) ")+
  theme(legend.position = "none")+
  scale_fill_manual(values= c("red", "blue"))+
  geom_text(aes(label=percent), size=6, vjust=0)
percent_p<- ggpar(percent_p, ylim= c(0,1))
percent_p

######filtered data by only including fibers that have nuclei########

df_nucleiZero<- filter(df, df$TotalMyonuclei!=0)
View(df_nucleiZero)



#########################################################################################
#same as above but the filtered versions
#########################################################################################
#########################################################################################
#########################################################################################

#CENTRAL NUCLEI GRAPH# ####filtered#########
################################################################################
################################################################################
pannel_CentralNuclei_filtered <- ggboxplot(df_nucleiZero, x="genotype", y="CentralNuclei",
                                  color = "genotype", palette = "jco", add="jitter" )+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  ylim(0,5)+
  stat_compare_means(method="t.test",
                     label.x= 1.6,
                     label.y= 5) +
  ggtitle("Central Nuclei (when a nucleus is detected by Myosight)")+
  xlab("")+
  scale_color_manual(values= c("red","blue"))
pannel_CentralNuclei_filtered


pannel_periNuclei_filtered <- ggboxplot(df_nucleiZero, x="genotype", y="",
                                           color = "genotype", palette = "jco", add="jitter" )+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  ylim(0,5)+
  stat_compare_means(method="t.test",
                     label.x= 1.6,
                     label.y= 5) +
  ggtitle("Central Nuclei (when a nucleus is detected by Myosight)")+
  xlab("")+
  scale_color_manual(values= c("red","blue"))
pannel_periNuclei_filtered


###############percent with filtered data########################
###############################################################
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}
percentage<-
  df_nucleiZero%>%
  group_by(genotype, age) %>%
  mutate(nn = sum(CentralNuclei != 0), n=n(), `percent`= nn/n) %>%
  select(genotype, age,`percent`) %>% distinct()
percentage<- round_df(percentage, 2)
#########
View(percentage)
percentage<- as.data.frame(percentage)
class(percentage)
View(percentage)
percent_p_filtered<- ggplot(percentage, aes(x=genotype, y=`percent`, fill=genotype)) +
  geom_bar(stat = "identity") +
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  xlab(" ")+
  ggtitle(" % of Fibers containing central nuclei (when a nucleus is detected by Myosight) ")+
  theme(legend.position = "none")+
  scale_fill_manual(values= c("red", "blue"))+
  geom_text(aes(label=percent), size=6, vjust=0)
percent_p_filtered<- ggpar(percent_p_filtered, ylim= c(0,1))
percent_p_filtered


grid.arrange(pannel_minFeret, his_minFeret, pannel_CentralNuclei_filtered, percent_p_filtered)


grid.arrange(pannel_minFeret, minFeret_pannel_norm, pannel_CentralNuclei_filtered, percent_p_filter)


grid.arrange(pannel_CentralNuclei, pannel_CentralNuclei_filtered, percent_p, percent_p_filter, nrow=2)



