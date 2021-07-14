#####analyze .txt files from output of myosight#####

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

setwd("~/projects/myosight/1month/1month/x25/")

#######lists all text files recursively from current working directory#######
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = FALSE) 


df_1month <- vroom(list_of_files, id = "x25")
View(df)



setwd("~/projects/myosight/4month/x25/") 
#######lists all text files recursively from current working directory#######
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = FALSE) 


df_4month <- vroom(list_of_files, id = "x25")

setwd("~/projects/myosight/1yr Pax7 Laminin/10x/") 
#######lists all text files recursively from current working directory#######
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = FALSE) 


df_1year <- vroom(list_of_files, id = "10x")

names(df_1month)



View(df_1year)
########PUT ALL DATA INTO 1 PLOT and tidy it###########
df_1and4<- rbind(df_1month, df_4month) 
View(df_1and4)

Month_Genotype<- str_sub(df_1and4$x25, 1, 11)
Month_Genotype <- as.data.frame(Month_Genotype)
Month_Genotype<- str_replace_all(Month_Genotype$Month_Genotype, "mdxJ", "mdxJAG")

#View(Month_Genotype)

df_1and4<- cbind(df_1and4, Month_Genotype) 
df_1and4<- df_1and4 %>%  select(Month_Genotype, Label:Perinuclei) 

df_1and4<- df_1and4 %>% separate(Month_Genotype, c("Age", "Genotype"), sep = " ")
df_1and4$Age<- factor(df_1and4$Age)
df1_and4$Genotype <- factor(df_1and4$Genotype)



pannel <- ggboxplot(df_1and4, aes(x=Genotype, y=MinFeret, group= Genotype)) + 
  geom_boxplot(aes(fill=Genotype))
pannel + facet_grid(. ~ Age )+ 
  stat_compare_means(method="t.test") +
  ggtitle("Minimum Ferret Diamter")+
  xlab("")




names(df_1and4)[names(df_1and4) == 'Central Nuclei'] <- 'CentralNuclei' 
names(df_1and4)

View(df_1and4
     )

pannel2 <- ggboxplot(df_1and4, aes(x=Genotype, y=CentralNuclei, group= Genotype)) + 
  geom_boxplot(aes(fill=Genotype))
pannel2<-ggpar(pannel2, ylim = c(0, 10)) 
pannel2 + facet_grid(. ~ Age )+ 
  stat_compare_means(method="t.test",
                     label.x= 1.6,
                     label.y= 10) +
  ggtitle("Central Nuclei")+
  xlab("")+
  scale_fill_manual(values= c("red","blue"))

#############scatter version######
pannel3 <- ggboxplot(df_1and4, x = "Genotype", y = "MinFeret",
                               color = "Genotype", palette = c("red", "blue"), add= "jitter", )
            
pannel3<-pannel3 + facet_grid(. ~ Age )+ 
  stat_compare_means(method="t.test") +
  ggtitle("Minimum Ferret Diamter")+
  xlab("")+
  ylab("microns")
pannel3


grid.arrange(pbox_central, percent_p,nrow = 2)

############renames data to remove mouse number and image number to reduce to genotype value######

 Genotype<- str_sub(df$x25, 8, 11 ) 

View(Genotype)
class(Genotype)
Genotype<- as.data.frame(Genotype)


Genotype<- str_replace_all(Genotype$Genotype, "mdxJ", "mdxJAG") ######renames mdxJ to mdxJAG

View(Genotype)
df2<- cbind(df, Genotype) 
df2<- df2 %>%  select(Genotype, Label:Perinuclei) 


#####pvals####

View(df2)


#####method 1 
df2$Genotype <- factor(df2$Genotype)



# df = dataframe
# old.var.name = The name you don't like anymore
# new.var.name = The name you want to get

names(df2)[names(df2) == 'Central Nuclei'] <- 'CentralNuclei' ####remaning so stat_compare_mean works

View(df2)

stat.test_feret_wil<- compare_means(MinFeret~Genotype, data= df2) 
stat.test_Central_wil<-compare_means(CentralNuclei~Genotype, data = df2)
##########Box and wisker plot 

pbox_feret<- ggboxplot(df2, x = "Genotype", y = "MinFeret",
               color = "Genotype", palette = "jco", add= "jitter"
               )


pbox_feret<- pbox_feret+ stat_compare_means(method = "t.test") +
              ylab("Minimum Feret Diameter (uM)")+ 
              xlab(" ")+
              ggtitle("Minimum Feret Diameter at 1 Months")
              
      
          
pbox_feret




pbox_centralNucl<- ggboxplot (df_1and4, x = "Genotype", y = "CentralNuclei",
                       color = "Genotype", palette = "jco", add="jitter" 
)


pbox_centralNucl<- pbox_centralNucl+ stat_compare_means() +
  ylab("Central Nuclei")+ 
  xlab(" ")+
  ggtitle("Central Nuclei ")+
  facet_grid(. ~ Age )+
  scale_color_manual(values=c("red", "blue"))
  


pbox_central<- ggpar(pbox_centralNucl, ylim = c(0, 10)) + ####changes y limit
  stat_compare_means(method = "t.test",  #####type of t.test
                     label.x = 2.0, 
                     label.y = 10) ####adjust location


pbox_central
######Violin plot


p_violin <- ggplot(df2, aes(x=Genotype, y=MinFeret, color = Genotype)) + 
  geom_violin() + geom_boxplot(width=0.1) 
  

p_violin <- p_violin+ coord_flip()

###rmarkdown###

p_violin

######Histrogram (binning)

p<- ggplot(df2, aes(x=MinFeret))+
  geom_histogram(aes(color=Genotype),fill="white",
                 position= "identity", bins=10, alpha=0.5)+
  xlab("xlab")+
  ylab("ylab")+
  ggtitle(" title")
p

p<- ggplot(df2, aes(x=CentralNuclei))+
  geom_histogram(aes(color=Genotype),fill="white",
                 position= "identity", bins=5, alpha=0.2)+
  xlab("xlab")+
  ylab("ylab")+
  ggtitle(" title")


p

his3<- ggplot(df_1and4, aes(x=MinFeret))+
  geom_histogram(aes(color=Genotype),
                 position= "identity", bins=30, alpha=0.3)+
  theme(legend.position = c(.4,.8))+
  #xlab("xlab")+
  #ylab("ylab")+
  ggtitle(" Distribution of Minimum Ferret Diameter")+
  facet_grid(. ~ Age )+
  scale_color_manual(values=c("red", "blue"))+
  scale_fill_manual(values=c("red", "blue"))
  
 


his3















#######plot grouped 1 month mouse mdx vs mdxjag Min Feret ##########
p<-ggplot(data=group25_mdx_mdxjag, aes(x= rownames(group25_mdx_mdxjag), y=MinFeret_mean, fill= rownames(group25_mdx_mdxjag))) +#####fill adds colors and legend
  geom_bar(stat="identity")+
  xlab("p=1.76e-18")+
  ylab("Mean MinFeret Diameter(uM)")+
  geom_errorbar(aes(x=rownames(group25_mdx_mdxjag), ymin=MinFeret_mean-MinFeret_se, ymax=MinFeret_mean+MinFeret_se))+ #####error bar
  ggtitle("Minimum Ferret Diameter, 1 Month MDX vs mdxjag")
p <- p + guides(fill=guide_legend("genotype"))
p<- p + scale_fill_manual(breaks= c("mdx", "mdxJAG"), values=c("red", "blue") )#####renames 
p
 
#######plot grouped 1 mouse Central BAR PLOT
p1<-ggplot(data=group25_mdx_mdxjag, aes(x= rownames(group25_mdx_mdxjag), y=`Central Nuclei_mean`, 
                                        fill= rownames(group25_mdx_mdxjag))) +
  geom_bar(stat="identity")+
  xlab("e=1.93e-27")+
  ylab("Mean Central Nuclei")+
  geom_errorbar(aes(x=rownames(group25_mdx_mdxjag), ymin= `Central Nuclei_mean` - `Central Nuclei_se`, ymax=`Central Nuclei_mean`+ `Central Nuclei_se`))+
  ggtitle("Central Nuclei 1 Month MDX vs mdxjag")
p1 <- p1 + guides(fill=guide_legend("genotype")) 
  p1<- p1 + scale_fill_manual(breaks= c("mdx", "mdxJAG"), values=c("red", "blue") )
p1


#####subset by file name value contains MDX"""""" for bar plot data
group25_mdx<- df %>%
  filter(str_detect(x25, "mdx ")) %>% 
  dplyr::summarise_each(funs(mean, sd, se=std.error), MinFeret, `Central Nuclei`)
rownames(group25_mdx)<- "mdx" 

View(group25_mdx)

##########Subset by file name mdxJAG######

group25_mdxjag<- df %>%
  filter(str_detect(x25, "mdxJAG")) %>% 
  dplyr::summarise_each(funs(mean, sd, se=std.error), MinFeret, `Central Nuclei`)
rownames(group25_mdxjag)<- "mdxJAG" 

group25_mdx_mdxjag<- rbind(group25_mdx,group25_mdxjag)

View(group25_mdx_mdxjag)
######summarizes by each file name,, adds labels####### seperates by image
#group25_summary<- df %>%
  group_by_(x25) %>% 
  summarise_each(funs(mean, sd, se=std.error), MinFeret,`Central Nuclei`) 
  rownames(group25)<- (c("MDX3_1" ,  "MDX3_2" ,   "MDX3_3" , "mdxjag3_1", "mdxjag3_2", "mdxjag3_3"))

#View(group25_summary)
View(group25_mdx_mdxjag)

rownames(group25_mdx_mdxjag)
###### Minimum Ferret bar plot with standard error
p<-ggplot(data=group25_mdx_mdxjag, aes(x= rownames(group25_mdx_mdxjag), y=MinFeret_mean, 
          fill=rownames(group25_mdx_mdxjag))) +
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Mean MinFeret Diameter")+
  geom_errorbar(aes(x=rownames(group25_mdx_mdxjag), ymin=MinFeret_mean-MinFeret_se, ymax=MinFeret_mean+MinFeret_se))+
  ggtitle("Minimum Ferret Diameter (uM), 4 Month MDX vs mdxjag") + 
  scale_fill_brewer(palette="Set1")
  
p

#####Central Nuclei bar plot with SE######

p1<-ggplot(data=group25_mdx_mdxjag, aes(x=rownames(group25_mdx_mdxjag), y=`Central Nuclei_mean`)) +
  geom_errorbar(aes(x=rownames(group25_mdx_mdxjag), ymin= `Central Nuclei_mean` - `Central Nuclei_se`, ymax=`Central Nuclei_mean`+ `Central Nuclei_se`))+
  geom_bar(stat="identity") + 
  xlab("25x 1 month, n=1, 3 pictures")+
  ylab("Mean of Central Nuclei")+
  ggtitle("Central Nuclei,  1 Month MDX vs mdxjag")
p1

#######df2 to bar plot


                                         
