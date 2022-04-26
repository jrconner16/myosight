#####analyze .txt files from output of myosight#####

library(vroom)
library(dplyr)
library(tidyr)
library(plotrix)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(stringr)

setwd("~/Desktop/1month/x25/")

#######lists all text files recursively from current working directory#######
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE) 


df <- vroom(list_of_files, id = "x25")
View(df)


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




############renames data to remove mouse number and image number to reduce to genotype value######

 Genotype<- str_sub(df$x25, 10, 13 ) 

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


pbox_feret<- pbox_feret+ stat_compare_means() +
              ylab("Minimum Feret Diameter (uM)")+ 
              xlab(" ")+
              ggtitle("Minimum Feret Diameter at 1 Month")
              
      
          
pbox_feret




pbox_centralNucl<- ggboxplot(df2, x = "Genotype", y = "CentralNuclei",
                       color = "Genotype", palette = "jco", add="jitter" 
)


pbox_centralNucl<- pbox_centralNucl+ stat_compare_means() +
  ylab("Central Nuclei")+ 
  xlab(" ")


pbox_centralNucl
######Violin plot

p_violin <- ggplot(df2, aes(x=Genotype, y=MinFeret, color = Genotype)) + 
  geom_violin() + geom_boxplot(width=0.1) 
  

p_violin <- p_violin+ coord_flip()



p_violin

######Histrogram (binning)

p<- ggplot(df2, aes(x=MinFeret))+
    geom_histogram(aes(color=Genotype),fill="white",
                    position= "identity", bins=25, alpha=0.2)+
  xlab("Min Feret (uM)")+
  ylab("Number of fibers")+
  ggtitle("Minimum Ferret Diameter, 1 Month MDX vs mdxjag")
  
p

p<- ggplot(df2, aes(x=CentralNuclei))+
  geom_histogram(aes(color=Genotype),fill="white",
                 position= "identity", bins=5, alpha=0.2)+
  xlab("xlab")+
  ylab("ylab")+
  ggtitle(" title")


p


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



######summarizes by each file name,, adds labels####### seperates by image
group25_summary<- df %>%
  group_by_(x25) %>% 
  summarise_each(funs(mean, sd, se=std.error), MinFeret,`Central Nuclei`) 
  rownames(group25)<- (c("MDX3_1" ,  "MDX3_2" ,   "MDX3_3" , "mdxjag3_1", "mdxjag3_2", "mdxjag3_3"))

View(group25_summary)



###### Minimum Ferret bar plot with standard error
p<-ggplot(data=group25, aes(x= rownames(group25), y=MinFeret_mean)) +
  geom_bar(stat="identity")+
  xlab("25x, n=1, 3 pictures")+
  ylab("Mean MinFeret Diameter")+
  geom_errorbar(aes(x=rownames(group25), ymin=MinFeret_mean-MinFeret_se, ymax=MinFeret_mean+MinFeret_se))+
  ggtitle("Minimum Ferret Diameter (uM), 1 Month MDX vs mdxjag")
p

#####Central Nuclei bar plot with SE######

p1<-ggplot(data=group25, aes(x=rownames(group25), y=`Central Nuclei_mean`)) +
  geom_errorbar(aes(x=rownames(group25), ymin= `Central Nuclei_mean` - `Central Nuclei_se`, ymax=`Central Nuclei_mean`+ `Central Nuclei_se`))+
  geom_bar(stat="identity") + 
  xlab("25x 1 month, n=1, 3 pictures")+
  ylab("Mean of Central Nuclei")+
  ggtitle("Central Nuclei,  1 Month MDX vs mdxjag")
p1


                                         
