
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
#list_of_files

df<- vroom(list_of_files, id = "fileName")
#View(df)

#########filters so each can be substringed differently

#########

df1<-df$fileName 
df1<- as.data.frame(df1)
#View(df1)
df2<-df1 %>% 
  mutate(age = str_extract(df1, "\\d+(yr|month)"),  
         genotype = str_extract(df1, "\\bmdx\\w*"),
         mice    = str_extract(df1, "#\\d+"),
         section = str_extract(df1, "(?<=\\d(x|X)_)\\d+"))
#View(df2)
#View(df)

df<- cbind(df2, df) 

df<- select(df, age:Perinuclei, -fileName) 
#View(df)

names(df)<- c("age", "genotype","mice", "section", "...2", "Label","Area","Feret","FeretX","FeretY","FeretAngle","MinFeret","CentralNuclei","TotalMyonuclei","Perinuclei")


df$age<- factor(df$age)
df$genotype <- factor(df$genotype)
df$mice <- factor(df$mice)
df$section<-factor(df$section)
df_grouped_withSection<- group_by(df, age, mice, genotype,section)

df_grouped_withoutSection<- group_by(df, age, genotype)
View(df_grouped_withoutSection)


#########min feret stats and graphing ######################
df_stat<- mutate(df, mean=mean(MinFeret), se=std.error(MinFeret), sd=sd(MinFeret)) %>% 
          select(age, genotype, mean, se, sd)

View(df_stat)

df_stat1<- unique(df_stat)
View(df_stat1)

p_stat1 <- ggboxplot(df_stat1, x = "genotype", y = "mean",
                             color = "genotype", palette = c("red", "blue"))
          

p_stat1<-p_stat1 + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")) )+

  stat_compare_means(method="t.test", 
                     label.x=.75,
                     label.y=40) +
  ggtitle("Minimum feret's diamter by myofiber")+
  xlab("")+
  ylab("microns")
p_stat1

p_stat_mice <- ggboxplot(df_stat1, x = "genotype", y = "mean",
                     color = "genotype", palette = c("red", "blue"))


p_stat_mice<-p_stat2 + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")) )+
  
  stat_compare_means(method="t.test", 
                     label.x=.75,
                     label.y=3) +
  ggtitle("Minimum Feret Diamter stats by mouse")+
  xlab("")+
  ylab("microns")
p_stat_mice

p_stat_image <- ggboxplot(df_stat1, x = "genotype", y = "mean",
                         color = "genotype", palette = c("red", "blue"))


p_stat_image<-p_stat_image + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")) )+
  
  stat_compare_means(method="t.test", 
                     label.x=.75,
                     label.y=3) +
  ggtitle("Minimum Feret Diamter stat's by section")+
  xlab("")+
  ylab("microns")
p_stat_image

grid.arrange(p_stat1, p_stat_mice, p_stat_image, ncol=3)

#######################################################################3
###############################################################333####33
####################central nuclei stats and graphing #########
df_stat<- mutate(df_grouped_withoutSection, mean=mean(CentralNuclei), se=std.error(CentralNuclei), sd=sd(CentralNuclei)) %>% 
    select(age, genotype, mean, se, sd)

stat_check<- 

View(df_stat)
df_stat<- unique(df_stat)
View(df_stat)
View(df)
df_nuc_stat<- df %>% select(CentralNuclei, genotype, age) 
                    

View(df_nuc_stat)


df_nuc_stat$age <- factor(df_nuc_stat$age, levels= c("1month", "4month", "1yr"))

head(df_nuc_stat)



groups(df_nuc_stat)
stat.test <- df_nuc_stat %>%
  group_by(age) %>%
  t_test(CentralNuclei ~ genotype) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


# Create a box plot
bxp <- ggboxplot(
  df_nuc_stat, x = "age", y = "CentralNuclei", 
  color = "genotype", palette = c("red", "blue")
)
bxp

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "age", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)
bxp + stat_pvalue_manual(
  stat.test,  label = "{p.adj.signif}", 
  tip.length = 0, hide.ns = TRUE )
   ########################## BAR PLOT

# Create a bar plot with error bars (mean +/- sd)



bp <- ggbarplot(
  df_nuc_stat, x ="age", y = "CentralNuclei", add = "mean_se", 
  color= "genotype", palette = c("red", "blue"),
  position = position_dodge(0.8)) 

# Add p-values onto the bar plots
stat.test <- stat.test %>%
  add_xy_position(fun = "mean_se", x = "age", dodge = 0.8) 
bp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length =0 
)

bp + stat_pvalue_manual(
  stat.test,  label = "p.adj.signif", tip.length = 0,
  bracket.nudge.y = 0) + ggtitle("Central nuclei per myofiber")

##############################################################################

# Move down the brackets using `bracket.nudge.y`

##############################


# Create a box plot
bxp <- ggboxplot(
  df_nuc_stat, x = "genotype", y = "CentralNuclei", ylim= c(0,8),
  color = "genotype", palette = c("red", "blue"), 
  
)
bxp + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "age", dodge = 0.8)
bxp + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+ 
  stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)





p<- ggbarplot(df_stat, x="genotype", y="mean", color="genotype", palette=c("red", "blue"))

              
p<- p + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))

################################################################################################
p2<- ggplot(df_stat, aes(x=genotype, y=mean, fill=genotype))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) 


p2<- p2 + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))
p2<- p2 + ggtitle("Central nuclei per miofiber")+ 
          scale_fill_manual("legend", values = c("mdx" = "red", "mdxJAG1" = "blue"))


p2


################################################################################################
p_stat_cen <- ggboxplot(df_nuc_stat, x = "genotype", y = "CentralNuclei", 
                          color = "genotype", palette = c("red", "blue"))+
            
              
                                          
p_stat_cen
       





p_stat_cen1<- p_stat_cen+ facet_grid(df_nuc_stat, ~factor(age, levels= c("1month", "4month", "1yr")) )+
  stat_compare_means(method="t.test")+
  ggtitle("Central nuclei")+
  xlab("")+
  ylab("central Nuclei")
p_stat_cen1




#year<- df %>% 
   # filter(str_detect(fileName, "1yr"))
#View(year)

#oneMonth<- df %>% 
 # filter(str_detect(fileName, "1month"))
#View(oneMonth)

## filter(str_detect(fileName, "4month"))
#View(fourMonth)

#View(fourMonth)
######4month/x25/4month mdx  #1 25x_1/Results/Results.txt	######
#######4month/x25/4month mdxJAG1 #1 25x_1/Results/Results.txt	

#View(df)
#summary(df)
#class(df)

#f1<- group_by(df, fileName)

#length(unique(df1$fileName))

#######year substring and relabel#######
#year_sub<- str_sub(year$fileName, 22, 54)
# View(year_sub)
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #1 Pax7 Laminin 10x_1/Res", "mdx 1yr #1 1")
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #2 Pax7 Laminin 10x_1/Res", "mdx 1yr #2 1")
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #3 Pax7 Laminin 10x_1/Res", "mdx 1yr #3 1")
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdx 1yr #4 Pax7 Laminin 10x_1/Res", "mdx 1yr #4 1")
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdxJAG1 1yr #1 Pax7 Laminin 10x_1", "mdxJAG 1yr #1 1")
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdxJAG1 1yr #2 Pax7 Laminin 10x_1", "mdxJAG 1yr #2 1")
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdxJAG1 1yr #3 Pax7 Laminin 10x_1", "mdxJAG 1yr #3 1")
# year_sub <- as.data.frame(year_sub)
# year_sub<- str_replace_all(year_sub$year_sub, "mdxJAG1 1yr #4 Pax7 Laminin 10x_1", "mdxJAG 1yr #4 1")
# year_sub <- as.data.frame(year_sub)
# View(year_sub)
# 
# year<- cbind(year, year_sub)
# View(year)
# year<- select(year, year_sub, Label:Perinuclei)
# View(year)
# names(year)<- c("genotype_age_mouse_section","Label","Area","Feret","FeretX","FeretY","FeretAngle","MinFeret","CentralNuclei","TotalMyonuclei","Perinuclei")   
# View(year)
# #################################################################################################################### year is labeled correctly ###########
# 
# ##############oneMonth substring and relablel########################
# View(oneMonth)
# oneMonth_sub_age<- str_sub(oneMonth$fileName, 19, 24)
# oneMonth_sub_age<- as.data.frame(oneMonth_sub_age)
# View(oneMonth_sub_age)
# 
# oneMonth_sub_genotype<- str_sub(oneMonth$fileName, 25, 32)
# oneMonth_sub_genotype<- as.data.frame(oneMonth_sub_genotype)
# oneMonth_sub_genotype<-str_replace_all(oneMonth_sub_genotype$oneMonth_sub_genotype, "mdx #3", "mdx")
# oneMonth_sub_genotype<- as.data.frame(oneMonth_sub_genotype)
# oneMonth_sub_genotype<-str_replace_all(oneMonth_sub_genotype$oneMonth_sub_genotype, "mdx #4", "mdx")
# oneMonth_sub_genotype<- as.data.frame(oneMonth_sub_genotype)
# View(oneMonth_sub_genotype)
# 
# oneMonth_sub_mouse<- str_sub(oneMonth$fileName, 25, 36)
# oneMonth_sub_mouse<- as.data.frame(oneMonth_sub_mouse)
# oneMonth_sub_mouse<-str_replace_all(oneMonth_sub_mouse$oneMonth_sub_mouse, "mdxJAG1 #4", "#4")
# oneMonth_sub_mouse<- as.data.frame(oneMonth_sub_mouse)
# oneMonth_sub_mouse<-str_replace_all(oneMonth_sub_mouse$oneMonth_sub_mouse, "mdxJAG1 #2", "#2")
# oneMonth_sub_mouse<- as.data.frame(oneMonth_sub_mouse)
# oneMonth_sub_mouse<-str_replace_all(oneMonth_sub_mouse$oneMonth_sub_mouse, "mdxJAG1 #1", "#1")
# oneMonth_sub_mouse<- as.data.frame(oneMonth_sub_mouse)
# oneMonth_sub_mouse<-str_replace_all(oneMonth_sub_mouse$oneMonth_sub_mouse, "mdx #3", "#3")
# oneMonth_sub_mouse<- as.data.frame(oneMonth_sub_mouse)
# oneMonth_sub_mouse<-str_replace_all(oneMonth_sub_mouse$oneMonth_sub_mouse, "mdx #4", "#4")
# oneMonth_sub_mouse<- as.data.frame(oneMonth_sub_mouse)
# 
# View(oneMonth_sub_mouse)
# 
# # "1month mdxJAG1 #4 25x_3"
# # "1month mdx #4 25x_1/Res"
# 
# oneMonth_subAge<- str_sub(oneMonth_sub, )
# 
# 
# oneMonth_sub<- str_replace_all(oneMonth_sub$oneMonth_sub, "1month mdxJ", "mdxJAG 1month")
# oneMonth_sub<- as.data.frame(oneMonth_sub)
# oneMonth_sub<- str_replace_all(oneMonth_sub$oneMonth_sub, "1month mdx", "mdx 1month")
# 
# oneMonth<- cbind(oneMonth, oneMonth_sub)
# 
# oneMonth<- select(oneMonth, oneMonth_sub, Label:Perinuclei)
# 
# names(oneMonth)<- c("genotype_age","Label","Area","Feret","FeretX","FeretY","FeretAngle","MinFeret","CentralNuclei","TotalMyonuclei","Perinuclei")   
# #View(oneMonth)
# ########################################################################################1 month is labeled correctly#######
# 
# ####################################### fixing 4 month to be labeled correctly #########
# View(fourMonth)
# fourMonth_sub<- str_sub(fourMonth$fileName, 12, 34)
# View(fourMonth_sub)
# fourMonth_sub<- as.data.frame(fourMonth_sub)
# #####4month mdxJAG1 #2 25x_1#######
# 
# replace_vector<- 
# 
# fourMonth_sub<-str_replace_all()
# 
# 
# 
# 
# View(fourMonth_sub)
# fourMonth_sub<- str_replace_all(fourMonth_sub$fourMonth_sub, "4month mdx", "mdx 4month")
# 
# 
# fourMonth<- cbind(fourMonth, fourMonth_sub)
# 
# fourMonth<- select(fourMonth, fourMonth_sub, Label:Perinuclei)
# 
# names(fourMonth)<- c("genotype_age","Label","Area","Feret","FeretX","FeretY","FeretAngle","MinFeret","CentralNuclei","TotalMyonuclei","Perinuclei")   
# #View(fourMonth)
# #############################################4 month labeled correctly ##########################################################################
# 
# 
# ################recombineds all and makes seperate colomns for genotype and age to make graphing possible ##################3
# df<- rbind(oneMonth, fourMonth, year) 
# 
# df <-  separate(data= df, into=c("genotype", "age"), col = genotype_age, sep= " ")
# View(df)
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
                     color = "genotype", palette = c("red", "blue") )

pannel_minFeret<-pannel_minFeret + facet_grid(~factor(age, levels= c("1month", "4month", "1yr")) )+ 
  ylim(0,75)+
  stat_compare_means(method="t.test",
                     label.x=.75,
                     label.y=3) +
  ggtitle("Minimum Feret Diamter")+
  xlab("")+
  ylab("microns")
pannel_minFeret
############################################################
############minFerret disribution pannel by age ##########

his_minFeret<- ggplot(df, aes(x=MinFeret))+
  geom_histogram(aes(color=genotype),
                 position= "identity", bins=30, alpha=.35)+
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
  ggtitle("Distribution of minimum feret's diameter")


minFeret_pannel_norm
#####################################
#####################################
################################################################################
#################### COMBINED the two into super pannel#########################
################################################################################
grid.arrange(pannel_minFeret,minFeret_pannel_norm , nrow=2 )
####################################################################################
################################################################################
################################################################################


#CENTRAL NUCLEI GRAPH#alll fiberes
################################################################################
################################################################################
pannel_CentralNuclei <- ggboxplot(df, x="genotype", y="CentralNuclei",
                                  color = "genotype", palette = "jco" )+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  ylim(0,4.5)+
  stat_compare_means(method="t.test",
                     label.x= 1.0,
                     label.y= 4.5) +
  ggtitle("Central Nuclei by fiber")+
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

df_nucleiZero<- filter(df, df$TotalMyonuclei!=0) %>% group_by(genotype, age)
View(df_nucleiZero)

summary<- summarise(df_nucleiZero, median=median(TotalMyonuclei), mean=mean(TotalMyonuclei), sd=sd(TotalMyonuclei), se=std.error(TotalMyonuclei))


class(summary)
summary<- group_by(summary, genotype, age)
groups(summary)

summary1 <- summary                                                 # Replicate original data
summary1$age <- factor(summary1$age,                                    # Change ordering manually
                  levels = c("1month","4month", "1yr"))



barplot<- ggplot(summary1, aes(x=age, y=mean, fill=genotype)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) + 
  scale_fill_manual(values= c('red', 'blue'))+ 
  ggtitle("Total nuclei per myofiber")+
 
                  
  
  
  

barplot

df_nucleiZero_groupBy_genotype_age
###############################################################
df_nucleiZero<- mutate(df_nucleiZero, PtoC= Perinuclei/CentralNuclei, n=n(),nuclei_density= sum(TotalMyonuclei)/n, CtoP= CentralNuclei/Perinuclei, ctoTotal=CentralNuclei/TotalMyonuclei, ptoTotal=Perinuclei/TotalMyonuclei, Normalized_ctoP= ctoTotal/ptoTotal, ctoPoverTotal= CtoP/TotalMyonuclei) %>% group_by(genotype) %>% summarise()

df_nucleiZero#####see significance when looking at rati
################################################################################
pannel_NuclearDensity <- ggboxplot(df_nucleiZero, x="genotype", y="TotalMyonuclei",
                                           color = "genotype", palette = "jco")+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  
  ggtitle("Total nuclei per myofiber")+
  ylim(.9,5)+
  xlab("")+
  ylab("")+
  scale_color_manual(values= c("red","blue"))
pannel_NuclearDensity



#########################################################################################
#same as above but the filtered versions
#########################################################################################
#########################################################################################
#########################################################################################

#CENTRAL NUCLEI GRAPH# ####filtered#########
################################################################################
################################################################################
pannel_CentralNuclei_filtered <- ggboxplot(df_nucleiZero, x="genotype", y="CentralNuclei",
                                  color = "genotype", palette = "jco" )+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  ylim(0,5)+
  stat_compare_means(method="t.test",
                     label.x= 1.0,
                     label.y= 4.25) +
  ggtitle("Central Nuclei (when a nucleus is detected)")+
  xlab("")+
  scale_color_manual(values= c("red","blue"))
pannel_CentralNuclei_filtered

#View(df_nucleiZero)


grid.arrange(pannel_CentralNuclei_filtered, percent_p_filtered, nrow=1)



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
  ggtitle(" Percent of Fibers containing central nuclei (when a nucleus is detected) ")+
  theme(legend.position = "none")+
  scale_fill_manual(values= c("red", "blue"))+
  geom_text(aes(label=percent), size=6, vjust=0)
percent_p_filtered<- ggpar(percent_p_filtered, ylim= c(0,1))
percent_p_filtered


grid.arrange(pannel_minFeret, his_minFeret, pannel_CentralNuclei_filtered, percent_p_filtered)


grid.arrange(pannel_minFeret, minFeret_pannel_norm, pannel_CentralNuclei_filtered, pannel_NuclearDensity)
 


grid.arrange(pannel_CentralNuclei, pannel_CentralNuclei_filtered, percent_p, percent_p_filter, nrow=2)


df_nucleiZero_1year<- filter(df_nucleiZero, age=='1yr')

group_by()
