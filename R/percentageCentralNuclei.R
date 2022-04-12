#####determine % of fibers with cental nuclei



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


View(df_4month)
########PUT ALL DATA INTO 1 PLOT and tidy it###########
df_all<- rbind(df_1month, df_4month, df_1year) 
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


df_1and4

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


percentage<-
df_1and4 %>%
  group_by(Genotype, Age) %>%
  mutate(nn = sum(CentralNuclei != 0), n=n(), `percent`= nn/n) %>% 
  select(Genotype, Age,`percent`) %>% distinct()

percentage<- round_df(percentage, 2)

#########
View(percentage)

percentage<- as.data.frame(percentage)
class(percentage)

View(percentage)


percent_p<- ggplot(percentage, aes(x=Genotype, y=`percent`, fill=Genotype)) + 
  geom_bar(stat = "identity") + 
  facet_grid(.~Age) + 
  xlab(" ")+ 
  ggtitle(" % of Fibers containing central nuclei")+
  theme(legend.position = "none")+
  scale_fill_manual(values= c("red", "blue"))+
  geom_text(aes(label=percent), size=6, vjust=0)
  
percent_p<- ggpar(percent_p, ylim= c(0,1))
percent_p
    

##########PRACTICE for grouping and counting 
df_nucleiZero_groupBy_genotype_age<- group_by(df, genotype, age) %>% 
  mutate(n=n())
View(df_nucleiZero_groupBy_genotype_age)

minFeret_percentage<-
ggplot(df_nucleiZero_groupBy_genotype_age,aes(x=MinFeret,fill=genotype))+
  geom_histogram(aes(y=0.5*..density..*10),
                 alpha=0.3,position='identity', bins=20)+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+ 
  scale_fill_manual(values=c("red", "blue"))+
  ylab("percent")+
  theme(legend.position = c(.9,.85))
  
  
minFeret_percentage





his_minFeret_norm<- ggplot(df_nucleiZero_groupBy_genotype_age, aes(x=MinFeret), group=Genotype)+
  geom_histogram(aes(y = stat(count / sum(count))),
                 position= "identity", bins=20, alpha=0.3)+
  theme(legend.position = c(.4,.8))+
  #xlab("xlab")+
  ylab("percent")+
  ggtitle(" Distribution of Minimum Feret Diameter")+
  facet_grid(~factor(age, levels= c("1month", "4month", "1yr")))+
  scale_color_manual(values=c("red", "blue"))+
  scale_fill_manual(values=c("red", "blue"))

his_minFeret_norm

View(df_nucleiZero_groupBy_genotype_age)




