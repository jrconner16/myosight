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


df_1and4


percentage<-
df_1and4 %>%
  group_by(Genotype, Age) %>%
  mutate(nn = sum(CentralNuclei != 0), n=n(), `percent`= nn/n) %>% 
  select(Genotype, Age,`percent`) %>% distinct() 

percentage

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
  scale_fill_manual(values= c("red", "blue", "red", "blue"))
  
percent_p<- ggpar(percent_p, ylim= c(0,1))
percent_p
    