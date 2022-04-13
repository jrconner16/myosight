######TPM3 
####goals are minferret, CSA, fiber type %

####
####
#### packages
####
library(tidyr)
library(ggplot2)
library(vroom)
library(dplyr)
library(stringr)


install.packages("stringr")





list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE) 



df <- vroom(list_of_files, id="Results")   #creats DF with folder path as column


df_black<- df %>% 
    mutate(Color= str_extract(df$Results, "BLACK")) %>%   ####cleans file path column
    filter(Color=="BLACK") %>% 
    select("Color", "Label", "Area", "MinFeret")
           
df_red<- df %>% 
  mutate(Color= str_extract(df$Results, "RED")) %>% 
  filter(Color=="RED") %>% 
  select("Color", "Label", "Area", "MinFeret")

df<- rbind(df_black, df_red)

df$Color<- factor(df$Color)
df$Label<- factor(df$Label)
df_red$Color<- factor(df_red$Color)
df_red$Label<- factor(df_red$Label)
df_black$Color<- factor(df_black$Color)
df_black$Label<- factor(df_black$Label)

BLACK<- summary(df_red)
RED<- summary(df_black)


class(BLACK)

summaryByColor<- rbind(BLACK,RED)

write.csv(summaryByColor, "summaryByColor.csv")
write.csv(RED, "summaryRed.csv")
