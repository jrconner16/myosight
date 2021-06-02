
library(vroom)

setwd("~/projects/myosight/4month/4month/x25")

list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.lsm", 
                            full.names = FALSE, all.files = TRUE) 

list_of_files

df_files<-as.data.frame(list_of_files)


write.table(df_files, file="Listsof4MonthFileNames")
