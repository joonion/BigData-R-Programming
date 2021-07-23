

##### 10-1

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_211') 



library(wordcloud)                              
library(KoNLP)                                  
library(RColorBrewer)                           


setwd("D:/source")

text <- readLines("mis_document.txt", encoding ="UTF-8" )   
buildDictionary(ext_dic = "woorimalsam")        
pal2 <- brewer.pal(8, "Dark2")                  
noun <- sapply(text,extractNoun, USE.NAMES=F)   
noun                                            


##### 10-2

noun2 <- unlist(noun)             
wordcount <- table(noun2)         
temp <- sort(wordcount, decreasing=T)[1:10]  
temp

temp <- temp[-1]                  
barplot(temp,                     
        names.arg = names(temp),  
        col ="lightblue",         
        main ="title", ylab = "ylab")


##### 10-3

wordcloud(names(wordcount),

          freq=wordcount,       

          scale=c(6,0.7),      

          min.freq=3,          

          random.order=F,       

          rot.per=.1,           

          colors=pal2)          


##### 10-4


buildDictionary(ext_dic = "woorimalsam",  

                user_dic=data.frame("aaa", "ncn"),

                replace_usr_dic = T)

noun <- sapply(text,extractNoun, USE.NAMES=F)

noun2 <- unlist(noun)            



noun2 <- noun2[nchar(noun2)>1]    
noun2 <- gsub("bbbb","", noun2)   
noun2 <- gsub("cccc","", noun2)   


wordcount <- table(noun2)         
wordcloud(names(wordcount),

          freq=wordcount,

          scale=c(6,0.7),

          min.freq=3,

          random.order=F,

          rot.per=.1,

          colors=pal2)
