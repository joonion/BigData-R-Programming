
##### 13-1

setwd("D:/source")
library(ggplot2)
library(ggmap)
library(readxl)



files <- c("201512","201606","201612","201706","201712")

columns <- c( "1", "2", "3", "4", 

              "5", "6", "7", "8", "9")     

ds.total <- NULL



for (i in 1:length(files)) {

  filename <- paste("seoul_", files[i], ".xlsx", sep="")

  cat("read ", filename, "...\n")         
  

  ds <- read_excel(filename)              
  ds <- data.frame(ds)                    
  ds <- ds[,columns]                      
  ds$한글 <- rep(i, nrow(ds))         
  ds.total <- rbind(ds.total,ds)          
}



head(ds.total)

##### 13-2

str(ds.total)

unique(ds.total$한글)                 
unique(ds.total$한글)         
unique(ds.total$한글)         
unique(ds.total$한글)         



sum(is.na(ds.total))




ds.201712 <- subset(ds.total, ds.total$한글== 5)

dim(ds.201712)




store.level_1 <- aggregate(ds.201712[,1], 

                           by=list(=ds.201712$한글),

                           FUN=length)

store.level_1

names(store.level_1)[2] = c("count")

ggplot(store.level_1, aes(x=한글, y=count)) +

  geom_bar(stat="identity", width=0.7, fill="steelblue") +

  ggtitle("한글한글") +

  theme(plot.title = element_text(color="black", size=14, face="bold"))




store.region <- aggregate(ds.201712[,1], 

                          by=list(한글=ds.201712$한글),

                          FUN=length)

store.region

names(store.region)[2] = c("count")

names(store.region)[2] = c("count")

ggplot(store.region, aes(x=한글, y=count)) +

  geom_bar(stat="identity", width=0.7, fill="steelblue") +

  ggtitle("한글 한글") +

  theme(plot.title = element_text(color="black", size=14, face="bold"),

        axis.text.x = element_text(angle = 45))

store.region.loc <- aggregate(ds.201712[,c("한글","한글")], 

                              by=list(한글=ds.201712$한글),

                              FUN=mean)

store.region <- data.frame(store.region, store.region.loc[,2:3])

register_google(key='AIzaSyCK....E9urxjSpPOA')

cen <- c(mean(store.region$한글),mean(store.region$한글))

map <- get_googlemap(center=cen,                
                     maptype="roadmap",

                     size=c(640,640),

                     zoom=11)

gmap <- ggmap(map)                              
gmap+geom_point(data=store.region,                        

                aes(x=한글,y=한글,size=count),

                alpha=0.5, col="red") +

  scale_size_continuous(range = c(1, 15))+      
  geom_text(data=store.region,                  
            aes(x=한글,y=한글),                 
            size=3,                             
            label=store.region$한글)          
store.dong <- aggregate(ds.201712[,1], 

                        by=list(한글=ds.201712$한글),

                        FUN=length)

store.dong

names(store.dong)[2] = c("count")

store.dong <- store.dong[order(store.dong$count,decreasing=T),]

dong.top10 <- store.dong[1:10,]

dong.top10

ggplot(dong.top10, aes(x=reorder(한글, -count), y=count)) +

  geom_bar(stat="identity", width=0.7, fill="steelblue") +

  ggtitle("한글 한글 한글 한글") +

  theme(plot.title = element_text(color="black", size=14, face="bold"),

        axis.text.x = element_text(angle = 45))


##### 13-3

store.change <- aggregate(ds.total[,1], 

                          by=list(한글=ds.total$한글,

                                    한글=ds.total$한글),

                          FUN=length)

head(store.change)

names(store.change)[3] <- c("count")

ggplot(store.change, aes(x=한글, y=count, colour=한글, group=한글)) + 

  geom_line() + 

  geom_point(size=6, shape=19, alpha=0.5) +

  ggtitle("한글 한글 한글(한글)") +

  ylab("한글") + 

  scale_x_continuous(breaks=1:5, 

                     labels=files) +

  theme(plot.title = element_text(color="black", size=14, face="bold"))



store.tmp <- aggregate(ds.total[,1], 

                       by=list(한글=ds.total$한글,

                                 한글=ds.total$한글),

                       FUN=length)

names(store.tmp)[3] <- c("count")

store.201512 <- store.tmp[store.tmp$한글==1,]

names(store.201512)[3] <- c("cnt_2015")

store.201712 <- store.tmp[store.tmp$한글==5,]

names(store.201712)[3] <- c("cnt_2017")

store.diff <- merge(store.201512[,2:3], store.201712[,2:3]) 

store.diff$diff <- abs(store.diff$cnt_2015-store.diff$cnt_2017)

store.diff <- store.diff[order(by=store.diff$diff, decreasing=T),] 

top10 <- store.diff[1:10,1]      
top10

store.change <- subset(store.tmp, store.tmp$한글 %in% top10)

ggplot(store.change, aes(x=한글, y=count, colour=한글, group=한글)) +

  geom_line() + 

  geom_point(size=6, shape=19, alpha=0.5) +

  ggtitle("한글 한글 한글 10 한글(한글)") +

  ylab("한글") + 

  scale_x_continuous(breaks=1:5, 

                     labels=files) +

  theme(plot.title = element_text(color="black", size=14, face="bold"))

store.gu <- aggregate(ds.total[,1], 

                      by=list(한글=ds.total$한글,

                                한글=ds.total$한글),

                      FUN=length)

names(store.gu)[3] <- c("count")

ggplot(store.gu, aes(x=한글, y=count, colour=한글, group=한글)) +

  geom_line() + 

  geom_point(size=6, shape=19, alpha=0.5) +

  ggtitle("한글 한글 한글 (한글)") +

  ylab("한글") + 

  scale_x_continuous(breaks=1:5, 

                     labels=files) +

  theme(plot.title = element_text(color="black", size=14, face="bold"))

store.tmp <- aggregate(ds.total[,1], 

                       by=list(한글=ds.total$한글,

                                 한글=ds.total$한글),

                       FUN=length)

names(store.tmp)[3] <- c("count")

store.dong.201512 <- store.tmp[store.tmp$한글==1,]

names(store.dong.201512)[3] <- c("cnt_2015")

store.dong.201712 <- store.tmp[store.tmp$한글==5,]

names(store.dong.201712)[3] <- c("cnt_2017")

store.diff <- merge(store.dong.201512[,2:3], store.dong.201712[,2:3]) 

store.diff$diff <- abs(store.diff$cnt_2015-store.diff$cnt_2017)

store.diff <- store.diff[order(by=store.diff$diff, decreasing=T),] 

top10 <- store.diff[1:10,1]      
top10

store.change <- subset(store.tmp, store.tmp$한글 %in% top10)

ggplot(store.change, aes(x=한글, y=count, colour=한글, group=한글)) +

  geom_line() + 

  geom_point(size=6, shape=19, alpha=0.5) +

  ggtitle("한글 한글 Top 10 한글") +

  ylab("한글") + 

  scale_x_continuous(breaks=1:5, 

                     labels=files) +

  theme(plot.title = element_text(color="black", size=14, face="bold"))


##### 13-4

ds.yeoksam <- subset(ds.total, ds.total$한글==5 &

                       ds.total$한글=="한글")

cen <- c(mean(ds.yeoksam$한글),mean(ds.yeoksam$한글))

map <- get_googlemap(center=cen,                
                     maptype="roadmap",

                     size=c(640,640),

                     zoom=15)

gmap <- ggmap(map)                              
gmap+geom_point(data = ds.yeoksam, 

                aes(x=한글,y=한글,color=한글),size=2,alpha=0.7) +

  labs(x = "Longitude", y = "Latitude",

       title="한글 한글 한글", color = "한글")



ds.yeoksam2 <- subset(ds.yeoksam, ds.yeoksam$한글== 

                        "한글/한글/한글")

gmap+geom_point(data = ds.yeoksam2, 

                aes(x=한글,y=한글), size=2, alpha=0.5, col="red") +

  labs(x = "Longitude", y = "Latitude",

       title="한글 한글" )
