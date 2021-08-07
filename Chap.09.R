
##### 09-1
library(ggmap)

register_google(key='AIzaSyCK...E9urxjSpPOA')

gc <- geocode(enc2utf8("한글한글"))
gc

cen <- as.numeric(gc)
cen

map <- get_googlemap(center=cen)

ggmap(map)

##### 09-2

gc <- geocode(enc2utf8("한글"))

cen <- as.numeric(gc)

map <- get_googlemap(center=cen,
                     zoom=9,
                     size=c(640,640),
                     maptype="roadmap")

ggmap(map)

##### 09-3

cen <- c(-118.233248, 34.085015)

map <- get_googlemap(center=cen)

ggmap(map)

##### 09-4

gc <- geocode(enc2utf8("한글"))

cen <- as.numeric(gc)

map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     marker=gc)

ggmap(map)

##### 09-5
library(ggplot2)

names <- c("1","2","3", "4","6","6")

addr <- c("1",
          "2",
          "3",
          "4",
          "5",
          "6")

gc <- geocode(enc2utf8(addr))
gc

df <- data.frame(name=names, lon=gc$lon, lat=gc$lat)
df

cen <- c(mean(df$lon),mean(df$lat))
map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=10,
                     size=c(640,640),
                     marker=gc)

ggmap(map)
gmap <- ggmap(map)
gmap + geom_text(data=df,
                 aes(x=lon,y=lat),
                 size=5,
                 label=df$name)

##### 09-6

sp <- sample(1:nrow(wind),50)
df <- wind[sp,]
head(df)
cen <- c(mean(df$lon), mean(df$lat))
gc <- data.frame(lon=df$lon, lat=df$lat)
head(gc)
map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=6,
                     marker=gc)

ggmap(map)

map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=6)

gmap <- ggmap(map)

gmap + geom_point(data=df,
                  aes(x=lon,y=lat,size=spd),
                  alpha=0.5,
                  col="blue") +
  scale_size_continuous(range = c(1, 14))