dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')
df <- dataset$table("chess_lnirt")$to_data_frame()
df<-df[!is.na(df$resp),]
library(irw)
library('ggpubr')
resp<-irw::long2resp(df)
resp$id<-NULL

##Now we have resp in a nice format. Let's consider the following descriptive approach to this problem.
##1. Let's first compute item-level mean responses. Given that (a) all items are dichotomous and (b) all respondents take virtually all items, we can use this as a rough proxy for item difficulty.
rs = colMeans(resp, na.rm=TRUE)

##2. Let's compute the average time for an item.



##3. If we look at the average item response as a function of average item response time, what do we see?
library("tidyverse")
df.summary <- df %>% 
  group_by(item) %>% 
  dplyr :: summarise(rt = mean(rt), difficulty = mean(resp))


ggplot(df.summary, mapping = aes(x =  rt,
                                       y = difficulty)) +
  geom_point(alpha=.8, size = 1)  + 
  stat_cor() + geom

not_data<-"metadata"
dataset<-redivis::organization("datapages")$dataset("Item Response Warehouse",version="v4.0")
dataset_tables <- dataset$list_tables()
##
print(length(dataset_tables))
names<-sapply(dataset_tables,function(x) x$name)
ii<-grep("metadata",names)
names(dataset_tables)<-names 
if (length(ii)>0) dataset_tables<-dataset_tables[-ii]


##
f<-function(table) table$list_variables()
nms<-lapply(dataset_tables,f)
##
f<-function(x) {
  nm<-sapply(x,function(x) x$name)
  "rt" %in% nm
}
test<-sapply(nms,f)
rt_data<-dataset_tables[test]
proc<-function(table) { 
  df<- table$to_data_frame()
  df$resp<-ifelse(df$resp=="NA",NA,df$resp)
  df$rt<-ifelse(df$rt=="NA",NA,df$rt)
  df<-df[!is.na(df$resp),]
  df<-df[!is.na(df$rt),]
  #
  z<-as.numeric(df$rt)
  z<-z[z>0 & z<60*30]
  z<-log(z)
  z<-(z-mean(z))/sd(z)
  qq<-qqnorm(z,plot.it=FALSE)
  qq<-cbind(qq$x,qq$y)
  qq<-qq[order(qq[,1]),]
  qq[(seq(1,nrow(qq),length.out=500)),]
}
dens<-lapply(rt_data,proc)

cc<-col2rgb("red")
cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=75)
par(mgp=c(2,1,0),mar=c(3,3,.1,.1))
plot(NULL,xlim=c(-6,6),ylim=c(-6,6),xlab="theoretical quantiles",ylab="sample quantiles")
abline(0,1,lwd=2)
for (i in 1:length(dens)) lines(dens[[i]],col=cc)
dev.off()
df <- do.call(rbind, lapply(seq_along(dens), function(i) {
  as.data.frame(dens[[i]]) %>%
    mutate(id = paste("Matrix", i))
}))
colnames(df) <- c("x", "y", "id")
ggplot(df, aes(x = x, y = y, group = id, color = id)) +
  geom_line(color = "red") +  # Draw each line in red
  labs(title = "difficulty vs. response time", x = "difficulty", y = "response time in lognormal") +
  theme_minimal() +
  theme(legend.position = "none")


hi<-function(table) { 
  df<- table$to_data_frame()
  df$resp<-ifelse(df$resp=="NA",NA,df$resp)
  df$rt<-ifelse(df$rt=="NA",NA,df$rt)
  df<-df[!is.na(df$resp),]
  df<-df[!is.na(df$rt),]
  #
  z<-as.numeric(df$rt)
  z<-z[z>0 & z<60*30]
  z<-log(z)
  z<-(z-mean(z))/sd(z)
  qq<-qqnorm(z,plot.it=FALSE)
  qq<-cbind(qq$x,qq$y)
  qq<-qq[order(qq[,1]),]
  qq[(seq(1,nrow(qq),length.out=500)),]
}
dens_more <-lapply(rt_data,hi)
