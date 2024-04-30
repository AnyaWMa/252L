#reliability 
dataset <- redivis::user("datapages")$dataset("item_response_warehouse",version='v5.0')


library(tidyverse)
library(tidyr)
library(dplyr)
library(psych)
#1 
dataset_names <- c("vocab_assessment_3_to_8_year_old_children")
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
nm<-dataset_names
df <- dataset$table(nm)$to_data_frame()

df.wider <- df %>% 
  select(targetword, resp, subjid) %>% 
  pivot_wider(names_from = targetword, values_from = resp)
dim(df.wider)
alpha(df.wider[,-1])

#2 
dataset_names <- c("roar_lexical")
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
nm<-dataset_names
df <- dataset$table(nm)$to_data_frame()

df.wider <- df %>% 
  select(item, resp, id) %>% 
  pivot_wider(names_from = item, values_from = resp)
dim(df.wider)
alpha(df.wider[,-1], check.keys=TRUE)


#3 
dataset_names <- c("4thgrade_math_sirt")
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
nm<-dataset_names
df <- dataset$table(nm)$to_data_frame()

df.wider <- df %>% 
  select(item, resp, id) %>% 
  pivot_wider(names_from = item, values_from = resp)
dim(df.wider)
alpha(df.wider[,-1])

#1 
library("mirt")
dataset_names <- c("vocab_assessment_3_to_8_year_old_children")
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
nm<-dataset_names
df.1 <- dataset$table(nm)$to_data_frame()

resp.1 <- df.1 %>% 
  filter(!targetword %in% c("strawberry", "plane", "telephone", "snake", "fish" )) %>% 
  select(targetword, resp, subjid) %>% 
  pivot_wider(names_from = targetword, values_from = resp) %>% 
  select(-subjid)

mod.1 <- mirt(as.matrix(resp.1),1,itemtype="Rasch", guessing = 0.25)

makeresponse<-function(x) {
  #make IR matrix
  nms<-unique(x$item)
  if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
  ##make response matrix
  id<-unique(x$id)
  L<-split(x,x$item)
  out<-list()
  for (i in 1:length(L)) {
    z<-L[[i]]
    index<-match(z$id,id)
    resp<-rep(NA,length(id))
    resp[index]<-z$resp
    out[[i]]<-resp
  }
  resp<-do.call("cbind",out)
  resp<-data.frame(resp)
  names(resp)<-names(L)
  resp$id<-id
  nr<-apply(resp,2,function(x) length(table(x)))
  resp<-resp[,nr>1]
  resp<-resp[rowSums(!is.na(resp))>1,]
  resp
}
imv.binary<-function(y, #outcomes
                     p1,#baseline
                     p2 #enhanced
) {
  ##
  ll<-function(x,p) {
    z<-log(p)*x+log(1-p)*(1-x)
    z<-sum(z)/length(x)
    exp(z)
  }    
  loglik1<-ll(y,p1)
  loglik2<-ll(y,p2)
  getcoins<-function(a) {
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
  }
  c1<-getcoins(loglik1)
  c2<-getcoins(loglik2)
  ew<-function(p1,p0) (p1-p0)/p0
  imv<-ew(c2,c1)
  imv
}
imv0mirt<-function(mod,
                   nfold=5,
                   fscores.options=(list(method="EAP"))
)
{
  x<-mod@Data$data
  id<-1:nrow(x)
  L<-list()
  for (i in 1:ncol(x)) L[[i]]<-data.frame(id=id,item=colnames(x)[i],resp=x[,i])
  x<-data.frame(do.call("rbind",L))
  x$group<-sample(1:nfold,nrow(x),replace=TRUE)
  ##
  call<-mod@Call
  call<-deparse(call)
  call<-gsub("data = data","data = train",call)
  call<-parse(text=call)
  ##
  om<-numeric()
  for (i in 1:nfold) {
    train<-makeresponse(x[x$group!=i,])
    id<-train$id
    train$id<-NULL
    mm<-eval(call)
    th<-do.call("fscores",c(list(object=mm),fscores.options))
    test<-x[x$group==i,]
    ll<-list()
    items<-unique(test$item)
    for (j in 1:length(items)) {
      it<-items[j]
      it<-extract.item(mm,it)
      pp<-probtrace(it,th[,1])
      ll[[j]]<-data.frame(id=id,item=names(coef(mm))[j],pr=pp[,2])
    }
    y<-data.frame(do.call("rbind",ll))
    y<-merge(test,y,all.x=TRUE)
    y$p0<-mean(x$resp[x$group!=i],na.rm=TRUE)
    y<-y[!is.na(y$pr),]
    om[i]<-imv.binary(y$resp,y$p0,y$pr)
  }
  om
}    

mean(imv0mirt(mod.1))
#2 
dataset_names <- c("roar_lexical")
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
nm<-dataset_names
df.2 <- dataset$table(nm)$to_data_frame()

resp.2 <- df.2 %>% 
  select(item, resp, id) %>% 
  pivot_wider(names_from = item, values_from = resp) %>% 
  select(-id)

mod.2 <- mirt(as.matrix(resp.2),1,itemtype="Rasch", guessing = 0.5)
mean(imv0mirt(mod.2))
#3 
dataset_names <- c("4thgrade_math_sirt")
dataset <- redivis::user("datapages")$dataset("item_response_warehouse")
nm<-dataset_names
df.3 <- dataset$table(nm)$to_data_frame()

resp.3 <- df.3 %>% 
  select(item, resp, id) %>% 
  pivot_wider(names_from = item, values_from = resp) %>% 
  select(-id)

mod.3 <- mirt(as.matrix(resp.3),1,itemtype="Rasch")
mean(imv0mirt(mod.3))
