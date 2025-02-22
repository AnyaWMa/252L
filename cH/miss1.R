sim<-function(ni=30,np=2000) {
    ##This will simulate response data in a long format to emphasize that this is the 'likelihood' view of the response data
    x<-expand.grid(id=1:np,item=1:ni)
    th<-rnorm(np)
    x<-merge(x,data.frame(id=1:np,th=th))
    a<-exp(rnorm(ni,sd=.3))
    b<-rnorm(ni)
    x<-merge(x,data.frame(item=1:ni,a=a,b=b))
    k<-x$th-x$b
    p<-1/(1+exp(-x$a*k))
    x$resp<-rbinom(length(p),1,p)
    x
}

x<-sim()
resp.original<-irw::long2resp(x)
resp.original$id<-NULL
##Now we'll induce purely random missingness 
x$resp.full<-x$resp
p<-.5
miss<-rbinom(nrow(x),1,p)
x$resp<-ifelse(miss==1,NA,x$resp.full)

##now we're ready for analysis
resp<-irw::long2resp(x)
id<-resp$id #might need this! 
resp$id<-NULL
library(mirt)
m<-mirt(resp,1,'2PL')

m.original<-mirt(resp.original,1,'2PL')

##Your task
##1. decide what you are going to analyze (options: abilities. item parameters.)
params <- data.frame(coef(m, IRTpars = TRUE, simplify = TRUE))
params.original <- data.frame(coef(m.original, IRTpars = TRUE, simplify = TRUE))

plot(params$items.a, params.original$items.a)

plot(params$items.b, params.original$items.b)
                     
##2. you can modify sim() if you need to
##3. you can also compare to the estimates we'd get from analysis of resp.full
##4. might want to turn p up to something larger to make it more obvious what is going on at the outset

