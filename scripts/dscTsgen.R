# generating ts

library(parallel)
library(abind)
library(reshape2)
library(lubridate)

readnc<-function(f, varlist=names(nc$var),dimlist=names(nc$dim),verboseT=F){
  require(ncdf)
  nc <- open.ncdf(f, write=FALSE, readunlim=FALSE, verbose=verboseT)
  lst <-list()
  for (i in dimlist){
    lst<- c(lst, list(get.var.ncdf(nc, i)))
    }
  names(lst)<- dimlist
  lst <- list(lst)
  for (i in varlist){
    lst<-c(lst, list(get.var.ncdf(nc, i)))
    }
  names(lst)<-c("dims", varlist)
  
  close.ncdf(nc)
  return(lst)
  }

path<-"data/mit0013"

## rewrite
ulst<-mclapply(list.files(path, pattern = "\\u10.nc$", full.names =T), readnc, varlist=c("uwnd"),dimlist=c(), mc.cores=8)
vlst<-mclapply(list.files(path, pattern = "\\v10.nc$", full.names =T), readnc, varlist=c("vwnd"),dimlist=c(), mc.cores=8)
# note remove the one timestep overlap at end of each file
ulst<-mclapply(ulst, function(x) x$uwnd[,,-dim(x$uwnd)[3]], mc.cores = 8)
vlst<-mclapply(vlst, function(x) x$vwnd[,,-dim(x$vwnd)[3]], mc.cores = 8)

ulst<-abind(ulst, along = 3)
vlst<-abind(vlst, along = 3)
spd<- sqrt(ulst^2 + vlst^2)

rm(ulst, vlst)

# 80mAGL calculations
alpha=0.11; z=80; z0=10
spd80<- spd * (z/z0)^(alpha)

rm (spd)

# P80
swt107P <- function(spd){
  require(bReeze)
  pc<-pc("Siemens_SWT-3.6MW-107m.pow")
  #   pcfit<-loess(P~v, pc)
  pcfit<-approxfun(pc$v, pc$P)
  p<-pcfit(spd)
  # not necessary to enforce using approxfun
  p[spd<4 | spd>25]<-0
  return(p)
  }

P80<-array(swt107P(as.vector(spd80)), dim(spd80))

rm(spd80)

sites<- data.frame(name=c("yanbu", "jeddah", "aqaba"), element=c(c(73,173), c(94,130),c(45,217)))
ts <- lapply(sites$element, function(x) P80[x[1],x[2],])
names(ts)<-sites$name
t<-seq(from=strptime("2000-01-01-00-00", "%Y-%m-%d-%H-%M", tz="UTC"), by = "3 hours", length.out = length(ts[[1]]))
ts<-data.frame(Y=year(t), m=month(t), d=day(t), H=hour(t), yanbu=ts$yanbu, jeddah=ts$jeddah, aqaba=ts$aqaba)
ts<- melt(ts, id= c("Y","m","d","H"), variable.name = "site", value.name = "P80")

# site bathymetry
save(ts, sites,file="data/dscTs.RData")