library(ncdf)
library(reshape2)
library(bReeze)

# power fitting function
power.curve <- pc('Siemens_SWT-3.6MW-107m.pow')
power.fun.tmp <- approxfun(power.curve$v, power.curve$P)
power.fun <- function(x) {
  y <- power.fun.tmp(x)
  y[x < 4]  <- 0
  y[x > 25] <- 0
  y
}
power.rated <- max(power.curve$P)

nlon  <- 159
nlat  <- 249
nyear <- 14

rho   <- 1.225 # air density
z10   <- 10    # data altitude
z50   <- 50 
z80   <- 80
alpha <- .11   # power law exponent

P50.year <- array(dim = c(nlon, nlat, nyear))
P80.year <- array(dim = c(nlon, nlat, nyear))
SWT.year <- array(dim = c(nlon, nlat, nyear))

for (i in 1:nyear) {
  print(paste("file:", i))
  file.prefix <- paste('data/MIT-01JAN', 
                       formatC(i-1, width = 2, format = "d", flag = "0"), 
                       '-01JAN',
                       formatC(i, width = 2, format = "d", flag = "0"), sep="")
  
  # Read u component
  nc.file <- open.ncdf(paste(file.prefix, '_u10.nc', sep=""))
  u <- get.var.ncdf(nc.file, 'uwnd')
  
  # Read v component
  nc.file <- open.ncdf(paste(file.prefix, '_v10.nc', sep=""))
  v <- get.var.ncdf(nc.file, 'vwnd')
  
  U10 <- sqrt(u*u + v*v)       # wind speed at 10m
  
  # ------------ <Refactor> -------------#
  
  U50 <- U10 * (z50/z10)^alpha # wind speed at 50m
  P50 <- .5 * rho * U50^3      # wind power density at 50m
  P50 <- apply(P50, c(1,2), mean) # annual averaged wpd at 50m  
  P50.year[,,i] <- P50
  
  U80 <- U10 * (z80/z10)^alpha # wind speed at 50m
  P80 <- .5 * rho * U80^3      # wind power density at 50m
  P80 <- apply(P80, c(1,2), mean) # annual averaged wpd at 50m  
  P80.year[,,i] <- P80
  
  SWT <- power.fun(U80)
  SWT <- array(SWT, dim(U80))
  SWT <- apply(SWT, c(1,2), mean)
  SWT.year[,,i] <- SWT
  
  # --------------- </Refactor> -------------------
}

P50 <- apply(P50.year, c(1, 2), mean)
P80 <- apply(P80.year, c(1, 2), mean)
SWT <- apply(SWT.year, c(1, 2), mean)
lat <- get.var.ncdf(nc.file, 'lat')
lon <- get.var.ncdf(nc.file, 'lon')

# %---------------- <Refactor> -----------------%

# reformat data
DATA.P50 <- data.frame(P50)
rownames(DATA.P50) <- lon
colnames(DATA.P50) <- lat
DATA.P50 <- melt(as.matrix(DATA.P50))
colnames(DATA.P50) <- c('long', 'lat', 'P50')

DATA.P80 <- data.frame(P80)
rownames(DATA.P80) <- lon
colnames(DATA.P80) <- lat
DATA.P80 <- melt(as.matrix(DATA.P80))
colnames(DATA.P80) <- c('long', 'lat', 'P80')

DATA.SWT <- data.frame(SWT)
rownames(DATA.SWT) <- lon
colnames(DATA.SWT) <- lat
DATA.SWT <- melt(as.matrix(DATA.SWT))
colnames(DATA.SWT) <- c('long', 'lat', 'SWT')

wind.year <- merge(DATA.P50, DATA.P80)
wind.year <- merge(wind.year, DATA.SWT)
wind.year$Cf <- wind.year$SWT/power.rated
wind.year$cf_pretty<- signif(100*wind.year$Cf,3)


# %----------------- </Refactor> ---------------------%


# Computing wind classes
wind.year$class50 <- cut(wind.year$P50,
                         breaks = c(-Inf,200,300,400,500,600,800,Inf),
                         right = FALSE)

save(wind.year, lon, lat, file='data/wind_year.Rda')