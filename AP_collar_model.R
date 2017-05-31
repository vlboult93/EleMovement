## FULL SCRIPT INCLUDING: PROCESSING OF NDVI DATA, RAIN DATA; CALCULATION OF PROTEIN CONTENT AND MIS; 
## MODELLED ABSENCE / PRESENCE; COLLAR ABSENCE / PRESENCE; STATISTICS FOR COMPARISON OF MODEL AND DATA
## Author: Victoria L. Boult
## Email: Victoria.Boult@pgr.reading.ac.uk
## Date: 1st June 2017

#----EXTRACT MEDIAN NDVI VALUES FOR ELEPHANTS' DISPERSAL AREAS AND SWAMP EDGE FROM ASCII FILES----

#install packages
install.packages("raster")
install.packages("maptools")
install.packages("miscTools")

#load packages
library(raster)
library(maptools)
library(miscTools)

#set working directory to directory holding all NDVI ascii files (and nothing else)
setwd("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/col_NDVI/")
#create list of files within working directory
ndvi <- list.files(path="C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/col_NDVI/", full.names=FALSE)
#at this point check the files are in the right order!
ndvi

#ensure R is reading ascii files as rasters
as.rast <- function(x) {
  f <- ndvi[x]
  r <- raster(f)
  return(r)
}
ndvi.rast <- sapply(1:23, as.rast)
#ensure class is now RasterLayer (notice coord. ref is NA)
ndvi.rast[1]
#stack all NDVI rasters
ndvi.stack <- stack(ndvi.rast)
#set coord. ref of all rasters in stack (using WKT for MODIS sinusoidal projection)
projection(ndvi.stack) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
#check coord. ref now set
ndvi.stack$m2011225ndviT
#reproject rasters into WGS84 UTM 37S (for Amboseli Ecosystem) to correspond to GIS shapefiles (using WKT for WGS84 UTM 37S)
ndvi.84 <- projectRaster(ndvi.stack, crs = "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#check coord. ref now set as WGS84 UTM 37S
ndvi.84$m2011225ndviT

#import shapefiles of swamp edge and dispersal areas
#swamp edge
SEG <- readShapePoly("C:/Users/nj832242/Documents/GIS/se_WGS8437S.shp")
#Vicky dispersal area
vic.sel <- readShapePoly("C:/Users/nj832242/Documents/GIS/v_SS_wgs8437s.shp")
#Willow dispersal area
wil.mes <- readShapePoly("C:/Users/nj832242/Documents/GIS/w_SS_wgs8437s.shp")
#Ida dispersal area
ida.kil <- readShapePoly("C:/Users/nj832242/Documents/GIS/i_SS_wgs8437s.shp")
#Lobelia dispersal area
lob.kil <- readShapePoly("C:/Users/nj832242/Documents/GIS/l_SS_wgs8437s.shp")
#Maureen dispersal area
mau.tan <- readShapePoly("C:/Users/nj832242/Documents/GIS/m_SS_wgs8437s.shp")

#plot polygon on top of NDVI raster - check they are in the right place!
plot(ndvi.84, 1)
plot(SEG, add = TRUE)
plot(vic.sel, add = TRUE)
plot(wil.mes, add = TRUE)
plot(ida.kil, add = TRUE)
plot(lob.kil, add = TRUE)
plot(mau.tan, add = TRUE)

#extract median NDVI of each raster within polygons
seg.ndvi <- extract(ndvi.84, SEG, fun = median, na.rm = TRUE)
v.sel <- extract(ndvi.84, vic.sel, fun = median, na.rm = TRUE)
w.mes <- extract(ndvi.84, wil.mes, fun = median, na.rm = TRUE)
m.tan <- extract(ndvi.84, mau.tan, fun = median, na.rm = TRUE)
l.kil <- extract(ndvi.84, lob.kil, fun = median, na.rm = TRUE)
i.kil <- extract(ndvi.84, ida.kil, fun = median, na.rm = TRUE)

#calculate single median value for each timestep for shapefiles with multiple polygons (swamp edge and Lobelia DA)
seg.ndvi.med <- colMedians(seg.ndvi, na.rm = TRUE)
l.kil.med <- colMedians(l.kil, na.rm = TRUE)

#read in file containing dates and ascii no.s of NDVI files
data.out <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/asc_nos.csv", header = TRUE)
#add median NDVI values to dataframe
data.out$v.ss.sel <- c(v.sel)
data.out$w.ss.mes <- c(w.mes)
data.out$m.ss.tan <- c(m.tan)
data.out$l.ss.kil <- c(l.kil.med)
data.out$i.ss.kil <- c(i.kil)
data.out$seg.ndvi <- c(seg.ndvi.med)

#adjust column names
colnames(data.out) <- c("date","asc.no","v.sel","w.mes","m.tan","l.kit","i.kit","se")

#save as .csv file
write.table(data.out, file = "C:/Users/nj832242/Dropbox/PhD/Publication/collar_med_NDVI.csv", sep = ",", row.names = FALSE)

#----SUBSET MONTHLY RAINFALL DATA FOR COLLAR PERIOD----

#read in all rainfall data
rainfall <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/monthly_rain.csv", header = TRUE)

#adjust date format
rainfall$date <- as.Date(rainfall$date, "%d/%m/%Y")

#subset to collar period
rain <- subset(rainfall, date > as.Date("2011-08-01") & date < as.Date("2012-08-01"))

#write to csv
write.csv(rain, file = "C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/col_rain.csv", row.names = FALSE)

#----CREATE DAILY NDVI, NDVI DIFF AND RAIN DATAFRAME FOR COLLAR PERIOD----

#install required packages
install.packages("plyr")
install.packages("lubridate")

#load required packages
library(plyr)
library(lubridate)

#read in data
#single column containing daily dates for collaring period
days <- read.csv("C:/Users/nj832242/Dropbox/Phd/Publication/Scripts&Data/col_days.csv", header = TRUE)
#median NDVI values of swamp edge and dispersal area over collar period
ndvi.df <- read.csv("C:/Users/nj832242/Dropbox/Phd/Publication/collar_med_NDVI.csv", header = TRUE)
#monthly rainfall over collar period
rain <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/col_rain.csv", header = TRUE)

#adjust date
ndvi.df$date <- as.Date(ndvi.df$date, "%d/%m/%Y")
days$date <- as.Date(days$date, "%d/%m/%Y")
rain$date <- as.Date(rain$date, "%Y-%m-%d")

#calculate difference between temporally adjacent NDVI values
cal.dif <- function(x) {
  else.diff <- c(NA, diff(x))
}

ndvi.df$v.sel.diff <- cal.dif(ndvi.df$v.sel)
ndvi.df$w.mes.diff <- cal.dif(ndvi.df$w.mes)
ndvi.df$m.tan.diff <- cal.dif(ndvi.df$m.tan)
ndvi.df$l.kit.diff <- cal.dif(ndvi.df$l.kit)
ndvi.df$i.kit.diff <- cal.dif(ndvi.df$i.kit)
ndvi.df$se.diff <- cal.dif(ndvi.df$se)

#merge ndvi dataframe with daily dates
day.ndvi <- merge(ndvi.df, days, by = "date", all = TRUE)

#NA fill ndvi data with nearest temporal neighbour
NAfill <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

day.ndvi$v.sel <- NAfill(day.ndvi$v.sel)
day.ndvi$w.mes <- NAfill(day.ndvi$w.mes)
day.ndvi$m.tan <- NAfill(day.ndvi$m.tan)
day.ndvi$l.kit <- NAfill(day.ndvi$l.kit)
day.ndvi$i.kit <- NAfill(day.ndvi$i.kit)
day.ndvi$se <- NAfill(day.ndvi$se)

day.ndvi$v.sel.diff <- NAfill(day.ndvi$v.sel.diff)
day.ndvi$w.mes.diff <- NAfill(day.ndvi$w.mes.diff)
day.ndvi$m.tan.diff <- NAfill(day.ndvi$m.tan.diff)
day.ndvi$l.kit.diff <- NAfill(day.ndvi$l.kit.diff)
day.ndvi$i.kit.diff <- NAfill(day.ndvi$i.kit.diff)
day.ndvi$se.diff <- NAfill(day.ndvi$se.diff)

#add month and year collumns to day.ndvi
day.ndvi <- mutate(day.ndvi, mon = month(date, label = TRUE))
day.ndvi <- mutate(day.ndvi, yr = year(date))

#add rain by month
add.rain <- function(x) {
  rain.list <- rain$rain
  f = day.ndvi[x,]
  
  if(f$mon == "Aug" & f$yr == 2011) {z <- rain.list[1]}
  if(f$mon == "Sep" & f$yr == 2011) {z <- rain.list[2]}
  if(f$mon == "Oct" & f$yr == 2011) {z <- rain.list[3]}
  if(f$mon == "Nov" & f$yr == 2011) {z <- rain.list[4]}
  if(f$mon == "Dec" & f$yr == 2011) {z <- rain.list[5]}
  
  if(f$mon == "Jan" & f$yr == 2012) {z <- rain.list[6]}
  if(f$mon == "Feb" & f$yr == 2012) {z <- rain.list[7]}
  if(f$mon == "Mar" & f$yr == 2012) {z <- rain.list[8]}
  if(f$mon == "Apr" & f$yr == 2012) {z <- rain.list[9]}
  if(f$mon == "May" & f$yr == 2012) {z <- rain.list[10]}
  if(f$mon == "Jun" & f$yr == 2012) {z <- rain.list[11]}
  if(f$mon == "Jul" & f$yr == 2012) {z <- rain.list[12]}
  if(f$mon == "Aug" & f$yr == 2012) {z <- rain.list[13]}
  
  return(z)}
day.ndvi$rain <- sapply(1:367, add.rain)

#write out data
write.table(day.ndvi, file = "C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_ndvi_rain.csv", sep = ",", row.names = FALSE)

#----CALCULATE DAILY PROTEIN FOR SWAMP EDGE AND DISPERSAL AREAS----

#read in data
#daily ndvi and rain data
day.ndvi <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_ndvi_rain.csv", header = TRUE)
#monthly rain  for collaar period data
rain <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/col_rain.csv", header = TRUE)

#add previous rain (rain last month)
add.prev.rain <- function(x) {
  prev.rain.list <- c(0.0, rain$rain)
  f = day.ndvi[x,]
  
  if(f$mon == "Aug" & f$yr == 2011) {z <- prev.rain.list[1]}
  if(f$mon == "Sep" & f$yr == 2011) {z <- prev.rain.list[2]}
  if(f$mon == "Oct" & f$yr == 2011) {z <- prev.rain.list[3]}
  if(f$mon == "Nov" & f$yr == 2011) {z <- prev.rain.list[4]}
  if(f$mon == "Dec" & f$yr == 2011) {z <- prev.rain.list[5]}
  
  if(f$mon == "Jan" & f$yr == 2012) {z <- prev.rain.list[6]}
  if(f$mon == "Feb" & f$yr == 2012) {z <- prev.rain.list[7]}
  if(f$mon == "Mar" & f$yr == 2012) {z <- prev.rain.list[8]}
  if(f$mon == "Apr" & f$yr == 2012) {z <- prev.rain.list[9]}
  if(f$mon == "May" & f$yr == 2012) {z <- prev.rain.list[10]}
  if(f$mon == "Jun" & f$yr == 2012) {z <- prev.rain.list[11]}
  if(f$mon == "Jul" & f$yr == 2012) {z <- prev.rain.list[12]}
  if(f$mon == "Aug" & f$yr == 2012) {z <- prev.rain.list[13]}
  
  return(z)}
day.ndvi$prev.rain <- sapply(1:367, add.prev.rain)

#add rain last month (true / false)
add.rlm <- function(x) {
  f = day.ndvi[x,]
  ifelse(f$prev.rain == 0.0, z <- "False", z <- "True")
  return(z)}
day.ndvi$rlm <- sapply(1:367, add.rlm)

#calculate SE protein
cal.SE.prot <- function(x) {
  f = day.ndvi[x,]
  ifelse(f$rlm == "True", z <- 11.8, z <- 8.4)
  return(z)
}
day.ndvi$se.prot <- sapply(1:367, cal.SE.prot)

#calculate dispersal area protein
cal.prot.v <- function(x) {
  f = day.ndvi[x,]
  ifelse(f$rlm == "True", ifelse(f$v.sel.diff > -0.03, z <- 23.0, z <- 10.0), z <- 10.0)
  return(z)
}
day.ndvi$v.sel.prot <- sapply(1:367, cal.prot.v)

cal.prot.w <- function(x) {
  f = day.ndvi[x,]
  ifelse(f$rlm == "True", ifelse(f$w.mes.diff > -0.03, z <- 23.0, z <- 10.0), z <- 10.0)
  return(z)
}
day.ndvi$w.mes.prot <- sapply(1:367, cal.prot.w)

cal.prot.m <- function(x) {
  f = day.ndvi[x,]
  ifelse(f$rlm == "True", ifelse(f$m.tan.diff > -0.03, z <- 23.0, z <- 10.0), z <- 10.0)
  return(z)
}
day.ndvi$m.tan.prot <- sapply(1:367, cal.prot.m)

cal.prot.l <- function(x) {
  f = day.ndvi[x,]
  ifelse(f$rlm == "True", ifelse(f$l.kit.diff > -0.03, z <- 23.0, z <- 10.0), z <- 10.0)
  return(z)
}
day.ndvi$l.prot <- sapply(1:367, cal.prot.l)

cal.prot.i <- function(x) {
  f = day.ndvi[x,]
  ifelse(f$rlm == "True", ifelse(f$i.kit.diff > -0.03, z <- 23.0, z <- 10.0), z <- 10.0)
  return(z)
}
day.ndvi$i.prot <- sapply(1:367, cal.prot.i)

#write out data
write.table(day.ndvi, file = "C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_prot.csv", sep = ",", row.names = FALSE)

#----CALCULATE DAILY MOTIVATION INDICES----

#read in data 
day.ndvi <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_prot.csv", header = TRUE)

#calculate dispersal area MIs
day.ndvi$v.sel.MI <- day.ndvi$v.sel * day.ndvi$v.sel.prot
day.ndvi$w.mes.MI <- day.ndvi$w.mes * day.ndvi$w.mes.prot
day.ndvi$m.tan.MI <- day.ndvi$m.tan * day.ndvi$m.tan.prot
day.ndvi$l.kit.MI <- day.ndvi$l.kit * day.ndvi$l.prot
day.ndvi$i.kit.MI <- day.ndvi$i.kit * day.ndvi$i.prot

#calculate SE MI
day.ndvi$se.MI <- day.ndvi$se * day.ndvi$se.prot

#write out data
write.table(day.ndvi, file = "C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_MI.csv", sep = ",", row.names = FALSE)

#----MODEL DAILY ABSENCE / PRESENCE FROM MIs----

#read in data
day.ndvi <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_MI.csv", header = TRUE)

#model absence / presence of individuals
abv <- function(x) {
  f <- day.ndvi[x,]
  ifelse((f$se.MI - f$v.sel.MI < -0.3) | (f$se.MI - f$v.sel.MI > 0.3),
         (ifelse(f$se.MI > f$v.sel.MI, r <- 0, r <- 1)), r <- NA)
  return(r)
}
day.ndvi$v.ap.mod <- sapply(1:367, abv)

abw <- function(x) {
  f <- day.ndvi[x,]
  ifelse((f$se.MI - f$w.mes.MI < -0.3) | (f$se.MI - f$w.mes.MI > 0.3),
         (ifelse(f$se.MI > f$w.mes.MI, r <- 0, r <- 1)), r <- NA)
  return(r)
}
day.ndvi$w.ap.mod <- sapply(1:367, abw)

abm <- function(x) {
  f <- day.ndvi[x,]
  ifelse((f$se.MI - f$m.tan.MI < -0.3) | (f$se.MI - f$m.tan.MI > 0.3),
         (ifelse(f$se.MI > f$m.tan.MI, r <- 0, r <- 1)), r <- NA)
  return(r)
}
day.ndvi$m.ap.mod <- sapply(1:367, abm)

abl <- function(x) {
  f <- day.ndvi[x,]
  ifelse((f$se.MI - f$l.kit.MI < -0.3) | (f$se.MI - f$l.kit.MI > 0.3),
         (ifelse(f$se.MI > f$l.kit.MI, r <- 0, r <- 1)), r <- NA)
  return(r)
}
day.ndvi$l.ap.mod <- sapply(1:367, abl)

abi <- function(x) {
  f <- day.ndvi[x,]
  ifelse((f$se.MI - f$i.kit.MI < -0.3) | (f$se.MI - f$i.kit.MI > 0.3),
         (ifelse(f$se.MI > f$i.kit.MI, r <- 0, r <- 1)), r <- NA)
  return(r)
}
day.ndvi$i.ap.mod <- sapply(1:367, abi)

#write out data
write.table(day.ndvi, file = "C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_modAP.csv", sep = ",", row.names = FALSE)

#----CALCULATE DAILY ABSENCE / PRESENCE FROM COLLAR DATA----

#if distance to the swamp is 0 at any point in the day, the female is deemed to be present for the day

#read in data
ida <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/Ida_swamp_dist.csv", header = TRUE)
lob <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/Lob_swamp_dist.csv", header = TRUE)
mau <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/Mau_swamp_dist.csv", header = TRUE)
vic <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/Vic_swamp_dist.csv", header = TRUE)
wil <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/Wil_swamp_dist.csv", header = TRUE)
day.ndvi <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_modAP.csv", header = TRUE)

#adjust date format
ida$date <- as.Date(ida$date, "%d/%m/%Y")
lob$date <- as.Date(lob$date, "%d/%m/%Y")
mau$date <- as.Date(mau$date, "%d/%m/%Y")
vic$date <- as.Date(vic$date, "%d/%m/%Y")
wil$date <- as.Date(wil$date, "%d/%m/%Y")
day.ndvi$date <- as.Date(day.ndvi$date, "%Y-%m-%d")

#create date lists
ida.date <- unique(c(as.Date(ida$date)))
lob.date <- unique(c(as.Date(lob$date)))
mau.date <- unique(c(as.Date(mau$date)))
vic.date <- unique(c(as.Date(vic$date)))
wil.date <- unique(c(as.Date(wil$date)))

#define functions to determine absence (1) / presence (0)
ida.ab.pr <- function(x) {
  unidate = ida[ida$date == x,]
  ifelse(any(unidate$dist.anp.swamp == 0), z<- 0, z<- 1)
  
  return(z)
}
lob.ab.pr <- function(x) {
  unidate = lob[lob$date == x,]
  ifelse(any(unidate$dist.anp.swamp == 0), z<- 0, z<- 1)
  
  return(z)
}
mau.ab.pr <- function(x) {
  unidate = mau[mau$date == x,]
  ifelse(any(unidate$dist.anp.swamp == 0), z<- 0, z<- 1)
  
  return(z)
}
vic.ab.pr <- function(x) {
  unidate = vic[vic$date == x,]
  ifelse(any(unidate$dist.anp.swamp == 0), z<- 0, z<- 1)
  
  return(z)
}
wil.ab.pr <- function(x) {
  unidate = wil[wil$date == x,]
  ifelse(any(unidate$dist.anp.swamp == 0), z<- 0, z<- 1)
  
  return(z)
}

#apply function to entire date list for each female
ida.abpr <- sapply(ida.date, ida.ab.pr)
lob.abpr <- sapply(lob.date, lob.ab.pr)
mau.abpr <- sapply(mau.date, mau.ab.pr)
vic.abpr <- sapply(vic.date, vic.ab.pr)
wil.abpr <- sapply(wil.date, wil.ab.pr)

#checking
mau[mau$date == mau.date[2],]

#create dataframes
ida.ap <- cbind.data.frame(ida.date, ida.abpr)
lob.ap <- cbind.data.frame(lob.date, lob.abpr)
mau.ap <- cbind.data.frame(mau.date, mau.abpr)
vic.ap <- cbind.data.frame(vic.date, vic.abpr)
wil.ap <- cbind.data.frame(wil.date, wil.abpr)

#rename columns
colnames(ida.ap) <- c("date","i.ap.col")
colnames(lob.ap) <- c("date","l.ap.col")
colnames(mau.ap) <- c("date","m.ap.col")
colnames(vic.ap) <- c("date","v.ap.col")
colnames(wil.ap) <- c("date","w.ap.col")

all.data <- merge(day.ndvi, ida.ap, by = "date", all.x = T)
all.data <- merge(all.data, lob.ap, by = "date", all.x = T)
all.data <- merge(all.data, mau.ap, by = "date", all.x = T)
all.data <- merge(all.data, vic.ap, by = "date", all.x = T)
all.data <- merge(all.data, wil.ap, by = "date", all.x = T)

#save as .csv file
write.table(all.data, file = "C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_colAP.csv", sep = ",", row.names = FALSE)

#----CALCULATE PHI COEFFICIENTS AND CHI-SQ P VALUES----

#install required packages
install.packages("psych")

#load packages
library(psych)

#read in data
day.ndvi <- read.csv("C:/Users/nj832242/Dropbox/PhD/Publication/Scripts&Data/daily_col_colAP.csv", header = TRUE)

#create contingency tables 
v.tab <- table(day.ndvi$v.ap.mod, day.ndvi$v.ap.col)
w.tab <- table(day.ndvi$w.ap.mod, day.ndvi$w.ap.col)
m.tab <- table(day.ndvi$m.ap.mod, day.ndvi$m.ap.col)
l.tab <- table(day.ndvi$l.ap.mod, day.ndvi$l.ap.col)
i.tab <- table(day.ndvi$i.ap.mod, day.ndvi$i.ap.col)

#change table names
dimnames(v.tab) = list(c("Pr_model", "Ab_model"), c("Pr_data", "Ab_data"))
dimnames(w.tab) = list(c("Pr_model", "Ab_model"), c("Pr_data", "Ab_data"))
dimnames(m.tab) = list(c("Pr_model", "Ab_model"), c("Pr_data", "Ab_data"))
dimnames(l.tab) = list(c("Pr_model", "Ab_model"), c("Pr_data", "Ab_data"))
dimnames(i.tab) = list(c("Pr_model", "Ab_model"), c("Pr_data", "Ab_data"))

#phi test
phi(v.tab)
phi(w.tab)
phi(m.tab)
phi(l.tab)
phi(i.tab)

#chi-squared
v.chi <- chisq.test(v.tab)
w.chi <- chisq.test(w.tab)
m.chi <- chisq.test(m.tab)
l.chi <- chisq.test(l.tab)
i.chi <- chisq.test(i.tab)

#results
v.chi
w.chi
m.chi
l.chi
i.chi

v.chi$observed #swap "statistic" for "parameter" "p.value" "method" "data.name" "observed" "expected"  "residuals" "stdres"

