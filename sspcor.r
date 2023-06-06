####sanjiang####
sj_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj_his_1850-2015.nc')
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj1266.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj2456.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj5856.nc')

######SOM####

jssp126_som <- ncvar_get(nc = sj_ssp126,varid = 'TOTSOMC')
jssp245_som <- ncvar_get(nc = sj_ssp245,varid = 'TOTSOMC')
jssp585_som <- ncvar_get(nc = sj_ssp585,varid = 'TOTSOMC')

jssp126_som <- jssp126_som[1:31390]
jssp245_som <- jssp245_som[1:31390]
jssp585_som <- jssp585_som[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatasom <- data.frame(year,jssp126_som,jssp245_som,jssp585_som)
 
sj_126som <- aggregate(jssp126_som,by = list(year),FUN=sum)
sj_245som <- aggregate(jssp245_som,by = list(year),FUN=sum)
sj_585som <- aggregate(jssp585_som,by = list(year),FUN=sum)


ssom <- cbind(sj_126som$Group.1,sj_126som$x,sj_245som$x,sj_585som$x)
colnames(ssom) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
ssom <- data.frame(ssom)
# ssom <- ssom[-1:-2,]
ssom <- ssom[-1,]
jsspd <- melt(ssom, id.vars ='Year', variable.name = 'Scenarios', value.name = 'SOM')  ##data.frame

##### CACEBIOS #####


jssp126_ab <- ncvar_get(nc = sj_ssp126,varid = 'CACEBIOS')
jssp245_ab <- ncvar_get(nc = sj_ssp245,varid = 'CACEBIOS')
jssp585_ab <- ncvar_get(nc = sj_ssp585,varid = 'CACEBIOS')

 
jssp126_ab <- jssp126_ab[1:8,1:31390]*12
jssp245_ab <- jssp245_ab[1:8,1:31390]*12
jssp585_ab <- jssp585_ab[1:8,1:31390]*12

 
jssp126_ab <- apply(jssp126_ab,2,FUN = mean)
jssp245_ab <- apply(jssp245_ab,2,FUN = mean)
jssp585_ab <- apply(jssp585_ab,2,FUN = mean)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataab <- data.frame(year,jssp126_ab,jssp245_ab,jssp585_ab)
 
sj_126ab <- aggregate(jssp126_ab,by = list(year),FUN=sum)
sj_245ab <- aggregate(jssp245_ab,by = list(year),FUN=sum)
sj_585ab <- aggregate(jssp585_ab,by = list(year),FUN=sum)
 
sab <- cbind(sj_126ab$Group.1,sj_126ab$x,sj_245ab$x,sj_585ab$x)
colnames(sab) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sab <- data.frame(sab)
sab <- sab[-1,]
jsspab <- melt(sab, id.vars ='Year', variable.name = 'Scenarios', value.name = 'AM')  ##data.frame

####HM####

jssp126_HM <- ncvar_get(nc = sj_ssp126,varid = 'CCO2BIOS')
jssp245_HM <- ncvar_get(nc = sj_ssp245,varid = 'CCO2BIOS')
jssp585_HM <- ncvar_get(nc = sj_ssp585,varid = 'CCO2BIOS')

 
jssp126_HM <- jssp126_HM[1:8,1:31390]*12
jssp245_HM <- jssp245_HM[1:8,1:31390]*12
jssp585_HM <- jssp585_HM[1:8,1:31390]*12

 
jssp126_HM <- apply(jssp126_HM,2,FUN = sum)
jssp245_HM <- apply(jssp245_HM,2,FUN = sum)
jssp585_HM <- apply(jssp585_HM,2,FUN = sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataHM <- data.frame(year,jssp126_HM,jssp245_HM,jssp585_HM)
  
sj_126HM <- aggregate(jssp126_HM,by = list(year),FUN=sum)
sj_245HM <- aggregate(jssp245_HM,by = list(year),FUN=sum)
sj_585HM <- aggregate(jssp585_HM,by = list(year),FUN=sum)
 
sHM <- cbind(sj_126HM$Group.1,sj_126HM$x,sj_245HM$x,sj_585HM$x)
colnames(sHM) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sHM <- data.frame(sHM)
sHM <- sHM[-1,]
jsspHM <- melt(sHM, id.vars ='Year', variable.name = 'Scenarios', value.name = 'HM')  ##data.frame

#### AER####

jssp126_AER <- ncvar_get(nc = sj_ssp126,varid = 'CAERCH4BIOS')
jssp245_AER <- ncvar_get(nc = sj_ssp245,varid = 'CAERCH4BIOS')
jssp585_AER <- ncvar_get(nc = sj_ssp585,varid = 'CAERCH4BIOS')

 
jssp126_AER <- jssp126_AER[1:8,1:31390]*12
jssp245_AER <- jssp245_AER[1:8,1:31390]*12
jssp585_AER <- jssp585_AER[1:8,1:31390]*12

 
jssp126_AER <- apply(jssp126_AER,2,sum)
jssp245_AER <- apply(jssp245_AER,2,sum)
jssp585_AER <- apply(jssp585_AER,2,sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataAER <- data.frame(year,jssp126_AER,jssp245_AER,jssp585_AER)
 
sj_126AER <- aggregate(jssp126_AER,by = list(year),FUN=sum)
sj_245AER <- aggregate(jssp245_AER,by = list(year),FUN=sum)
sj_585AER <- aggregate(jssp585_AER,by = list(year),FUN=sum)
 
sAER <- cbind(sj_126AER$Group.1,sj_126AER$x,sj_245AER$x,sj_585AER$x)
colnames(sAER) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sAER <- data.frame(sAER)
sAER <- sAER[-1,]
jsspAER <- melt(sAER, id.vars ='Year', variable.name = 'Scenarios', value.name = 'AER')  ##data.frame

#### ANAER####

jssp126_ANAER <- ncvar_get(nc = sj_ssp126,varid = 'CANAERCH4BIOS')
jssp245_ANAER <- ncvar_get(nc = sj_ssp245,varid = 'CANAERCH4BIOS')
jssp585_ANAER <- ncvar_get(nc = sj_ssp585,varid = 'CANAERCH4BIOS')


jssp126_ANAER <- jssp126_ANAER[1:8,1:31390]*12
jssp245_ANAER <- jssp245_ANAER[1:8,1:31390]*12
jssp585_ANAER <- jssp585_ANAER[1:8,1:31390]*12

jssp126_ANAER <- apply(jssp126_ANAER,2,sum)
jssp245_ANAER <- apply(jssp245_ANAER,2,sum)
jssp585_ANAER <- apply(jssp585_ANAER,2,sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataANAER <- data.frame(year,jssp126_ANAER,jssp245_ANAER,jssp585_ANAER)
sj_126ANAER <- aggregate(jssp126_ANAER,by = list(year),FUN=sum)
sj_245ANAER <- aggregate(jssp245_ANAER,by = list(year),FUN=sum)
sj_585ANAER <- aggregate(jssp585_ANAER,by = list(year),FUN=sum)

sANAER <- cbind(sj_126ANAER$Group.1,sj_126ANAER$x,sj_245ANAER$x,sj_585ANAER$x)
colnames(sANAER) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sANAER <- data.frame(sANAER)
sANAER <- sANAER[-1,]
jsspANAER <- melt(sANAER, id.vars ='Year', variable.name = 'Scenarios', value.name = 'ANAER')  ##data.frame

######CH4########

jssp126_ch4 <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_NETFLUX')
jssp245_ch4 <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_NETFLUX')
jssp585_ch4 <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_NETFLUX')

jssp126_ch4 <- jssp126_ch4[1:31390]*3600*24*16
jssp245_ch4 <- jssp245_ch4[1:31390]*3600*24*16
jssp585_ch4 <- jssp585_ch4[1:31390]*3600*24*16
year <- rep(2014:2099,each = 365)
sspdata <- data.frame(year,jssp126_ch4,jssp245_ch4,jssp585_ch4)
sj_126 <- aggregate(jssp126_ch4,by = list(year),FUN=sum)
sj_245 <- aggregate(jssp245_ch4,by = list(year),FUN=sum)
sj_585 <- aggregate(jssp585_ch4,by = list(year),FUN=sum)

sch4 <- cbind(sj_126$Group.1,sj_126$x,sj_245$x,sj_585$x)
colnames(sch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sch4 <- data.frame(sch4)
sch4 <- sch4[-1,]
jssp <- melt(sch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

#####TBOT####

jssp126_t <- ncvar_get(nc = sj_ssp126,varid = 'TBOT')
jssp245_t <- ncvar_get(nc = sj_ssp245,varid = 'TBOT')
jssp585_t <- ncvar_get(nc = sj_ssp585,varid = 'TBOT')


jssp126_t <- jssp126_t[1:31390]
jssp245_t <- jssp245_t[1:31390]
jssp585_t <- jssp585_t[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatat <- data.frame(year,jssp126_t,jssp245_t,jssp585_t)

sj_126t <- aggregate(jssp126_t,by = list(year),FUN=mean)
sj_245t <- aggregate(jssp245_t,by = list(year),FUN=mean)
sj_585t <- aggregate(jssp585_t,by = list(year),FUN=mean)


st <- cbind(sj_126t$Group.1,sj_126t$x,sj_245t$x,sj_585t$x)
colnames(st) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
st <- data.frame(st)
st <- st[-1,]
#lm
jssptbo <- melt(st, id.vars ='Year', variable.name = 'Scenarios', value.name = 'tbo')  ##data.frame

#####RAIN####

jssp126_RAIN <- ncvar_get(nc = sj_ssp126,varid = 'RAIN')
jssp245_RAIN <- ncvar_get(nc = sj_ssp245,varid = 'RAIN')
jssp585_RAIN <- ncvar_get(nc = sj_ssp585,varid = 'RAIN')


jssp126_RAIN <- jssp126_RAIN[1:31390]*3600*24
jssp245_RAIN <- jssp245_RAIN[1:31390]*3600*24
jssp585_RAIN <- jssp585_RAIN[1:31390]*3600*24
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataRAIN <- data.frame(year,jssp126_RAIN,jssp245_RAIN,jssp585_RAIN)
sj_126RAIN <- aggregate(jssp126_RAIN,by = list(year),FUN=sum)
sj_245RAIN <- aggregate(jssp245_RAIN,by = list(year),FUN=sum)
sj_585RAIN <- aggregate(jssp585_RAIN,by = list(year),FUN=sum)


sRAIN <- cbind(sj_126RAIN$Group.1,sj_126RAIN$x,sj_245RAIN$x,sj_585RAIN$x)
colnames(sRAIN) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sRAIN <- data.frame(sRAIN)
sRAIN <- sRAIN[-1,]
#lm
jssprain <- melt(sRAIN, id.vars ='Year', variable.name = 'Scenarios', value.name = 'rain')  ##data.frame

####SWC#####

jssp126_swc <- ncvar_get(nc = sj_ssp126,varid = 'H2OSOI')
jssp245_swc <- ncvar_get(nc = sj_ssp245,varid = 'H2OSOI')
jssp585_swc <- ncvar_get(nc = sj_ssp585,varid = 'H2OSOI')


jssp126_swc <- jssp126_swc[1,1:31390] 
jssp245_swc <- jssp245_swc[1,1:31390] 
jssp585_swc <- jssp585_swc[1,1:31390] 

# jhis_swc <- apply(jhis_swc,2,sum)
# jssp126_swc <- apply(jssp126_swc,2,sum)
# jssp245_swc <- apply(jssp245_swc,2,sum)
# jssp585_swc <- apply(jssp585_swc,2,sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataswc <- data.frame(year,jssp126_swc,jssp245_swc,jssp585_swc)
sj_126swc <- aggregate(jssp126_swc,by = list(year),FUN=mean)
sj_245swc <- aggregate(jssp245_swc,by = list(year),FUN=mean)
sj_585swc <- aggregate(jssp585_swc,by = list(year),FUN=mean)


sswc <- cbind(sj_126swc$Group.1,sj_126swc$x,sj_245swc$x,sj_585swc$x)
colnames(sswc) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sswc <- data.frame(sswc)
sswc <- sswc[-1,]
#lm
jsspswc <- melt(sswc, id.vars ='Year', variable.name = 'Scenarios', value.name = 'swc')  ##data.frame

####sT####

jssp126_st <- ncvar_get(nc = sj_ssp126,varid = 'TSOI_10CM')
jssp245_st <- ncvar_get(nc = sj_ssp245,varid = 'TSOI_10CM')
jssp585_st <- ncvar_get(nc = sj_ssp585,varid = 'TSOI_10CM')

jssp126_st <- jssp126_st[1:31390] 
jssp245_st <- jssp245_st[1:31390] 
jssp585_st <- jssp585_st[1:31390] 

# jhis_st <- apply(jhis_st,2,sum)
# jssp126_st <- apply(jssp126_st,2,sum)
# jssp245_st <- apply(jssp245_st,2,sum)
# jssp585_st <- apply(jssp585_st,2,sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatast <- data.frame(year,jssp126_st,jssp245_st,jssp585_st)
sj_126st <- aggregate(jssp126_st,by = list(year),FUN=mean)
sj_245st <- aggregate(jssp245_st,by = list(year),FUN=mean)
sj_585st <- aggregate(jssp585_st,by = list(year),FUN=mean)


sst <- cbind(sj_126st$Group.1,sj_126st$x,sj_245st$x,sj_585st$x)
colnames(sst) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sst <- data.frame(sst)
sst <- sst[-1,]
#lm
jsspst <- melt(sst, id.vars ='Year', variable.name = 'Scenarios', value.name = 'st')  ##data.frame

######QVEGE######
jssp126_QVEGE <- ncvar_get(nc = sj_ssp126,varid = 'QVEGE')
jssp245_QVEGE <- ncvar_get(nc = sj_ssp245,varid = 'QVEGE')
jssp585_QVEGE <- ncvar_get(nc = sj_ssp585,varid = 'QVEGE')

jssp126_QVEGE <- jssp126_QVEGE[1:31390]*3600*24
jssp245_QVEGE <- jssp245_QVEGE[1:31390]*3600*24
jssp585_QVEGE <- jssp585_QVEGE[1:31390]*3600*24

# jhis_QVEGE <- apply(jhis_QVEGE,2,sum)
# jssp126_QVEGE <- apply(jssp126_QVEGE,2,sum)
# jssp245_QVEGE <- apply(jssp245_QVEGE,2,sum)
# jssp585_QVEGE <- apply(jssp585_QVEGE,2,sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataQVEGE <- data.frame(year,jssp126_QVEGE,jssp245_QVEGE,jssp585_QVEGE)
sj_126QVEGE <- aggregate(jssp126_QVEGE,by = list(year),FUN=mean)
sj_245QVEGE <- aggregate(jssp245_QVEGE,by = list(year),FUN=mean)
sj_585QVEGE <- aggregate(jssp585_QVEGE,by = list(year),FUN=mean)


sQVEGE <- cbind(sj_126QVEGE$Group.1,sj_126QVEGE$x,sj_245QVEGE$x,sj_585QVEGE$x)
colnames(sQVEGE) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sQVEGE <- data.frame(sQVEGE)
sQVEGE <- sQVEGE[-1,]
#lm
jsspQVEGE <- melt(sQVEGE, id.vars ='Year', variable.name = 'Scenarios', value.name = 'QVEGE')  ##data.frame




#######cor####
mdata <- cbind(jssp,jsspd$SOM,jsspab$AM,jsspHM$HM,jsspAER$AER,jsspANAER$ANAER,jssptbo$tbo,
      jssprain$rain,jsspswc$swc,jsspst$st,jsspQVEGE$QVEGE)
colnames(mdata) <- c("Year","Scenarios","Methane","SOM","AM",'HM','AER','ANAER','TBOT','RAIN',
                     'SWC','ST','EVAP')
library(PerformanceAnalytics)
my_data <- mdata[, c(-1,-2)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

my_data1 <- scale(my_data)
chart.Correlation(my_data1, histogram=TRUE, pch=19)
