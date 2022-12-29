#### transport ####
library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(multcompView)
library(cowplot)
library(showtext)
####FUN###
# mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
####sanjiang####
sj_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj_his_1850-2015.nc')
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp124-2014-2100.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp245-2014-2100.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp585-2014-2100.nc')


jhis_t <- ncvar_get(nc = sj_his,varid = 'TBOT')
jssp126_t <- ncvar_get(nc = sj_ssp126,varid = 'TBOT')
jssp245_t <- ncvar_get(nc = sj_ssp245,varid = 'TBOT')
jssp585_t <- ncvar_get(nc = sj_ssp585,varid = 'TBOT')

jhis_t <- jhis_t[1:60225]
jssp126_t <- jssp126_t[1:31390]
jssp245_t <- jssp245_t[1:31390]
jssp585_t <- jssp585_t[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatat <- data.frame(year,jssp126_t,jssp245_t,jssp585_t)
sj_hist <- aggregate(jhis_t,by = list(Y1),FUN = mean)
sj_126t <- aggregate(jssp126_t,by = list(year),FUN=mean)
sj_245t <- aggregate(jssp245_t,by = list(year),FUN=mean)
sj_585t <- aggregate(jssp585_t,by = list(year),FUN=mean)


st <- cbind(sj_126t$Group.1,sj_126t$x,sj_245t$x,sj_585t$x)
colnames(st) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
st_2016 <- st[3,]
st <- st[-1:-3,]
st <- data.frame(st)
jsspt <- melt(st, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Temperature')

ggplot(jsspt, aes(x = Year, y =  Temperature, color = Scenarios))+
  geom_line(size = 2)+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8)
  )
####ch4#####

jhis_ch4 <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_NETFLUX')
jssp126_ch4 <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_NETFLUX')
jssp245_ch4 <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_NETFLUX')
jssp585_ch4 <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_NETFLUX')
jhis_ch4 <- jhis_ch4[1:60225]*3600*24*16
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
ch4_2016 <- sch4[3,]
sch4 <- sch4[-1:-3,]
sch4 <- data.frame(sch4)
jssp <- melt(sch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

ggplot(jssp, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.8)
  )
#############
st <- as.data.frame(st)
sch4 <- as.data.frame(sch4)

data <- data.frame(st$SSP1.2.6,sch4$SSP1.2.6)
data <- data[which(data$st.SSP1.2.6 > st_2016[2]),]
data <- data[which(data$sch4.SSP1.2.6 <10),]

T10 <- log10((data$st.SSP1.2.6 - st_2016[2])/10)
R10 <- log10(data$sch4.SSP1.2.6/ch4_2016[2])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10126 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_126_sj <- matrix(ncol = 1,nrow = 56)
Q10_126_sj[1,1] <- q10126
for(i in 2:56){ 
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10126 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_126_sj[i,1] <- q10126
  
}
d <- Q10_126_sj[which(Q10_126_sj < 100)]
##
data <- data.frame(st$SSP2.4.5,sch4$SSP2.4.5)
data <- data[which(data$st.SSP2.4.5 > st_2016[3]),]
data <- data[which(data$sch4.SSP2.4.5 <10),]

T10 <- log10((data$st.SSP2.4.5 -st_2016[3])/10)
R10 <- log10(data$sch4.SSP2.4.5/ch4_2016[3])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10245 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_245_sj <- matrix(ncol = 1,nrow = 85)
Q10_245_sj[1,1] <- q10245
for(i in 2:82){
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10245 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_245_sj[i,1] <- q10245
  
}
d1 <- Q10_245_sj[which(Q10_245_sj< 100)]
##585
data <- data.frame(st$SSP5.8.5,sch4$SSP5.8.5)
data <- data[which(data$st.SSP5.8.5 > st_2016[4]),]
data <- data[which(data$sch4.SSP5.8.5 <10),]

T10 <- log10((data$st.SSP5.8.5-st_2016[4])/10)
R10 <- log10(data$sch4.SSP5.8.5/ch4_2016[4])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10585 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_585_sj <- matrix(ncol = 1,nrow = 85)
Q10_585_sj[1,1] <- q10585
for(i in 2:82){
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10585 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_585_sj[i,1] <- q10585
  
}
d2 <- Q10_585_sj[which(Q10_585_sj< 100)]

######
data <- data.frame(st$SSP1.2.6,sch4$SSP1.2.6)
Q10 <- (data$sch4.SSP1.2.6/ch4_2016[2])^(10/(data$st.SSP1.2.6 - st_2016[2]))
Q10 <- Q10[which(Q10 <100)]
data_q10 <- matrix(ncol = 6,nrow = 4)
data_q10[1,1] <- mean(Q10)
data_q10[1,2] <- sd(Q10)/sqrt(length(Q10))
data <- data.frame(st$SSP2.4.5,sch4$SSP2.4.5)
Q10 <- (data$sch4.SSP2.4.5/ch4_2016[3])^(10/(data$st.SSP2.4.5 - st_2016[3]))
Q10 <- Q10[which(Q10 <100)]
data_q10[1,3] <- mean(Q10)
data_q10[1,4] <- sd(Q10)/sqrt(length(Q10))
data <- data.frame(st$SSP5.8.5,sch4$SSP5.8.5)
Q10 <- (data$sch4.SSP5.8.5/ch4_2016[4])^(10/(data$st.SSP5.8.5 - st_2016[4]))
Q10 <- Q10[which(Q10 <100)]
data_q10[1,5] <- mean(Q10)
data_q10[1,6] <- sd(Q10)/sqrt(length(Q10))


# 
# T10 <- log10((st$SSP1.2.6-st_2016[2])/10)
# R10 <- log10(sch4$SSP1.2.6/ch4_2016[2])
# Q10_A <- lm(R10~T10)
# Q10_SSP126 <- 10^(Q10_A$coefficients[2])+Q10_A$coefficients[1]
# 
# T10 <- log10((st$SSP2.4.5-st_2016[3])/10)
# R10 <- log10(sch4$SSP2.4.5/ch4_2016[3])
# Q10_B <- lm(R10~T10)
# Q10_SSP245 <- 10^(Q10_B$coefficients[2])+Q10_B$coefficients[1]
# 
# T10 <- log10((st$SSP5.8.5-st_2016[4])/10)
# R10 <- log10(sch4$SSP5.8.5/ch4_2016[4])
# Q10_C <- lm(R10~T10)
# Q10_SSP585 <- 10^(Q10_C$coefficients[2])+Q10_C$coefficients[1]
# 
# a <- sch4$SSP1.2.6/ch4_2016[2]
# b <- 10/(st$SSP1.2.6 - st_2016[2])
# SJQ10126 <- a^b
# mean(SJQ10126)




#####CBS######
cbs_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbs_his_1850-2015.nc')
cbs_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp126-2014-2100.nc')
cbs_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp245-2014-2100.nc')
cbs_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp585-2014-2100.nc')


cbshis_t <- ncvar_get(nc = cbs_his,varid = 'TBOT')
cbsssp126_t <- ncvar_get(nc = cbs_ssp126,varid = 'TBOT')
cbsssp245_t <- ncvar_get(nc = cbs_ssp245,varid = 'TBOT')
cbsssp585_t <- ncvar_get(nc = cbs_ssp585,varid = 'TBOT')

cbshis_t <- cbshis_t[1:60225]
cbsssp126_t <- cbsssp126_t[1:31390]
cbsssp245_t <- cbsssp245_t[1:31390]
cbsssp585_t <- cbsssp585_t[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatat <- data.frame(year,cbsssp126_t,cbsssp245_t,cbsssp585_t)
cbs_hist <- aggregate(cbshis_t,by = list(Y1),FUN = mean)
cbs_126t <- aggregate(cbsssp126_t,by = list(year),FUN=mean)
cbs_245t <- aggregate(cbsssp245_t,by = list(year),FUN=mean)
cbs_585t <- aggregate(cbsssp585_t,by = list(year),FUN=mean)


cbsst <- cbind(cbs_126t$Group.1,cbs_126t$x,cbs_245t$x,cbs_585t$x)
colnames(cbsst) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
cbsst_2016 <- cbsst[3,]
cbsst <- cbsst[-1:-3,]
cbsst <- data.frame(cbsst)

#####ch4

cbshis_ch4 <- ncvar_get(nc = cbs_his,varid = 'CH4_SURF_NETFLUX')
cbsssp126_ch4 <- ncvar_get(nc = cbs_ssp126,varid = 'CH4_SURF_NETFLUX')
cbsssp245_ch4 <- ncvar_get(nc = cbs_ssp245,varid = 'CH4_SURF_NETFLUX')
cbsssp585_ch4 <- ncvar_get(nc = cbs_ssp585,varid = 'CH4_SURF_NETFLUX')
cbshis_ch4 <- cbshis_ch4[1:60225]*3600*24*16
cbsssp126_ch4 <- cbsssp126_ch4[1:31390]*3600*24*16
cbsssp245_ch4 <- cbsssp245_ch4[1:31390]*3600*24*16
cbsssp585_ch4 <- cbsssp585_ch4[1:31390]*3600*24*16
year <- rep(2014:2099,each = 365)
sspdata <- data.frame(year,cbsssp126_ch4,cbsssp245_ch4,cbsssp585_ch4)
cbs_126 <- aggregate(cbsssp126_ch4,by = list(year),FUN=sum)
cbs_245 <- aggregate(cbsssp245_ch4,by = list(year),FUN=sum)
cbs_585 <- aggregate(cbsssp585_ch4,by = list(year),FUN=sum)

cbsch4 <- cbind(cbs_126$Group.1,cbs_126$x,cbs_245$x,cbs_585$x)
colnames(cbsch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
cbsch4_2016 <- cbsch4[3,]
cbsch4 <- cbsch4[-1:-3,]
cbsch4 <- data.frame(cbsch4)

# T10 <- log10((cbsst$SSP1.2.6-cbsst_2016[2])/10)
# R10 <- log10(cbsch4$SSP1.2.6/cbsch4_2016[2])
# Q10_A <- lm(R10~T10)
# Q10_SSP126 <- 10^(Q10_A$coefficients[2])+Q10_A$coefficients[1]
# 
# T10 <- log10((cbsst$SSP2.4.5-cbsst_2016[3])/10)
# R10 <- log10(cbsch4$SSP2.4.5/cbsch4_2016[3])
# Q10_B <- lm(R10~T10)
# Q10_SSP245 <- 10^(Q10_B$coefficients[2])+Q10_B$coefficients[1]
# 
# T10 <- log10((cbsst$SSP5.8.5-cbsst_2016[4])/10)
# R10 <- log10(cbsch4$SSP5.8.5/cbsch4_2016[4])
# Q10_C <- lm(R10~T10)
# Q10_SSP585 <- 10^(Q10_C$coefficients[2])+Q10_C$coefficients[1]
###########
cbsst <- as.data.frame(cbsst)
cbsch4 <- as.data.frame(cbsch4)

data <- data.frame(cbsst$SSP1.2.6,cbsch4$SSP1.2.6)
data <- data[which(data$cbsst.SSP1.2.6 > cbsst_2016[2]),]
data <- data[which(data$cbsch4.SSP1.2.6 <10),]

T10 <- log10((data$cbsst.SSP1.2.6 - cbsst_2016[2])/10)
R10 <- log10(data$cbsch4.SSP1.2.6/ch4_2016[2])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10126 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_126_sj <- matrix(ncol = 1,nrow = 56)
Q10_126_sj[1,1] <- q10126
for(i in 2:56){ 
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10126 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_126_sj[i,1] <- q10126
  
}
d3 <- Q10_126_sj[which(Q10_126_sj < 250)]
##
data <- data.frame(cbsst$SSP2.4.5,cbsch4$SSP2.4.5)
data <- data[which(data$cbsst.SSP2.4.5 > cbsst_2016[3]),]
data <- data[which(data$cbsch4.SSP2.4.5 <10),]

T10 <- log10((data$cbsst.SSP2.4.5 -cbsst_2016[3])/10)
R10 <- log10(data$cbsch4.SSP2.4.5/ch4_2016[3])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10245 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_245_sj <- matrix(ncol = 1,nrow = 85)
Q10_245_sj[1,1] <- q10245
for(i in 2:82){
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10245 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_245_sj[i,1] <- q10245
  
}
d4 <- Q10_245_sj[which(Q10_245_sj< 100)]
##585
data <- data.frame(cbsst$SSP5.8.5,cbsch4$SSP5.8.5)
data <- data[which(data$cbsst.SSP5.8.5 > cbsst_2016[4]),]
data <- data[which(data$cbsch4.SSP5.8.5 <10),]

T10 <- log10((data$cbsst.SSP5.8.5-cbsst_2016[4])/10)
R10 <- log10(data$cbsch4.SSP5.8.5/ch4_2016[4])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10585 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_585_sj <- matrix(ncol = 1,nrow = 85)
Q10_585_sj[1,1] <- q10585
for(i in 2:82){
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10585 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_585_sj[i,1] <- q10585
  
}
d5 <- Q10_585_sj[which(Q10_585_sj< 100)]

######
data <- data.frame(cbsst$SSP1.2.6,cbsch4$SSP1.2.6)
Q10 <- (data$cbsch4.SSP1.2.6/cbsch4_2016[2])^(10/(data$cbsst.SSP1.2.6 - cbsst_2016[2]))
Q10 <- Q10[which(Q10 <100)]
data_q10[2,1] <- mean(Q10)
data_q10[2,2] <- sd(Q10)/sqrt(length(Q10))
data <- data.frame(cbsst$SSP2.4.5,cbsch4$SSP2.4.5)
Q10 <- (data$cbsch4.SSP2.4.5/cbsch4_2016[3])^(10/(data$cbsst.SSP2.4.5 - cbsst_2016[3]))
Q10 <- Q10[which(Q10 <100)]
data_q10[2,3] <- mean(Q10)
data_q10[2,4] <- sd(Q10)/sqrt(length(Q10))
data <- data.frame(cbsst$SSP5.8.5,cbsch4$SSP5.8.5)
Q10 <- (data$cbsch4.SSP5.8.5/cbsch4_2016[4])^(10/(data$cbsst.SSP5.8.5 - cbsst_2016[4]))
Q10 <- Q10[which(Q10 <100)]
data_q10[2,5] <- mean(Q10)
data_q10[2,6] <- sd(Q10)/sqrt(length(Q10))


nc_close(cbs_his)
nc_close(cbs_ssp126)
nc_close(cbs_ssp245)
nc_close(cbs_ssp585)
######xxal####
xxal_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxal_his_1850-2015.nc')
xxal_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp126_2014-2100.nc')
xxal_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp245_2014-2100.nc')
xxal_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/5852xxal.nc')


xxalhis_t <- ncvar_get(nc = xxal_his,varid = 'TBOT')
xxalssp126_t <- ncvar_get(nc = xxal_ssp126,varid = 'TBOT')
xxalssp245_t <- ncvar_get(nc = xxal_ssp245,varid = 'TBOT')
xxalssp585_t <- ncvar_get(nc = xxal_ssp585,varid = 'TBOT')

xxalhis_t <- xxalhis_t[1:60225]
xxalssp126_t <- xxalssp126_t[1:31390]
xxalssp245_t <- xxalssp245_t[1:31390]
xxalssp585_t <- xxalssp585_t[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatat <- data.frame(year,xxalssp126_t,xxalssp245_t,xxalssp585_t)
xxal_hist <- aggregate(xxalhis_t,by = list(Y1),FUN = mean)
xxal_126t <- aggregate(xxalssp126_t,by = list(year),FUN=mean)
xxal_245t <- aggregate(xxalssp245_t,by = list(year),FUN=mean)
xxal_585t <- aggregate(xxalssp585_t,by = list(year),FUN=mean)


xxalst <- cbind(xxal_126t$Group.1,xxal_126t$x,xxal_245t$x,xxal_585t$x)
colnames(xxalst) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
xxalst_2016 <- xxalst[3,]
xxalst <- xxalst[-1:-3,]
xxalst <- data.frame(xxalst)

#####ch4

xxalhis_ch4 <- ncvar_get(nc = xxal_his,varid = 'CH4_SURF_NETFLUX')
xxalssp126_ch4 <- ncvar_get(nc = xxal_ssp126,varid = 'CH4_SURF_NETFLUX')
xxalssp245_ch4 <- ncvar_get(nc = xxal_ssp245,varid = 'CH4_SURF_NETFLUX')
xxalssp585_ch4 <- ncvar_get(nc = xxal_ssp585,varid = 'CH4_SURF_NETFLUX')
xxalhis_ch4 <- xxalhis_ch4[1:60225]*3600*24
xxalssp126_ch4 <- xxalssp126_ch4[1:31390]*3600*24
xxalssp245_ch4 <- xxalssp245_ch4[1:31390]*3600*24
xxalssp585_ch4 <- xxalssp585_ch4[1:31390]*3600*24
year <- rep(2014:2099,each = 365)
sspdata <- data.frame(year,xxalssp126_ch4,xxalssp245_ch4,xxalssp585_ch4)
xxal_126 <- aggregate(xxalssp126_ch4,by = list(year),FUN=sum)
xxal_245 <- aggregate(xxalssp245_ch4,by = list(year),FUN=sum)
xxal_585 <- aggregate(xxalssp585_ch4,by = list(year),FUN=sum)

xxalch4 <- cbind(xxal_126$Group.1,xxal_126$x,xxal_245$x,xxal_585$x)
colnames(xxalch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
xxalch4_2016 <- xxalch4[3,]
xxalch4 <- xxalch4[-1:-3,]
xxalch4 <- data.frame(xxalch4)

# T10 <- log10((xxalst$SSP1.2.6-xxalst_2016[2])/10)
# R10 <- log10(xxalch4$SSP1.2.6/xxalch4_2016[2])
# Q10_A <- lm(R10~T10)
# Q10_SSP126 <- 10^(Q10_A$coefficients[2])+Q10_A$coefficients[1]
# 
# T10 <- log10((xxalst$SSP2.4.5-xxalst_2016[3])/10)
# R10 <- log10(xxalch4$SSP2.4.5/xxalch4_2016[3])
# Q10_B <- lm(R10~T10)
# Q10_SSP245 <- 10^(Q10_B$coefficients[2])+Q10_B$coefficients[1]
# 
# T10 <- log10((xxalst$SSP5.8.5-xxalst_2016[4])/10)
# R10 <- log10(xxalch4$SSP5.8.5/xxalch4_2016[4])
# Q10_C <- lm(R10~T10)
# Q10_SSP585 <- 10^(Q10_C$coefficients[2])+Q10_C$coefficients[1]
###########
xxalst <- as.data.frame(xxalst)
xxalch4 <- as.data.frame(xxalch4)

data <- data.frame(xxalst$SSP1.2.6,xxalch4$SSP1.2.6)
data <- data[which(data$xxalst.SSP1.2.6 > xxalst_2016[2]),]
data <- data[which(data$xxalch4.SSP1.2.6 <10),]

T10 <- log10((data$xxalst.SSP1.2.6 - xxalst_2016[2])/10)
R10 <- log10(data$xxalch4.SSP1.2.6/ch4_2016[2])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10126 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_126_sj <- matrix(ncol = 1,nrow = 56)
Q10_126_sj[1,1] <- q10126
for(i in 2:56){ 
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10126 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_126_sj[i,1] <- q10126
  
}
d6 <- Q10_126_sj[which(Q10_126_sj < 250)]
##
data <- data.frame(xxalst$SSP2.4.5,xxalch4$SSP2.4.5)
data <- data[which(data$xxalst.SSP2.4.5 > xxalst_2016[3]),]
data <- data[which(data$xxalch4.SSP2.4.5 <10),]

T10 <- log10((data$xxalst.SSP2.4.5 -xxalst_2016[3])/10)
R10 <- log10(data$xxalch4.SSP2.4.5/ch4_2016[3])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10245 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_245_sj <- matrix(ncol = 1,nrow = 85)
Q10_245_sj[1,1] <- q10245
for(i in 2:82){
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10245 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_245_sj[i,1] <- q10245
  
}
d7 <- Q10_245_sj[which(Q10_245_sj< 100)]
##585
data <- data.frame(xxalst$SSP5.8.5,xxalch4$SSP5.8.5)
data <- data[which(data$xxalst.SSP5.8.5 > xxalst_2016[4]),]
data <- data[which(data$xxalch4.SSP5.8.5 <10),]

T10 <- log10((data$xxalst.SSP5.8.5-xxalst_2016[4])/10)
R10 <- log10(data$xxalch4.SSP5.8.5/ch4_2016[4])
sspq10 <- data.frame(
  T101 <- T10[1:2],
  R101 <- R10[1:2]
)
lmq10 <- lm(R101~T101,data = sspq10)
q10585 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
Q10_585_sj <- matrix(ncol = 1,nrow = 85)
Q10_585_sj[1,1] <- q10585
for(i in 2:82){
  if(is.na(T10[i]) == TRUE) 
    next
  sspq10 <- data.frame(
    T101 <- T10[i:(i+1)],
    R101 <- R10[i:(i+1)]
  )
  lmq10 <- lm(R101~T101,data = sspq10)
  q10585 <- 10^lmq10$coefficients[2]+lmq10$coefficients[1]
  Q10_585_sj[i,1] <- q10585
  
}
d8 <- Q10_585_sj[which(Q10_585_sj< 1000)]
#########
data <- data.frame(xxalst$SSP1.2.6,xxalch4$SSP1.2.6)
Q10 <- (data$xxalch4.SSP1.2.6/xxalch4_2016[2])^(10/(data$xxalst.SSP1.2.6 - xxalst_2016[2]))
Q10 <- Q10[which(Q10 <100)]
data_q10[3,1] <- mean(Q10)
data_q10[3,2] <- sd(Q10)/sqrt(length(Q10))
data <- data.frame(xxalst$SSP2.4.5,xxalch4$SSP2.4.5)
Q10 <- (data$xxalch4.SSP2.4.5/xxalch4_2016[3])^(10/(data$xxalst.SSP2.4.5 - xxalst_2016[3]))
Q10 <- Q10[which(Q10 <100)]
data_q10[3,3] <- mean(Q10)
data_q10[3,4] <- sd(Q10)/sqrt(length(Q10))
data <- data.frame(xxalst$SSP5.8.5,xxalch4$SSP5.8.5)
Q10 <- (data$xxalch4.SSP5.8.5/xxalch4_2016[4])^(10/(data$xxalst.SSP5.8.5 - xxalst_2016[4]))
Q10 <- Q10[which(Q10 <100)]
data_q10[3,5] <- mean(Q10)
data_q10[3,6] <- sd(Q10)/sqrt(length(Q10))


nc_close(xxal_his)
nc_close(xxal_ssp126)
nc_close(xxal_ssp245)
nc_close(xxal_ssp585)

####plot#####
data_q10 <- data_q10[1:3,]
q10_mean <- data_q10[,c(1,3,5)]
q10_sd <- data_q10[,c(2,4,6)]
colnames(q10_mean) <- c('SSP1-2.6','SSP2-4.5','SSP5-8.5')
colnames(q10_sd) <- c('SSP1-2.6','SSP2-4.5','SSP5-8.5')
q10_mean <- as.data.frame(q10_mean)
q10_sd <- as.data.frame(q10_sd)
q10_mean$Site <- c('Sanjiang Plain','Changbai Mountain','Lesser Khingan Mountain')
q10_sd$Site <- c('Sanjiang Plain','Changbai Mountain','Lesser Khingan Mountain')
a <- melt(q10_mean, id.vars =c('Site'), variable.name = 'Scenarios', value.name = 'Q10') 
b <- melt(q10_sd, id.vars =c('Site'), variable.name = 'Scenarios', value.name = 'Q10sd') 

q10plot <- cbind(a,b$Q10sd)
colnames(q10plot) <- c('Site','Scenarios','mean','sd')
q10plot$Site <- factor(q10plot$Site,levels = c('Sanjiang Plain','Changbai Mountain','Lesser Khingan Mountain'))
ggplot(q10plot, aes(x = factor(Site), y = mean, fill = Scenarios, colour = Scenarios)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x=" ", y="Q10 for CH4 fluxes") +
  scale_fill_brewer(palette="Set1")+
  scale_color_brewer(palette="Set1")+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75))+
  theme(
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.title = element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=22,colour='black' ),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5)
  )
