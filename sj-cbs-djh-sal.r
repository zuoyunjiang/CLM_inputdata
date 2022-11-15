#####Yunjiang zuo####
#####Sanjiang Plain#######
library(ncdf4)
library(ggplot2)
library(caret)
library(cowplot)
library(reshape2)
library(plot3D)
library(RColorBrewer)
library(dplyr)
library(scales)
library(ggpmisc)
# install.packages("coolbutuseless/threed")
# devtools::install_github('coolbutuseless/ggthreed')
library(ggthreed)
# install.packages('plotrix')
library(plotrix)
library(grid)

###################

#############gpp############
##sj
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/test_t.clm2.h0.2012-01-01-00000 (428).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2012-01-01-00000 (3).nc')
OBS <- read.csv(file = 'D:/Rtest/2012_CO2.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
NOGEN <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_OBS <-  OBS$GPP[1:153]
GPP_MODEL <- GPP_MODEL*24*3600  #g/m2/d
NOGEN <- NOGEN*24*3600

DOY <- matrix(nrow =854 ,ncol =1)
DOY[1:124,1] <- 150:273
DOY[125:489,1] <- 1:365
DOY[490:854,1] <- 1:365
OBS <- GPP_OBS[30:153]
MODEL <- GPP_MODEL
NOGEN <- NOGEN
GPP <- matrix(nrow =854 ,ncol =1)
GPP[1:124,1] <- OBS
GPP[125:489,1] <- MODEL
GPP[490:854,1] <- NOGEN
TYPE <- c(rep('OBS',124),rep('MODEL_GEN',365),rep('MODEL_NOGEN',365))
mydata_sj <- data.frame(DOY,GPP,TYPE)
mydata1_sj <- mydata_sj[1:489,]
##cbs

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/cbs_t.clm2.h0.2011-01-01-00000 (110).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/mic-gen/cbs_nogens.clm2.h0.2011-01-01-00000 (3).nc')
OBS <- read.table(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/fluxdata/nee-gpp-er.csv',header =T, sep = ',')
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
NOGEN <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_MODEL <- GPP_MODEL*1000  #mg/m2/s
NOGEN <- NOGEN*1000
GPP_OBS <-  OBS[1:89,17]*-1
GPP_day <- as.matrix(OBS[1:89,16])

DOY <- matrix(nrow =819 ,ncol =1)
DOY[1:89,1] <- GPP_day
DOY[90:454,1] <- 1:365
DOY[455:819,1] <- 1:365
OBS <- GPP_OBS
MODEL <- GPP_MODEL
NOGEN <- NOGEN
GPP <- matrix(nrow =819 ,ncol =1)
GPP[1:89,1] <- OBS
GPP[90:454,1] <- MODEL
GPP[455:819,1] <- NOGEN
TYPE <- c(rep('OBS',89),rep('MODEL_GEN',365),rep('MODEL_NOGEN',365))
mydata_cbs <- data.frame(DOY,GPP,TYPE)
mydata_cbs$GPP <- mydata_cbs$GPP*86.4 #g/m2/d
mydata1_cbs <- mydata_cbs[1:454,]

##djh

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (82).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_nogens.clm2.h0.2019-01-01-00000 (3).nc')
OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/microbe/djh/19GPP.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
nogen <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_OBS <-  OBS$mean/8
GPP_MODEL <- GPP_MODEL*86.4 #kg/m2/d 
nogen <- nogen*86.4

DOY <- matrix(nrow =776 ,ncol =1)
DOY[1:46,1] <- OBS$DOY
DOY[47:411,1] <- 1:365
DOY[412:776,1] <- 1:365
OBS <- GPP_OBS
MODEL <- GPP_MODEL
GPP <- matrix(nrow =776 ,ncol =1)
GPP[1:46,1] <- OBS
GPP[47:411] <- MODEL
GPP[412:776] <- nogen
TYPE <- c(rep('OBS',46),rep('MODEL_GEN',365),rep('MODEL_NOGEN',365))
mydata_djh <- data.frame(DOY,GPP,TYPE)
mydata_djh$GPP <- mydata_djh$GPP*1000  #g/m2/d

mydata1_djh <- mydata_djh[1:411,]

##sallie

try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/t_sal.clm2.h0.2010-01-01-00000 (182).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/tsal_t.clm2.h0.2010-01-01-00000 (96).nc ')

OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/microbe/sallie fen/GPP.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
NOGEN <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_MODEL <- GPP_MODEL*86400  #g/m2/d
NOGEN <- NOGEN*86400
GOBS <- OBS[which(OBS$YEAR == 2010),]
GPP_OBS <-  GOBS$GPP
GPP_day <- GOBS$DOY[1:46]

DOY <- matrix(nrow =776 ,ncol =1)
DOY[1:46,1] <- GPP_day
DOY[47:411,1] <- 1:365
DOY[412:776,1] <- 1:365
OBS <- GPP_OBS
MODEL <- GPP_MODEL
NOGEN <- NOGEN
GPP <- matrix(nrow =776 ,ncol =1)
GPP[1:46,1] <- OBS
GPP[47:411,1] <- MODEL
GPP[412:776,1] <- NOGEN
TYPE <- c(rep('OBS',46),rep('MODEL_GEN',365),rep('MODEL_NOGEN',365))
mydata_sal <- data.frame(DOY,GPP,TYPE)
mydata_sal$GPP <- as.double(mydata_sal$GPP)
mydata1_sal <- mydata_sal[1:411,]

######
gppdata <- rbind(mydata_sj,mydata_cbs,mydata_djh,mydata_sal)# 854,819,776,776
gppdata$SITE <- c(rep('(a) Sanjiang Plain',854),rep('(b) Changbai Mountain',819),rep('(c) Dajiuhu Peatland',776),
                  rep("(d) Sallie's fen",776))
ggplot(gppdata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 3)+
  geom_point(size = 7,alpha = 0.8)+
  facet_wrap(~SITE,scales = 'free',nrow = 2) +
  labs(y = expression(GPP/gC*m^{-2}*d^{-1}), x = "DOY")+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL_GEN' = 'black','MODEL_NOGEN' = 'blue'),
                     labels = c('OBS','MODEL_GEN','MODEL_NOGEN'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL_GEN' = NA,'MODEL_NOGEN' =NA),
                     labels = c('OBS','MODEL_GEN','MODEL_NOGEN'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL_GEN' = 1,'MODEL_NOGEN' =2),
                        labels = c('OBS','MODEL_GEN','MODEL_NOGEN'))+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.4,0.9),
    legend.text=element_text(colour='black',size=25,face="bold"),
    axis.text.x=element_text(size=25,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=25,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 30,face = 'bold'),
    axis.title.x = element_text(size = 30,face = 'bold'),
    strip.text = element_text(size = 25,face = 'bold')
  )


#
gppdata1 <- rbind(mydata1_sj,mydata1_cbs,mydata1_djh,mydata1_sal)# 489  454 411 411
gppdata1$SITE <- c(rep('(a) Sanjiang Plain',489),rep('(b) Changbai Mountain',454),rep('(c) Dajiuhu Peatland',411),
                  rep("(d) Sallie's fen",411))
ggplot(gppdata1,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 3)+
  geom_point(size = 7,alpha = 0.8)+
  facet_wrap(~SITE,scales = 'free',nrow = 2) +
  labs(y = expression(GPP/gC*m^{-2}*d^{-1}), x = "DOY")+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL_GEN' = 'black'),
                     labels = c('OBS','MODEL_GEN'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL_GEN' = NA),
                     labels = c('OBS','MODEL_GEN'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL_GEN' = 1),
                        labels = c('OBS','MODEL_GEN'))+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.4,0.9),
    legend.text=element_text(colour='black',size=25,face="bold"),
    axis.text.x=element_text(size=25,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=25,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 30,face = 'bold'),
    axis.title.x = element_text(size = 30,face = 'bold'),
    strip.text = element_text(size = 25,face = 'bold')
  )



######## methane ########
#sj
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2012-01-01-00000 (459).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2012-01-01-00000 (3).nc')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
mnogen <- ncvar_get(nc = try_data1,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/Rtest/clm/三江甲烷排放.csv',header = TRUE)
methane_obs <- methane_sanjiang$X2012.CH4.mgm.2d.1.[1:153]
methane_model <- methane_model*1382400000 #mg/m2/d
mnogen <- mnogen*1382400000
x <- 121:273
data_me <- data.frame(x,methane_obs)

#
DOY <- matrix(nrow =883 ,ncol =1)
DOY[1:153,1] <- 121:273
DOY[154:518] <- 1:365
DOY[519:883] <- 1:365
OBS <- as.matrix(methane_obs)
MODEL <- as.matrix(methane_model)
mnogen1 <- as.matrix(mnogen)
METHANE <- matrix(nrow =883 ,ncol =1)
METHANE[1:153,1] <- OBS
METHANE[154:518] <- MODEL
METHANE[519:883] <- mnogen1
TYPE <- c(rep('OBS',153),rep('MODEL_GEN',365),rep('MODEL_NOGEN',365))
medata_sj <- data.frame(DOY,METHANE,TYPE)
medata1_sj <- medata_sj[1:518,]

##cbs
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/cbs_t.clm2.h0.2011-01-01-00000 (110).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/mic-gen/cbs_nogens.clm2.h0.2011-01-01-00000 (3).nc')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
mnogen <- ncvar_get(nc = try_data1,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/fluxdata/methane.csv', sep = ',',header = T )
methane_obs <- as.double(as.matrix(methane_sanjiang[,2])) #mg/m2/h
methane_model <- methane_model*12*3600*1000*24  #mg/m2/d
mnogen <- mnogen*12*3600*1000*24
methane_obs <- methane_obs*24
# x <- 121:273
# data_me <- data.frame(x,methane_obs)
m_doy <- methane_sanjiang[1:6,1]
#
DOY <- matrix(nrow =736 ,ncol =1)
DOY[1:6,1] <- m_doy
DOY[7:371] <- 1:365
DOY[372:736] <- 1:365
OBS <- as.matrix(methane_obs)
MODEL <- as.matrix(methane_model)
mnogen1 <- as.matrix(mnogen)
METHANE <- matrix(nrow =736 ,ncol =1)
METHANE[1:6,1] <- OBS
METHANE[7:371] <- MODEL
METHANE[372:736] <- mnogen1
TYPE <- c(rep('OBS',6),rep('MODEL_GEN',365),rep('MODEL_NOGEN',365))
medata_cbs <- data.frame(DOY,METHANE,TYPE)
medata1_cbs <- medata_cbs[1:371,]

##djh

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (231).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_nogens.clm2.h0.2019-01-01-00000 (11).nc')
OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/microbe/神农架/dajiulong.csv',header = TRUE) #nmol/m2/s
CH4_MODEL <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
MNOGEN <- ncvar_get(nc = try_data1,varid = 'CH4_SURF_NETFLUX')
CH4_OBS <-  OBS$nmol.m2.s[2:12]
CH4_MODEL <- CH4_MODEL*10^3
MNOGEN <- MNOGEN*10^3
MONTH <- c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
modeldata <- data.frame(CH4_MODEL,MONTH)
CH4_MODEL1 <- aggregate(modeldata$CH4_MODEL, by = list(MONTH),FUN = mean,na.rm = T)
m1 <- data.frame(MNOGEN,MONTH)
MNOGEN1 <- aggregate(m1$MNOGEN, by = list(MONTH),FUN = mean,na.rm = T)

Month <- matrix(nrow = 35,ncol = 1)
Month[1:11,1] <- 1:11
Month[12:23,1] <- 1:12
Month[24:35,1] <- 1:12
METHANE <- matrix(nrow = 35,ncol = 1)
METHANE[1:11,1] <- CH4_OBS
METHANE[12:23,1] <- CH4_MODEL1$x
METHANE[24:35,1] <- MNOGEN1$x
TYPE <- c(rep('OBS',11),rep('MODEL_GEN',12),rep('MODEL_NOGEN',12))
medata_djh <- data.frame(Month,METHANE,TYPE)
medata_djh$METHANE <- medata_djh$METHANE*3600*24*16*(10^-9)
colnames(medata_djh) <- c('DOY','METHANE','TYPE')
medata1_djh <- medata_djh[1:23,]

##
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/t_sal.clm2.h0.2010-01-01-00000 (182).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/tsal_t.clm2.h0.2010-01-01-00000 (96).nc ')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
mnogen <- ncvar_get(nc = try_data1,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/AAAA-资料E盘/CLM/microbe/sallie fen/methane-08-11.csv',header = TRUE)
methane_obs <- methane_sanjiang$methane.2
methane_model <- methane_model*16000*3600*24
mnogen <- mnogen*16000*3600*24
# x <- 121:273
# data_me <- data.frame(x,methane_obs)
m_doy <- methane_sanjiang$DOY.2
#
DOY <- matrix(nrow =756 ,ncol =1)
DOY[1:26,1] <- m_doy
DOY[27:391] <- 1:365
DOY[392:756] <- 1:365
OBS <- as.matrix(methane_obs)
MODEL <- as.matrix(methane_model)
mnogen1 <- as.matrix(mnogen)
METHANE <- matrix(nrow =756 ,ncol =1)
METHANE[1:26,1] <- OBS
METHANE[27:391] <- MODEL
METHANE[392:756] <- mnogen1
TYPE <- c(rep('OBS',26),rep('MODEL_GEN',365),rep('MODEL_NOGEN',365))
medata_sal <- data.frame(DOY,METHANE,TYPE)
medata1_sal <- medata_sal[1:391,]
######
methanedata <- rbind(medata_sj,medata_cbs,medata_djh,medata_sal)# 883,736,35,756
methanedata$SITE <- c(rep('(a) Sanjiang Plain',883),rep('(b) Changbai Mountain',736),rep('(c) Dajiuhu Peatland',35),
                  rep("(d) Sallie's fen",756))
ggplot(methanedata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 3)+
  geom_point(size = 7,alpha = 0.8)+
  facet_wrap(~SITE,scales = 'free',nrow = 2) +
  labs(y = expression(Methane /gC*m^{-2}*d^{-1}), x = "DOY")+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL_GEN' = 'black','MODEL_NOGEN' = 'blue'),
                     labels = c('OBS','MODEL_GEN','MODEL_NOGEN'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL_GEN' = NA,'MODEL_NOGEN' =NA),
                     labels = c('OBS','MODEL_GEN','MODEL_NOGEN'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL_GEN' = 1,'MODEL_NOGEN' =2),
                        labels = c('OBS','MODEL_GEN','MODEL_NOGEN'))+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.4,0.9),
    legend.text=element_text(colour='black',size=25,face="bold"),
    axis.text.x=element_text(size=25,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=25,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 30,face = 'bold'),
    axis.title.x = element_text(size = 30,face = 'bold'),
    strip.text = element_text(size = 25,face = 'bold')
  )

#
methanedata1 <- rbind(mydata1_sj,mydata1_cbs,mydata1_djh,mydata1_sal)# 489  454 411 411
methanedata1$SITE <- c(rep('(a) Sanjiang Plain',518),rep('(b) Changbai Mountain',371),rep('(c) Dajiuhu Peatland',23),
                   rep("(d) Sallie's fen",391))
ggplot(gppdata1,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 3)+
  geom_point(size = 7,alpha = 0.8)+
  facet_wrap(~SITE,scales = 'free',nrow = 2) +
  labs(y = expression(GPP/gC*m^{-2}*d^{-1}), x = "DOY")+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL_GEN' = 'black'),
                     labels = c('OBS','MODEL_GEN'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL_GEN' = NA),
                     labels = c('OBS','MODEL_GEN'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL_GEN' = 1),
                        labels = c('OBS','MODEL_GEN'))+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.position = c(0.4,0.9),
    legend.text=element_text(colour='black',size=25,face="bold"),
    axis.text.x=element_text(size=25,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=25,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 30,face = 'bold'),
    axis.title.x = element_text(size = 30,face = 'bold'),
    strip.text = element_text(size = 25,face = 'bold')
  )


