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
