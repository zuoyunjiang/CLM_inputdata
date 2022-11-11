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

#############gpp############
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/t_sal.clm2.h0.2010-01-01-00000 (182).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/tsal_t.clm2.h0.2010-01-01-00000 (96).nc ')

OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/microbe/sallie fen/GPP.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
NOGEN <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_MODEL <- GPP_MODEL*86400
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
mydata <- data.frame(DOY,GPP,TYPE)
mydata$GPP <- as.double(mydata$GPP)
#
ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sallie'fen")+
  xlab('DOY')+
  ylab(expression(GPP/g*m^{-2}*d^{-1}))+
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
    legend.position = c(0.85,0.9),
    legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))

mydata1 <- mydata[1:411,]

#
ggplot(mydata1,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Changbai Mountains")+
  xlab('DOY')+
  ylab(expression(GPP/g*m^{-2}*d^{-1}))+
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
    legend.position = c(0.85,0.9),
    legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))

##### methane #####
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
mydata <- data.frame(DOY,METHANE,TYPE)
ggplot(mydata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sallie'fen")+
  xlab('DOY')+
  ylab(expression(Methane/mg*m^{-2}*d^{-1}))+
  # scale_x_continuous(position = 'top')+
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
    legend.position = c(0.85,0.9),
    legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))

mydata1 <- mydata[1:391,]
ggplot(mydata1,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sallie's fen")+
  xlab('DOY')+
  ylab(expression(Methane/mg*m^{-2}*d^{-1}))+
  # scale_x_continuous(position = 'top')+
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
    legend.position = c(0.85,0.9),
    legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))

