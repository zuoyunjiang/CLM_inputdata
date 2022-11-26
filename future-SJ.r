######YUNJINAG ZUO#######
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

# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj_transient.clm2.h0.2012-01-01-00000 (13).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj_transient1.clm2.h0.2012-01-01-00000 (8).nc')
OBS <- read.csv(file = 'D:/Rtest/2012_CO2.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
# NOGEN <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_OBS <-  OBS$GPP[1:153]
GPP_MODEL <- GPP_MODEL*24*3600
# NOGEN <- NOGEN*24*3600

DOY <- matrix(nrow =489 ,ncol =1)
DOY[1:124,1] <- 150:273
DOY[125:489,1] <- 1:365
# DOY[490:854,1] <- 1:365
OBS <- GPP_OBS[30:153]
MODEL <- GPP_MODEL
# NOGEN <- NOGEN
GPP <- matrix(nrow =489 ,ncol =1)
GPP[1:124,1] <- OBS
GPP[125:489,1] <- MODEL
# GPP[490:854,1] <- NOGEN
TYPE <- c(rep('OBS',124),rep('MODEL_GEN',365))
mydata <- data.frame(DOY,GPP,TYPE)

# obs1 <-GPP_OBS[30:153]
# MODEL1 <- GPP_MODEL[150:273]
# model2 <- NOGEN[150:273]
# 
# postResample(obs1,MODEL1)
# postResample(obs1,model2)

#
ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sanjiang Plain")+
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


# 
# #
# ggplot(mydata1,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
#   geom_line(size = 2)+
#   geom_point(size = 4)+
#   labs(title = "Sanjiang Plain")+
#   xlab('DOY')+
#   ylab(expression(GPP/g*m^{-2}*d^{-1}))+
#   scale_color_manual(name = '',
#                      values = c('OBS' = 'red','MODEL_GEN' = 'black'),
#                      labels = c('OBS','MODEL_GEN'))+
#   scale_shape_manual(name = '',
#                      values = c('OBS' = 19,'MODEL_GEN' = NA),
#                      labels = c('OBS','MODEL_GEN'))+
#   scale_linetype_manual(name = '',
#                         values = c('OBS' = 0,'MODEL_GEN' = 1),
#                         labels = c('OBS','MODEL_GEN'))+
#   theme_bw()+
#   theme(
#     legend.position = c(0.85,0.9),
#     legend.title=element_text(colour='black'),
#     legend.margin=margin(grid::unit(0,"cm")),
#     legend.text=element_text(colour='black',size=22,face="bold"),
#     legend.key.height=grid::unit(0.5,"cm"),
#     legend.key.width=grid::unit(0.5,"cm"),
#     axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
#     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
#     axis.title.y = element_text(size = 22,face = 'bold'),
#     axis.title.x = element_text(size = 22,face = 'bold'),
#     axis.ticks=element_line(size=0.5),
#     plot.background=element_blank(),
#     # panel.border=element_blank(), # tianjia bianjie
#     plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
#     plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))

# 
# ##ggline##
# gppdata <- matrix(ncol = 3,nrow = 124)
# colnames(gppdata) <- c('Modeled_GPP','Observed_GPP','Station')
# gppdata[1:124,1] <- GPP_MODEL[150:273] 
# gppdata[1:124,2] <- GPP_OBS[30:153]
# gppdata[1:124,3] <- rep('(a) Sanjiang Plain',124)
# gppdata <- data.frame(gppdata)
# attach(gppdata)
# gppdata[,1] <- as.double(gppdata[,1])
# gppdata[,2] <- as.double(gppdata[,2])
# gppdata[,1:2] <- round(gppdata[,1:2],2)
# ggplot(data = gppdata,aes(x = Observed_GPP, y = Modeled_GPP),color = 'black')+
#   geom_point(size = 4)+
#   geom_smooth(method = 'lm',formula = y~x ,se = F, size = 2)+
#   # geom_abline(intercept = 0, slope = 1,color = 'blue')+
#   facet_wrap(~Station,scales = 'free',nrow = 1) +
#   stat_poly_eq(aes(label = paste(stat(rr.label),
#                                  stat(p.value.label),
#                                  sep ="*\",\"*")), 
#                label.x.npc = "right", label.y.npc = 0.1,
#                formula = y ~ x, parse = TRUE, size = 10)+
#   # ylab(expression(Modeled_GPP/g*m^{-2}*d^{-1}))+
#   # xlab(expression(Observed_GPP/g*m^{-2}*d^{-1}))+
#   scale_y_continuous(labels = label_number(accuracy = 0.1))+
#   theme(
#     # axis.title.y.left = 'Modeled_GPP/g*m^{-2}*d^{-1}',
#     # axis.title.x.bottom = 'Observed_GPP/g*m^{-2}*d^{-1}',
#     axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
#     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
#     axis.title.y = element_text(size = 22,face = 'bold'),
#     axis.title.x = element_text(size = 22,face = 'bold'),
#     strip.text = element_text(size = 20,face = 'bold')# change title size
#   )

###
#### methane #####      
# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/wed_t.clm2.h0.2012-01-01-00000 (25).nc')
# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2012-01-01-00000 (459).nc')
# try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2012-01-01-00000 (3).nc')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
# mnogen <- ncvar_get(nc = try_data1,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/Rtest/clm/三江甲烷排放.csv',header = TRUE)
methane_obs <- methane_sanjiang$X2012.CH4.mgm.2d.1.[1:153]
methane_model <- methane_model*1382400000
# mnogen <- mnogen*1382400000
x <- 121:273
data_me <- data.frame(x,methane_obs)

#
DOY <- matrix(nrow =518 ,ncol =1)
DOY[1:153,1] <- 121:273
DOY[154:518] <- 1:365
# DOY[519:883] <- 1:365
OBS <- as.matrix(methane_obs)
MODEL <- as.matrix(methane_model)
mnogen1 <- as.matrix(mnogen)
METHANE <- matrix(nrow =518 ,ncol =1)
METHANE[1:153,1] <- OBS
METHANE[154:518] <- MODEL
# METHANE[519:883] <- mnogen1
TYPE <- c(rep('OBS',153),rep('MODEL_GEN',365))
mydata <- data.frame(DOY,METHANE,TYPE)
# 
# obs1 <- OBS[,1]
# MODEL1 <- MODEL[121:273]
# model2 <- mnogen1[121:273]
# 
# postResample(obs1,MODEL1)
# postResample(obs1,model2)


ggplot(mydata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sanjiang Plain")+
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


