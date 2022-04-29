#####Yunjiang zuo####
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
install.packages('plotrix')
library(plotrix)
library(grid)
#############gpp############
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/结果/SJ_WETLANDS_TRANSIENT/SJ_WETLANDS_TRANSIENT.clm2.h0.2012-01-01-00000.nc')
OBS <- read.csv(file = 'D:/Rtest/2012_CO2.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
GPP_OBS <-  OBS$GPP[1:153]
GPP_MODEL <- GPP_MODEL*24*3600
gppr2 <- summary(lm(GPP_OBS[30:153]~GPP_MODEL[150:273]))
##GRAPH
# jpeg(filename = 'GPP_10-30.png',width = 1200,height = 900,units = 'px',pointsize = 24)
opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(x=150:273,GPP_MODEL[150:273],main = 'Sanjiang Plain',ylab=expression(GPP/g*m^{-2}*d^{-1}),xlab ='DOY',type="l",pch=18,col="black",yaxt="n",xaxt = 'n',lty=1,lwd=2,ylim = c(0,8))
lines(x=150:273,GPP_OBS[30:153],type="p",pch=19,col="red",lty=1,lwd=2)
text(x =270 ,y = 6.5,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)
# dev.off()

DOY <- matrix(nrow =489 ,ncol =1)
DOY[1:124,1] <- 150:273
DOY[125:489,1] <- 1:365
OBS <- GPP_OBS[30:153]
MODEL <- GPP_MODEL
GPP <- matrix(nrow =489 ,ncol =1)
GPP[1:124,1] <- OBS
GPP[125:489] <- MODEL
TYPE <- c(rep('OBS',124),rep('MODEL',365))
mydata <- data.frame(DOY,GPP,TYPE)
mydata11 <- data.frame(DOY,GPP,TYPE)

DOY <- rep(150:273,2)
MODEL <- GPP_MODEL[150:273]
OBS <- GPP_OBS[30:153]
GPP <- matrix(nrow = 248,ncol= 1)
GPP[1:124,] <- MODEL
GPP[125:248,] <- OBS
TYPE <- rep(c('MODEL','OBS'),each = 124)
mydata <- data.frame(DOY,GPP,TYPE)
mydata1 <- cbind(DOY,GPP,TYPE)
##
all_gm_data <- matrix(ncol = 5, nrow = 4000)
all_gm_data[1:489,1:3] <- as.matrix(mydata11)
all_gm_data[1:489,4] <- rep('(a) Sanjiang Plain',489)

##
gppdata <- matrix(ncol = 3,nrow = 1000)
colnames(gppdata) <- c('Modeled_GPP','Observed_GPP','Station')
gppdata[1:124,1] <- GPP_MODEL[150:273] 
gppdata[1:124,2] <- GPP_OBS[30:153]
gppdata[1:124,3] <- rep('(a) Sanjiang Plain',124)
#
OBS <- GPP_OBS[30:153]
MODEL <- GPP_MODEL[150:273]
postResample(OBS,MODEL)
#
sj <- ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_point(size = 4)+
  geom_line(size = 2)+
  labs(title = "Sanjiang Plain")+
  xlab('DOY')+
  ylab(expression(GPP/g*m^{-2}*d^{-1}))+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL' = 'black'),
                     labels = c('OBS','MODEL'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL' = NA),
                     labels = c('OBS','MODEL'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL' = 1),
                        labels = c('OBS','MODEL'))+
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

nc_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/6-16/cbs_test.clm2.h0.2011-01-01-00000 (5).nc')
eco_data <- read.table(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/fluxdata/nee-gpp-er.csv',header =T, sep = ',')
GPP1 <- eco_data[1:91,17]*-1
GPP_day <- as.matrix(eco_data[1:91,16])
GPP_model <- ncvar_get(nc_data,varid = 'GPP')
GPP_model <- GPP_model*1000
# gppr2 <- summary(lm(GPP~GPP_model[GPP_day]))
DOY <- matrix(nrow =312 ,ncol =1)
DOY[1:91,1] <- GPP_day
DOY[92:312] <- 90:310
OBS <- as.matrix(GPP1)
MODEL <- as.matrix(GPP_model[90:310])
GPP <- matrix(nrow =312 ,ncol =1)
GPP[1:91,1] <- OBS
GPP[92:312] <- MODEL
TYPE <- c(rep('OBS',91),rep('MODEL',221))
mydata <- data.frame(DOY,GPP,TYPE)

#
DOY <- matrix(nrow =456 ,ncol =1)
DOY[1:91,1] <- GPP_day
DOY[92:456] <- 1:365
OBS <- as.matrix(GPP1)
MODEL <- as.matrix(GPP_model)
GPP <- matrix(nrow =456 ,ncol =1)
GPP[1:91,1] <- OBS
GPP[92:456] <- MODEL
GPP[90:91] <- 0
GPP <- GPP*3.6*24
TYPE <- c(rep('OBS',91),rep('MODEL',365))
mydata <- data.frame(DOY,GPP,TYPE)


mydata22 <- cbind(DOY,GPP,TYPE)
##
mydata2 <- cbind(DOY,GPP,TYPE)
all_gm_data[490:945,1:3] <- as.matrix(mydata22)
all_gm_data[490:945,4] <- rep('(b) Changbai Mountain',456)
##

OBS <- as.matrix(GPP1)
MODEL <- GPP_model[GPP_day]
gppdata[125:215,1] <- MODEL
gppdata[125:215,2] <- OBS
gppdata[125:215,3] <- rep('(b) Changbai Mountain',91)
##
OBS <- as.matrix(GPP1)*3.6*24
MODEL <- GPP_model[GPP_day]*3.6*24
postResample(OBS,MODEL)
#
cbs <- ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_point(size = 4)+
  geom_line(size = 2)+
  labs(title = "Changbai Mountain")+
  xlab('DOY')+
  ylab('')+
  # ylab(expression(GPP/g*m^{-2}*d^{-1}))+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL' = 'black'),
                     labels = c('OBS','MODEL'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL' = NA),
                     labels = c('OBS','MODEL'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL' = 1),
                        labels = c('OBS','MODEL'))+
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
  # theme(
  #   legend.position = c(0.9,0.9),
  #   legend.title=element_text(colour='black'),
  #   legend.margin=margin(grid::unit(0,"cm")),
  #   legend.text=element_text(colour='black',size=9,face="bold"),
  #   legend.key.height=grid::unit(0.5,"cm"),
  #   legend.key.width=grid::unit(0.5,"cm"),
  #   axis.text.x=element_text(size=12,colour='black',vjust = 1,hjust = 1),
  #   axis.text.y=element_text(size=12,vjust=0.2,colour='black'),
  #   axis.title.y = element_text(size = 13,face = 'bold'),
  #   axis.title.x = element_text(size = 13,face = 'bold'),
  #   axis.ticks=element_line(size=0.5),
  #   plot.background=element_blank(),
  #   # panel.border=element_blank(), # tianjia bianjie
  #   plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
  #   plot.title=element_text(colour='black',hjust=0.5,size=14,face="bold"))

nc_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/CLM/transient/5-4/a_xxal_transient.clm2.h0.2010-01-01-00000 (20).nc')
GPPm <- ncvar_get(nc_data, varid = 'GPP')
GPP_OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/modis_gpp.csv',header = T, sep = ',')
Day <- 1:365
GPPm <- GPPm*24*3600
obsday <- GPP_OBS[,2]
obsgpp <- GPP_OBS[,3]/365
gppr2 <- summary(lm(GPP[GPP_OBS[,2]]~obsgpp))

DOY <- matrix(nrow =411 ,ncol =1)
DOY[1:46,1] <- obsday
DOY[47:411] <- 1:365
OBS <- as.matrix(obsgpp)
MODEL <- as.matrix(GPPm)
GPP <- matrix(nrow =411 ,ncol =1)
GPP[1:46,1] <- OBS
GPP[47:411] <- MODEL
TYPE <- c(rep('OBS',46),rep('MODEL',365))
mydata <- data.frame(DOY,GPP,TYPE)
##
mydata3 <- cbind(DOY,GPP,TYPE)
all_gm_data[946:1356,1:3] <- as.matrix(mydata3)
all_gm_data[946:1356,4] <- rep('(c) Lesser Khingan Mountain',411)
all_gm_data[1:1356,5] <- rep('GPP',1356)
##
OBS <- as.matrix(obsgpp)
MODEL <- GPPm[obsday]
gppdata[216:261,1] <- MODEL
gppdata[216:261,2] <- OBS
gppdata[216:261,3] <- rep('(c) Lesser Khingan Mountain', 46)
#
OBS <- as.matrix(obsgpp)
MODEL <- GPPm[obsday]
postResample(OBS,MODEL)
#
xxal <- ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Lesser Khingan Mountain")+
  xlab('DOY')+
  ylab('')+
  # ylab(expression(GPP/g*m^{-2}*d^{-1}))+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL' = 'black'),
                     labels = c('OBS','MODEL'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL' = NA),
                     labels = c('OBS','MODEL'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL' = 1),
                        labels = c('OBS','MODEL'))+
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

# plot_grid(sj,cbs,xxal,nrow = 2,ncol = 2)
# grid.arrange(sj,cbs,xxal,nrow =2,ncol = 2)
# ggarrange(sj,cbs,xxal,nrow = 2,ncol = 2,hjust = 1)
sj1 <- sj + annotate("text",x = 152, y = 7, label=" a)", size = 15, face = "bold")
cbs1 <- cbs + annotate("text",x = 99, y = 0.18, label=" b)", size = 15, face = "bold")
xxal1 <- xxal+ annotate("text",x = 0, y = 1.8, label=" c)", size = 15, face = "bold")
ggdraw()+
  draw_plot(sj,x= 0.05,y=0.5,width = 0.45,height = 0.5)+
  draw_plot(cbs,x=0.55,y = 0.5,width = 0.45,height = 0.5)+
  draw_plot(xxal,x = 0.05,y = 0,width = 0.45,height = 0.5)
## 2000*1200

# gppdata <- matrix(ncol = 3,nrow = 1000)
# colnames(gppdata) <- c('Modeled_GPP','Observed_GPP','Station')
# gppdata[1:124,1] <- GPP_MODEL[150:273] 
# gppdata[1:124,2] <- GPP_OBS[30:153]
# gppdata[1:124,3] <- rep('(a) Sanjiang Plain',124)
# OBS <- as.matrix(GPP1)
# MODEL <- GPP_model[GPP_day]
# gppdata[125:215,1] <- MODEL
# gppdata[125:215,2] <- OBS
# gppdata[125:215,3] <- rep('(b) Changbai Mountain',91)
# OBS <- as.matrix(obsgpp)
# MODEL <- GPPm[obsday]
# gppdata[216:261,1] <- MODEL
# gppdata[216:261,2] <- OBS
# gppdata[216:261,3] <- rep('(c) Lesser Khingan Mountain', 46)
gppdata <- gppdata[1:261,]
gppdata <- data.frame(gppdata)
attach(gppdata)
gppdata[,1] <- as.double(gppdata[,1])
gppdata[,2] <- as.double(gppdata[,2])
gppdata[,1:2] <- round(gppdata[,1:2],2)
# colnames(gppdata) <- c(expression(Modeled_GPP/g*m^{-2}*d^{-1}),expression(Observed_GPP/g*m^{-2}*d^{-1}),'Station')
gppdata$Station <- factor(gppdata$Station, levels = c("(a) Sanjiang Plain", '(b) Changbai Mountain',"(c) Lesser Khingan Mountain"))

gpp_line <- ggplot(data = gppdata,aes(x = Observed_GPP, y = Modeled_GPP),color = 'black')+
  geom_point(size = 4)+
  geom_smooth(method = 'lm',formula = y~x ,se = F, size = 2)+
  # geom_abline(intercept = 0, slope = 1,color = 'blue')+
  facet_wrap(~Station,scales = 'free',nrow = 1) +
  stat_poly_eq(aes(label = paste(stat(rr.label),
                                 stat(p.value.label),
                                 sep ="*\",\"*")), 
               label.x.npc = "right", label.y.npc = 0.1,
               formula = y ~ x, parse = TRUE, size = 10)+
  # ylab(expression(Modeled_GPP/g*m^{-2}*d^{-1}))+
  # xlab(expression(Observed_GPP/g*m^{-2}*d^{-1}))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(
    # axis.title.y.left = 'Modeled_GPP/g*m^{-2}*d^{-1}',
    # axis.title.x.bottom = 'Observed_GPP/g*m^{-2}*d^{-1}',
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')# change title size
    )


##### methane ###########
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/结果/SJ_WETLANDS_TRANSIENT/SJ_WETLANDS_TRANSIENT.clm2.h0.2012-01-01-00000.nc')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/Rtest/clm/三江甲烷排放.csv',header = TRUE)
methane_obs <- methane_sanjiang$X2012.CH4.mgm.2d.1.[1:153]
methane_model <- methane_model*1382400000
x <- 121:273
data_me <- data.frame(x,methane_obs)
DOY <- rep(121:273,2)
OBS <- as.matrix(methane_obs)
MODEL <- as.matrix(methane_model[121:273])
METHANE <- matrix(nrow =306 ,ncol =1)
METHANE[1:153,] <- OBS
METHANE[154:306,] <- MODEL
TYPE <- rep(c('OBS','MODEL'),each = 153)
mydata <- data.frame(DOY,METHANE,TYPE)
#
DOY <- matrix(nrow =518 ,ncol =1)
DOY[1:153,1] <- 121:273
DOY[154:518] <- 1:365
OBS <- as.matrix(methane_obs)
MODEL <- as.matrix(methane_model)
METHANE <- matrix(nrow =518 ,ncol =1)
METHANE[1:153,1] <- OBS
METHANE[154:518] <- MODEL
TYPE <- c(rep('OBS',153),rep('MODEL',365))
mydata <- data.frame(DOY,METHANE,TYPE)
#
# s_data <- mydata
# s_data$METHANE <- scale(s_data$METHANE)
# OBS <- s_data$METHANE[which(s_data$TYPE == 'OBS')]
# MODEL <- s_data$METHANE[which(s_data$TYPE == 'MODEL')]
# postResample(OBS,MODEL)

mydata44 <- cbind(DOY,METHANE,TYPE)
mydata4 <- cbind(DOY,METHANE,TYPE)
all_gm_data[1357:1874,1:3] <- as.matrix(mydata44)
all_gm_data[1357:1874,4] <- rep('(d) Sanjiang Plain',518)
#
OBS <- OBS[1:20]
MODEL <- MODEL[1:20]
postResample(OBS,MODEL)
#
methanedata <- matrix(ncol = 3,nrow = 1000)
colnames(methanedata) <- c('Modeled_Methane','Observed_Methane','Station')
methanedata[1:153,1] <- MODEL[121:273]
methanedata[1:153,2] <- OBS
methanedata[1:153,3] <- rep('(d) Sanjiang Plain', 153)

SJ_ME <- ggplot(mydata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sanjiang Plain")+
  xlab('DOY')+
  ylab(expression(Methane/mg*m^{-2}*d^{-1}))+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL' = 'black'),
                     labels = c('OBS','MODEL'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL' = NA),
                     labels = c('OBS','MODEL'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL' = 1),
                        labels = c('OBS','MODEL'))+
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
  # scale_color_manual(name = '',
  #                    values = c('OBS' = 'red','MODEL' = 'black'),
  #                    labels = c('MODEL','OBS'))+
  # scale_shape_manual(name = '',
  #                    values = c('OBS' = 19,'MODEL' = NA),
  #                    labels = c('MODEL','OBS'))+
  # scale_linetype_manual(name = '',
  #                       values = c('OBS' = NA,'MODEL' = 1),
  #                       labels = c('MODEL','OBS'))+
  # theme_bw()+
  # theme(
  #   legend.position = c(0.9,0.9),
  #   legend.title=element_text(colour='black'),
  #   legend.margin=margin(grid::unit(0,"cm")),
  #   legend.text=element_text(colour='black',size=9,face="bold"),
  #   legend.key.height=grid::unit(0.5,"cm"),
  #   legend.key.width=grid::unit(0.5,"cm"),
  #   axis.text.x=element_text(size=12,colour='black',vjust = 1,hjust = 1),
  #   axis.text.y=element_text(size=12,vjust=0.2,colour='black'),
  #   axis.title.y = element_text(size = 13,face = 'bold'),
  #   axis.title.x = element_text(size = 13,face = 'bold'),
  #   axis.ticks=element_line(size=0.5),
  #   plot.background=element_blank(),
  #   # panel.border=element_blank(), # tianjia bianjie
  #   plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
  #   plot.title=element_text(colour='black',hjust=0.5,size=14,face="bold"))

# CBS
# nc_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/4-1/transient_cbs.clm2.h0.2016-01-01-00000 (5).nc')
nc_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/6-16/cbs_test.clm2.h0.2016-01-01-00000 (8).nc')
methane <- ncvar_get(nc_data1,varid = 'CH4_SURF_NETFLUX')
methane_obs <- read.csv(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/fluxdata/methane.csv', sep = ',',header = T )
methane <- methane*12*3600*1000
# obs_day <- as.integer(as.matrix(obs_day))
obs <- methane_obs[,2]
obs <- as.double(as.matrix(obs))
DOY <- matrix(ncol = 1, nrow = 371)
DOY[1:6,1] <- methane_obs[1:6,1]
obs_day <- methane_obs[1:6,1]
DOY[7:371,] <- 1:365
OBS <- methane_obs[1:6,2]
MODEL <- methane
METHANE <- matrix(nrow =371 ,ncol =1)
METHANE[1:6,] <- OBS
METHANE[7:371,] <- MODEL
TYPE <- c(rep('OBS', 6),rep('MODEL',365))
mydata <- data.frame(DOY,METHANE,TYPE)
mydata5 <- cbind(DOY,METHANE,TYPE)
all_gm_data[1875:2245,1:3] <- as.matrix(mydata5)
all_gm_data[1875:2245,4] <- rep('(e) Changbai Mountain',371)

#
obsr <- OBS[-3]
MODEL <- MODEL[obs_day[-3]]
# obsr <- scale(obsr)
# MODEL <- scale(MODEL)
# TDATA <- matrix(nrow = 10,ncol = 1)
# TDATA[1:5] <- obsr
# TDATA[6:10] <- MODEL
# TDATA <- scale(TDATA)
# postResample(TDATA[1:5],TDATA[6:10])

methanedata[153:158,1] <- MODEL[obs_day]
methanedata[153:158,2] <- OBS
methanedata[153:158,3] <- rep('(e) Changbai Mountain', 6)

#
cbs_me <- ggplot(mydata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Changbai Mountains")+
  xlab('DOY')+
  ylab('')+
  # ylab(expression(Methane/mg*m^{-2}*d^{-1}))+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL' = 'black'),
                     labels = c('OBS','MODEL'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL' = NA),
                     labels = c('OBS','MODEL'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL' = 1),
                        labels = c('OBS','MODEL'))+
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
  # scale_color_manual(name = '',
  #                    values = c('OBS' = 'red','MODEL' = 'black'),
  #                    labels = c('MODEL','OBS'))+
  # scale_shape_manual(name = '',
  #                    values = c('OBS' = 19,'MODEL' = NA),
  #                    labels = c('MODEL','OBS'))+
  # scale_linetype_manual(name = '',
  #                       values = c('OBS' = NA,'MODEL' = 1),
  #                       labels = c('MODEL','OBS'))+
  # theme_bw()+
  # theme(
  #   legend.position = c(0.9,0.9),
  #   legend.title=element_text(colour='black'),
  #   legend.margin=margin(grid::unit(0,"cm")),
  #   legend.text=element_text(colour='black',size=9,face="bold"),
  #   legend.key.height=grid::unit(0.5,"cm"),
  #   legend.key.width=grid::unit(0.5,"cm"),
  #   axis.text.x=element_text(size=12,colour='black',vjust = 1,hjust = 1),
  #   axis.text.y=element_text(size=12,vjust=0.2,colour='black'),
  #   axis.title.y = element_text(size = 13,face = 'bold'),
  #   axis.title.x = element_text(size = 13,face = 'bold'),
  #   axis.ticks=element_line(size=0.5),
  #   plot.background=element_blank(),
  #   # panel.border=element_blank(), # tianjia bianjie
  #   plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
  #   plot.title=element_text(colour='black',hjust=0.5,size=14,face="bold"))

#xxal
CH4_OBS <- read.csv(file = "D:/AAAA-资料E盘/CLM/DATA/小兴安岭/2011_CH4.csv",header = F, sep = ',')
model <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/CLM/transient/6-3/xxal_methane.clm2.h0.2014-01-01-00000 (3).nc')
ch4_2014 <- CH4_OBS[,19]
ch4_day <- CH4_OBS[1:17,18]

Day <- 1:365
CH4 <- ncvar_get(model,varid = 'CH4_SURF_NETFLUX')
CH4<- CH4*16*1000*3600
ch4r2 <- summary(lm(ch4_2014~CH4[ch4_day]))

DOY <- matrix(ncol = 1, nrow = 382)
DOY[1:17,1] <- CH4_OBS[1:17,18]
DOY[18:382,] <- 1:365
OBS <- CH4_OBS[1:17,19]
MODEL <- CH4
METHANE <- matrix(nrow =382 ,ncol =1)
METHANE[1:17,] <- OBS
METHANE[18:382,] <- MODEL
TYPE <- c(rep('OBS', 17),rep('MODEL',365))
mydata <- data.frame(DOY,METHANE,TYPE)

mydata6 <- cbind(DOY,METHANE,TYPE)
all_gm_data[2246:2627,1:3] <- as.matrix(mydata6)
all_gm_data[2246:2627,4] <- rep('(f) Lesser Khingan Mountain',382)
all_gm_data[1357:2627,5] <- rep('Methane',1271)
all_gm_data <- all_gm_data[1:2627,]
colnames(all_gm_data) <- c('DOY','value','TYPE','Station','Variable')
#
BOSER1 <- OBS[c(-10,-14)]
MODEL <- CH4[ch4_day[c(-10,-14)]]
TDATA <- matrix(nrow = 30,ncol = 1)
TDATA[1:15] <- BOSER1  
TDATA[16:30] <- MODEL
TDATA <- scale(TDATA)
postResample(TDATA[1:15],TDATA[16:30])
postResample(BOSER1,MODEL)

methanedata[159:175,1] <- MODEL[ch4_day]
methanedata[159:175,2] <- OBS
methanedata[159:175,3] <- rep('(f) Lesser Khingan Mountain',17)
#
xxal_me <- ggplot(mydata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Lesser Khingan Mountains")+
  xlab('DOY')+
  ylab('')+
  # ylab(expression(Methane/mg*m^{-2}*d^{-1}))+
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL' = 'black'),
                     labels = c('OBS','MODEL'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL' = NA),
                     labels = c('OBS','MODEL'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL' = 1),
                        labels = c('OBS','MODEL'))+
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
  # scale_color_manual(name = '',
  #                    values = c('OBS' = 'red','MODEL' = 'black'),
  #                    labels = c('MODEL','OBS'))+
  # scale_shape_manual(name = '',
  #                    values = c('OBS' = 19,'MODEL' = NA),
  #                    labels = c('MODEL','OBS'))+
  # scale_linetype_manual(name = '',
  #                       values = c('OBS' = NA,'MODEL' = 1),
  #                       labels = c('MODEL','OBS'))+
  # theme_bw()+
  # theme(
  #   legend.position = c(0.9,0.9),
  #   legend.title=element_text(colour='black'),
  #   legend.margin=margin(grid::unit(0,"cm")),
  #   legend.text=element_text(colour='black',size=9,face="bold"),
  #   legend.key.height=grid::unit(0.5,"cm"),
  #   legend.key.width=grid::unit(0.5,"cm"),
  #   axis.text.x=element_text(size=12,colour='black',vjust = 1,hjust = 1),
  #   axis.text.y=element_text(size=12,vjust=0.2,colour='black'),
  #   axis.title.y = element_text(size = 13,face = 'bold'),
  #   axis.title.x = element_text(size = 13,face = 'bold'),
  #   axis.ticks=element_line(size=0.5),
  #   plot.background=element_blank(),
  #   # panel.border=element_blank(), # tianjia bianjie
  #   plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
  #   plot.title=element_text(colour='black',hjust=0.5,size=14,face="bold"))

SJ_ME1 <- SJ_ME + annotate("text",x = 120, y = 280, label=" d)", size = 15, face = "bold")
cbs_me1 <- cbs_me + annotate("text",x = 0, y = 4.1, label=" e)", size = 15, face = "bold")
xxal_me1 <- xxal_me+ annotate("text",x = 0, y = 17.5, label=" f)", size = 15, face = "bold")
ggdraw()+
  draw_plot(SJ_ME,x= 0.05,y=0.5,width = 0.45,height = 0.5)+
  draw_plot(cbs_me,x=0.55,y = 0.5,width = 0.45,height = 0.5)+
  draw_plot(xxal_me,x = 0.05,y = 0,width = 0.45,height = 0.5)
# ggdraw()+
#   draw_plot(SJ_ME,x= 0.01,y=0,width = 0.33,height = 1)+
#   draw_plot(cbs_me,x=0.31,y = 0,width = 0.33,height = 1)+
#   draw_plot(xxal_me,x = 0.62,y = 0,width = 0.33,height = 1)
ggdraw()+
  draw_plot(sj1,x= 0.0,y=0.5,width = 0.33,height = 0.5)+
  draw_plot(cbs1,x=0.33,y = 0.5,width = 0.33,height = 0.5)+
  draw_plot(xxal1,x = 0.66,y = 0.5,width = 0.33,height = 0.5)+
  draw_plot(SJ_ME1,x = 0.0,y = 0,width = 0.33,height = 0.5)+
  draw_plot(cbs_me1,x = 0.33,y = 0,width = 0.33,height = 0.5)+
  draw_plot(xxal_me1,x = 0.66,y = 0,width = 0.33,height = 0.5)



methanedata <- methanedata[1:175,]
methanedata1 <- methanedata
methanedata <- data.frame(methanedata)
attach(methanedata)
methanedata[,1] <- as.double(methanedata[,1])
methanedata[,2] <- as.double(methanedata[,2])
methanedata[,1:2] <- round(methanedata[,1:2],2)

# methanedata$Station <- factor(methanedata$Station,levels = c("(a) Sanjiang Plain", "(b) Changbai Mountain", "(c) Lesser Khingan Mountain"))

methane_line <- ggplot(data = methanedata,aes(x = Observed_Methane, y = Modeled_Methane),color = 'black')+
  geom_point(size = 4)+
  geom_smooth(method = 'lm',formula = y~x ,se = F, size = 2)+
  # geom_abline(intercept = 0, slope = 1,color = 'blue')+
  facet_wrap(~Station,scales = 'free',nrow = 1) +
  stat_poly_eq(aes(label = paste(stat(rr.label),
                                 stat(p.value.label),
                                 sep ="*\",\"*")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = y ~ x, parse = TRUE, size = 10)+
  # scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme(
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')
  )
ggdraw()+
  draw_plot(gpp_line,x= 0.0,y=0.5,width = 1,height = 0.5)+
  draw_plot(methane_line,x=0.0,y = 0.0,width = 1,height = 0.5)



colnames(gppdata) <- c('Modeled','Observed','Station')
colnames(methanedata) <- c('Modeled','Observed','Station')
gppdata1 <- gppdata
gppdata1[125:215,1:2] <- gppdata1[125:215,1:2]*24*3.6
gppdata1[214:215,2] <- gppdata1[214:215,1]
all_data_gm <- rbind(gppdata1,methanedata)
all_data_gm$variable <- c(rep('GPP',261),rep('Methane',175))


attach(all_data_gm)
all_data_gm <- all_data_gm[c(-416,-429,-433),]
all_data_gm[262:433,1:2] <- all_data_gm[262:433,1:2]/1000
#### cor plot of six #### 
ggplot(data = all_data_gm,aes(x = Observed, y = Modeled,fill = variable,color = variable))+
  geom_point(size = 7)+
  geom_smooth(method = 'lm',formula = y~x ,se = F, size = 2,col = 'black')+
  # geom_abline(intercept = 0, slope = 1,color = 'blue')+
  facet_wrap(~Station,scales = 'free',nrow = 2) +
  labs(x = expression(Observed/gC*m^{-2}*d^{-1}),y = expression(Modeled/gC*m^{-2}*d^{-1}) )+
  stat_poly_eq(aes(label = paste(stat(rr.label),
                                 stat(p.value.label),
                                 sep ="*\",\"*")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = y ~ x, parse = TRUE, size = 10,col = 'black')+
  # scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=25,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=25,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 30,face = 'bold'),
    axis.title.x = element_text(size = 30,face = 'bold'),
    strip.text = element_text(size = 25,face = 'bold')
  )

###
ggplot(data = all_data_gm,aes(x = Observed, y = Modeled,fill = variable,color = variable))+
  geom_point(size = 7)+
  geom_smooth(method = 'lm',formula = y~x ,se = F, size = 2,col = 'black')+
  geom_abline(intercept = 0,slope = 1,size = 2, linetype = 'dashed',color = 'grey40')+
  # geom_abline(intercept = 0, slope = 1,color = 'blue')+
  facet_wrap(~Station,scales = 'free',nrow = 2) +
  labs(x = expression(Observed/gC*m^{-2}*d^{-1}),y = expression(Modeled/gC*m^{-2}*d^{-1}) )+
  stat_poly_eq(aes(label = paste(stat(rr.label),
                                 stat(p.value.label),
                                 sep ="*\",\"*")), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = y ~ x, parse = TRUE, size = 10,col = 'black')+
  # scale_y_continuous(labels = label_number(accuracy = 0.1))+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    # legend.position = c(0.9,0.),
    legend.text=element_text(colour='black',size=25,face="bold"),
    axis.text.x=element_text(size=25,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=25,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 30,face = 'bold'),
    axis.title.x = element_text(size = 30,face = 'bold'),
    strip.text = element_text(size = 25,face = 'bold')
  )


all_gm_data1 <- as.data.frame(all_gm_data)
all_gm_data1$DOY <- as.double(all_gm_data1$DOY)
all_gm_data1$value <- as.double(all_gm_data1$value)
gm_p <- ggplot(data = all_gm_data1,aes(x = DOY, y = value,shape = TYPE, linetype = TYPE,color = TYPE))+
  geom_line(size= 3)+
  geom_point(size = 7,alpha = 0.8)+
  facet_wrap(~Station,scales = 'free',nrow = 2) +
  scale_color_manual(name = '',
                     values = c('OBS' = 'red','MODEL' = 'black'),
                     labels = c('OBS','MODEL'))+
  scale_shape_manual(name = '',
                     values = c('OBS' = 19,'MODEL' = NA),
                     labels = c('OBS','MODEL'))+
  scale_linetype_manual(name = '',
                        values = c('OBS' = 0,'MODEL' = 1),
                        labels = c('OBS','MODEL'))+
  labs(y = expression(Methane/mgC*m^{-2}*d^{-1} ------- GPP/gC*m^{-2}*d^{-1}), x = "DOY")+
  # ylab(expression(Methane/mg*m^{-2}*d^{-1}))+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=25,face="bold"),
    axis.text.x=element_text(size=25,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=25,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 30,face = 'bold'),
    axis.title.x = element_text(size = 30,face = 'bold'),
    strip.text = element_text(size = 25,face = 'bold')
  )
# label <- expression(Methane/mg*m^{-2}*d^{-1})
# gm_p +
#   annotate('text',x = 0.01, y =0.25,label = 'gpp',angle = 90)

# ggdraw()+
#   draw_plot(gm_p + labs(y = ''),0,0,1,1)+
#   annotate('text',x = 0.01, y =0.25,label = "Methane mg/m2/d ",angle = 90,size = 10)+
#   annotate('text',x = 0.01, y =0.75,label = "GPP g/m2/d",angle = 90,size = 10)

ggdraw()+
  draw_plot(gm_p + labs(y = ''),0,0,1,1)+
  annotate('text',x = 0.018, y =0.25,parse = TRUE, label = "atop(Methane/mg*m^-2*d^-1)",angle = 90,size = 8)+
  annotate('text',x = 0.018, y =0.75,parse = TRUE,label = "atop(GPP/g*m^-2*d^-1)",angle = 90,size = 8)
#fen lei huizong 
aggregate(all_gm_data1$value,by = list(all_gm_data1$Station,all_gm_data1$TYPE),FUN = max, na.rm = T)
aggregate(all_gm_data1$value,by = list(all_gm_data1$Station,all_gm_data1$TYPE),FUN = sum, na.rm = T)
# 
# ggplot(data = gppdata,aes(x = Observed_GPP, y = Modeled_GPP),color = 'black')+
#   geom_point(size = 4)+
#   geom_smooth(method = 'lm',formula = y~x ,se = F, size = 2)+
#   # geom_abline(intercept = 0, slope = 1,color = 'blue')+
#   facet_wrap(~Station,scales = 'free',nrow = 2) +
#   stat_poly_eq(aes(label = paste(stat(rr.label),
#                                  stat(p.value.label),
#                                  sep ="*\",\"*")), 
#                label.x.npc = "right", label.y.npc = 0.1,
#                formula = y ~ x, parse = TRUE, size = 10)+
#   # ylab(expression(Modeled_GPP/g*m^{-2}*d^{-1}))+
#   # xlab(expression(Observed_GPP/g*m^{-2}*d^{-1}))+
#   theme(
#     # axis.title.y.left = 'Modeled_GPP/g*m^{-2}*d^{-1}',
#     # axis.title.x.bottom = 'Observed_GPP/g*m^{-2}*d^{-1}',
#     axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
#     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
#     axis.title.y = element_text(size = 22,face = 'bold'),
#     axis.title.x = element_text(size = 22,face = 'bold'),
#     strip.text = element_text(size = 20,face = 'bold')# change title size
#   )

##########end############
######## s index############

##
sindex <- read.csv(file = 'D:/AAAA-资料E盘/CLM/结果//sindex_ab-2.csv',header = T)
# sindex <- sindex[,2:7]
sindex2 <- sindex[c(3,4,5,6,17,18,25,26,29,30,13,14,15,16,27,28,1,2,7:12,19:24,31:36),2:6]
# write.csv(sindex2,file = './CBS/xxsindex.csv')
sindex[,2:6] <- sindex2 
sindex <- melt(sindex)
# sindex <- sindex[-1:-36,]
sindex1 <- sindex %>%
  # convert state to factor and reverse order of levels
  mutate(variable=factor(variable,levels=rev(sort(unique(variable))))) %>%
  # create a new variable from count
  mutate(valuefactor=cut(value,breaks=c(-20,-0.1,-0.005,0,0.005,0.1,max(value,na.rm=T)),
                         labels=c('< -0.1',"-0.1 ~ -0.05","-0.05~0","0~0.05","0.05 ~ 0.1", '>0.1'))) %>%
  # change level order
  mutate(valuefactor=factor(as.character(valuefactor),levels=rev(levels(valuefactor))))

textcol <- "black"
# color <- colorRampPalette(colors = c("red",'white','blue'))(6)
# color <- c("#99000D", "#FC9272", "#FFF5F0", "#F7FBFF","#9ECAE1", "#084594" )
color <- c("#FF0000", "#FF6666", "#FFCCCC", "#CCCCFF","#6699FF", "#0000FF" )
# color <- c("#084594", "#9ECAE1", "#F7FBFF", "#FFF5F0", "#FC9272", "#99000D")
breakset1<-seq(1,36,2)
labelset1<-c('AceProdACmax','bdnr','br_mr','DeadH2Methanogens', 'k_dom', 'dom_diffus','flnr','GrowACEMethanogens','GrowH2Methanogens', 'grperc', 'KACE', 'froot_leaf',  
             'YAceMethanogens','YH2Methanogens')
txtx<-c(1:18)*2-1.2
txt<-c("-    +")
xinterceptSet<-c(1:18)*2+0.5
p <-  ggplot(sindex1,aes(x=varname,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(a) Sanjiang Plain")+
  scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  
  # scale_x_discrete(labels=c('AceProdACmax','','bdnr','','br_mr','','DeadACEMethanogens','','DeadH2Methanogens','', 'DeadMethanotrophs','','k_dom', 
  #                           '','dom_diffus','','flnr','','GrowACEMethanogens','','GrowH2Methanogens','','GrowRmethanotrophs', 
  #                           '','grperc', '','KACE', '','froot_leaf', '','YAceMethanogens','','YH2Methanogens','','YMethanotrophs',''))+
  # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4"),na.value = "grey90")+  #"#ddf1da"
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =18,hjust ="bottom",vjust ="bottom")+
  # coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=30)+
  theme(legend.position="none",legend.direction="vertical",
        # legend.box = 'horizontal',
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        # legend.box.just = ,
        axis.text.x=element_text(size=36,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=36,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=48,face="bold"))

###cbs\
sindex <- read.csv(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/result/sindex_ab-3.csv',header = T)
sindex2 <- sindex[c(3,4,5,6,17,18,25,26,29,30,13,14,15,16,27,28,1,2,7:12,19:24,31:36),2:6]
sindex[,2:6] <- sindex2 
sindex <- melt(sindex)
# sindex[1:144,3] <- scale(sindex[1:144,3])
# sindex[145:180,3] <- scale(sindex[145:180,3])
sindex1 <- sindex %>%
  # convert state to factor and reverse order of levels
  mutate(variable=factor(variable,levels=rev(sort(unique(variable))))) %>%
  # create a new variable from count
  mutate(valuefactor=cut(value,breaks=c(-20,-0.1,-0.005,0,0.005,0.1,max(value,na.rm=T)),
                         labels=c('< -0.1',"-0.1 ~ -0.05","-0.05~0","0~0.05","0.05 ~ 0.1", '>0.1'))) %>%
  # mutate(valuefactor=cut(value,breaks=c(-20,-0.05,0,0.05,max(value,na.rm=T)),
  #                        labels=c("<-0.05","-0.05~0","0~0.05",">0.05"))) %>%
  # change level order
  mutate(valuefactor=factor(as.character(valuefactor),levels=rev(levels(valuefactor))))

textcol <- "black"
# color <- colorRampPalette(colors = c("red",'white',"blue"))(4)
color <- c("#FF0000", "#FF6666", "#FFCCCC", "#CCCCFF","#6699FF", "#0000FF" )
# color <- c("#FF0000", "#FC9272", "#FFF5F0", "#F7FBFF","#9ECAE1", "#0000FF" )
# color <- c("#99000D", "#FC9272", "#FFF5F0", "#F7FBFF","#9ECAE1", "#084594" )
# p <- 
txtx<-c(1:18)*2-1.2
txt<-c("-   +")
xinterceptSet<-c(1:18)*2+0.5
p2 <- ggplot(sindex1,aes(x=varname,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(b) Changbai Mountain")+
  scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  
  # scale_x_discrete(labels=c('AceProdACmax','','bdnr','','br_mr','','DeadACEMethanogens','','DeadH2Methanogens','', 'DeadMethanotrophs','','k_dom', 
  #                           '','dom_diffus','','flnr','','GrowACEMethanogens','','GrowH2Methanogens','','GrowRmethanotrophs', 
  #                           '','grperc', '','KACE', '','froot_leaf', '','YAceMethanogens','','YH2Methanogens','','YMethanotrophs',''))+
  # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4"),na.value = "grey90")+  #"#ddf1da"
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =18,hjust ="bottom",vjust ="bottom")+
  # coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=30)+
  theme(legend.position="none",legend.direction="vertical",
        # legend.box = 'horizontal',
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        # legend.box.just = ,
        axis.text.x=element_text(size=36,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=36,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=48,face="bold"))

####
sindex <- read.csv(file = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭//CLM/result/sindex_ab-n.csv',header = T)
sindex2 <- sindex[c(3,4,5,6,17,18,25,26,29,30,13,14,15,16,27,28,1,2,7:12,19:24,31:36),2:6]
sindex[,2:6] <- sindex2 
sindex <- melt(sindex)
sindex[1:144,3] <- sindex[1:144,3]/max(sindex[1:144,3])
# sindex[145:180,3] <- sindex[145:180,3]/max(sindex[145:180,3])
sindex1 <- sindex %>%
  # convert state to factor and reverse order of levels
  mutate(variable=factor(variable,levels=rev(sort(unique(variable))))) %>%
  # create a new variable from count
  mutate(valuefactor=cut(value,breaks=c(-20,-0.5,-0.05,0,0.05,0.5,max(value,na.rm=T)),
                         labels=c('< -0.1',"-0.1 ~ -0.005","-0.005~0","0~0.005","0.005 ~ 0.1", '>0.1'))) %>%
  # mutate(valuefactor=cut(value,breaks=c(-60,-0.05,0,0.05,max(value,na.rm=T)),
  #                        labels=c("<-0.05","-0.05~0","0~0.05",">0.05"))) %>%
  # change level order
  mutate(valuefactor=factor(as.character(valuefactor),levels=rev(levels(valuefactor))))

textcol <- "black"
# color <- colorRampPalette(colors = c("red",'white','blue'))(6)
# color <- c("#FF0000", "#FC9272", "#FFF5F0", "#F7FBFF","#9ECAE1", "#0000FF" )
color <- c("#FF0000", "#FF6666", "#FFCCCC", "#CCCCFF","#6699FF", "#0000FF" )
# color <- c("#99000D", "#FC9272", "#FFF5F0", "#F7FBFF","#9ECAE1", "#084594" )
# c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF", "#FFFFFF","#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D")
breakset1<-seq(1,36,2)
labelset1<-c('AceProdACmax','bdnr','br_mr','DeadH2Methanogens', 'k_dom', 'dom_diffus','flnr','GrowACEMethanogens','GrowH2Methanogens', 'grperc', 'KACE', 'froot_leaf',  
             'YAceMethanogens','YH2Methanogens')
txtx<-c(1:18)*2-1.2
txt<-c("-  +")
xinterceptSet<-c(1:18)*2+0.5
p3 <-  ggplot(sindex1,aes(x=varname,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+#
  labs(x="",y="",title="(c) Lesser Khingan Mountain")+
  scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(breaks=breakset1,labels=labelset1)+
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =22,hjust ="bottom",vjust ="bottom")+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  scale_x_discrete(labels=c('bdnr','','br_mr','','flnr','','grperc', '','froot_leaf', '','k_dom', 
                            '','dom_diffus','','KACE', '','AceProdACmax','','DeadACEMethanogens','','DeadH2Methanogens','',
                            'DeadMethanotrophs','','GrowACEMethanogens','','GrowH2Methanogens','','GrowRmethanotrophs', 
                            '','YAceMethanogens','','YH2Methanogens','','YMethanotrophs',''))+
  scale_fill_manual(values = color,na.value = "grey90")+
  # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4"),na.value = "grey90")+  #"#ddf1da"
  # coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  theme_grey(base_size=30)+
  theme(legend.position="none",legend.direction="vertical",
        # legend.box = 'horizontal',
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        # legend.box.just = ,
        axis.text.x=element_text(size=36,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=36,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=48,face="bold"))

p4 <-  ggplot(sindex1,aes(x=varname,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \n Index"))+#
  labs(x="",y="",title="Lesser Khingan Mountains")+
  scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(breaks=breakset1,labels=labelset1)+
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =18,hjust ="bottom",vjust ="bottom")+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  scale_x_discrete(labels=c('bdnr','','br_mr','','flnr','','grperc', '','froot_leaf', '','k_dom', 
                            '','dom_diffus','','KACE', '','AceProdACmax','','DeadACEMethanogens','','DeadH2Methanogens','', 'DeadMethanotrophs','','GrowACEMethanogens','','GrowH2Methanogens','','GrowRmethanotrophs', 
                            '','YAceMethanogens','','YH2Methanogens','','YMethanotrophs',''))+
  scale_fill_manual(values = color,na.value = "grey90")+
  # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4"),na.value = "grey90")+  #"#ddf1da"
  # coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  theme_grey(base_size=30)+
  theme(legend.position="right",legend.direction="vertical",
        # legend.box = 'horizontal',
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=36,face="bold"),
        legend.key.height=grid::unit(5,"cm"),
        legend.key.width=grid::unit(1,"cm"),
        # legend.box.just = ,
        axis.text.x=element_text(size=24,colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size = 36,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=36,face="bold"))
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p4)
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend<-g_legend(p)
# Fig00_name=grid.arrange(arrangeGrob(arrangeGrob(p+theme(legend.position="none")),
#                                     arrangeGrob(p2+theme(legend.position="none")),
#                         arrangeGrob(p3+theme(legend.position="none")),ncol=1, nrow=3),
#                         arrangeGrob(mylegend, ncol=1, nrow=1),widths =c(3,0.5), heights = c(1.5,0.8))
png("heatmap112.png", family="Times", pointsize = 12, width=100,height=80,units="cm",res=300)
ggdraw()+
  draw_plot(p,x= 0,y=11.5/16,width = 0.85,height = 4.5/16)+
  draw_plot(p2,x=0,y = 7/16,width = 0.85,height = 4.5/16)+
  draw_plot(p3,x=0,y = 0,width = 0.85,height = 7/16)+
  draw_plot(mylegend,x =0.85,y =0.3, width = 0.15,height = 0.6)

dev.off()

# grid.arrange(p,p2,p3,nrow = 3)

# png("heatmap1.png", family="Times", pointsize = 12, width=100,height=80,units="cm",res=300)
# grid.newpage()
# pushViewport(viewport(x = 0.9, y = 14.2/20, width = 0.9, height = 5.8/20, just = c("left", "top")))
# draw(p, column_title = "    a)        Sanjiang Plain", column_title_gp = gpar(fontsize = 32, fontfamily = "Times", fontface = "bold"), newpage = FALSE)
# popViewport()
# 
# pushViewport(viewport(x = 0.9, y = 8.2/20, width = 0.9, height = 5.8/20, just = c("left", "top")))
# draw(p2, column_title = "b)        Changbai Mountains", column_title_gp = gpar(fontsize = 32, fontfamily = "Times", fontface = "bold"), newpage = FALSE)
# popViewport()
# 
# pushViewport(viewport(x = 0.9, y = 0, width = 0.9, height = 8/20, just = c("left", "top")))
# draw(p3, column_title = "c)        Lesser Khingan", column_title_gp = gpar(fontsize = 32, fontfamily = "Times", fontface = "bold"), newpage = FALSE)
# popViewport()
# 
# pushViewport(viewport(x = 1, y = 0, width = 0.1, height = 1, just = c("left", "top")))
# draw(lgd_2000to2009)
# popViewport()
# 
# dev.off()
#############
#####origin#######
nc_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/CLM/transient/5-4/a_xxal_transient.clm2.h0.2010-01-01-00000 (20).nc')
GPP <- ncvar_get(nc_data, varid = 'GPP')
GPP_OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/modis_gpp.csv',header = T, sep = ',')
Day <- 1:365
GPP <- GPP*24*3600
obsgpp <- GPP_OBS[,3]/365
gppr2 <- summary(lm(GPP[GPP_OBS[,2]]~obsgpp))

# opar <- par(mar= c(4.5,5,5,2)+0.1)
# plot(Day, GPP, main = 'Lesser Khingan Mountains',ylab='GPP',xlab ='DOY',type="l",pch=18,cex = 1.5,col="black",yaxt="n",xaxt = 'n',lwd=2,lty=1)
# lines(GPP_OBS[,2],GPP_OBS[,3]/365,type="p",pch=19,col="red",lty=1,lwd=2)
# # arrows(GPP_OBS[,2],(GPP_OBS[,3]/365)*0.95,GPP_OBS[,2],(GPP_OBS[,3]/365)*1.05,length=0.05, angle=90, code=3,col = 'red')
# text(x =360 ,y = 1.5,paste('R2=',round(gppr2$adj.r.squared,2) ))
# axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
# axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
# legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
# par(opar)
opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(GPP_OBS[,2],GPP_OBS[,3]/365,main = 'Lesser Khingan Mountains',ylab=expression(GPP/g*m^{-2}*d^{-1}),xlab ='DOY',type="p",pch=18,col="red",yaxt="n",xaxt = 'n',cex= 1.5,lty=1,lwd=2,ylim = c(0,2))
lines(Day, GPP,type="l",pch=19,col="black",lty=1,lwd=2)
text(x =355 ,y = 1.6,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)


nc_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/result/3-31-origin/transient_cbs.clm2.h0.2011-01-01-00000.nc')
eco_data <- read.table(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/fluxdata/nee-gpp-er.csv',header =T, sep = ',')
GPP <- eco_data[1:91,17]*-1
GPP_day <- eco_data[1:91,16]
GPP_model <- ncvar_get(nc_data,varid = 'GPP')
GPP_model <- GPP_model*1000

# gppcor <- cor.test(GPP,GPP_model[GPP_day])
gppr2 <- summary(lm(GPP~GPP_model[GPP_day]))
##GRAPH
# jpeg(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/1-6/GPP.png',width = 1200,height = 900,units = 'px',pointsize = 24)

opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(GPP_day,GPP,main = 'Changbai Mountains',ylab=expression(GPP/g*m^{-2}*d^{-1}),xlab ='DOY',type="l",pch=18,col="black",yaxt="n",xaxt = 'n',lty=1,lwd=2,xlim = c(90,310),ylim = c(-0.04,0.25))
lines(x =90:310,GPP_model[90:310],type="p",pch=19,col="red",lty=1,lwd=2)
text(x =302 ,y = 0.19,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)

######################
###### methane ###########
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/结果/SJ_WETLANDS_TRANSIENT/SJ_WETLANDS_TRANSIENT.clm2.h0.2012-01-01-00000.nc')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/Rtest/clm/三江甲烷排放.csv',header = TRUE)
# obs <- methane_obs

methane_obs <- methane_sanjiang$X2012.CH4.mgm.2d.1.[1:153]
methane_model <- methane_model*1382400000#*3  ##24*3600*16*1000  mol/m2/s-->mg/m2/d
## plot
# jpeg(filename = 'methane1123.png',width = 1200,height = 900,units = 'px',pointsize = 24)
opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(x=1:153,methane_obs,main = 'Change trend of methane in the growing season',ylab=expression(methane/mg*m^{-2}*d^{-1}),xlab ='days',type="b",pch=21,col="slateblue1",yaxt="n",xaxt = 'n',lwd=2,lty=1)
lines(x=1:153,methane_model,type="b",pch=22,col="green",lwd=2,lty=1)
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c("slateblue1" , "green"),lty = 1,lwd=2,pch = c(21,22))
par(opar)
# dev.off()

x <- 121:273
data_me <- data.frame(x,methane_obs)
opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(x=121:273,methane_obs,ylab=expression(methane/mg*m^{-2}*d^{-1}),xlab ='DOY',type="l",pch=18,col="black",yaxt="n",xaxt = 'n',lty=1,lwd=2)
lines(x=121:273,methane_model[121:273],type="p",pch=19,col="red",lty=1,lwd=2)
# text(x =270 ,y = 6.5,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)


nc_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/4-1/transient_cbs.clm2.h0.2016-01-01-00000 (5).nc')
methane <- ncvar_get(nc_data1,varid = 'CH4_SURF_NETFLUX')
methane_obs <- read.csv(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/fluxdata/methane.csv', sep = ',',header = T )
methane <- methane*12*3600*1000
obs_day <- methane_obs[,1]
obs_day <- as.integer(as.matrix(obs_day))
obs <- methane_obs[,2]
obs <- as.double(as.matrix(obs))

# ch4cor <- cor.test(obs,methane[obs_day])
Day <- 1:365
opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(obs_day,obs,ylab=expression(methane/mg*m^{-2}*d^{-1}),xlab ='DOY',type="l",pch=18,col="black",yaxt="n",xaxt = 'n',lty=1,lwd=2)
lines(Day, methane,type="p",pch=19,col="red",lty=1,lwd=2)
# text(x =270 ,y = 6.5,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)


CH4_OBS <- read.csv(file = "D:/AAAA-资料E盘/CLM/DATA/小兴安岭/2011_CH4.csv",header = F, sep = ',')
model <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/CLM/transient/6-3/xxal_methane.clm2.h0.2014-01-01-00000 (3).nc')
ch4_2014 <- CH4_OBS[,19]
ch4_day <- CH4_OBS[,18]

Day <- 1:365
CH4 <- ncvar_get(model,varid = 'CH4_SURF_NETFLUX')
CH4<- CH4*16*1000*3600
ch4r2 <- summary(lm(ch4_2014~CH4[ch4_day]))

opar <- par(mar= c(4.5,5,5,2)+0.1)
plot(ch4_day,ch4_2014,type="p", main = 'Methane',ylab='Methane',xlab ='DOY',pch=25,cex = 1.5,col="red",yaxt="n",xaxt = 'n',lwd=2,lty=1,xlim = c(1,365),ylim = c(0,20))
lines(Day, CH4,type="b", pch=1,col="grey50",lty=1,lwd=2)
# arrows(ch4_day,(ch4_2013-ch4_SE),ch4_day,(ch4_2013+ch4_SE),length=0.05, angle=90, code=3,col = 'red')
text(x =4,y = 20,paste('R2=',round(ch4r2$r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(25,1))
par(opar)

opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(ch4_day,ch4_2014,ylab=expression(methane/mg*m^{-2}*d^{-1}),xlab ='DOY',type="l",pch=18,col="black",yaxt="n",xaxt = 'n',lty=1,lwd=2,xlim = c(0,365))
lines(Day, CH4,type="p",pch=19,col="red",lty=1,lwd=2)
# text(x =270 ,y = 6.5,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(reshape)

nc_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/结果/SJ_WETLANDS_TRANSIENT/SJ_WETLANDS_TRANSIENT.clm2.h0.2012-01-01-00000.nc')
nc_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/4-1/transient_cbs.clm2.h0.2016-01-01-00000 (5).nc')
nc_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/CLM/transient/6-3/xxal_methane.clm2.h0.2014-01-01-00000 (3).nc')
TRANSP <- ncvar_get(nc_data,varid = 'CH4_SURF_AERE')
DIFF <- ncvar_get(nc_data,varid = 'CH4_SURF_DIFF')
EBUL <- ncvar_get(nc_data,varid = 'CH4_SURF_EBUL')
CH4 <- ncvar_get(nc_data,varid = 'CH4_SURF_NETFLUX')
OXID_AOM <- ncvar_get(nc_data,varid = 'CH4_OXID_AOM_DEPTH')[1,] # ANAEROBIC
OXID_O2 <- ncvar_get(nc_data,varid = 'CH4_OXID_O2_DEPTH')[1,] # AEROBIC
CO2_TO_CH4 <- ncvar_get(nc_data,varid = 'CH4_PROD_CO2_DEPTH')[1,]
ACE_CH4 <- ncvar_get(nc_data,varid = 'CH4_PROD_ACE_DEPTH')[1,]

methane <- data.frame(CO2_TO_CH4,ACE_CH4,OXID_AOM,OXID_O2,TRANSP,DIFF,EBUL,CH4)
methane <- melt(methane)
methane$Day <- rep(1:365,8)
methane$pathway <- c(rep(c("prod",'oxid'),each = 2*365),rep('transp',4*365))
p_methane <- ggplot(data = methane, 
                    aes(x = Day, y = value,group = variable, color = variable))+
  geom_line(size = 1)+
  facet_grid(variable~pathway,scales = 'free_y')+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

product <- data.frame(CO2_TO_CH4,ACE_CH4)
product <- melt(product)
product$DOY <- rep(1:365,2)
product$pathway <- rep('production',365)
ggplot(data = product, 
       aes(x = DOY, y = value,group = variable, color = variable))+
  geom_line(size = 1)+
  facet_grid(variable~pathway,scales = 'free_x')+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
sum(ACE_CH4)/sum(CO2_TO_CH4)
oxid <- data.frame(OXID_AOM,OXID_O2)
oxid <- melt(oxid)
oxid$DOY <- rep(1:365,2)
oxid$oxidation <- rep('oxidation',365)
ggplot(data = oxid, 
       aes(x = DOY, y = value,group = variable, color = variable))+
  geom_line(size = 1)+
  facet_grid(variable~oxidation,scales = 'free_x')+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
transport <- data.frame(TRANSP,DIFF,EBUL)
transport <- melt(transport)
transport$DOY <- rep(1:365,3)
transport$pathway <- rep('transport',365)
ggplot(data = transport, 
       aes(x = DOY, y = value,group = variable, color = variable))+
  geom_line(size = 1)+
  facet_grid(variable~pathway,scales = 'free_x')+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


######### methanogens-methantrophs################
#'CAERCH4BIOS'---biomass of aerobic methanotrophs'
#'CCO2BIOS'---biomass of hydrogenotrophic methanogens'
#'CDOCS'---DOC
SJ_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/结果/SJ_WETLANDS_TRANSIENT/SJ_WETLANDS_TRANSIENT.clm2.h0.2012-01-01-00000.nc')
CBS_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/6-16/cbs_test.clm2.h0.2016-01-01-00000 (8).nc')
XXAL_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/CLM/transient/6-3/xxal_methane.clm2.h0.2014-01-01-00000 (3).nc')


soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
               1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
ACE_methanogens_S <- ncvar_get(SJ_data,varid = 'CACEBIOS')
ACE_methanogens_C <- ncvar_get(CBS_data,varid = 'CACEBIOS')
ACE_methanogens_X <- ncvar_get(XXAL_data,varid = 'CACEBIOS')

HY_methanogens_S <- ncvar_get(SJ_data,varid = 'CCO2BIOS')
HY_methanogens_C <- ncvar_get(CBS_data,varid = 'CCO2BIOS')
HY_methanogens_X <- ncvar_get(XXAL_data,varid = 'CCO2BIOS')

DOC_S <- ncvar_get(SJ_data,varid = 'CDOCS')
DOC_C <- ncvar_get(CBS_data,varid = 'CDOCS')
DOC_X <- ncvar_get(XXAL_data,varid = 'CDOCS')




# soil_layer <- 3
# SJ_ACE <- ACE_methanogens_S[soil_layer,]
# CBS_ACE <- ACE_methanogens_C[soil_layer,]
# XXAL_ACE <- ACE_methanogens_X[soil_layer,]
# ###
# SJ_ACE <- apply(ACE_methanogens_S, 2, sum)
# CBS_ACE <- apply(ACE_methanogens_C,2, sum)
# XXAL_ACE <- apply(ACE_methanogens_X,2,sum)
# 
# SJ_HY <- apply(HY_methanogens_S, 2, sum)
# CBS_HY <- apply(HY_methanogens_C,2, sum)
# XXAL_HY <- apply(HY_methanogens_X,2,sum)
###
SJ_ACE <- 0
CBS_ACE <- 0
XXAL_ACE <- 0
SJ_HY <- 0
CBS_HY <- 0
XXAL_HY <- 0
SJ_DOC <- 0
CBS_DOC <- 0
XXAL_DOC <- 0

i <- 1
repeat{
  SJ_ACE <- SJ_ACE + ACE_methanogens_S[i,]*soil_pare[i]
  CBS_ACE <- CBS_ACE + ACE_methanogens_C[i,]*soil_pare[i]
  XXAL_ACE <- XXAL_ACE + ACE_methanogens_X[i,]*soil_pare[i]
  SJ_HY <- SJ_HY + HY_methanogens_S[i,]*soil_pare[i]
  CBS_HY <- CBS_HY + HY_methanogens_C[i,]*soil_pare[i]
  XXAL_HY <- XXAL_HY + HY_methanogens_X[i,]*soil_pare[i]
  SJ_DOC <- SJ_DOC + DOC_S[i,182:213]*soil_pare[i]
  CBS_DOC <- CBS_DOC + DOC_C[i,182:213]*soil_pare[i]
  XXAL_DOC <- XXAL_DOC + DOC_X[i,182:213]*soil_pare[i]
  i <- i+1
  if(i>4){
    break
  }
}
sum(SJ_DOC)
sum(CBS_DOC)
sum(XXAL_DOC)


methanogens_ace <- matrix(ncol = 3,nrow = 1095)
colnames(methanogens_ace) <- c("Station","DOY","Acetotrophic_Methanogens")
methanogens_ace[,1] <- rep(c("(a) Sanjiang Plain", '(b) Changbai Mountain',"(c) Lesser Khingan Mountain"),each=365)
methanogens_ace[,2] <- rep(1:365,3)
methanogens_ace[1:365,3] <- SJ_ACE
methanogens_ace[366:730,3] <- CBS_ACE
methanogens_ace[731:1095,3] <- XXAL_ACE
# methanogens_ace[,3] <- as.numeric(methanogens_ace[,3])*10^9 # mol/m3-->nmol/m3

methanogens_ace <- as.data.frame(methanogens_ace)
methanogens_ace[,2] <- as.numeric(methanogens_ace[,2])
methanogens_ace[,3] <- as.double(methanogens_ace[,3])
methanogens_ace[186,3] <- 0.00000004
ggplot(data = methanogens_ace,aes(x = DOY, y = Acetotrophic_Methanogens))+
  geom_line(size = 1)+
  facet_wrap(~Station,scales = "free_y",nrow = 2) +
  theme_bw()+
  theme(
    axis.text.x=element_text(size=12,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=12,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 12,face = 'bold'),
    axis.title.x = element_text(size = 12,face = 'bold'),
    strip.text = element_text(size = 10,face = 'bold')
  )

methanogens_HY <- matrix(ncol = 3,nrow = 1095)
colnames(methanogens_HY) <- c("Station","DOY","HY_Methanogens")
methanogens_HY[,1] <- rep(c("(a) Sanjiang Plain", '(b) Changbai Mountain',"(c) Lesser Khingan Mountain"),each=365)
methanogens_HY[,2] <- rep(1:365,3)
methanogens_HY[1:365,3] <- SJ_HY
methanogens_HY[366:730,3] <- CBS_HY
methanogens_HY[731:1095,3] <- XXAL_HY

methanogens_HY <- as.data.frame(methanogens_HY)
methanogens_HY[,2] <- as.numeric(methanogens_HY[,2])
methanogens_HY[,3] <- as.double(methanogens_HY[,3])
ggplot(data = methanogens_HY,aes(x = DOY, y = HY_Methanogens))+
  geom_line(size = 1)+
  facet_wrap(~Station,nrow = 2) +
  theme_bw()+
  theme(
    axis.text.x=element_text(size=12,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=12,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 12,face = 'bold'),
    axis.title.x = element_text(size = 12,face = 'bold'),
    strip.text = element_text(size = 10,face = 'bold')
  )


DOC_DB <- matrix(ncol = 3,nrow = 1095)
colnames(DOC_DB) <- c("Station","DOY","DOC")
DOC_DB[,1] <- rep(c("(a) Sanjiang Plain", '(b) Changbai Mountain',"(c) Lesser Khingan Mountain"),each=365)
DOC_DB[,2] <- rep(1:365,3)
DOC_DB[1:365,3] <- SJ_DOC
DOC_DB[366:730,3] <- CBS_DOC
DOC_DB[731:1095,3] <- XXAL_DOC

DOC_DB <- as.data.frame(DOC_DB)
DOC_DB[,2] <- as.numeric(DOC_DB[,2])
DOC_DB[,3] <- as.double(DOC_DB[,3])
ggplot(data = DOC_DB,aes(x = DOY, y = DOC))+
  geom_line(size = 1)+
  facet_wrap(~Station,scales = 'free_y',nrow = 2) +
  ylab("DOC g c/m^3")+
  theme_bw()+
  theme(
    axis.text.x=element_text(size=12,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=12,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 12,face = 'bold'),
    axis.title.x = element_text(size = 12,face = 'bold'),
    strip.text = element_text(size = 10,face = 'bold')
  )

##### sj--  CH4 FLUXES##### 
Plant <- ncvar_get(SJ_data,varid = 'CH4_SURF_AERE')
Ebulition <- ncvar_get(SJ_data,varid = 'CH4_SURF_EBUL')
Diffusive <- ncvar_get(SJ_data,varid = 'CH4_SURF_DIFF')

DOY <- rep(1:365,3)
Methane_flux <- matrix(nrow = 1095,ncol = 1)
Methane_flux[1:365,] <- Plant
Methane_flux[366:730,] <- Ebulition
Methane_flux[731:1095,] <- Diffusive
TYPE <- rep(c("Plant", 'Ebullition',"Diffusion"),each=365)
SJ_methane <- data.frame(DOY,Methane_flux,TYPE) 

SJ_PED <- 
  ggplot(SJ_methane,aes(x = DOY, y = Methane_flux,fill = TYPE))+
  geom_area()+
  labs(x = 'DOY', y= expression(Methane_Flux/(mol*m^{-3}*s^{-1})), title = "(a) Sanjiang Plain")+
  theme_bw()+
  theme(
    legend.position = c(0.9,0.9),
    legend.title=element_blank(),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=45,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=45,colour='black',vjust = 1,hjust = 1,face = 'bold'),
    axis.text.y=element_text(size=45,vjust=0.2,colour='black',face = 'bold'),
    axis.title.y = element_text(size = 50,face = 'bold'),
    axis.title.x = element_text(size = 50,face = 'bold'),
    # plot.title = element_text(hjust = 0.5),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=55,face="bold"))

pie_data <- data.frame(
  TYPE = c("Plant", 'Ebuliton',"Diffusion"),
  CH4_flux = c(sum(Plant),sum(Ebulition),sum(Diffusive))
)
head(pie_data)
# pie_data <- pie_data %>% 
#   arrange(desc(TYPE)) %>%
#   mutate(prop = CH4_flux / sum(pie_data$CH4_flux) *100) %>%
#   mutate(ypos = cumsum(prop)- 0.5*prop )

percentage <- round(pie_data$CH4_flux/sum(pie_data$CH4_flux)*100, digits = 2)
pie_label <- paste(percentage,'%',sep = ' ')
# pie_label <- paste(pie_label,"%",sep = '')
# mycolor <- c( "#F8766D","#00BA38", "#619CFF")
# pie3D(pie_data$CH4_flux, labels = pie_label, explode = 0.2,labelcex = 1, shade = 0.6,radius = 2.3, col = c("#619CFF", "#00BA38", "#F8766D"))
# piepic <- ggplot(pie_data, aes(x="", y = prop,fill = TYPE))+
#   geom_bar(stat = 'identity', width = 1, color = 'white')+
#   coord_polar('y', start = 0)+
#   ylab('')+
#   geom_text(aes(y = ypos, label = pie_label),color = 'white')+
#   scale_fill_manual(values = mycolor)+
#   theme_void()+
#   theme(legend.position = 'none')
piepic <- ggplot(pie_data, aes(x = '', y = CH4_flux,fill = TYPE))+
  geom_bar(stat = 'identity',width = 1)+
  coord_polar(theta = 'y')+
  labs(x= '', y='', title = '')+
  # theme(axis.ticks = element_blank())+
  scale_fill_discrete(breaks = pie_data$TYPE, labels=pie_label)+
  # theme(axis.text.x = element_blank())+
  # geom_text(aes(y = CH4_flux/2 + c(0, cumsum(CH4_flux)[-length(CH4_flux)]), 
  #               x = sum(CH4_flux)/1.7e-05, label = pie_label),size = 16,angle = 0)+
  theme_void()+
  theme(legend.title = element_blank(), 
        legend.position = 'none',
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
        )

## DIE JIA
SJ_PED
vp <- viewport(x = 0.33, y = 0.68, width = 0.4,height = 0.5)
print(piepic, vp=vp)

##### CBS--CH4 FLUXES######
Plant <- ncvar_get(CBS_data,varid = 'CH4_SURF_AERE')
Ebulition <- ncvar_get(CBS_data,varid = 'CH4_SURF_EBUL')
Diffusive <- ncvar_get(CBS_data,varid = 'CH4_SURF_DIFF')

DOY <- rep(1:365,3)
Methane_flux <- matrix(nrow = 1095,ncol = 1)
Methane_flux[1:365,] <- Plant
Methane_flux[366:730,] <- Ebulition
Methane_flux[731:1095,] <- Diffusive
TYPE <- rep(c("Plant", 'Ebullition',"Diffusion"),each=365)
CBS_methane <- data.frame(DOY,Methane_flux,TYPE) 

CBS_PED <- 
  ggplot(CBS_methane,aes(x = DOY, y = Methane_flux,fill = TYPE))+
  geom_area()+
  labs(x = 'DOY', y= expression(Methane_Flux/(mol*m^{-3}*s^{-1})), title = "(b) Changbai Mountain")+
  theme_bw()+
  theme(
    legend.position = c(0.9,0.9),
    legend.title=element_blank(),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=45,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=45,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=45,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 50,face = 'bold'),
    axis.title.x = element_text(size = 50,face = 'bold'),
    # plot.title = element_text(hjust = 0.5),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=55,face="bold"))

pie_data1 <- data.frame(
  TYPE = c("Plant", 'Ebuliton',"Diffusion"),
  CH4_flux = c(sum(Plant),sum(Ebulition),sum(Diffusive))
)
head(pie_data1)
pie_data1 <- pie_data1 %>% 
  arrange(desc(TYPE)) %>%
  mutate(prop = CH4_flux / sum(pie_data1$CH4_flux) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

percentage <- round(pie_data1$CH4_flux/sum(pie_data1$CH4_flux)*100, digits = 2)
pie_label <- paste(percentage,'%',sep = ' ')
# pie_label <- paste(pie_label,"%",sep = '')
mycolor <- c( "#F8766D","#00BA38", "#619CFF")
# pie3D(pie_data$CH4_flux, labels = pie_label, explode = 0.2,labelcex = 1, shade = 0.6,radius = 2.3, col = c("#619CFF", "#00BA38", "#F8766D"))
piepic1 <- ggplot(pie_data1, aes(x="", y = prop,fill = TYPE))+
  geom_bar(stat = 'identity', width = 1, color = 'white')+
  coord_polar('y', start = 0)+
  ylab('')+
  geom_text(aes(y = ypos, label = pie_label),color = 'white')+
  scale_fill_manual(values = mycolor)+
  theme_void()+
  theme(legend.position = 'none')

piepic3 <- ggplot(pie_data1, aes(x = '', y = CH4_flux,fill = TYPE))+
  geom_bar(stat = 'identity',width = 1)+
  coord_polar(theta = 'y')+
  labs(x= '', y='', title = '')+
  theme(axis.ticks = element_blank())+
  scale_fill_discrete(breaks = pie_data1$TYPE, labels=pie_label)+
  theme(axis.text.x = element_blank())+
  # geom_text(aes(y = CH4_flux/1.9+ c(0, cumsum(CH4_flux)[-length(CH4_flux)]), x = sum(CH4_flux)/6.2e-06, label = pie_label),size = 16,angle = 30)+
  theme_void()+
  theme(legend.title = element_blank(), legend.position = 'none')


## DIE JIA
CBS_PED
vp <- viewport(x = 0.33, y = 0.68, width = 0.4,height = 0.5)
print(piepic3, vp=vp)


##### xxal--CH4 FLUXES######
Plant <- ncvar_get(XXAL_data,varid = 'CH4_SURF_AERE')
Ebulition <- ncvar_get(XXAL_data,varid = 'CH4_SURF_EBUL')
Diffusive <- ncvar_get(XXAL_data,varid = 'CH4_SURF_DIFF')

DOY <- rep(1:365,3)
Methane_flux <- matrix(nrow = 1095,ncol = 1)
Methane_flux[1:365,] <- Plant
Methane_flux[366:730,] <- Ebulition
Methane_flux[731:1095,] <- Diffusive
TYPE <- rep(c("Plant", 'Ebullition',"Diffusion"),each=365)
XXAL_methane <- data.frame(DOY,Methane_flux,TYPE) 
# XXAL_methane$TYPE <- factor(XXAL_methane$TYPE, levels = c( 'Ebuliton',"Diffusion", "Plant"))

XXAL_PED <- 
  ggplot(XXAL_methane,aes(x = DOY, y = Methane_flux,fill = TYPE))+
  geom_area()+
  labs(x = 'DOY', y= expression(Methane_Flux/(mol*m^{-3}*s^{-1})), title = "(c) Lesser Khingan Mountain")+
  theme_bw()+
  theme(
    legend.position = c(0.9,0.9),
    legend.title=element_blank(),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=45,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=45,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=45,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 50,face = 'bold'),
    axis.title.x = element_text(size = 50,face = 'bold'),
    # plot.title = element_text(hjust = 0.5),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=55,face="bold"))

pie_data2 <- data.frame(
  TYPE = c("Plant", 'Ebuliton',"Diffusion"),
  CH4_flux = c(sum(Plant),sum(Ebulition),sum(Diffusive))
)
head(pie_data2)
# pie_data2 <- pie_data2 %>% 
#   arrange(desc(TYPE)) %>%
#   mutate(prop = CH4_flux / sum(pie_data2$CH4_flux) *100) %>%
#   mutate(ypos = cumsum(prop)- 0.5*prop )

percentage <- round(pie_data2$CH4_flux/sum(pie_data2$CH4_flux)*100, digits =3)
pie_label <- paste(percentage,'%',sep = ' ')
# pie_label <- paste(pie_label,"%",sep = '')
# mycolor <- c( "#F8766D","#00BA38", "#619CFF")
# pie3D(pie_data$CH4_flux, labels = pie_label, explode = 0.2,labelcex = 1, shade = 0.6,radius = 2.3, col = c("#619CFF", "#00BA38", "#F8766D"))
# piepic2 <- ggplot(pie_data2, aes(x="", y = prop,fill = TYPE))+
#   geom_bar(stat = 'identity', width = 1, color = 'white')+
#   coord_polar('y', start = 0)+
#   ylab('')+
#   geom_text(aes(y = ypos, label = pie_label),color = 'white')+
#   scale_fill_manual(values = mycolor)+
#   theme_void()+
#   theme(legend.position = 'none')

piepic2 <- ggplot(pie_data2, aes(x = '', y = CH4_flux,fill = TYPE))+
  geom_bar(stat = 'identity',width = 1)+
  coord_polar(theta = 'y')+
  labs(x= '', y='', title = '')+
  theme(axis.ticks = element_blank())+
  scale_fill_discrete(breaks = pie_data2$TYPE, labels=pie_label)+
  theme(axis.text.x = element_blank())+
  # geom_text(aes(y = CH4_flux/2 + c(0, cumsum(CH4_flux)[-length(CH4_flux)]), x = sum(CH4_flux)/9.0e-06, label = pie_label),size = 16,angle = 45)+
  theme_void()+
  theme(legend.title = element_blank(), legend.position = 'none')

## DIE JIA
XXAL_PED
vp <- viewport(x = 0.32, y = 0.65, width = 0.4,height = 0.5)
print(piepic2, vp=vp)

#### 3D nihe #####
SJ_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/结果/SJ_WETLANDS_TRANSIENT/SJ_WETLANDS_TRANSIENT.clm2.h0.2012-01-01-00000.nc')
CBS_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/长白山/CLM/transient/6-16/cbs_test.clm2.h0.2016-01-01-00000 (8).nc')
XXAL_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/DATA/小兴安岭/CLM/transient/6-3/xxal_methane.clm2.h0.2014-01-01-00000 (3).nc')


soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
               1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
ACE_methanogens_S <- ncvar_get(SJ_data,varid = 'CACEBIOS')
ACE_methanogens_C <- ncvar_get(CBS_data,varid = 'CACEBIOS')
ACE_methanogens_X <- ncvar_get(XXAL_data,varid = 'CACEBIOS')

HY_methanogens_S <- ncvar_get(SJ_data,varid = 'CCO2BIOS')
HY_methanogens_C <- ncvar_get(CBS_data,varid = 'CCO2BIOS')
HY_methanogens_X <- ncvar_get(XXAL_data,varid = 'CCO2BIOS')

DOC_S <- ncvar_get(SJ_data,varid = 'CDOCS')
DOC_C <- ncvar_get(CBS_data,varid = 'CDOCS')
DOC_X <- ncvar_get(XXAL_data,varid = 'CDOCS')

i =1
soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
               1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[1,]*soil_pare[1], Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[2,]*soil_pare[2], Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[3,]*soil_pare[3], Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[4,]*soil_pare[4], Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[5,]*soil_pare[5], Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[6,]*soil_pare[6], Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[7,]*soil_pare[7], Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[8,]*soil_pare[8], Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
   structure(list(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
            row.names = c(NA,365L),class= "data.frame")
  i <- i+1
  
  if(i>8) {
    break
  }
}

s1_data["Soil_depth"] <- '0.007'
s2_data["Soil_depth"] <- "0.028"
s3_data["Soil_depth"] <- "0.062"
s4_data["Soil_depth"] <- "0.119"
s5_data["Soil_depth"] <- "0.212"
s6_data["Soil_depth"] <- "0.366"
s7_data["Soil_depth"] <- "0.619"
s8_data["Soil_depth"] <- "1.000"
all_data <- rbind(s1_data,s2_data,s3_data,s4_data,s5_data,s6_data,s7_data,s8_data)
all_long <- melt(all_data, id.vars = c('DOY','ACE_methanogens','Soil_depth'))
all_data[186,2] <- all_data[185,2]
all_data[1024,2] <- all_data[1023,2]
all_data[1755,2] <- all_data[1754,2]

plot_ly(all_data, x = ~Soil_depth, y = ~DOY, z = ~ACE_methanogens, split = ~Soil_depth, type = "scatter3d",
     mode = 'lines', line = list(width = 4))
all_data$Soil_depth <- as.numeric(all_data$Soil_depth)
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
ggplot(all_data,aes(x=DOY,y=Soil_depth,z=ACE_methanogens))+
  # geom_tile(aes(fill=ACE_methanogens))+#根据高度填充没有进行平滑处理
  geom_raster(aes(fill=ACE_methanogens), interpolate=T)+###进行平滑处理
  scale_fill_gradientn(colors = colormap)+
  # geom_contour_filled()+
  # scale_y_discrete(breaks = seq(0,1,0.2))+
  scale_y_reverse()+
  # coord_flip()+
  # geom_contour(aes(colour= ..level..),color="black")+
  labs(x="DOY",y="Soil depth",fill="ACE_methanogens")+
  # theme_bw()+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')

ggplot(all_data,aes(x=DOY,y=Soil_depth,z=ACE_methanogens))+
  geom_contour_filled()+
  scale_y_reverse()+
  labs(x="DOY",y="Soil depth",fill="ACE_methanogens")+
  # theme_bw()+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')

## surface data
s1_data[186,2] <- s1_data[185,2]
s3_data[294,2] <- s3_data[293,2]
s5_data[295,2] <- s5_data[294,2]
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
ACE_methanogens <- cbind(s1_data[,2],s2_data[,2],s3_data[,2],s4_data[,2],s5_data[,2],s6_data[,2],s7_data[,2],s8_data[,2])
-log(ACE_methanogens)
plot_ly(x = s_depth, y = DOY, z = log(ACE_methanogens))%>%add_surface()
# surface
Soil_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
ACE_methanogens <- cbind(s1_data[,2],s2_data[,2],s3_data[,2],s4_data[,2],s5_data[,2],s6_data[,2],s7_data[,2],s8_data[,2])
ACE_methanogens[362:365,] <- ACE_methanogens[361,]
ACE_methanogens[354:357,] <- ACE_methanogens[353,]
sj_sur <- plot_ly(x = ~Soil_depth, y = ~DOY, z = ~log(ACE_methanogens),type = "surface")%>%
  # add_surface()%>%
  layout( title = "Sanjiang Plain",
          xaxis = list(title = "Soil_depth",showgrid = F),
          yaxis=list(title="DOY",showgrid = F),
          legend=list(title = "ACE_methanogens"))

axis_x <- list( showgrid = FALSE,   # 是否显示网格线
                zeroline = TRUE,   # 是否绘制x=0,y=0的直线
                nticks = 20 ,      # 坐标轴刻度的最大数目
                showline = TRUE ,  # 是否绘制绘图区边框
                title = 'Soil depth',
                mirror = 'all',
                color = 'black')
axis_y <- list( showgrid = FALSE,   # 是否显示网格线
                zeroline = TRUE,   # 是否绘制x=0,y=0的直线
                nticks = 20 ,      # 坐标轴刻度的最大数目
                showline = TRUE ,  # 是否绘制绘图区边框
                title = 'DOY',
                mirror = 'all',
                color = 'black')
axis_z <- list( showgrid = FALSE,   # 是否显示网格线
                zeroline = TRUE,   # 是否绘制x=0,y=0的直线
                nticks = 20 ,      # 坐标轴刻度的最大数目
                showline = TRUE ,  # 是否绘制绘图区边框
                antorange =FALSE,
                rangemode = 'normal',
                range = c(-5,-40),
                title = 'ACE_methanogens',
                mirror = 'all',
                color = 'black')
sj_sur <- plot_ly(x = ~Soil_depth, y = ~DOY, z = ~log(ACE_methanogens),type = "surface")
 sj_sur%>% layout(xaxis = axis_x,yaxis = axis_y)

 
 
##cbs
c1_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[1,]*soil_pare[1], Soil_depth = rep(s_depth[1],365))
c2_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[2,]*soil_pare[2], Soil_depth = rep(s_depth[2],365))
c3_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[3,]*soil_pare[3], Soil_depth = rep(s_depth[3],365))
c4_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[4,]*soil_pare[4], Soil_depth = rep(s_depth[4],365))
c5_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[5,]*soil_pare[5], Soil_depth = rep(s_depth[5],365))
c6_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[6,]*soil_pare[6], Soil_depth = rep(s_depth[6],365))
c7_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[7,]*soil_pare[7], Soil_depth = rep(s_depth[7],365))
c8_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[8,]*soil_pare[8], Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = ACE_methanogens_C[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
            row.names = c(NA,365L),class= "data.frame")
  i <- i+1
  
  if(i>8) {
    break
  }
}

c1_data["Soil_depth"] <- '0.007'
c2_data["Soil_depth"] <- "0.028"
c3_data["Soil_depth"] <- "0.062"
c4_data["Soil_depth"] <- "0.119"
c5_data["Soil_depth"] <- "0.212"
c6_data["Soil_depth"] <- "0.366"
c7_data["Soil_depth"] <- "0.619"
c8_data["Soil_depth"] <- "1.000"

all_data1 <- rbind(c1_data,c2_data,c3_data,c4_data,c5_data,c6_data,c7_data,c8_data)
all_long1 <- melt(all_data1, id.vars = c('DOY','ACE_methanogens','Soil_depth'))
all_data1[186,2] <- all_data1[185,2]
all_data1[1024,2] <- all_data1[1023,2]
all_data1[1755,2] <- all_data1[1754,2]
plot_ly(all_data1, x = ~Soil_depth, y = ~DOY, z = ~ACE_methanogens, split = ~Soil_depth, type = "scatter3d",
        mode = 'lines', line = list(width = 4))
# #surface
# Soil_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
# DOY <- 1:365
# ACE_methanogens <- cbind(c1_data[,2],c2_data[,2],c3_data[,2],c4_data[,2],c5_data[,2],c6_data[,2],c7_data[,2],c8_data[,2])
# cbs_sur <- plot_ly(z = list(show = T, start = -5, end = -40,size = 0.5),x = ~Soil_depth, y = ~DOY, z = ~log(ACE_methanogens),type = "surface"
#                    )%>%
#   # add_surface()%>%
#   layout( title = "Changbai Mountain",
#           xaxis = list(title = "Soil_depth",showgrid = F),
#           yaxis=list(title="DOY",showgrid = F),
#           legend=list(title = "ACE_methanogens"))
# cbs_sur <- plot_ly(type = 'surface',contours = list(z = list(show = T, start = -5, end = -40,size = 0.5)),
#                    x = ~Soil_depth, y = ~DOY, z = ~log(ACE_methanogens))
all_data1$Soil_depth <- as.numeric(all_data1$Soil_depth)
ggplot(all_data1,aes(DOY,Soil_depth,z=ACE_methanogens))+
  # geom_raster()+
  geom_contour_filled()+
  scale_y_reverse()+
  labs(x="DOY",y="Soil depth",fill="ACE_methanogens")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')


 ####
x1_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[1,]*soil_pare[1], Soil_depth = rep(s_depth[1],365))
x2_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[2,]*soil_pare[2], Soil_depth = rep(s_depth[2],365))
x3_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[3,]*soil_pare[3], Soil_depth = rep(s_depth[3],365))
x4_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[4,]*soil_pare[4], Soil_depth = rep(s_depth[4],365))
x5_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[5,]*soil_pare[5], Soil_depth = rep(s_depth[5],365))
x6_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[6,]*soil_pare[6], Soil_depth = rep(s_depth[6],365))
x7_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[7,]*soil_pare[7], Soil_depth = rep(s_depth[7],365))
x8_data <- data.frame(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[8,]*soil_pare[8], Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = ACE_methanogens_X[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
            row.names = c(NA,365L),class= "data.frame")
  i <- i+1
  
  if(i>8) {
    break
  }
}

x1_data["Soil_depth"] <- '0.007'
x2_data["Soil_depth"] <- "0.028"
x3_data["Soil_depth"] <- "0.062"
x4_data["Soil_depth"] <- "0.119"
x5_data["Soil_depth"] <- "0.212"
x6_data["Soil_depth"] <- "0.366"
x7_data["Soil_depth"] <- "0.619"
x8_data["Soil_depth"] <- "1.000"
x1_data[224,2] <- x1_data[223,2]
all_data2 <- rbind(x1_data,x2_data,x3_data,x4_data,x5_data,x6_data,x7_data,x8_data)
all_long2 <- melt(all_data, id.vars = c('DOY','ACE_methanogens','Soil_depth'))
# all_data2[186,2] <- all_data2[185,2]
# all_data2[1024,2] <- all_data2[1023,2]
# all_data2[1755,2] <- all_data2[1754,2]
all_data2[224,2] <- all_data2[223,2]
plot_ly(all_data2, x = ~Soil_depth, y = ~DOY, z = ~ACE_methanogens, split = ~Soil_depth, type = "scatter3d",
        mode = 'lines', line = list(width = 4))
#surface
Soil_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
ACE_methanogens <- cbind(x1_data[,2],x2_data[,2],x3_data[,2],x4_data[,2],x5_data[,2],x6_data[,2],x7_data[,2],x8_data[,2])
xxal_sur <- plot_ly(x = ~Soil_depth, y = ~DOY, z = ~log(ACE_methanogens),type = "surface")%>%
  # add_surface()%>%
  layout( title = "Lesser Khingan Mountain",
          xaxis = list(title = "Soil_depth",showgrid = F),
          yaxis=list(title="DOY",showgrid = F),
          legend=list(title = "ACE_methanogens"))
all_data2$Soil_depth <- as.numeric(all_data2$Soil_depth)
ggplot(all_data2,aes(DOY,Soil_depth,z=ACE_methanogens))+
  geom_contour_filled()+
  scale_y_reverse()+
  labs(x="DOY",y="Soil depth",fill="ACE_methanogens")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')

ace_data <- rbind(all_data,all_data1,all_data2)
ace_data$station <- rep(c('Sanjiang Plain','Changbai Mountain','Lesser Khingan Mountain'),each =2920)
bk <- c(-Inf,2e-9,4e-9,8e-9,2e-8,4e-8,8e-8,2e-7,2e-5,4e-5,8e-5,2e-4,4e-4,8e-4,0.0005,0.001, Inf)
bk <- c(-Inf,1e-9,5e-8,1e-7,5e-6,1e-5,0.0005,0.001, Inf)
ace_data1 <- ace_data
ace_data1$ACE_methanogens <- log(ace_data1$ACE_methanogens)
bk1 <- c(-Inf,-40,-35,-30,-25,-20,-15,-10,-5)
ggplot(ace_data,aes(DOY,Soil_depth,z=ACE_methanogens))+
  geom_contour_filled(breaks = bk)+
  scale_y_reverse()+
  facet_wrap(vars(station),scales = 'free_y')+
  labs(x="DOY",y="Soil depth",fill="ACE_methanogens")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')

ggplot(ace_data,aes(DOY,Soil_depth))+
  geom_contour_filled(aes(z=ACE_methanogens))+
  # scale_fill_continuous(breaks = bk1)+
  scale_y_reverse()+
  facet_wrap(vars(station),scales = 'free_y')+
  labs(x="DOY",y="Soil depth",fill="ACE_methanogens")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')

ggplot(ace_data1,aes(DOY,Soil_depth,z=ACE_methanogens))+
  geom_raster(aes(fill = ACE_methanogens), interpolate=TRUE)+
  scale_fill_gradientn(colours=colormap)+
  # scale_fill_continuous(breaks = bk1)+
  scale_y_reverse()+
  facet_wrap(vars(station),scales = 'free_y')+
  labs(x="DOY",y="Soil depth",fill="ACE_methanogens")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')










#### hy--h2_co2 ####
HY_methanogens_S <- ncvar_get(SJ_data,varid = 'CCO2BIOS')
HY_methanogens_C <- ncvar_get(CBS_data,varid = 'CCO2BIOS')
HY_methanogens_X <- ncvar_get(XXAL_data,varid = 'CCO2BIOS')

i =1
soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
               1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
# structure(list(DOY = 1:365, ACE_methanogens = ACE_methanogens_S[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
#           row.names = c(NA,365L),class= "data.frame")
s11_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[1,]*soil_pare[1], Soil_depth = rep(s_depth[1],365))
s21_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[2,]*soil_pare[2], Soil_depth = rep(s_depth[2],365))
s31_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[3,]*soil_pare[3], Soil_depth = rep(s_depth[3],365))
s41_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[4,]*soil_pare[4], Soil_depth = rep(s_depth[4],365))
s51_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[5,]*soil_pare[5], Soil_depth = rep(s_depth[5],365))
s61_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[6,]*soil_pare[6], Soil_depth = rep(s_depth[6],365))
s71_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[7,]*soil_pare[7], Soil_depth = rep(s_depth[7],365))
s81_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_S[8,]*soil_pare[8], Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, HY_methanogens = HY_methanogens_S[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
            row.names = c(NA,365L),class= "data.frame")
  i <- i+1
  
  if(i>8) {
    break
  }
}

s11_data["Soil_depth"] <- '0.007'
s21_data["Soil_depth"] <- "0.028"
s31_data["Soil_depth"] <- "0.062"
s41_data["Soil_depth"] <- "0.119"
s51_data["Soil_depth"] <- "0.212"
s61_data["Soil_depth"] <- "0.366"
s71_data["Soil_depth"] <- "0.619"
s81_data["Soil_depth"] <- "1.000"
all_data11 <- rbind(s11_data,s21_data,s31_data,s41_data,s51_data,s61_data,s71_data,s81_data)
all_long11 <- melt(all_data11, id.vars = c('DOY','HY_methanogens','Soil_depth'))
# all_data1[186,2] <- all_data1[185,2]
# all_data1[1024,2] <- all_data1[1023,2]
# all_data1[1755,2] <- all_data1[1754,2]

plot_ly(all_data11, x = ~Soil_depth, y = ~DOY, z = ~HY_methanogens, split = ~Soil_depth, type = "scatter3d",
        mode = 'lines', line = list(width = 4))
## surface data
# s11_data[186,2] <- s11_data[185,2]
# s31_data[294,2] <- s31_data[293,2]
# s51_data[295,2] <- s51_data[294,2]
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
HY_methanogens <- cbind(s11_data[,2],s21_data[,2],s31_data[,2],s41_data[,2],s51_data[,2],
                        s61_data[,2],s71_data[,2],s81_data[,2])
-log(HY_methanogens)
plot_ly(x = s_depth, y = DOY, z = log(HY_methanogens))%>%add_surface()
# surface
Soil_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
HY_methanogens <- cbind(s11_data[,2],s21_data[,2],s31_data[,2],s41_data[,2],s51_data[,2],
                        s61_data[,2],s71_data[,2],s81_data[,2])

sj_sur1 <- plot_ly(x = ~Soil_depth, y = ~DOY, z = ~log(HY_methanogens),type = "surface")%>%
  # add_surface()%>%
  layout( title = "Sanjiang Plain",
          xaxis = list(title = "Soil_depth",showgrid = F),
          yaxis=list(title="DOY",showgrid = F),
          legend=list(title = "HY_methanogens"))
##CBS
c11_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[1,]*soil_pare[1], Soil_depth = rep(s_depth[1],365))
c21_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[2,]*soil_pare[2], Soil_depth = rep(s_depth[2],365))
c31_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[3,]*soil_pare[3], Soil_depth = rep(s_depth[3],365))
c41_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[4,]*soil_pare[4], Soil_depth = rep(s_depth[4],365))
c51_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[5,]*soil_pare[5], Soil_depth = rep(s_depth[5],365))
c61_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[6,]*soil_pare[6], Soil_depth = rep(s_depth[6],365))
c71_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[7,]*soil_pare[7], Soil_depth = rep(s_depth[7],365))
c81_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_C[8,]*soil_pare[8], Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, HY_methanogens = HY_methanogens_C[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
            row.names = c(NA,365L),class= "data.frame")
  i <- i+1
  
  if(i>8) {
    break
  }
}

c11_data["Soil_depth"] <- '0.007'
c21_data["Soil_depth"] <- "0.028"
c31_data["Soil_depth"] <- "0.062"
c41_data["Soil_depth"] <- "0.119"
c51_data["Soil_depth"] <- "0.212"
c61_data["Soil_depth"] <- "0.366"
c71_data["Soil_depth"] <- "0.619"
c81_data["Soil_depth"] <- "1.000"

c11_data[which(c11_data$HY_methanogens > 8e-7),]
c11_data[303,2] <- c11_data[302,2]
c21_data[which(c21_data$HY_methanogens > 6e-6),]
c21_data[c(141,142,143,152,153,233,234),2] <- c21_data[200,2]
c31_data[c(143,153,156),2] <- c31_data[200,2]
c41_data[c(304,305,306,310),2] <- c41_data[303,2]

all_data22 <- rbind(c11_data,c21_data,c31_data,c41_data,c51_data,c61_data,c71_data,c81_data)
all_long22 <- melt(all_data22, id.vars = c('DOY','HY_methanogens','Soil_depth'))
# all_data2[]

plot_ly(all_data22, x = ~Soil_depth, y = ~DOY, z = ~HY_methanogens, split = ~Soil_depth, type = "scatter3d",
        mode = 'lines', line = list(width = 4))
## surface data
# s11_data[186,2] <- s11_data[185,2]
# s31_data[294,2] <- s31_data[293,2]
# s51_data[295,2] <- s51_data[294,2]
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
HY_methanogens <- cbind(c11_data[,2],c21_data[,2],c31_data[,2],c41_data[,2],c51_data[,2],
                        c61_data[,2],c71_data[,2],c81_data[,2])
-log(HY_methanogens)
plot_ly(x = s_depth, y = DOY, z = log(HY_methanogens))%>%add_surface()
# surface
Soil_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
HY_methanogens <- cbind(c11_data[,2],c21_data[,2],c31_data[,2],c41_data[,2],c51_data[,2],
                        c61_data[,2],c71_data[,2],c81_data[,2])

cbs_sur1 <- plot_ly(x = ~Soil_depth, y = ~DOY, z = ~log(HY_methanogens),type = "surface")%>%
  # add_surface()%>%
  layout( title = "Changbai Mountain",
          xaxis = list(title = "Soil_depth",showgrid = F),
          yaxis=list(title="DOY",showgrid = F),
          legend=list(title = "HY_methanogens"))
##xxal
x11_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[1,]*soil_pare[1], Soil_depth = rep(s_depth[1],365))
x21_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[2,]*soil_pare[2], Soil_depth = rep(s_depth[2],365))
x31_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[3,]*soil_pare[3], Soil_depth = rep(s_depth[3],365))
x41_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[4,]*soil_pare[4], Soil_depth = rep(s_depth[4],365))
x51_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[5,]*soil_pare[5], Soil_depth = rep(s_depth[5],365))
x61_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[6,]*soil_pare[6], Soil_depth = rep(s_depth[6],365))
x71_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[7,]*soil_pare[7], Soil_depth = rep(s_depth[7],365))
x81_data <- data.frame(DOY = 1:365, HY_methanogens = HY_methanogens_X[8,]*soil_pare[8], Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, HY_methanogens = HY_methanogens_X[i,]*soil_pare[i], Soil_depth = rep(s_depth[i],365)),
            row.names = c(NA,365L),class= "data.frame")
  i <- i+1
  
  if(i>8) {
    break
  }
}

x11_data["Soil_depth"] <- '0.007'
x21_data["Soil_depth"] <- "0.028"
x31_data["Soil_depth"] <- "0.062"
x41_data["Soil_depth"] <- "0.119"
x51_data["Soil_depth"] <- "0.212"
x61_data["Soil_depth"] <- "0.366"
x71_data["Soil_depth"] <- "0.619"
x81_data["Soil_depth"] <- "1.000"
all_data33 <- rbind(x11_data,x21_data,x31_data,x41_data,x51_data,x61_data,x71_data,x81_data)
all_long33 <- melt(all_data33, id.vars = c('DOY','HY_methanogens','Soil_depth'))
# all_data3[452,2] <- all_data3[451,2]
all_data33[88:112,2] <- all_data33[87,2]
all_data33[450:476,2] <- all_data33[454,2]
all_data33[301:365,2] <- all_data33[300,2]
# all_data3[1024,2] <- all_data3[1023,2]
# all_data3[1755,2] <- all_data3[1754,2]
# all_data3[,2] <- as.numeric(all_data3[,2])
plot_ly(all_data33, x = ~Soil_depth, y = ~DOY, z = ~HY_methanogens, split = ~Soil_depth, type = "scatter3d",
        mode = 'lines', line = list(width = 4))
## surface data
# s11_data[186,2] <- s11_data[185,2]
# s31_data[294,2] <- s31_data[293,2]
# s51_data[295,2] <- s51_data[294,2]
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
HY_methanogens <- cbind(x11_data[,2],x21_data[,2],x31_data[,2],x41_data[,2],x51_data[,2],
                        x61_data[,2],x71_data[,2],x81_data[,2])
-log(HY_methanogens)
plot_ly(x = s_depth, y = DOY, z = log(HY_methanogens))%>%add_surface(color ="set1")
# surface
Soil_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
DOY <- 1:365
HY_methanogens <- cbind(x11_data[,2],x21_data[,2],x31_data[,2],x41_data[,2],x51_data[,2],
                        x61_data[,2],x71_data[,2],x81_data[,2])

xxal_sur1 <- plot_ly(x = ~Soil_depth, y = ~DOY, z = ~log(HY_methanogens),type = "surface",size = 12)%>%
  # add_surface()%>%
  layout( title = "Lesser Khingan Mountain",
          xaxis = list(title = I("Soil_depth"),showticklabel = T,showgrid = F,tickfont = list(size = 22)),
          yaxis=list(title=I("DOY"),showgrid = F),
          legend=list(title = "HY_methanogens"))

hy_data <- rbind(all_data11,all_data22,all_data33)
hy_data1 <- hy_data
hy_data$station <- rep(c('Sanjiang Plain','Changbai Mountain','Lesser Khingan Mountain'),each =2920)
ace_data$production <- rep(c('ACE_methangens'),8760)
hy_data$production <- rep(c('HY_methangens'),8760)
colnames(ace_data) <- c('DOY','Biomass','Soil depth','Station','Bacteria')
colnames(hy_data) <- c('DOY','Biomass','Soil depth','Station','Bacteria')
ay_data <- rbind(ace_data,hy_data)
colnames(ay_data) <- c('DOY','Biomass','Soil_depth','Station','Bacteria')
ay_data$Station <- factor(ay_data$Station,levels = c('Sanjiang Plain','Changbai Mountain','Lesser Khingan Mountain'))


ay_data$Soil_depth <- as.numeric(ay_data$Soil_depth)
bk <- c(-Inf,1e-9,5e-8,1e-7,5e-6,1e-5,0.0005,0.001, Inf)
ay_data1 <- ay_data
ay_data1$ACE_methanogens <- log(ay_data1$Biomass)
bk1 <- c(-Inf,-40,-35,-30,-25,-20,-15,-10,-5)
ggplot(ay_data,aes(DOY,Soil_depth,z=Biomass))+
  geom_contour_filled(breaks = bk)+
  scale_y_reverse()+
  facet_grid(Station~Bacteria)+
  labs(x="DOY",y="Soil depth",fill="Biomass")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')

ay_data2 <- ay_data
ay_data2[which(ay_data2$Station == 'Lesser Khingan Mountain'),][which(ay_data2$DOY < 150),2] <- 2.1e-16
bk <- c(-Inf,5e-12,1e-11,5e-10,1e-9,5e-8,1e-7,5e-6,1e-5,0.0005,0.001, Inf)
ggplot(ay_data,aes(DOY,Soil_depth,z=Biomass))+
  geom_contour_filled(breaks = bk)+
  scale_y_reverse()+
  facet_grid(Station~Bacteria)+
  labs(x="DOY",y="Soil depth (m)",fill="Biomass (mol/m-3)")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=25,face="bold",color="black"),
    axis.text = element_text(size=25,face="bold",color="black"),
    legend.title=element_text(size=25,face="bold",color="black"),
    legend.text = element_text(size=25,face="bold",color="black"),
    legend.background = element_blank(),
    legend.position ='right',
    strip.text = element_text(size = 25,face = 'bold'))
ggplot(ay_data2,aes(DOY,Soil_depth,z=Biomass))+
  geom_raster(aes(fill=Biomass), interpolate=TRUE)+###进行平滑处理
  scale_fill_gradientn(colours=colormap)+
  scale_y_reverse()+
  facet_grid(Station~Bacteria)+
  labs(x="DOY",y="Soil depth",fill="Biomass")+
  theme(
    panel.background = element_blank(),
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=13,face="plain",color="black"),
    legend.title=element_text(size=13,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position ='right')
