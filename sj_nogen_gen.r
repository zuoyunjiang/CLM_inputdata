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

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/test_t.clm2.h0.2012-01-01-00000 (428).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2012-01-01-00000 (3).nc')
OBS <- read.csv(file = 'D:/Rtest/2012_CO2.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
NOGEN <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_OBS <-  OBS$GPP[1:153]
GPP_MODEL <- GPP_MODEL*24*3600
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
mydata <- data.frame(DOY,GPP,TYPE)

#
ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sanjiang Plain")+
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

##ggline##
gppdata <- matrix(ncol = 3,nrow = 124)
colnames(gppdata) <- c('Modeled_GPP','Observed_GPP','Station')
gppdata[1:124,1] <- GPP_MODEL[150:273] 
gppdata[1:124,2] <- GPP_OBS[30:153]
gppdata[1:124,3] <- rep('(a) Sanjiang Plain',124)
gppdata <- data.frame(gppdata)
attach(gppdata)
gppdata[,1] <- as.double(gppdata[,1])
gppdata[,2] <- as.double(gppdata[,2])
gppdata[,1:2] <- round(gppdata[,1:2],2)
ggplot(data = gppdata,aes(x = Observed_GPP, y = Modeled_GPP),color = 'black')+
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

###
#### methane #####      
# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/wed_t.clm2.h0.2012-01-01-00000 (25).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2012-01-01-00000 (459).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2012-01-01-00000 (3).nc')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
mnogen <- ncvar_get(nc = try_data1,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/Rtest/clm/三江甲烷排放.csv',header = TRUE)
methane_obs <- methane_sanjiang$X2012.CH4.mgm.2d.1.[1:153]
methane_model <- methane_model*1382400000
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
mydata <- data.frame(DOY,METHANE,TYPE)
ggplot(mydata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sanjiang Plain")+
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

#####gen#####

try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2020-01-01-00000 (388).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2019-01-01-00000 (119).nc')

soil_pare <- rep(1,15)
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CACEBIOS') 
ACE_methanogens_S1 <- ncvar_get(try_data1,varid = 'CACEBIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[1,244:365],ACE_methanogens_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[2,244:365],ACE_methanogens_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[3,244:365],ACE_methanogens_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[4,244:365],ACE_methanogens_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[5,244:365],ACE_methanogens_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[6,244:365],ACE_methanogens_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[7,244:365],ACE_methanogens_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[8,244:365],ACE_methanogens_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
ACE_10 <- data.frame(s1_data$DOY,s1_data$ACE_methanogens+s2_data$ACE_methanogens+s3_data$ACE_methanogens+s4_data$ACE_methanogens)
colnames(ACE_10) <- c("DOY","ACE_methanogens")
ggplot(ACE_10,aes(x = DOY,y = ACE_methanogens) )+
  geom_line(size = 2)
ACE_20 <- data.frame(s1_data$DOY,s5_data$ACE_methanogens)
colnames(ACE_20) <- c("DOY","ACE_methanogens")
ACE_40 <- data.frame(s1_data$DOY,s6_data$ACE_methanogens)
colnames(ACE_40) <- c("DOY","ACE_methanogens")
ACE_60 <- data.frame(s1_data$DOY,s7_data$ACE_methanogens)
colnames(ACE_60) <- c("DOY","ACE_methanogens")
ACE_100 <- data.frame(s1_data$DOY,s8_data$ACE_methanogens)
colnames(ACE_100) <- c("DOY","ACE_methanogens")

## vertical
v_data <- cbind(s1_data[,1:2],s2_data$ACE_methanogens,s3_data$ACE_methanogens,s4_data$ACE_methanogens
                ,s5_data$ACE_methanogens,s6_data$ACE_methanogens,s7_data$ACE_methanogens,s8_data$ACE_methanogens)
colnames(v_data) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata <- melt(v_data,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'ACE')


mydata1 <- mydata
mydata1$Dep <- NA
mydata1$Dep[which(mydata1$Depth == 'S1')] <- 0.007
mydata1$Dep[which(mydata1$Depth == 'S2')]<- 0.028
mydata1$Dep[which(mydata1$Depth == 'S3')] <- 0.062
mydata1$Dep[which(mydata1$Depth == 'S4')] <- 0.119
mydata1$Dep[which(mydata1$Depth == 'S5')] <- 0.212
mydata1$Dep[which(mydata1$Depth == 'S6')] <- 0.366
mydata1$Dep[which(mydata1$Depth == 'S7')] <- 0.619
mydata1$Dep[which(mydata1$Depth == 'S8')] <- 1.000


mean_data2 <- aggregate(mydata1$ACE,by = list(mydata1$Dep),mean,na.rm = T)

########CO2
soil_pare <- rep(1,15)
CHO_S <- ncvar_get(try_data,varid = 'CCO2BIOS')
CHO_S1 <- ncvar_get(try_data1,varid = 'CCO2BIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[1,244:365],CHO_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[2,244:365],CHO_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[3,244:365],CHO_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[4,244:365],CHO_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[5,244:365],CHO_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[6,244:365],CHO_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[7,244:365],CHO_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[8,244:365],CHO_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
s10 <- data.frame(s1_data$DOY,s1_data$CHO+s2_data$CHO+s3_data$CHO+s4_data$CHO)
colnames(s10) <- c("DOY","CHO")
# ggplot(s10,aes(x = DOY,y = CHO) )+
#   geom_line(size = 2)
s20 <- data.frame(s1_data$DOY,s5_data$CHO)
colnames(s20) <- c("DOY","CHO")
s40 <- data.frame(s1_data$DOY,s6_data$CHO)
colnames(s40) <- c("DOY","CHO")
s60 <- data.frame(s1_data$DOY,s7_data$CHO)
colnames(s60) <- c("DOY","CHO")
s100 <- data.frame(s1_data$DOY,s8_data$CHO)
colnames(s100) <- c("DOY","CHO")

####vertical####
v_data1 <- cbind(s1_data[,1:2],s2_data$CHO,s3_data$CHO,s4_data$CHO
                 ,s5_data$CHO,s6_data$CHO,s7_data$CHO,s8_data$CHO)
colnames(v_data1) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata2 <- melt(v_data1,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'CHO')


mydata3 <- mydata2
mydata3$Dep <- NA
mydata3$Dep[which(mydata3$Depth == 'S1')] <- 0.007
mydata3$Dep[which(mydata3$Depth == 'S2')]<- 0.028
mydata3$Dep[which(mydata3$Depth == 'S3')] <- 0.062
mydata3$Dep[which(mydata3$Depth == 'S4')] <- 0.119
mydata3$Dep[which(mydata3$Depth == 'S5')] <- 0.212
mydata3$Dep[which(mydata3$Depth == 'S6')] <- 0.366
mydata3$Dep[which(mydata3$Depth == 'S7')] <- 0.619
mydata3$Dep[which(mydata3$Depth == 'S8')] <- 1.000


mean_data3 <- aggregate(mydata3$CHO,by = list(mydata3$Dep),mean,na.rm = T)

#M51
MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
ggplot(data = MODEL_AC1,aes(x = DEPTH,y  = VALUE,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line()+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D"))+
  scale_shape_manual(values=c(21,22))+
  theme_classic()

MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)

min.max.norm <- function(x){
  ((x-min(x))/(max(x)-min(x)))
} 
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)

#### obs ####
mic_data <- read.csv("D:/AAAA-资料E盘/CLM/DATA/microbe/ace.csv",header = T)
mic_data1 <- mic_data[1:40,12:15]
mic_data1$DEPTH <- rep(seq(10,100,10),4)
mic_data1$SEASON <- factor(mic_data1$SEASON,levels = c('spring','summer','autumn','winter'))
mean_data <- aggregate(mic_data1$M00357M,by = list(mic_data1$DEPTH),mean,na.rm = T)
mean_data1 <- aggregate(mic_data1$M00567M,by = list(mic_data1$DEPTH),mean,na.rm = T)
MODEL_AC2 <- cbind(mean_data,mean_data1$x)
colnames(MODEL_AC2) <- c('DEPTH','M00357M','M00567M')
MODEL_AC3 <- melt(MODEL_AC2,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC3$bz <- scale(MODEL_AC3$VALUE)
MODEL_AC3$bz1 <- min.max.norm(MODEL_AC3$VALUE)
####jiehe ####

model_com <- rbind(MODEL_AC1,MODEL_AC3)
model_com$DEPTH[17:36] <- model_com$DEPTH[17:36]/100
ggplot(data = model_com,aes(x = DEPTH,y  = bz1,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633','#33FF99'))+
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()

ggplot(data = model_com,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size = 4,colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_color_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.1),
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
    
    
    
#####nogen#####
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2020-01-01-00000 (3).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2019-01-01-00000 (3).nc')

soil_pare <- rep(1,15)
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CACEBIOS') 
ACE_methanogens_S1 <- ncvar_get(try_data1,varid = 'CACEBIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[1,244:365],ACE_methanogens_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[2,244:365],ACE_methanogens_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[3,244:365],ACE_methanogens_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[4,244:365],ACE_methanogens_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[5,244:365],ACE_methanogens_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[6,244:365],ACE_methanogens_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[7,244:365],ACE_methanogens_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[8,244:365],ACE_methanogens_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
ACE_10 <- data.frame(s1_data$DOY,s1_data$ACE_methanogens+s2_data$ACE_methanogens+s3_data$ACE_methanogens+s4_data$ACE_methanogens)
colnames(ACE_10) <- c("DOY","ACE_methanogens")
ggplot(ACE_10,aes(x = DOY,y = ACE_methanogens) )+
  geom_line(size = 2)
ACE_20 <- data.frame(s1_data$DOY,s5_data$ACE_methanogens)
colnames(ACE_20) <- c("DOY","ACE_methanogens")
ACE_40 <- data.frame(s1_data$DOY,s6_data$ACE_methanogens)
colnames(ACE_40) <- c("DOY","ACE_methanogens")
ACE_60 <- data.frame(s1_data$DOY,s7_data$ACE_methanogens)
colnames(ACE_60) <- c("DOY","ACE_methanogens")
ACE_100 <- data.frame(s1_data$DOY,s8_data$ACE_methanogens)
colnames(ACE_100) <- c("DOY","ACE_methanogens")

## vertical
v_data <- cbind(s1_data[,1:2],s2_data$ACE_methanogens,s3_data$ACE_methanogens,s4_data$ACE_methanogens
                ,s5_data$ACE_methanogens,s6_data$ACE_methanogens,s7_data$ACE_methanogens,s8_data$ACE_methanogens)
colnames(v_data) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata <- melt(v_data,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'ACE')


mydata1 <- mydata
mydata1$Dep <- NA
mydata1$Dep[which(mydata1$Depth == 'S1')] <- 0.007
mydata1$Dep[which(mydata1$Depth == 'S2')]<- 0.028
mydata1$Dep[which(mydata1$Depth == 'S3')] <- 0.062
mydata1$Dep[which(mydata1$Depth == 'S4')] <- 0.119
mydata1$Dep[which(mydata1$Depth == 'S5')] <- 0.212
mydata1$Dep[which(mydata1$Depth == 'S6')] <- 0.366
mydata1$Dep[which(mydata1$Depth == 'S7')] <- 0.619
mydata1$Dep[which(mydata1$Depth == 'S8')] <- 1.000


mean_data2 <- aggregate(mydata1$ACE,by = list(mydata1$Dep),mean,na.rm = T)

########CO2
soil_pare <- rep(1,15)
CHO_S <- ncvar_get(try_data,varid = 'CCO2BIOS')
CHO_S1 <- ncvar_get(try_data1,varid = 'CCO2BIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[1,244:365],CHO_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[2,244:365],CHO_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[3,244:365],CHO_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[4,244:365],CHO_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[5,244:365],CHO_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[6,244:365],CHO_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[7,244:365],CHO_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[8,244:365],CHO_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
s10 <- data.frame(s1_data$DOY,s1_data$CHO+s2_data$CHO+s3_data$CHO+s4_data$CHO)
colnames(s10) <- c("DOY","CHO")
# ggplot(s10,aes(x = DOY,y = CHO) )+
#   geom_line(size = 2)
s20 <- data.frame(s1_data$DOY,s5_data$CHO)
colnames(s20) <- c("DOY","CHO")
s40 <- data.frame(s1_data$DOY,s6_data$CHO)
colnames(s40) <- c("DOY","CHO")
s60 <- data.frame(s1_data$DOY,s7_data$CHO)
colnames(s60) <- c("DOY","CHO")
s100 <- data.frame(s1_data$DOY,s8_data$CHO)
colnames(s100) <- c("DOY","CHO")

####vertical####
v_data1 <- cbind(s1_data[,1:2],s2_data$CHO,s3_data$CHO,s4_data$CHO
                 ,s5_data$CHO,s6_data$CHO,s7_data$CHO,s8_data$CHO)
colnames(v_data1) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata2 <- melt(v_data1,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'CHO')


mydata3 <- mydata2
mydata3$Dep <- NA
mydata3$Dep[which(mydata3$Depth == 'S1')] <- 0.007
mydata3$Dep[which(mydata3$Depth == 'S2')]<- 0.028
mydata3$Dep[which(mydata3$Depth == 'S3')] <- 0.062
mydata3$Dep[which(mydata3$Depth == 'S4')] <- 0.119
mydata3$Dep[which(mydata3$Depth == 'S5')] <- 0.212
mydata3$Dep[which(mydata3$Depth == 'S6')] <- 0.366
mydata3$Dep[which(mydata3$Depth == 'S7')] <- 0.619
mydata3$Dep[which(mydata3$Depth == 'S8')] <- 1.000


mean_data3 <- aggregate(mydata3$CHO,by = list(mydata3$Dep),mean,na.rm = T)

#M51
MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
ggplot(data = MODEL_AC1,aes(x = DEPTH,y  = VALUE,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line()+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D"))+
  scale_shape_manual(values=c(21,22))+
  theme_classic()

MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)

min.max.norm <- function(x){
  ((x-min(x))/(max(x)-min(x)))
} 
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)

#### obs ####
mic_data <- read.csv("D:/AAAA-资料E盘/CLM/DATA/microbe/ace.csv",header = T)
mic_data1 <- mic_data[1:40,12:15]
mic_data1$DEPTH <- rep(seq(10,100,10),4)
mic_data1$SEASON <- factor(mic_data1$SEASON,levels = c('spring','summer','autumn','winter'))
mean_data <- aggregate(mic_data1$M00357M,by = list(mic_data1$DEPTH),mean,na.rm = T)
mean_data1 <- aggregate(mic_data1$M00567M,by = list(mic_data1$DEPTH),mean,na.rm = T)
MODEL_AC2 <- cbind(mean_data,mean_data1$x)
colnames(MODEL_AC2) <- c('DEPTH','M00357M','M00567M')
MODEL_AC3 <- melt(MODEL_AC2,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC3$bz <- scale(MODEL_AC3$VALUE)
MODEL_AC3$bz1 <- min.max.norm(MODEL_AC3$VALUE)
####jiehe ####

model_com <- rbind(MODEL_AC1,MODEL_AC3)
model_com$DEPTH[17:36] <- model_com$DEPTH[17:36]/100
ggplot(data = model_com,aes(x = DEPTH,y  = bz1,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633','#33FF99'))+
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()

ggplot(data = model_com,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size = 4,colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_color_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.1),
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

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/test_t.clm2.h0.2012-01-01-00000 (428).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2012-01-01-00000 (3).nc')
OBS <- read.csv(file = 'D:/Rtest/2012_CO2.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
NOGEN <- ncvar_get(nc = try_data1,varid = 'GPP')
GPP_OBS <-  OBS$GPP[1:153]
GPP_MODEL <- GPP_MODEL*24*3600
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
mydata <- data.frame(DOY,GPP,TYPE)

#
ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sanjiang Plain")+
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

mydata1 <- mydata[1:489,]

#
ggplot(mydata1,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
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


##ggline##
gppdata <- matrix(ncol = 3,nrow = 124)
colnames(gppdata) <- c('Modeled_GPP','Observed_GPP','Station')
gppdata[1:124,1] <- GPP_MODEL[150:273] 
gppdata[1:124,2] <- GPP_OBS[30:153]
gppdata[1:124,3] <- rep('(a) Sanjiang Plain',124)
gppdata <- data.frame(gppdata)
attach(gppdata)
gppdata[,1] <- as.double(gppdata[,1])
gppdata[,2] <- as.double(gppdata[,2])
gppdata[,1:2] <- round(gppdata[,1:2],2)
ggplot(data = gppdata,aes(x = Observed_GPP, y = Modeled_GPP),color = 'black')+
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

###
#### methane #####      
# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/wed_t.clm2.h0.2012-01-01-00000 (25).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2012-01-01-00000 (459).nc')
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2012-01-01-00000 (3).nc')
methane_model <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
mnogen <- ncvar_get(nc = try_data1,varid = 'CH4_SURF_NETFLUX')
methane_sanjiang <- read.csv(file = 'D:/Rtest/clm/三江甲烷排放.csv',header = TRUE)
methane_obs <- methane_sanjiang$X2012.CH4.mgm.2d.1.[1:153]
methane_model <- methane_model*1382400000
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
mydata <- data.frame(DOY,METHANE,TYPE)
ggplot(mydata,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  labs(title = "Sanjiang Plain")+
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


mydata1 <- mydata[1:518,]
ggplot(mydata1,aes(DOY,METHANE,shape = TYPE,colour = TYPE, linetype = TYPE))+
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


#### gen ####

try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2020-01-01-00000 (388).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2019-01-01-00000 (119).nc')

soil_pare <- rep(1,15)
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CACEBIOS') 
ACE_methanogens_S1 <- ncvar_get(try_data1,varid = 'CACEBIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[1,244:365],ACE_methanogens_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[2,244:365],ACE_methanogens_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[3,244:365],ACE_methanogens_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[4,244:365],ACE_methanogens_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[5,244:365],ACE_methanogens_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[6,244:365],ACE_methanogens_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[7,244:365],ACE_methanogens_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[8,244:365],ACE_methanogens_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
ACE_10 <- data.frame(s1_data$DOY,s1_data$ACE_methanogens+s2_data$ACE_methanogens+s3_data$ACE_methanogens+s4_data$ACE_methanogens)
colnames(ACE_10) <- c("DOY","ACE_methanogens")
ggplot(ACE_10,aes(x = DOY,y = ACE_methanogens) )+
  geom_line(size = 2)
ACE_20 <- data.frame(s1_data$DOY,s5_data$ACE_methanogens)
colnames(ACE_20) <- c("DOY","ACE_methanogens")
ACE_40 <- data.frame(s1_data$DOY,s6_data$ACE_methanogens)
colnames(ACE_40) <- c("DOY","ACE_methanogens")
ACE_60 <- data.frame(s1_data$DOY,s7_data$ACE_methanogens)
colnames(ACE_60) <- c("DOY","ACE_methanogens")
ACE_100 <- data.frame(s1_data$DOY,s8_data$ACE_methanogens)
colnames(ACE_100) <- c("DOY","ACE_methanogens")

## vertical
v_data <- cbind(s1_data[,1:2],s2_data$ACE_methanogens,s3_data$ACE_methanogens,s4_data$ACE_methanogens
                ,s5_data$ACE_methanogens,s6_data$ACE_methanogens,s7_data$ACE_methanogens,s8_data$ACE_methanogens)
colnames(v_data) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata <- melt(v_data,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'ACE')


mydata1 <- mydata
mydata1$Dep <- NA
mydata1$Dep[which(mydata1$Depth == 'S1')] <- 0.007
mydata1$Dep[which(mydata1$Depth == 'S2')]<- 0.028
mydata1$Dep[which(mydata1$Depth == 'S3')] <- 0.062
mydata1$Dep[which(mydata1$Depth == 'S4')] <- 0.119
mydata1$Dep[which(mydata1$Depth == 'S5')] <- 0.212
mydata1$Dep[which(mydata1$Depth == 'S6')] <- 0.366
mydata1$Dep[which(mydata1$Depth == 'S7')] <- 0.619
mydata1$Dep[which(mydata1$Depth == 'S8')] <- 1.000


mean_data2 <- aggregate(mydata1$ACE,by = list(mydata1$Dep),mean,na.rm = T)

########CO2
soil_pare <- rep(1,15)
CHO_S <- ncvar_get(try_data,varid = 'CCO2BIOS')
CHO_S1 <- ncvar_get(try_data1,varid = 'CCO2BIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[1,244:365],CHO_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[2,244:365],CHO_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[3,244:365],CHO_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[4,244:365],CHO_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[5,244:365],CHO_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[6,244:365],CHO_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[7,244:365],CHO_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[8,244:365],CHO_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
s10 <- data.frame(s1_data$DOY,s1_data$CHO+s2_data$CHO+s3_data$CHO+s4_data$CHO)
colnames(s10) <- c("DOY","CHO")
# ggplot(s10,aes(x = DOY,y = CHO) )+
#   geom_line(size = 2)
s20 <- data.frame(s1_data$DOY,s5_data$CHO)
colnames(s20) <- c("DOY","CHO")
s40 <- data.frame(s1_data$DOY,s6_data$CHO)
colnames(s40) <- c("DOY","CHO")
s60 <- data.frame(s1_data$DOY,s7_data$CHO)
colnames(s60) <- c("DOY","CHO")
s100 <- data.frame(s1_data$DOY,s8_data$CHO)
colnames(s100) <- c("DOY","CHO")

####vertical####
v_data1 <- cbind(s1_data[,1:2],s2_data$CHO,s3_data$CHO,s4_data$CHO
                 ,s5_data$CHO,s6_data$CHO,s7_data$CHO,s8_data$CHO)
colnames(v_data1) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata2 <- melt(v_data1,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'CHO')


mydata3 <- mydata2
mydata3$Dep <- NA
mydata3$Dep[which(mydata3$Depth == 'S1')] <- 0.007
mydata3$Dep[which(mydata3$Depth == 'S2')]<- 0.028
mydata3$Dep[which(mydata3$Depth == 'S3')] <- 0.062
mydata3$Dep[which(mydata3$Depth == 'S4')] <- 0.119
mydata3$Dep[which(mydata3$Depth == 'S5')] <- 0.212
mydata3$Dep[which(mydata3$Depth == 'S6')] <- 0.366
mydata3$Dep[which(mydata3$Depth == 'S7')] <- 0.619
mydata3$Dep[which(mydata3$Depth == 'S8')] <- 1.000


mean_data3 <- aggregate(mydata3$CHO,by = list(mydata3$Dep),mean,na.rm = T)

#M51
MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
ggplot(data = MODEL_AC1,aes(x = DEPTH,y  = VALUE,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line()+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D"))+
  scale_shape_manual(values=c(21,22))+
  theme_classic()

MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)

min.max.norm <- function(x){
  ((x-min(x))/(max(x)-min(x)))
} 
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)

#### obs ####
mic_data <- read.csv("D:/AAAA-资料E盘/CLM/DATA/microbe/ace.csv",header = T)
mic_data1 <- mic_data[1:40,12:15]
mic_data1$DEPTH <- rep(seq(10,100,10),4)
mic_data1$SEASON <- factor(mic_data1$SEASON,levels = c('spring','summer','autumn','winter'))
mean_data <- aggregate(mic_data1$M00357M,by = list(mic_data1$DEPTH),mean,na.rm = T)
mean_data1 <- aggregate(mic_data1$M00567M,by = list(mic_data1$DEPTH),mean,na.rm = T)
MODEL_AC2 <- cbind(mean_data,mean_data1$x)
colnames(MODEL_AC2) <- c('DEPTH','M00357M','M00567M')
MODEL_AC3 <- melt(MODEL_AC2,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC3$bz <- scale(MODEL_AC3$VALUE)
MODEL_AC3$bz1 <- min.max.norm(MODEL_AC3$VALUE)
####jiehe ####

model_com <- rbind(MODEL_AC1,MODEL_AC3)
model_com$DEPTH[17:36] <- model_com$DEPTH[17:36]/100
ggplot(data = model_com,aes(x = DEPTH,y  = bz1,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633','#33FF99'))+
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()

model_coma <- model_com
model_coma$MICROBE_PRO <- c(rep('AM',8),rep('MM',8),rep('AM_OBS',10),rep('MM_OBS',10))

ggplot(data = model_coma,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_color_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.1),
    legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=13,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=13,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 16,face = 'bold'),
    axis.title.x = element_text(size = 16,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=16,face="bold"))


#####nogen#####
try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2020-01-01-00000 (3).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2019-01-01-00000 (3).nc')

soil_pare <- rep(1,15)
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CACEBIOS') 
ACE_methanogens_S1 <- ncvar_get(try_data1,varid = 'CACEBIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[1,244:365],ACE_methanogens_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[2,244:365],ACE_methanogens_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[3,244:365],ACE_methanogens_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[4,244:365],ACE_methanogens_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[5,244:365],ACE_methanogens_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[6,244:365],ACE_methanogens_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[7,244:365],ACE_methanogens_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[8,244:365],ACE_methanogens_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
ACE_10 <- data.frame(s1_data$DOY,s1_data$ACE_methanogens+s2_data$ACE_methanogens+s3_data$ACE_methanogens+s4_data$ACE_methanogens)
colnames(ACE_10) <- c("DOY","ACE_methanogens")
ggplot(ACE_10,aes(x = DOY,y = ACE_methanogens) )+
  geom_line(size = 2)
ACE_20 <- data.frame(s1_data$DOY,s5_data$ACE_methanogens)
colnames(ACE_20) <- c("DOY","ACE_methanogens")
ACE_40 <- data.frame(s1_data$DOY,s6_data$ACE_methanogens)
colnames(ACE_40) <- c("DOY","ACE_methanogens")
ACE_60 <- data.frame(s1_data$DOY,s7_data$ACE_methanogens)
colnames(ACE_60) <- c("DOY","ACE_methanogens")
ACE_100 <- data.frame(s1_data$DOY,s8_data$ACE_methanogens)
colnames(ACE_100) <- c("DOY","ACE_methanogens")

## vertical
v_data <- cbind(s1_data[,1:2],s2_data$ACE_methanogens,s3_data$ACE_methanogens,s4_data$ACE_methanogens
                ,s5_data$ACE_methanogens,s6_data$ACE_methanogens,s7_data$ACE_methanogens,s8_data$ACE_methanogens)
colnames(v_data) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata <- melt(v_data,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'ACE')


mydata1 <- mydata
mydata1$Dep <- NA
mydata1$Dep[which(mydata1$Depth == 'S1')] <- 0.007
mydata1$Dep[which(mydata1$Depth == 'S2')]<- 0.028
mydata1$Dep[which(mydata1$Depth == 'S3')] <- 0.062
mydata1$Dep[which(mydata1$Depth == 'S4')] <- 0.119
mydata1$Dep[which(mydata1$Depth == 'S5')] <- 0.212
mydata1$Dep[which(mydata1$Depth == 'S6')] <- 0.366
mydata1$Dep[which(mydata1$Depth == 'S7')] <- 0.619
mydata1$Dep[which(mydata1$Depth == 'S8')] <- 1.000


mean_data2 <- aggregate(mydata1$ACE,by = list(mydata1$Dep),mean,na.rm = T)

########CO2
soil_pare <- rep(1,15)
CHO_S <- ncvar_get(try_data,varid = 'CCO2BIOS')
CHO_S1 <- ncvar_get(try_data1,varid = 'CCO2BIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[1,244:365],CHO_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[2,244:365],CHO_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[3,244:365],CHO_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[4,244:365],CHO_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[5,244:365],CHO_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[6,244:365],CHO_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[7,244:365],CHO_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[8,244:365],CHO_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
s10 <- data.frame(s1_data$DOY,s1_data$CHO+s2_data$CHO+s3_data$CHO+s4_data$CHO)
colnames(s10) <- c("DOY","CHO")
# ggplot(s10,aes(x = DOY,y = CHO) )+
#   geom_line(size = 2)
s20 <- data.frame(s1_data$DOY,s5_data$CHO)
colnames(s20) <- c("DOY","CHO")
s40 <- data.frame(s1_data$DOY,s6_data$CHO)
colnames(s40) <- c("DOY","CHO")
s60 <- data.frame(s1_data$DOY,s7_data$CHO)
colnames(s60) <- c("DOY","CHO")
s100 <- data.frame(s1_data$DOY,s8_data$CHO)
colnames(s100) <- c("DOY","CHO")

####vertical####
v_data1 <- cbind(s1_data[,1:2],s2_data$CHO,s3_data$CHO,s4_data$CHO
                 ,s5_data$CHO,s6_data$CHO,s7_data$CHO,s8_data$CHO)
colnames(v_data1) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata2 <- melt(v_data1,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'CHO')


mydata3 <- mydata2
mydata3$Dep <- NA
mydata3$Dep[which(mydata3$Depth == 'S1')] <- 0.007
mydata3$Dep[which(mydata3$Depth == 'S2')]<- 0.028
mydata3$Dep[which(mydata3$Depth == 'S3')] <- 0.062
mydata3$Dep[which(mydata3$Depth == 'S4')] <- 0.119
mydata3$Dep[which(mydata3$Depth == 'S5')] <- 0.212
mydata3$Dep[which(mydata3$Depth == 'S6')] <- 0.366
mydata3$Dep[which(mydata3$Depth == 'S7')] <- 0.619
mydata3$Dep[which(mydata3$Depth == 'S8')] <- 1.000


mean_data3 <- aggregate(mydata3$CHO,by = list(mydata3$Dep),mean,na.rm = T)

#M51
MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
ggplot(data = MODEL_AC1,aes(x = DEPTH,y  = VALUE,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line()+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D"))+
  scale_shape_manual(values=c(21,22))+
  theme_classic()

MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)

min.max.norm <- function(x){
  ((x-min(x))/(max(x)-min(x)))
} 
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)

#### obs ####
mic_data <- read.csv("D:/AAAA-资料E盘/CLM/DATA/microbe/ace.csv",header = T)
mic_data1 <- mic_data[1:40,12:15]
mic_data1$DEPTH <- rep(seq(10,100,10),4)
mic_data1$SEASON <- factor(mic_data1$SEASON,levels = c('spring','summer','autumn','winter'))
mean_data <- aggregate(mic_data1$M00357M,by = list(mic_data1$DEPTH),mean,na.rm = T)
mean_data1 <- aggregate(mic_data1$M00567M,by = list(mic_data1$DEPTH),mean,na.rm = T)
MODEL_AC2 <- cbind(mean_data,mean_data1$x)
colnames(MODEL_AC2) <- c('DEPTH','M00357M','M00567M')
MODEL_AC3 <- melt(MODEL_AC2,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC3$bz <- scale(MODEL_AC3$VALUE)
MODEL_AC3$bz1 <- min.max.norm(MODEL_AC3$VALUE)
####jiehe ####

model_com <- rbind(MODEL_AC1,MODEL_AC3)
model_com$DEPTH[17:36] <- model_com$DEPTH[17:36]/100
ggplot(data = model_com,aes(x = DEPTH,y  = bz1,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633','#33FF99'))+
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()

ggplot(data = model_com,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size = 4,colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_color_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.1),
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


model_comb <- model_com
model_comb$MICROBE_PRO <- c(rep('AM',8),rep('MM',8),rep('AM_OBS',10),rep('MM_OBS',10))

ggplot(data = model_comb,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  scale_color_manual(values=c("#FF9641","#FF5B4E","#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.1),
    legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=13,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=13,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 16,face = 'bold'),
    axis.title.x = element_text(size = 16,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=16,face="bold"))


#######AOM-OM########

try_data1 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2020-01-01-00000 (388).nc')
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2019-01-01-00000 (119).nc')

soil_pare <- rep(1,15)
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CAERCH4BIOS') 
ACE_methanogens_S1 <- ncvar_get(try_data1,varid = 'CAERCH4BIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[1,244:365],ACE_methanogens_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[2,244:365],ACE_methanogens_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[3,244:365],ACE_methanogens_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[4,244:365],ACE_methanogens_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[5,244:365],ACE_methanogens_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[6,244:365],ACE_methanogens_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[7,244:365],ACE_methanogens_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[8,244:365],ACE_methanogens_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,244:365],ACE_methanogens_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
ACE_10 <- data.frame(s1_data$DOY,s1_data$ACE_methanogens+s2_data$ACE_methanogens+s3_data$ACE_methanogens+s4_data$ACE_methanogens)
colnames(ACE_10) <- c("DOY","ACE_methanogens")
ggplot(ACE_10,aes(x = DOY,y = ACE_methanogens) )+
  geom_line(size = 2)
ACE_20 <- data.frame(s1_data$DOY,s5_data$ACE_methanogens)
colnames(ACE_20) <- c("DOY","ACE_methanogens")
ACE_40 <- data.frame(s1_data$DOY,s6_data$ACE_methanogens)
colnames(ACE_40) <- c("DOY","ACE_methanogens")
ACE_60 <- data.frame(s1_data$DOY,s7_data$ACE_methanogens)
colnames(ACE_60) <- c("DOY","ACE_methanogens")
ACE_100 <- data.frame(s1_data$DOY,s8_data$ACE_methanogens)
colnames(ACE_100) <- c("DOY","ACE_methanogens")

## vertical
v_data <- cbind(s1_data[,1:2],s2_data$ACE_methanogens,s3_data$ACE_methanogens,s4_data$ACE_methanogens
                ,s5_data$ACE_methanogens,s6_data$ACE_methanogens,s7_data$ACE_methanogens,s8_data$ACE_methanogens)
colnames(v_data) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata <- melt(v_data,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'ACE')


mydata1 <- mydata
mydata1$Dep <- NA
mydata1$Dep[which(mydata1$Depth == 'S1')] <- 0.007
mydata1$Dep[which(mydata1$Depth == 'S2')]<- 0.028
mydata1$Dep[which(mydata1$Depth == 'S3')] <- 0.062
mydata1$Dep[which(mydata1$Depth == 'S4')] <- 0.119
mydata1$Dep[which(mydata1$Depth == 'S5')] <- 0.212
mydata1$Dep[which(mydata1$Depth == 'S6')] <- 0.366
mydata1$Dep[which(mydata1$Depth == 'S7')] <- 0.619
mydata1$Dep[which(mydata1$Depth == 'S8')] <- 1.000


mean_data2 <- aggregate(mydata1$ACE,by = list(mydata1$Dep),mean,na.rm = T)

########CO2
soil_pare <- rep(1,15)
CHO_S <- ncvar_get(try_data,varid = 'CANAERCH4BIOS')
CHO_S1 <- ncvar_get(try_data1,varid = 'CANAERCH4BIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[1,244:365],CHO_S1[1,1:243]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[2,244:365],CHO_S1[2,1:243]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[3,244:365],CHO_S1[3,1:243]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[4,244:365],CHO_S1[4,1:243]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[5,244:365],CHO_S1[5,1:243]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[6,244:365],CHO_S1[6,1:243]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[7,244:365],CHO_S1[7,1:243]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[8,244:365],CHO_S1[8,1:243]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, CHO = c(CHO_S[i,244:365],CHO_S1[i,1:243]), Soil_depth = rep(s_depth[i],365)),
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

#0-10
s10 <- data.frame(s1_data$DOY,s1_data$CHO+s2_data$CHO+s3_data$CHO+s4_data$CHO)
colnames(s10) <- c("DOY","CHO")
# ggplot(s10,aes(x = DOY,y = CHO) )+
#   geom_line(size = 2)
s20 <- data.frame(s1_data$DOY,s5_data$CHO)
colnames(s20) <- c("DOY","CHO")
s40 <- data.frame(s1_data$DOY,s6_data$CHO)
colnames(s40) <- c("DOY","CHO")
s60 <- data.frame(s1_data$DOY,s7_data$CHO)
colnames(s60) <- c("DOY","CHO")
s100 <- data.frame(s1_data$DOY,s8_data$CHO)
colnames(s100) <- c("DOY","CHO")

####vertical####
v_data1 <- cbind(s1_data[,1:2],s2_data$CHO,s3_data$CHO,s4_data$CHO
                 ,s5_data$CHO,s6_data$CHO,s7_data$CHO,s8_data$CHO)
colnames(v_data1) <- c('DOY','S1','S2','S3','S4','S5','S6','S7','S8')

mydata2 <- melt(v_data1,id.vars = c('DOY'),variable.name = 'Depth',value.name = 'CHO')


mydata3 <- mydata2
mydata3$Dep <- NA
mydata3$Dep[which(mydata3$Depth == 'S1')] <- 0.007
mydata3$Dep[which(mydata3$Depth == 'S2')]<- 0.028
mydata3$Dep[which(mydata3$Depth == 'S3')] <- 0.062
mydata3$Dep[which(mydata3$Depth == 'S4')] <- 0.119
mydata3$Dep[which(mydata3$Depth == 'S5')] <- 0.212
mydata3$Dep[which(mydata3$Depth == 'S6')] <- 0.366
mydata3$Dep[which(mydata3$Depth == 'S7')] <- 0.619
mydata3$Dep[which(mydata3$Depth == 'S8')] <- 1.000


mean_data3 <- aggregate(mydata3$CHO,by = list(mydata3$Dep),mean,na.rm = T)

#M51
MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
ggplot(data = MODEL_AC1,aes(x = DEPTH,y  = VALUE,fill = MICROBE_PRO,shape = MICROBE_PRO))+
  geom_line()+
  geom_point(size  =4, colour = 'black')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#FF9641","#38C25D"))+
  scale_shape_manual(values=c(21,22))+
  theme_classic()

MODEL_om2 <- MODEL_AC1
MODEL_om2$MICROBE_PRO <- rep(c('OM','AOM'),each = 8)
MODEL_om2$VALUE[1:8] <- min.max.norm(MODEL_om2$VALUE[1:8])
MODEL_om2$VALUE[9:16] <- min.max.norm(MODEL_om2$VALUE[9:16])
ggplot(data = MODEL_om2,aes(x = DEPTH,y  = VALUE,fill = MICROBE_PRO,color = MICROBE_PRO))+
  geom_line(size =2 )+
  geom_point(size  =4)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(name = '',values=c("#FF9641","#38C25D"))+
  scale_color_manual(name = '',values=c("#FF9641","#38C25D"))+
  # scale_shape_manual(values=c(21,22))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  theme_base()+
  theme(
  legend.position = c(0.9,0.9),
  legend.title=element_text(colour='black'),
  # legend.margin=margin(grid::unit(0,"cm")),
  legend.text=element_text(colour='black',size=16,face="bold"),
  legend.key.height=grid::unit(0.5,"cm"),
  legend.key.width=grid::unit(0.5,"cm"),
  axis.text.x=element_text(size=13,colour='black',vjust = 1,hjust = 1),
  # axis.text.x.top = element_text(size = 22,face = 'bold'),
  axis.text.y=element_text(size=13,vjust=0.2,colour='black'),
  axis.title.y = element_text(size = 16,face = 'bold'),
  axis.title.x = element_text(size = 16,face = 'bold'),
  # axis.title.x.top = element_text(size = 22,face = 'bold'),
  axis.ticks=element_line(size=0.5),
  plot.background=element_blank(),
  # panel.border=element_blank(), # tianjia bianjie
  plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
  plot.title=element_text(colour='black',hjust=0.5,size=16,face="bold"))


#### variable ####
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2019-01-01-00000 (119).nc')
# nogen_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2019-01-01-00000 (3).nc')

TSA <- ncvar_get(nc = try_data,varid = 'TSA')
RAIN <- ncvar_get(nc = try_data,varid = 'RAIN')
ZWT <- ncvar_get(nc = try_data,varid = 'ZWT')
Soil_temp <- ncvar_get(nc = try_data,varid = 'TSOI_10CM')
Soil_WATER <- ncvar_get(nc = try_data,varid = 'H2OSOI')
Soil_WATER_10 <- apply(as.matrix(Soil_WATER[1:3,]),2,FUN = mean)
DOC <- ncvar_get(nc = try_data,varid = 'CDOCS')
DOC_10 <- apply(as.matrix(DOC[1:3,]),2,FUN = mean)
#CACES_PROD
ACEPRO <- ncvar_get(nc = try_data,varid = 'CACES_PROD')
ACEPRO_10 <- apply(as.matrix(ACEPRO[1:3,]),2,FUN = sum)
#CCON_CO2S_UNSAT 
CO2 <- ncvar_get(nc = try_data,varid = 'CCON_CO2S_UNSAT')
S_CO2 <- apply(as.matrix(CO2[1:3,]),2,FUN = sum)
H2 <- ncvar_get(nc = try_data,varid = 'CCON_H2S_UNSAT')
S_H2 <- apply(as.matrix(H2[1:3,]),2,FUN = sum)

CO2PROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_10 <- apply(as.matrix(CO2PROCH4[1:3,]),2,FUN = sum)
ACEPROCH4_10 <- apply(as.matrix(ACEPROCH4[1:3,]),2,FUN = sum)
CH4OXIDAER_10 <- apply(as.matrix(CH4OXIDAER[1:3,]),2,FUN = sum)
CH4OXIDANAER_10<- apply(as.matrix(CH4OXIDANAER[1:3,]),2,FUN = sum)


sj_variable <- cbind(TSA,RAIN,Soil_temp,Soil_WATER_10,DOC_10,ACEPRO_10,S_CO2,S_H2,CO2PROCH4_10,
                     ACEPROCH4_10,CH4OXIDAER_10,CH4OXIDANAER_10,ZWT)
colnames(sj_variable) <- c('Air_temp','Rain','soiltemp','SWC','DOC','ACE','CO2','H2','CO2PROCH4',
                           'ACEPROCH4','CH4OXIDAER','ANAER','ZWT')
sj_variable <- as.data.frame(sj_variable)
SJGEN <- sj_variable[150:270,]


ggplot(SJGEN, aes(x= CO2PROCH4, y=Air_temp)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
ggplot(SJGEN, aes(x= ACEPROCH4, y=Air_temp)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")


ggplot(SJGEN, aes(x= CH4OXIDAER, y=SWC)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")





###nogen###
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2019-01-01-00000 (3).nc')


TSA <- ncvar_get(nc = try_data,varid = 'TSA')
RAIN <- ncvar_get(nc = try_data,varid = 'RAIN')
Soil_temp <- ncvar_get(nc = try_data,varid = 'TSOI_10CM')
ZWT <- ncvar_get(nc = try_data,varid = 'ZWT')
Soil_WATER <- ncvar_get(nc = try_data,varid = 'H2OSOI')
Soil_WATER_10 <- apply(as.matrix(Soil_WATER[1:3,]),2,FUN = mean)
DOC <- ncvar_get(nc = try_data,varid = 'CDOCS')
DOC_10 <- apply(as.matrix(DOC[1:3,]),2,FUN = mean)
#CACES_PROD
ACEPRO <- ncvar_get(nc = try_data,varid = 'CACES_PROD')
ACEPRO_10 <- apply(as.matrix(ACEPRO[1:3,]),2,FUN = sum)
#CCON_CO2S_UNSAT 
CO2 <- ncvar_get(nc = try_data,varid = 'CCON_CO2S_UNSAT')
S_CO2 <- apply(as.matrix(CO2[1:3,]),2,FUN = sum)
H2 <- ncvar_get(nc = try_data,varid = 'CCON_H2S_UNSAT')
S_H2 <- apply(as.matrix(H2[1:3,]),2,FUN = sum)

CO2PROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_10 <- apply(as.matrix(CO2PROCH4[1:3,]),2,FUN = sum)
ACEPROCH4_10 <- apply(as.matrix(ACEPROCH4[1:3,]),2,FUN = sum)
CH4OXIDAER_10 <- apply(as.matrix(CH4OXIDAER[1:3,]),2,FUN = sum)
CH4OXIDANAER_10<- apply(as.matrix(CH4OXIDANAER[1:3,]),2,FUN = sum)


sj_variable1 <- cbind(TSA,RAIN,Soil_temp,Soil_WATER_10,DOC_10,ACEPRO_10,S_CO2,S_H2,CO2PROCH4_10,
                     ACEPROCH4_10,CH4OXIDAER_10,CH4OXIDANAER_10,ZWT)
colnames(sj_variable1) <- c('Air_temp','Rain','soiltemp','SWC','DOC','ACE','CO2','H2','CO2PROCH4',
                           'ACEPROCH4','CH4OXIDAER','ANAER','ZWT')
sj_variable1 <- as.data.frame(sj_variable1)
SJNOGEN <- sj_variable1[150:270,]

###
ggplot(SJNOGEN, aes(x= CO2PROCH4, y=Air_temp)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
ggplot(SJNOGEN, aes(x= ACEPROCH4, y=Air_temp)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
ggplot(SJNOGEN, aes(x= SWC, y=CH4OXIDAER)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
######combind########

SJ_COMB <- rbind(SJGEN,SJNOGEN)
SJ_COMB$Group <- rep(c('GEN','NOGEN'),each = 121)

SJ_COMB$Air_temp <- min.max.norm(SJ_COMB$Air_temp)
SJ_COMB$ZWT <- min.max.norm(SJ_COMB$ZWT)
SJ_COMB$CO2PROCH4 <- min.max.norm(SJ_COMB$CO2PROCH4)
SJ_COMB$ACEPROCH4 <- min.max.norm(SJ_COMB$ACEPROCH4)

ggplot(SJ_COMB, aes(x= CO2PROCH4, y=Air_temp,color = Group,shape = Group)) + 
  geom_point(size =4 )+
  geom_smooth(method=lm, aes(fill = Group))+
  theme_base()+
  theme(
    legend.position = c(0.85,0.1),
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

ggplot(SJ_COMB, aes(x= ACEPROCH4, y=Air_temp,color = Group,shape = Group)) + 
  geom_point(size =4)+
  geom_smooth(method=lm, aes(fill = Group))+
  theme_base()+
  theme(
    legend.position = c(0.85,0.1),
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


SJ_COMB1 <- rbind(sj_variable[120:150,],sj_variable1[120:150,])
# SJ_COMB1 <- SJ_COMB1[120:150,]
SJ_COMB1$Group <- rep(c('GEN','NOGEN'),each = 31)
SJ_COMB1$Air_temp <- min.max.norm(SJ_COMB1$Air_temp)
SJ_COMB1$ZWT <- min.max.norm(SJ_COMB1$ZWT)
SJ_COMB1$CO2PROCH4 <- min.max.norm(SJ_COMB1$CO2PROCH4)
SJ_COMB1$ACEPROCH4 <- min.max.norm(SJ_COMB1$ACEPROCH4)
SJ_COMB1$CH4OXIDAER <- min.max.norm(SJ_COMB1$CH4OXIDAER)
SJ_COMB1$ANAER <- min.max.norm(SJ_COMB1$ANAER)

ggplot(SJ_COMB1, aes(x= CH4OXIDAER, y=ZWT,color = Group,shape = Group)) + 
  geom_point(size =4)+
  geom_smooth(method=lm, aes(fill = Group))+
  theme_base()+
  theme(
    legend.position = c(0.85,0.1),
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

ggplot(SJ_COMB1, aes(x= ANAER, y=ZWT,color = Group,shape = Group)) + 
  geom_point(size =4)+
  geom_smooth(method=lm, aes(fill = Group))+
  theme_base()+
  theme(
    legend.position = c(0.85,0.1),
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

ggplot(SJ_COMB, aes(x= CO2PROCH4, y=ZWT,color = Group,shape = Group)) + 
  geom_point(size =4)+
  geom_smooth(method=lm, aes(fill = Group))+
  theme_base()+
  theme(
    legend.position = c(0.85,0.1),
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

#####FACETWRAP####
A <- SJ_COMB[,c(1,9,10,14)]
B <- melt(A,id.var = c('Air_temp','Group'))

B$Type <- rep(c('CO2->CH4','ACE->CH4'),each = 242)

ggplot(B, aes(x=Air_temp, y= value,color = Group,shape = Group)) + 
  geom_point(size =4 )+
  geom_smooth(method=lm, aes(fill = Group))+
  facet_wrap(~Type)+
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22,colour = 'black'),
    legend.position = c(0.1,0.9),
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

A1 <- SJ_COMB1[,c(11,12,13,14)]
B1 <- melt(A1,id.var = c('ZWT','Group'))

B1$Type <- rep(c('AER','ANAER'),each = 62)

ggplot(B1, aes(x= ZWT, y=value,color = Group,shape = Group)) + 
  geom_point(size =4)+
  geom_smooth(method=lm, aes(fill = Group))+
  facet_wrap(~Type)+
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = c(0.1,0.9),
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
