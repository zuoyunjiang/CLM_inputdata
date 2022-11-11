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


##obs


D_data <- read.csv('D:/AAAA-资料E盘/CLM/microbe/sallie fen/MG-SALLIE FEN.csv',header = T)
sallie <- D_data[1:3,14:16]
colnames(sallie) <- c("Depth",'ACE','CHO')

sallie1 <- melt(sallie,id.vars = 'Depth',variable.name = 'type',value.name = 'microbial biomass')
sallie1$bz1[1:3] <- min.max.norm(sallie1$`microbial biomass`[1:3])
sallie1$bz1[4:6] <- min.max.norm(sallie1$`microbial biomass`[4:6])
sallie2 <- sallie1
sallie2$bz2 <- min.max.norm(sallie1$`microbial biomass`)
ggplot(data = sallie2,aes(x = Depth,y  = bz2,color = type))+
  geom_point(size  =2)+
  geom_line(size =1)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  # scale_fill_manual(values=c("#FF9641","#38C25D"))+
  # scale_shape_manual(values=c(15,16,17,18))+
  labs(y = 'Microbial biomass', x = 'Depth(cm)',fill = 'Group')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    legend.title=element_text(colour='black',size=18,face="bold"),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=18,face="bold"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')
  )

#### microbial-model ####
# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/t_sal.clm2.h0.2014-01-01-00000 (7).nc')
# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/tsal_t.clm2.h0.2014-01-01-00000 (5).nc')
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CACEBIOS')
soil_pare <- rep(1,15)
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
# structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S [i,1:365]), Soil_depth = rep(s_depth[i],365)),
#           row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[1,1:365]), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[2,1:365]), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[3,1:365]), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[4,1:365]), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[5,1:365]), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[6,1:365]), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[7,1:365]), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[8,1:365]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S[i,1:365]), Soil_depth = rep(s_depth[i],365)),
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

ACE_20 <- data.frame(s1_data$DOY,s5_data$ACE_methanogens)
colnames(ACE_20) <- c("DOY","ACE_methanogens")
ACE_40 <- data.frame(s1_data$DOY,s6_data$ACE_methanogens)
colnames(ACE_40) <- c("DOY","ACE_methanogens")
###  vertical-ACE ###
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
###CO2 ###
CHO_S <- ncvar_get(try_data,varid = 'CCO2BIOS')
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
# structure(list(DOY = 1:365, CHO = c(CHO_S[i,1:365] ), Soil_depth = rep(s_depth[i],365)),
#           row.names = c(NA,365L),class= "data.frame")
s1_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[1,1:365] ), Soil_depth = rep(s_depth[1],365))
s2_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[2,1:365] ), Soil_depth = rep(s_depth[2],365))
s3_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[3,1:365] ), Soil_depth = rep(s_depth[3],365))
s4_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[4,1:365] ), Soil_depth = rep(s_depth[4],365))
s5_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[5,1:365] ), Soil_depth = rep(s_depth[5],365))
s6_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[6,1:365] ), Soil_depth = rep(s_depth[6],365))
s7_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[7,1:365] ), Soil_depth = rep(s_depth[7],365))
s8_data <- data.frame(DOY = 1:365, CHO = c(CHO_S[8,1:365]), Soil_depth = rep(s_depth[8],365))

i <-  1
repeat{
  structure(list(DOY = 1:365, CHO = c(CHO_S[i,1:365]), Soil_depth = rep(s_depth[i],365)),
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

s20 <- data.frame(s1_data$DOY,s5_data$CHO)
colnames(s20) <- c("DOY","CHO")
s40 <- data.frame(s1_data$DOY,s6_data$CHO)
colnames(s40) <- c("DOY","CHO")
s60 <- data.frame(s1_data$DOY,s7_data$CHO)
colnames(s60) <- c("DOY","CHO")
s100 <- data.frame(s1_data$DOY,s8_data$CHO)
colnames(s100) <- c("DOY","CHO")

##vertical
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
MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)
MODEL_AC2 <- MODEL_AC1
MODEL_AC2 <- MODEL_AC2[c(1:7,9:15),1:3]
MODEL_AC2$bz1 <- min.max.norm(MODEL_AC2$VALUE)
MODEL_AC2$bz1[1:7] <- min.max.norm(MODEL_AC2$VALUE[1:7])
MODEL_AC2$bz1[8:13] <- min.max.norm(MODEL_AC2$VALUE[8:13])
# MODEL_AC2$bz <- scale(MODEL_AC2$VALUE)

MODEL_AC3 <- MODEL_AC2
MODEL_AC3$bz2[1:7] <- log2(MODEL_AC3$VALUE[1:7])
MODEL_AC3$bz2[8:13] <- log2(MODEL_AC3$VALUE[8:13])
MODEL_AC4 <- MODEL_AC2
MODEL_AC4$bz3[1:7] <- scale(MODEL_AC4$VALUE[1:7])
MODEL_AC4$bz3[8:13] <- scale(MODEL_AC4$VALUE[8:13])

ggplot(data = MODEL_AC2,aes(x = DEPTH,y  = bz1,color = MICROBE_PRO))+
  geom_point(size  =4)+
  geom_line(size =1)+
  
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  # scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633'))+
  scale_shape_manual(values=c(15,16))+
  labs(y = 'Microbial biomass', x = 'Depth(m)',color = 'Group')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    legend.title=element_text(colour='black',size=18,face="bold"),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=18,face="bold"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')
  )
#####end#############
MODEL_AC2$bz2 <- min.max.norm(MODEL_AC2$VALUE)
ggplot(data = MODEL_AC2,aes(x = DEPTH,y  = bz2,color = MICROBE_PRO))+
  geom_point(size  =4)+
  geom_line(size =1)+
  
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  # scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633'))+
  scale_shape_manual(values=c(15,16))+
  labs(y = 'Microbial biomass', x = 'Depth(m)',color = 'Group')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    legend.title=element_text(colour='black',size=18,face="bold"),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=18,face="bold"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')
  )
