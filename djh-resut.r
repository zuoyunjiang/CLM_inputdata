#####DA jiu long####
#####2022-7-19########
#####zuoyunjiang zuoyunjiang@iga.ac.cn#########
library(ggplot2)
library(ncdf4)
library(ggthemes)
library(dplyr)
library(reshape2)
####function####
min.max.norm <- function(x){
  ((x-min(x))/(max(x)-min(x)))
} 
##############

D_data <- read.csv('D:/AAAA-资料E盘/CLM/DATA/microbe/djl.csv',header = T)
djl <- D_data[,9:12]
colnames(djl) <- c('CHO','ACE',"Depth",'Month')
d1 <- aggregate(djl$CHO,by = list(djl$Depth),mean,na.rm = T)
d2 <- aggregate(djl$ACE,by = list(djl$Depth),mean,na.rm = T)
djl1 <- rbind(d1,d2)
djl1$bz <- min.max.norm(djl1$x)
djl1$type <- rep(c('cho','ace'),each = 4)
djl1$bz1 <- min.max.norm(djl1$x)
djl1$bz1[1:4] <- min.max.norm(djl1$x[1:4])
djl1$bz1[5:8] <- min.max.norm(djl1$x[5:8])
ggplot(data = djl1,aes(x = Group.1,y  = bz1,color = type))+
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
    # legend.position = c(0.8,0.2),
    legend.title=element_text(colour='black',size=18,face="bold"),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=18,face="bold"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')
  )






###### GPP ######

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (82).nc')
OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/microbe/djh/19GPP.csv',header = TRUE)
GPP_MODEL <- ncvar_get(nc = try_data,varid = 'GPP')
GPP_OBS <-  OBS$mean/8
GPP_MODEL <- GPP_MODEL*86.4
# gppr2 <- summary(lm(GPP_OBS[30:153]~GPP_MODEL[150:273]))
##GRAPH
# jpeg(filename = 'GPP_10-30.png',width = 1200,height = 900,units = 'px',pointsize = 24)
DOY <- OBS$DOY
opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(x=1:365,GPP_MODEL,main = 'DAJIUHU',ylab=expression(GPP/KgC*m^{-2}*d^{-1}),xlab ='DOY',type="l",pch=18,col="black",yaxt="n",xaxt = 'n',lty=1,lwd=2)
lines(x=DOY,GPP_OBS,type="p",pch=19,col="red",lty=1,lwd=2)
# text(x =270 ,y = 6.5,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)
# dev.off()

DOY <- matrix(nrow =411 ,ncol =1)
DOY[1:46,1] <- OBS$DOY
DOY[47:411,1] <- 1:365
OBS <- GPP_OBS
MODEL <- GPP_MODEL
GPP <- matrix(nrow =411 ,ncol =1)
GPP[1:46,1] <- OBS
GPP[47:411] <- MODEL
TYPE <- c(rep('OBS',46),rep('MODEL',365))
mydata <- data.frame(DOY,GPP,TYPE)

#
OBS <- GPP_OBS
MODEL <- GPP_MODEL
postResample(OBS,MODEL)
#
ggplot(mydata,aes(DOY,GPP,shape = TYPE,colour = TYPE, linetype = TYPE))+
  geom_point(size = 4)+
  geom_line(size = 2)+
  labs(title = "Dajiuhu peatland")+
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

#####methane#########

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (221).nc')
OBS <- read.csv(file = 'D:/AAAA-资料E盘/CLM/microbe/神农架/dajiulong.csv',header = TRUE)
CH4_MODEL <- ncvar_get(nc = try_data,varid = 'CH4_SURF_NETFLUX')
CH4_OBS <-  OBS$nmol.m2.s[2:12]
CH4_MODEL <- CH4_MODEL*10^9
MONTH <- c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
modeldata <- data.frame(CH4_MODEL,MONTH)
CH4_MODEL1 <- aggregate(modeldata$CH4_MODEL, by = list(MONTH),FUN = mean,na.rm = T)
##GRAPH

opar <- par(mar=c(4.5,5,5,2)+0.1)
plot(x=1:12,CH4_MODEL1[1:12,2],main = 'DAJIUHU',ylab=expression(methane/KgC*m^{-2}*d^{-1}),xlab ='MONTH',type="l",pch=18,col="black",yaxt="n",xaxt = 'n',lty=1,lwd=2)
lines(x=1:11,CH4_OBS,type="p",pch=19,col="red",lty=1,lwd=2)
# text(x =270 ,y = 6.5,paste('R2=',round(gppr2$adj.r.squared,2) ))
axis(side = 1,col.axis="black",cex.axis=1,tck=-0.01)
axis(side = 2,col.axis="black",cex.axis=1,tck=-0.01,mai=1)
legend('topright',legend = c('OBS','MODEL'),col = c('red','black'),lty = 1,lwd=2,pch = c(19,18))
par(opar)

#### microbial-model ####
# try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (115).nc')
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CACEBIOS')
soil_pare <- rep(1,15)
i =1
# soil_pare <- c(0.0175,0.0276,0.0455,0.0750,0.1236,0.2038,0.3360,0.5539,0.9133,
#                1.5058,2.4826,4.0931,6.7484,11.1262,13.8512)
s_depth <- c(0.007,0.028,0.062,0.119,0.212,0.366,0.619,1.000)
s_depth <- as.character(s_depth)
structure(list(DOY = 1:365, ACE_methanogens = c(ACE_methanogens_S [i,1:365]), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
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
structure(list(DOY = 1:365, CHO = c(CHO_S[i,1:365] ), Soil_depth = rep(s_depth[i],365)),
          row.names = c(NA,365L),class= "data.frame")
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
MODEL_AC2 <- MODEL_AC2[c(1:6,9:14),1:3]
MODEL_AC2$bz1 <- min.max.norm(MODEL_AC2$VALUE)
MODEL_AC2$bz1[1:6] <- min.max.norm(MODEL_AC2$VALUE[1:6])
MODEL_AC2$bz1[7:12] <- min.max.norm(MODEL_AC2$VALUE[7:12])
MODEL_AC2$bz <- scale(MODEL_AC2$VALUE)

# MODEL_AC3 <- MODEL_AC2
# MODEL_AC3$bz2[1:6] <- log2(MODEL_AC3$VALUE[1:6])
# MODEL_AC3$bz2[7:12] <- log2(MODEL_AC3$VALUE[7:12])

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


####combine--mic####
#@@  djl1 ---obs
#@@@  MODEL_AC2 ---model
obs_mic <- djl1[,c(1,2,4,5)]
model_mic <- MODEL_AC2[,c(1,3,2,4)]
colnames(obs_mic) <- c('Depth','Microbial_biomass','Type','bz1')
colnames(model_mic) <- c('Depth','Microbial_biomass','Type','bz1')
obs_mic$Type <- toupper(obs_mic$Type)
model_mic$Depth <- model_mic$Depth*100

com_mic <- rbind(obs_mic,model_mic)
com_mic$Group <- c(rep('OBS',8),rep('MODEL',12))


ggplot(data = com_mic,aes(x = Depth,y  = bz1,shape = Group,color = Type))+
  geom_point(size  =4)+
  geom_line(size =1)+
  
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  # scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633'))+
  scale_shape_manual(values=c(15,16,17,18))+
  labs(y = 'Microbial biomass', x = 'Depth(cm)',color = 'Type',shape = 'Group')+
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



