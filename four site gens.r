min.max.norm <- function(x){
  ((x-min(x))/(max(x)-min(x)))
} 
#####sj#######
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
MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)
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
#####combine#######
model_com <- rbind(MODEL_AC1,MODEL_AC3)
model_com$DEPTH[17:36] <- model_com$DEPTH[17:36]/100
model_coma <- model_com
model_coma$MICROBE_PRO <- c(rep('AM',8),rep('MM',8),rep('AM_OBS',10),rep('MM_OBS',10))
model_coma1 <- model_coma[1:16,]

ggplot(data = model_coma1,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 4)+
  geom_point(size = 9,alpha = 0.8)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#B887C3","#38C25D"))+
  scale_color_manual(values=c("#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    legend.title = element_blank(),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 24,face = 'bold'),
    axis.title.x = element_text(size = 24,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))

model_coma2 <- model_coma[17:36,]
ggplot(data = model_coma2,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 4)+
  geom_point(size = 9,alpha = 0.8)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#B887C3","#38C25D"))+
  scale_color_manual(values=c("#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.85,0.2),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    legend.title = element_blank(),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 24,face = 'bold'),
    axis.title.x = element_text(size = 24,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))


######cn######


ggplot(data = model_coma1,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 4)+
  geom_point(size = 9,alpha = 0.8)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#B887C3","#38C25D"))+
  scale_color_manual(values=c("#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('深度(m)')+
  ylab('相对丰度')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    legend.title = element_blank(),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 24,face = 'bold'),
    axis.title.x = element_text(size = 24,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))

ggplot(data = model_coma2,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 4)+
  geom_point(size = 9,alpha = 0.8)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#B887C3","#38C25D"))+
  scale_color_manual(values=c("#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('深度(m)')+
  ylab('相对丰度')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.85,0.2),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    legend.title = element_blank(),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 24,face = 'bold'),
    axis.title.x = element_text(size = 24,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))


###cbs####

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/cbs_t.clm2.h0.2011-01-01-00000 (110).nc')
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
# MODEL_AC2$bz <- scale(MODEL_AC2$VALUE)

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

##obs
D_data <- read.csv('D:/AAAA-资料E盘/CLM/microbe/cbs/mic-gen/cbsgen.csv',header = T)
cbs <- D_data[1:6,6:8]
colnames(cbs) <- c("Depth",'ACE','CHO')

cbs1 <- melt(cbs,id.vars = 'Depth',variable.name = 'type',value.name = 'microbial biomass')
cbs1$bz1[1:6] <- min.max.norm(cbs1$`microbial biomass`[1:6])
cbs1$bz1[7:12] <- min.max.norm(cbs1$`microbial biomass`[7:12])
ggplot(data = cbs1,aes(x = Depth,y  = bz1,color = type))+
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

#######cn#####
cbs1$site <- rep('Changbai Mountain',12)
cbs1$type <- rep('Observation',12)
cbs2 <- MODEL_AC2
cbs2$site <- rep('Changbai Mountain',12)
cbs2$type <- rep('Modeled',12)

######djl#######

try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (221).nc')
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
MODEL_AC2 <- MODEL_AC2[c(1:7,9:15),1:3]
MODEL_AC2$bz1 <- min.max.norm(MODEL_AC2$VALUE)
MODEL_AC2$bz1[1:7] <- min.max.norm(MODEL_AC2$VALUE[1:7])
MODEL_AC2$bz1[8:14] <- min.max.norm(MODEL_AC2$VALUE[8:14])
# MODEL_AC2$bz <- scale(MODEL_AC2$VALUE)

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

######obs#######

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
djl1$bz2 <- min.max.norm(djl1$x)

djl1$Group.1 <- djl1$Group.1/100
ggplot(data = djl1,aes(x =  Group.1,y = bz1,fill = type,color  = type))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#B887C3","#38C25D"))+
  scale_color_manual(values=c("#B887C3","#38C25D"))+
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

djl1
djl2 <- MODEL_AC2
#######sal#####
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/tsal_t.clm2.h0.2010-01-01-00000 (96).nc ')
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
MODEL_AC2 <- MODEL_AC2[c(1:7,9:15),1:3]
MODEL_AC2$bz1 <- min.max.norm(MODEL_AC2$VALUE)
# MODEL_AC2$bz1[1:7] <- min.max.norm(MODEL_AC2$VALUE[1:7])
# MODEL_AC2$bz1[8:14] <- min.max.norm(MODEL_AC2$VALUE[8:14])
# MODEL_AC2$bz <- scale(MODEL_AC2$VALUE)

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

########
D_data <- read.csv('D:/AAAA-资料E盘/CLM/microbe/sallie fen/MG-SALLIE FEN.csv',header = T)
sallie <- D_data[1:3,14:16]
colnames(sallie) <- c("Depth",'ACE','CHO')

sallie1 <- melt(sallie,id.vars = 'Depth',variable.name = 'type',value.name = 'microbial biomass')
sallie1$bz1[1:3] <- min.max.norm(sallie1$`microbial biomass`[1:3])
sallie1$bz1[4:6] <- min.max.norm(sallie1$`microbial biomass`[4:6])
sallie2 <- sallie1
sallie2$bz2 <- min.max.norm(sallie1$`microbial biomass`)
sallie2$Depth <- sallie2$Depth/100
ggplot(data = sallie2,aes(x = Depth,y = bz2,fill = type,color  = type))+
  geom_line(size = 2)+
  geom_point(size = 4)+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'right')+
  scale_fill_manual(values=c("#B887C3","#38C25D"))+
  scale_color_manual(values=c("#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.5,0.1),
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

sallie2
sallie3 <- MODEL_AC2

########结合########
model_coma1$site <- rep('Sanjiang Plain',16)
model_coma1$type <- rep('Modeled',16)
model_coma2$site <- rep('Sanjiang Plain',20)
model_coma2$type <- rep('Observation',20)

sjgen <- rbind(model_coma1,model_coma2)
sjgen <- sjgen[,c(1,2,5,6,7)]

cbs1$site <- rep('Changbai Mountain',12)
cbs1$type <- rep('Observation',12)
colnames(cbs1) <- c("DEPTH" , "type", "microbial biomass", "bz1", "site"  )
cbs1$DEPTH <- cbs1$DEPTH/100
# cbs2 <- MODEL_AC2
cbs2$site <- rep('Changbai Mountain',12)
cbs2$type <- rep('Modeled',12)
cbsgen <- rbind(cbs1[,c(1,4,5,2)],cbs2[,c(1,4,5,6)])

cbsgen$MICROBE_PRO <- rep(c('AM_OBS','MM_OBS','AM','MM'),each = 6)
cbsgen <- cbsgen[,c(1,5,2,4,3)]

djl11 <- djl1
djl22 <- djl2
djl1 <- djl1[,c(1,4,5)]
djl2 <- djl2[,c(1,2,4)]
colnames(djl1) <- c('DEPTH','MICROBE_PRO','bz1')
colnames(djl2) <- c('DEPTH','MICROBE_PRO','bz1')
djl1$site <- rep('Dajiuhu Peatland',8)
djl1$type <- rep('Observation',8)
djl1 <- djl1[,c(1,2,3,5,4)]
djl1$MICROBE_PRO <- rep(c('MM_OBS','AM_OBS'),each =4 )
djl2$site <- rep('Dajiuhu Peatland',14)
djl2$type <- rep('Modeled',14)
djl2$MICROBE_PRO <- rep(c('AM','MM'),each = 7)

djlgen <- rbind(djl1,djl2)

sallie21 <- sallie2
sallie31 <- sallie3
sallie2 <- sallie2[,c(1,2,5)]
sallie3 <- sallie3[,c(1,2,4)]
colnames(sallie2) <- c('DEPTH','MICROBE_PRO','bz1')
colnames(sallie3) <- c('DEPTH','MICROBE_PRO','bz1')
sallie2$site <- rep('Sallie', 6)
sallie2$type <- rep('Observation',6)
sallie3$site <- rep('Sallie',14)
sallie3$type <- rep('Modeled',14)
sallie2$MICROBE_PRO <- rep(c('AM_OBS','MM_OBS'),each = 3)
sallie3$MICROBE_PRO <- rep(c('AM','MM'),each = 7)
salgen <- rbind(sallie2,sallie3)

foursite <- rbind(sjgen,cbsgen,djlgen,salgen)
foursite$site <- factor(foursite$site,levels = c('Sanjiang Plain','Changbai Mountain','Dajiuhu Peatland',"Sallie"))

ggplot(data = foursite,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 1.5)+
  geom_point(size = 6,alpha = 1)+
  facet_grid(site~type,scales = 'free')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'left')+
  # scale_fill_manual(values=c("#B887C3","#38C25D"))+
  # scale_color_manual(values=c("#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('Depth(m)')+
  ylab('Relative abundance')+
  
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.35,0.1),
    # legend.title=element_text(colour='black'),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    # legend.key.height=grid::unit(0.5,"cm"),
    # legend.key.width=grid::unit(0.5,"cm"),
    legend.title = element_blank(),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 0.5),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 24,face = 'bold'),
    axis.title.x = element_text(size = 24,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    strip.text.x = element_text(size = 22,  face = "bold"), # 这里设置x轴方向的字体类型，
    strip.text.y = element_text(size = 22,  face = "bold") # 这里设置y轴方向的字体类型， 
    
    # panel.border=element_blank(), # tianjia bianjie
    # plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    # plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold")
    )

foursite$SITE1 <- c(rep('三江平原',36),rep('长白山',24),rep('大九湖',22),rep('Sallie',20))
foursite$SITE1 <- factor(foursite$SITE1,levels = c('三江平原','长白山','大九湖',"Sallie"))
foursite$type1 <- NA
foursite$type1[foursite$type == 'Observation'] <- '观测值'
foursite$type1[foursite$type == 'Modeled'] <- '模拟值'
ggplot(data = foursite,aes(x = DEPTH,y = bz1,fill = MICROBE_PRO,color  = MICROBE_PRO))+
  geom_line(size = 2)+
  geom_point(size = 6)+
  facet_grid(SITE1~type1,scales = 'free')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'left')+
  # scale_fill_manual(values=c("#B887C3","#38C25D"))+
  # scale_color_manual(values=c("#B887C3","#38C25D"))+
  # scale_shape_manual(values = c(21,22,23,24))+
  xlab('深度(m)')+
  ylab('相对丰度')+
  
  # labs(color = '',)+
  theme_base()+
  theme(
    legend.position = c(0.35,0.1),
    # legend.title=element_text(colour='black'),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=16,face="bold"),
    # legend.key.height=grid::unit(0.5,"cm"),
    # legend.key.width=grid::unit(0.5,"cm"),
    legend.title = element_blank(),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 0.5),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 24,face = 'bold'),
    axis.title.x = element_text(size = 24,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    strip.text.x = element_text(size = 22), # 这里设置x轴方向的字体类型，
    strip.text.y = element_text(size = 22) # 这里设置y轴方向的字体类型， 
    
    # panel.border=element_blank(), # tianjia bianjie
    # plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    # plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold")
  )
