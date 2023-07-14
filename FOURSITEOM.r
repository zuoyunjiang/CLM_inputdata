
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

#####cbs#####
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/cbs_t.clm2.h0.2011-01-01-00000 (110).nc')
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CAERCH4BIOS')
soil_pare <- rep(1,15)
i =1
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
CHO_S <- ncvar_get(try_data,varid = 'CANAERCH4BIOS')
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

MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)
MODEL_AC2 <- MODEL_AC1
# MODEL_AC2 <- MODEL_AC2[c(1:6,9:14),1:3]
# MODEL_AC2$bz1 <- min.max.norm(MODEL_AC2$VALUE)
MODEL_AC2$bz1[1:8] <- min.max.norm(MODEL_AC2$VALUE[1:8])
MODEL_AC2$bz1[9:16] <- min.max.norm(MODEL_AC2$VALUE[9:16])
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
CBSOM <- MODEL_AC2

######djl#########
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (221).nc')
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CAERCH4BIOS')
soil_pare <- rep(1,15)
i =1
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
CHO_S <- ncvar_get(try_data,varid = 'CANAERCH4BIOS')
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

MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)
MODEL_AC2 <- MODEL_AC1
# MODEL_AC2 <- MODEL_AC2[c(1:6,9:14),1:3]
# MODEL_AC2$bz1 <- min.max.norm(MODEL_AC2$VALUE)
MODEL_AC2$bz1[1:8] <- min.max.norm(MODEL_AC2$VALUE[1:8])
MODEL_AC2$bz1[9:16] <- min.max.norm(MODEL_AC2$VALUE[9:16])
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
DJLOM <- MODEL_AC2

####SALLIE#####
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/tsal_t.clm2.h0.2010-01-01-00000 (96).nc ')
ACE_methanogens_S <- ncvar_get(try_data,varid = 'CAERCH4BIOS')
soil_pare <- rep(1,15)
i =1
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
CHO_S <- ncvar_get(try_data,varid = 'CANAERCH4BIOS')
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

MODEL_AC <- cbind(mean_data2,mean_data3$x)
colnames(MODEL_AC) <- c('DEPTH','ACE','CHO')
MODEL_AC1 <- melt(MODEL_AC,id.vars = c('DEPTH'),variable.name = 'MICROBE_PRO',value.name = 'VALUE')
MODEL_AC1$bz <- scale(MODEL_AC1$VALUE)
MODEL_AC1$bz1 <- min.max.norm(MODEL_AC1$VALUE)
MODEL_AC2 <- MODEL_AC1
# MODEL_AC2 <- MODEL_AC2[c(1:6,9:14),1:3]
# MODEL_AC2$bz1 <- min.max.norm(MODEL_AC2$VALUE)
MODEL_AC2$bz1[1:8] <- min.max.norm(MODEL_AC2$VALUE[1:8])
MODEL_AC2$bz1[9:16] <- min.max.norm(MODEL_AC2$VALUE[9:16])
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
SALOM <- MODEL_AC2


#######
sjom1 <- MODEL_om2
CBSOM1 <- CBSOM
DJLOM1 <- DJLOM
SALOM1 <- SALOM
colnames(sjom1) <- c('DEPTH','TYPE','VALUE')
CBSOM1 <- CBSOM1[,c(1,2,5)]
colnames(CBSOM1) <- c('DEPTH','TYPE','VALUE')
DJLOM1 <- DJLOM1[,c(1,2,5)]
colnames(DJLOM1) <- c('DEPTH','TYPE','VALUE')
SALOM1 <- SALOM1[,c(1,2,5)]
colnames(SALOM1) <- c('DEPTH','TYPE','VALUE')

sjom1$SITE <- rep('(a) Sanjiang Plain',16)
CBSOM1$SITE <- rep('(b) Changbai Mountain',16)
DJLOM1$SITE <- rep('(c) Dajiuhu Peatland',16)
SALOM1$SITE <- rep('(d) Sallie', 16 )

CBSOM1$TYPE <- rep(c('OM','AOM'),each = 8)
DJLOM1$TYPE <- rep(c('OM','AOM'),each = 8)
SALOM1$TYPE <- rep(c('OM','AOM'),each = 8)

omdata <- rbind(sjom1,CBSOM1,DJLOM1,SALOM1)


ggplot(data = omdata,aes(x = DEPTH,y  = VALUE,color = TYPE))+
  geom_point(size  =4)+
  geom_line(size =1.5)+
  facet_grid(~SITE,scales = 'free')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'left')+
  # scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633'))+
  scale_shape_manual(values=c(15,16))+
  labs(y = 'Microbial biomass(mol/m3)', x = 'Depth(m)',color = 'Group')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    legend.title=element_text(colour='black',size=18,face="bold"),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=18,face="bold"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 0.5),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')
  )

omdata$station <- rep(c('(a) 三江平原','(b) 长白山','(c) 大九湖','(d) Sallie'),each =16)

ggplot(data = omdata,aes(x = DEPTH,y  = VALUE,color = TYPE))+
  geom_point(size  =4)+
  geom_line(size =1.5)+
  facet_grid(~station,scales = 'free')+
  coord_flip()+
  scale_x_reverse()+
  scale_y_continuous(position = 'left')+
  # scale_fill_manual(values=c("#FF9641","#38C25D",'#FF6633'))+
  scale_shape_manual(values=c(15,16))+
  labs(y = '微生物量相对丰度(mol/m3)', x = '深度(m)',color = '图例')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    legend.title=element_text(colour='black',size=18,face="bold"),
    # legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=18,face="bold"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 0.5),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    strip.text = element_text(size = 20,face = 'bold')
  )
