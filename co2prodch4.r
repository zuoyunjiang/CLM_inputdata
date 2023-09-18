library(ggplot2)
library(ncdf4)
library(ggpmisc)
library(dplyr)
library(reshape2)
library(ggthemes)
library(customLayout)
###sanjiang 
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2019-01-01-00000 (119).nc')
nogen_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2019-01-01-00000 (3).nc')


CO2PROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_10 <- apply(as.matrix(CO2PROCH4[1:3,]),2,FUN = sum)
ACEPROCH4_10 <- apply(as.matrix(ACEPROCH4[1:3,]),2,FUN = sum)
CH4OXIDAER_10 <- apply(as.matrix(CH4OXIDAER[1:3,]),2,FUN = sum)
CH4OXIDANAER_10<- apply(as.matrix(CH4OXIDANAER[1:3,]),2,FUN = sum)


CO2PROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER1 <- ncvar_get(nc = nogen_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER1 <- ncvar_get(nc = nogen_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_101 <- apply(as.matrix(CO2PROCH41[1:3,]),2,FUN = sum)
ACEPROCH4_101 <- apply(as.matrix(ACEPROCH41[1:3,]),2,FUN = sum)
CH4OXIDAER_101 <- apply(as.matrix(CH4OXIDAER1[1:3,]),2,FUN = sum)
CH4OXIDANAER_101 <- apply(as.matrix(CH4OXIDANAER1[1:3,]),2,FUN = sum)

ACEDATA <- cbind(CO2PROCH4_10,CO2PROCH4_101)
df <- ACEDATA
df <- data.frame(df)
df$DOY <- 1:365
df$type <- rep('MM',365)
df$site <- rep('Sanjiang Plain',365)
colnames(df) <- c('Parameterization','Default','DOY','TYPE','SITE')

ggplot(df, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization*10^8, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default*10^8, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
df1 <- ACEDATA
df1 <- data.frame(df1)
df1$DOY <- 1:365
df1$type <- rep('AM',365)
df1$site <- rep('Sanjiang Plain',365)
colnames(df1) <- c('Parameterization','Default','DOY','TYPE','SITE')
# cdf <- rbind(df,df1)
# fill <- c("#56B4E9", "#ff69b4")

ggplot(df1, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization*10^7, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default*10^7, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))
####cbs########
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/cbs_t.clm2.h0.2011-01-01-00000 (110).nc')
nogen_data<- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/mic-gen/cbs_nogens.clm2.h0.2011-01-01-00000 (3).nc')

CO2PROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_10 <- apply(as.matrix(CO2PROCH4[1:3,]),2,FUN = sum)
ACEPROCH4_10 <- apply(as.matrix(ACEPROCH4[1:3,]),2,FUN = sum)
CH4OXIDAER_10 <- apply(as.matrix(CH4OXIDAER[1:3,]),2,FUN = sum)
CH4OXIDANAER_10<- apply(as.matrix(CH4OXIDANAER[1:3,]),2,FUN = sum)


CO2PROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER1 <- ncvar_get(nc = nogen_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER1 <- ncvar_get(nc = nogen_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_101 <- apply(as.matrix(CO2PROCH41[1:3,]),2,FUN = sum)
ACEPROCH4_101 <- apply(as.matrix(ACEPROCH41[1:3,]),2,FUN = sum)
CH4OXIDAER_101 <- apply(as.matrix(CH4OXIDAER1[1:3,]),2,FUN = sum)
CH4OXIDANAER_101 <- apply(as.matrix(CH4OXIDANAER1[1:3,]),2,FUN = sum)

ACEDATA <- cbind(CO2PROCH4_10,CO2PROCH4_101)
df2 <- ACEDATA
df2 <- data.frame(df2)
df2$DOY <- 1:365
df2$type <- rep('MM',365)
df2$site <- rep('Changbai Mountain',365)
colnames(df2) <- c('Parameterization','Default','DOY','TYPE','SITE')

ggplot(df2, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization*10^8, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*0.3, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default*10^8/0.3, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
df3 <- ACEDATA
df3 <- data.frame(df3)
df3$DOY <- 1:365
df3$type <- rep('AM',365)
df3$site <- rep('Changbai Mountain',365)
colnames(df3) <- c('Parameterization','Default','DOY','TYPE','SITE')
# fill <- c("#56B4E9", "#ff69b4")

ggplot(df3, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization*10^7, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*0.3, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default*10^7/0.3, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))

######djh######
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (310).nc')
nogen_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_nogens.clm2.h0.2019-01-01-00000 (11).nc')


CO2PROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CO2PROCH4_10 <- apply(as.matrix(CO2PROCH4[1:8,]),2,FUN = sum)
ACEPROCH4_10 <- apply(as.matrix(ACEPROCH4[1:8,]),2,FUN = sum)


CO2PROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CO2PROCH4_101 <- apply(as.matrix(CO2PROCH41[1:8,]),2,FUN = sum)
ACEPROCH4_101 <- apply(as.matrix(ACEPROCH41[1:8,]),2,FUN = sum)



ACEDATA <- cbind(CO2PROCH4_10,CO2PROCH4_101)
df4 <- ACEDATA
df4 <- data.frame(df4)
df4$DOY <- 1:365
df4$type <- rep('MM',365)
df4$site <- rep('Dajiuhu Peatland',365)
df4$Month <- c(rep('1',31),rep('2',28),rep('3',31),rep('4',30),rep('5',31),rep('6',30),rep('7',31)
               ,rep('8',31),rep('9',30),rep('10',31),rep('11',30),rep('12',31))
colnames(df4) <- c('Parameterization','Default','DOY','TYPE','SITE','Month')
a <- aggregate(df4$Default,by = list(df4$Month),mean)
a$Group.1 <- as.numeric(a$Group.1)
a <- a[order(a$Group.1),]
a1 <- aggregate(df4$Parameterization,by = list(df4$Month),mean)
a1$Group.1 <- as.numeric(a1$Group.1)
a1 <- a1[order(a1$Group.1),]
df41 <- data.frame(
   Parameterization <- a1$x,
   Default <- a$x,
   DOY <- 1:12,
   TYPE <- rep('MM',12),
   SITE <- rep('Dajiuhu Peatland',12)
)
colnames(df41) <- c('Parameterization','Default','DOY','TYPE','SITE')

ggplot(df41, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
df5 <- ACEDATA
df5 <- data.frame(df5)
df5$DOY <- 1:365
df5$type <- rep('AM',365)
df5$site <- rep('Dajiuhu Peatland',365)
df5$Month <- c(rep('1',31),rep('2',28),rep('3',31),rep('4',30),rep('5',31),rep('6',30),rep('7',31)
               ,rep('8',31),rep('9',30),rep('10',31),rep('11',30),rep('12',31))
colnames(df5) <- c('Parameterization','Default','DOY','TYPE','SITE','Month')
a <- aggregate(df5$Default,by = list(df5$Month),mean)
a$Group.1 <- as.numeric(a$Group.1)
a <- a[order(a$Group.1),]
a1 <- aggregate(df5$Parameterization,by = list(df5$Month),mean)
a1$Group.1 <- as.numeric(a1$Group.1)
a1 <- a1[order(a1$Group.1),]
df51 <- data.frame(
  Parameterization <- a1$x,
  Default <- a$x,
  DOY <- 1:12,
  TYPE <- rep('AM',12),
  SITE <- rep('Dajiuhu Peatland',12)
)
# fill <- c("#56B4E9", "#ff69b4")
colnames(df51) <- c('Parameterization','Default','DOY','TYPE','SITE')
ggplot(df51, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization*10^7, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default*10^7/1, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))

#####sal######
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/tsal_t.clm2.h0.2010-01-01-00000 (96).nc ')
nogen_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sallie/t_sal.clm2.h0.2010-01-01-00000 (182).nc')

DOC <- ncvar_get(nc = try_data,varid = 'CDOCS')
DOC_10 <- apply(as.matrix(DOC[1:3,]),2,FUN = sum)

CO2PROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH4 <- ncvar_get(nc = try_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER <- ncvar_get(nc = try_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_10 <- apply(as.matrix(CO2PROCH4[1:3,]),2,FUN = sum)
ACEPROCH4_10 <- apply(as.matrix(ACEPROCH4[1:3,]),2,FUN = sum)
CH4OXIDAER_10 <- apply(as.matrix(CH4OXIDAER[1:3,]),2,FUN = sum)
CH4OXIDANAER_10<- apply(as.matrix(CH4OXIDANAER[1:3,]),2,FUN = sum)

DOC1 <- ncvar_get(nc = nogen_data,varid = 'CDOCS')
DOC_101 <- apply(as.matrix(DOC1[1:3,]),2,FUN = sum)

CO2PROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_CO2_DEPTH_SAT')
ACEPROCH41 <- ncvar_get(nc = nogen_data,varid = 'CH4_PROD_ACE_DEPTH_SAT')
CH4OXIDAER1 <- ncvar_get(nc = nogen_data,varid = 'CH4_OXID_O2_DEPTH_UNSAT')
CH4OXIDANAER1 <- ncvar_get(nc = nogen_data,varid = 'CH4_OXID_AOM_DEPTH_SAT')
CO2PROCH4_101 <- apply(as.matrix(CO2PROCH41[1:3,]),2,FUN = sum)
ACEPROCH4_101 <- apply(as.matrix(ACEPROCH41[1:3,]),2,FUN = sum)
CH4OXIDAER_101 <- apply(as.matrix(CH4OXIDAER1[1:3,]),2,FUN = sum)
CH4OXIDANAER_101 <- apply(as.matrix(CH4OXIDANAER1[1:3,]),2,FUN = sum)



ACEDATA <- cbind(CO2PROCH4_10,CO2PROCH4_101)
df6 <- ACEDATA
df6 <- data.frame(df6)
df6$DOY <- 1:365
df6$type <- rep('MM',365)
df6$site <- rep("Sallie's fen",365)
colnames(df6) <- c('Parameterization','Default','DOY','TYPE','SITE')

ggplot(df6, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization*10^6, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default*10^6/1, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
df7 <- ACEDATA
df7 <- data.frame(df7)
df7$DOY <- 1:365
df7$type <- rep('AM',365)
df7$site <- rep("Sallie's fen",365)
colnames(df7) <- c('Parameterization','Default','DOY','TYPE','SITE')
# fill <- c("#56B4E9", "#ff69b4")

ggplot(df7, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization*10^7, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default*10^7/1, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))


######cdf#####
cdf
cdf <- rbind(df,df1,df2,df3,df41,df51,df6,df7)
df
df_data <- melt(df, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df_data$value <- min.max.norm(df_data$value)
df1
df1_data <- melt(df1, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df1_data$value <- min.max.norm(df1_data$value)
df2
df2_data <- melt(df2, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df2_data$value <- min.max.norm(df2_data$value)
df3
df3_data <- melt(df3, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df3_data$value <- min.max.norm(df3_data$value)
df41
df41_data <- melt(df41, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df41_data$value <- min.max.norm(df41_data$value)
df51
df51_data <- melt(df51, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df51_data$value <- min.max.norm(df51_data$value)
df6
df6_data <- melt(df6, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df6_data$value <- min.max.norm(df6_data$value)
df7
df7_data <- melt(df7, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df7_data$value <- min.max.norm(df7_data$value)

cdf_data <- rbind(df_data,df1_data,df2_data,df3_data,df41_data,df51_data,df6_data,df7_data)
gg <- ggplot(cdf_data) +
  geom_line(aes(x = DOY, y = value, color = variable), linetype = "solid", size = 1) +
  labs(
    title = "",
    x = "DOY",
    y = "Methane production"
  ) +
  scale_color_manual(values = c("Parameterization" = "#1f77b4", "Default" = "#ff7f0e")) +  # 设置颜色
  facet_grid(SITE ~ TYPE, scales = "free_y") +  # 根据站点和类型分面
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",  # 将图例放在图的右侧
    legend.title = element_blank(),  # 去掉图例标题
    legend.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray", size = 0.5)
  )

# 将图表分成八个子图，每个站点两个子图
gg + facet_wrap(~SITE +TYPE, nrow = 2, scales = "free_x")



cdf <- rbind(df,df1,df2,df3,df41,df51,df6,df7)
cdf$SITE <- factor(cdf$SITE,levels = c('Sanjiang Plain','Changbai Mountain','Dajiuhu Peatland',"Sallie's fen"))
ggplot(cdf, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization",size = 3)) +
  # 设置左侧y轴标签和颜色
  scale_y_continuous(name = "Parameterization", 
                     sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default",size = 3),) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  facet_grid(TYPE~SITE,scales = 'free') +
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))
cdf2 <- rbind(df,df1)
ggplot(cdf2, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +

  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 

  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题

  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2)',x = 'DOY')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))

cdf3 <- rbind(df2,df3)
ggplot(cdf3, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2)',x = 'DOY')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))

cdf4 <- rbind(df6,df7)
ggplot(cdf4, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2)',x = 'DOY')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))
cdf1 <- rbind(df41,df51)
# cdf1$DOY <- as.numeric(cdf1$DOY)
ggplot(cdf1, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  geom_point(aes(y = Parameterization, color = "Parameterization"),size =6) +
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  geom_point(aes(y = Default, color = "Default"),size =6) + 
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  scale_x_continuous(limits = c(1,12),breaks = seq(2,12,2))+
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2)',x = 'Month')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))

####layerout#####


# 创建拼图画布
lay1 <- lay_new(mat = matrix(1:4, ncol = 2), 
# 矩阵分布，mat表示指定排版的数字矩阵 
widths = c(2,2),             
# 设定宽度比例
heights = c(2,2))
lay_show(lay1)
lay_set(lay1)
set.seed(123)
ggplot(cdf2, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2)',x = 'DOY')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))

ggplot(cdf2, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2/s)',x = 'DOY',title = '(a) Sanjiang Plain')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))
ggplot(cdf3, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2/s)',x = 'DOY', title = '(b) Changbai Mountain')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = 'none',
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))
ggplot(cdf1, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  scale_x_continuous(limits = c(1,12),breaks = seq(2,12,2))+
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2/s)',x = 'Month',title = '(c) Dajiuhu Peatland')+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = 'none',
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))
ggplot(cdf4, aes(x = DOY)) + 
  geom_line(aes(y = Parameterization, color = "Parameterization"),size =3) +
  
  # 设置左侧y轴标签和颜色
  # scale_y_continuous(name = "Parameterization",
  #                    sec.axis = sec_axis(~.*1, name = "Default")) +
  # 添加右侧y轴，并指定标签和颜色
  geom_line(aes(y = Default, color = "Default"),size =3) + 
  
  scale_color_manual("", values = c("Parameterization" = "#F8766D", "Default" = "#00BFC4")) +
  # 自定义主题
  
  facet_wrap(~TYPE,scales = 'free_y',ncol =1) +
  labs(y = 'Methane Production(mol/m2/s)',x = 'DOY',title = "(d) Sallie's fen")+
  theme_base() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(linetype = "dotted", color = "gray"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(size = 1, color = "black"),
        axis.line.y = element_line(size = 1, color = "black"),
        axis.ticks = element_line(size = 1, color = "black"),
        axis.title.x = element_text(face = "bold", size = 22, vjust = -0.5),
        axis.title.y = element_text(face = "bold", size = 22, vjust = 0.5),
        axis.text = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 22, vjust = 0.5),
        strip.text.x = element_text(face = 'bold',size = 22, colour = "black"),
        legend.position = 'none',
        legend.title = element_blank(),
        # legend.key.size = unit(1.5, 'cm'),
        legend.text = element_text(size = 18,face = 'bold'))

######cdf-validation--CV#######
cdf1
cdf11 <- melt(cdf1,id.vars = c('DOY','TYPE','SITE'),variable.name = 'Method',value.name = 'Methane')

cdf11$Methane[1:12] <- scale(cdf11$Methane[1:12])
cdf11$Methane[13:24] <- scale(cdf11$Methane[13:24])
cdf11$Methane[25:36] <- scale(cdf11$Methane[25:36])
cdf11$Methane[37:48] <- scale(cdf11$Methane[37:48])

ggplot(cdf11,aes(x = DOY,y = Methane,color = Method,shape = TYPE))+
  geom_point(size = 4)+
  scale_colour_brewer(palette = 'Set1')+
  geom_smooth(method = 'lm')

sd(cdf11$Methane[1:12])/mean(cdf11$Methane[1:12])
sd(cdf11$Methane[13:24])/mean(cdf11$Methane[13:24])
sd(cdf11$Methane[25:36])/mean(cdf11$Methane[25:36])
sd(cdf11$Methane[37:48])/mean(cdf11$Methane[37:48])

cdf2
sd(cdf2$Parameterization[1:365])/mean(cdf2$Parameterization[1:365])
sd(cdf2$Parameterization[366:730])/mean(cdf2$Parameterization[366:730])
sd(cdf2$Default[1:365])/mean(cdf2$Default[1:365])
sd(cdf2$Default[366:730])/mean(cdf2$Default[366:730])

cdf3
sd(cdf3$Parameterization[1:365])/mean(cdf3$Parameterization[1:365])
sd(cdf3$Parameterization[366:730])/mean(cdf3$Parameterization[366:730])
sd(cdf3$Default[1:365])/mean(cdf3$Default[1:365])
sd(cdf3$Default[366:730])/mean(cdf3$Default[366:730])
cdf4
sd(cdf4$Parameterization[1:365])/mean(cdf4$Parameterization[1:365])
sd(cdf4$Parameterization[366:730])/mean(cdf4$Parameterization[366:730])
sd(cdf4$Default[1:365])/mean(cdf4$Default[1:365])
sd(cdf4$Default[366:730])/mean(cdf4$Default[366:730])


######compare value#####
sum(cdf1$Parameterization[1:12])/sum(cdf1$Default[1:12])
sum(cdf1$Parameterization[13:24])/sum(cdf1$Default[13:24])

cdf
cdf <- rbind(df,df1,df2,df3,df41,df51,df6,df7)
df
df_data <- melt(df, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df_data$value <- min.max.norm(df_data$value)
df1
df1_data <- melt(df1, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df1_data$value <- min.max.norm(df1_data$value)
df2
df2_data <- melt(df2, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df2_data$value <- min.max.norm(df2_data$value)
df3
df3_data <- melt(df3, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df3_data$value <- min.max.norm(df3_data$value)
df41
df41_data <- melt(df41, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df41_data$value <- min.max.norm(df41_data$value)
df51
df51_data <- melt(df51, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df51_data$value <- min.max.norm(df51_data$value)
df6
df6_data <- melt(df6, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df6_data$value <- min.max.norm(df6_data$value)
df7
df7_data <- melt(df7, id.vars = c("DOY", "SITE", "TYPE"), measure.vars = c("Parameterization", "Default"))
df7_data$value <- min.max.norm(df7_data$value)

cdf_data <- rbind(df_data,df1_data,df2_data,df3_data,df41_data,df51_data,df6_data,df7_data)
ggplot(cdf_data) +
  geom_line(aes(x = DOY, y = value, color = variable), linetype = "solid", size = 1) +
  labs(
    title = "",
    x = "DOY",
    y = "Methane production"
  ) +
  scale_color_manual(values = c("Parameterization" = "#1f77b4", "Default" = "#ff7f0e")) +  # 设置颜色
  facet_grid(TYPE ~ SITE, scales = "free_x") +  # 根据站点和类型分面
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",  # 将图例放在图的右侧
    legend.title = element_blank(),  # 去掉图例标题
    legend.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray", size = 0.5)
  )
cdf_data$SITE[which(cdf_data$SITE == "Sallie's fen")] <- c('Sallie')
cdf_data$SITE <- factor(cdf_data$SITE,levels = c('Sanjiang Plain','Changbai Mountain','Dajiuhu Peatland',"Sallie"))
# 加载必要的包
library(ggplot2)
facet_labels <- data.frame(site = unique(cdf_data$SITE), label = c("(a)", "(b)", "(c)", "(d)"))
facet_labels$site <- factor(facet_labels$site,levels =c('Sanjiang Plain','Changbai Mountain','Dajiuhu Peatland',"Sallie"))

# 修改字体和大小
font_size <- 14
font_family <- "Times New Roman"

# 创建适合出版的主题
custom_theme <- theme(
  text = element_text(family = font_family, size = font_size),
  plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = font_family),
  # axis.title.x = element_text(size = font_size, margin = margin(t = 10)),
  # axis.title.y = element_text(size = font_size, margin = margin(r = 10)),
  axis.text.x = element_text(size = font_size,color = "black"),
  axis.text.y = element_text(size = font_size,color = "black"),
  legend.title = element_blank(),  # 去掉图例标题
  legend.text = element_text(size = font_size,color = "black"),
  legend.position = c(0.92,0.45),
  panel.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, size = 1),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.text = element_text(size = font_size, face = "bold")  # 设置strip的背景为透明
)

# 创建图形并应用自定义主题
ggplot(cdf_data) +
  geom_line(aes(x = DOY, y = value, color = variable), linetype = "solid", size = 1) +
  labs(
    title = "",
    x = "DOY",
    y = "Methane production"
  ) +
  scale_color_manual(values = c("Parameterization" = "#8c564b", "Default" = "#888888")) +  # 设置颜色
  facet_grid(TYPE ~ SITE, scales = "free_x") + 
  theme_minimal()+# 根据站点和类型分面
  custom_theme 
