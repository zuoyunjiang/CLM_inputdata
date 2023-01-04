#### future ####
library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(multcompView)

####FUN###
# mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
####sanjiang####
sj_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj_his_1850-2015.nc')
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj1266.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj2456.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj5856.nc')


jhis_npp <- ncvar_get(nc = sj_his,varid = 'NPP')
jssp126_npp <- ncvar_get(nc = sj_ssp126,varid = 'NPP')
jssp245_npp <- ncvar_get(nc = sj_ssp245,varid = 'NPP')
jssp585_npp <- ncvar_get(nc = sj_ssp585,varid = 'NPP')

jhis_npp <- jhis_npp[1:60225]*3600*24
jssp126_npp <- jssp126_npp[1:31390]*3600*24
jssp245_npp <- jssp245_npp[1:31390]*3600*24
jssp585_npp <- jssp585_npp[1:31390]*3600*24
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataN <- data.frame(year,jssp126_npp,jssp245_npp,jssp585_npp)
sj_hisn <- aggregate(jhis_npp,by = list(Y1),FUN = sum)
sj_126N <- aggregate(jssp126_npp,by = list(year),FUN=sum)
sj_245N <- aggregate(jssp245_npp,by = list(year),FUN=sum)
sj_585N <- aggregate(jssp585_npp,by = list(year),FUN=sum)


snpp <- cbind(sj_126N$Group.1,sj_126N$x,sj_245N$x,sj_585N$x)
colnames(snpp) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
snpp <- data.frame(snpp)
snpp <- snpp[-1,]
jsspn <- melt(snpp, id.vars ='Year', variable.name = 'Scenarios', value.name = 'NPP')  ##data.frame

ggplot(jsspn, aes(x = Year, y =  NPP, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'NPP (gC/m2)')+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )
####DOC####

jhis_doc <- ncvar_get(nc = sj_his,varid = 'CDOCS')
jssp126_doc <- ncvar_get(nc = sj_ssp126,varid = 'CDOCS')
jssp245_doc <- ncvar_get(nc = sj_ssp245,varid = 'CDOCS')
jssp585_doc <- ncvar_get(nc = sj_ssp585,varid = 'CDOCS')

jhis_doc <- jhis_doc[1:15,1:60225]
jssp126_doc <- jssp126_doc[1:15,1:31390]
jssp245_doc <- jssp245_doc[1:15,1:31390]
jssp585_doc <- jssp585_doc[1:15,1:31390]

jhis_doc <- apply(jhis_doc,2,FUN = mean)
jssp126_doc <- apply(jssp126_doc, 2, FUN = mean)
jssp245_doc <- apply(jssp245_doc, 2, FUN = mean)
jssp585_doc <- apply(jssp585_doc, 2, FUN = mean)



Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatad <- data.frame(year,jssp126_doc,jssp245_doc,jssp585_doc)
sj_hisd <- aggregate(jhis_doc,by = list(Y1),FUN = mean)
sj_126d <- aggregate(jssp126_doc,by = list(year),FUN=mean)
sj_245d <- aggregate(jssp245_doc,by = list(year),FUN=mean)
sj_585d <- aggregate(jssp585_doc,by = list(year),FUN=mean)
sj_hisd_2000_2014 <- sj_hisd[151:165,]
colnames(sj_hisd_2000_2014) <- c('Year','DOC')
sdoc <- cbind(sj_126d$Group.1,sj_126d$x,sj_245d$x,sj_585d$x)
colnames(sdoc) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sdoc <- data.frame(sdoc)
sdoc <- sdoc[-1,]
jsspd <- melt(sdoc, id.vars ='Year', variable.name = 'Scenarios', value.name = 'DOC')  ##data.frame

ggplot(jsspd, aes(x = Year, y =  DOC, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'DOC(gC/m3)')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )
ggplot(sj_hisd_2000_2014,aes(x =Year,y = DOC ))+
  geom_line(size =2)+
  theme_base()+
  theme()

sj_hisd_2000_2014$Scenarios <- rep('History',15)
sj_hisd_2000_2014 <- sj_hisd_2000_2014[,c(1,3,2)]
his15<- matrix(c(2015,'History',91.37),ncol =3)
colnames(his15) <- c('Year','Scenarios','DOC')

his_ssp_doc <- rbind(sj_hisd_2000_2014,his15,jsspd)
his_ssp_doc$DOC <- as.double(his_ssp_doc$DOC)
his_ssp_doc$Year <- as.double(his_ssp_doc$Year)
ggplot(his_ssp_doc, aes(x = Year, y =  DOC, color = Scenarios))+
  geom_line(size = 2)+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2)
  )
######SOM#######

jhis_som <- ncvar_get(nc = sj_his,varid = 'TOTSOMC_1m')
jssp126_som <- ncvar_get(nc = sj_ssp126,varid = 'TOTSOMC_1m')
jssp245_som <- ncvar_get(nc = sj_ssp245,varid = 'TOTSOMC_1m')
jssp585_som <- ncvar_get(nc = sj_ssp585,varid = 'TOTSOMC_1m')

jhis_som <- jhis_som[1:60225]
jssp126_som <- jssp126_som[1:31390]
jssp245_som <- jssp245_som[1:31390]
jssp585_som <- jssp585_som[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatasom <- data.frame(year,jssp126_som,jssp245_som,jssp585_som)
sj_hissom <- aggregate(jhis_som,by = list(Y1),FUN = sum)
sj_126som <- aggregate(jssp126_som,by = list(year),FUN=sum)
sj_245som <- aggregate(jssp245_som,by = list(year),FUN=sum)
sj_585som <- aggregate(jssp585_som,by = list(year),FUN=sum)


ssom <- cbind(sj_126som$Group.1,sj_126som$x,sj_245som$x,sj_585som$x)
colnames(ssom) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
ssom <- data.frame(ssom)
# ssom <- ssom[-1:-2,]
ssom <- ssom[-1,]
jsspd <- melt(ssom, id.vars ='Year', variable.name = 'Scenarios', value.name = 'SOM')  ##data.frame

ggplot(jsspd, aes(x = Year, y =  SOM, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'SOM (gC/m2)')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.2),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )
####ch4#####

jhis_ch4 <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_NETFLUX')
jssp126_ch4 <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_NETFLUX')
jssp245_ch4 <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_NETFLUX')
jssp585_ch4 <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_NETFLUX')
jhis_ch4 <- jhis_ch4[1:60225]*3600*24*16
jssp126_ch4 <- jssp126_ch4[1:31390]*3600*24*16
jssp245_ch4 <- jssp245_ch4[1:31390]*3600*24*16
jssp585_ch4 <- jssp585_ch4[1:31390]*3600*24*16
year <- rep(2014:2099,each = 365)
sspdata <- data.frame(year,jssp126_ch4,jssp245_ch4,jssp585_ch4)
sj_126 <- aggregate(jssp126_ch4,by = list(year),FUN=sum)
sj_245 <- aggregate(jssp245_ch4,by = list(year),FUN=sum)
sj_585 <- aggregate(jssp585_ch4,by = list(year),FUN=sum)

sch4 <- cbind(sj_126$Group.1,sj_126$x,sj_245$x,sj_585$x)
colnames(sch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sch4 <- data.frame(sch4)
sch4 <- sch4[-1,]
jssp <- melt(sch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

ggplot(jssp, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year', y = 'Methane flux (gC/m2)')+
  theme_base()+
  theme(
    legend.position = c(0.9,0.8),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )

###bar plot
sj_16_25 <-  jssp[which(jssp$Year%in% c(2016:2025) ),]
sj_50s <-  jssp[which(jssp$Year%in% c(2050:2059) ),]
sj_90s <-  jssp[which(jssp$Year%in% c(2090:2099) ),]
a <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = mean)
b <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = sd)
c <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = mean)
d <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = sd)
e <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = mean)
f <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = sd)

g <- cbind(a,b$x)
h <- cbind(c,d$x)
i <- cbind(e,f$x)
colnames(g) <- c('Scenarios','mean','sd')
colnames(h) <- c('Scenarios','mean','sd')
colnames(i) <- c('Scenarios','mean','sd')
bar_data <- rbind(g,h,i)
bar_data$year <- rep(c('16-25','50s','90s'),each =3)
anovadata <- rbind(sj_16_25,sj_50s,sj_90s)
anovadata$date <- rep(c('1625','50s','90s'),each =30)
anovadata$Scenarios <- factor(anovadata$Scenarios)
anovadata$date <- factor(anovadata$date)
anova <- aov(Methane~date*Scenarios, data = anovadata)
summary(anova)
tukey <- TukeyHSD(anova)
tukey.cld <- multcompLetters4(anova, tukey)
cld <- as.data.frame.list(tukey.cld$`date:Scenarios`)
bar_data$Tukey <- rep('a',9)
bar_data$se <- bar_data$sd/sqrt(10)
# coloured barplot
ggplot(bar_data, aes(x = factor(year), y = mean, fill = Scenarios, colour = Scenarios)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge(0.9), width = 0.25) +
  labs(x=" ", y="Methane") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.9, 0.85)) +
  geom_text(aes(y=mean+se+0.5,label=Tukey), position = position_dodge(0.90), size = 17, 
             hjust=0.5, colour = "gray25")+
  theme(
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )
  # ylim(0, 1500) +
  # geom_text(aes(label=Scenarios, y = 25), position = position_dodge(0.90))




####ACE####

jhis_ace <- ncvar_get(nc = sj_his,varid = 'CACES_PROD')
jssp126_ace <- ncvar_get(nc = sj_ssp126,varid = 'CACES_PROD')
jssp245_ace <- ncvar_get(nc = sj_ssp245,varid = 'CACES_PROD')
jssp585_ace <- ncvar_get(nc = sj_ssp585,varid = 'CACES_PROD')

jhis_ace <- jhis_ace[1:15,1:60225]*3600*24*12
jssp126_ace <- jssp126_ace[1:15,1:31390]*3600*24*12
jssp245_ace <- jssp245_ace[1:15,1:31390]*3600*24*12
jssp585_ace <- jssp585_ace[1:15,1:31390]*3600*24*12

jhis_ace <- apply(jhis_ace,2,FUN = sum)
jssp126_ace <- apply(jssp126_ace,2,FUN = sum)
jssp245_ace <- apply(jssp245_ace,2,FUN = sum)
jssp585_ace <- apply(jssp585_ace,2,FUN = sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataa <- data.frame(year,jssp126_ace,jssp245_ace,jssp585_ace)
sj_hisa <- aggregate(jhis_ace,by = list(Y1),FUN = sum)
sj_126a <- aggregate(jssp126_ace,by = list(year),FUN=sum)
sj_245a <- aggregate(jssp245_ace,by = list(year),FUN=sum)
sj_585a <- aggregate(jssp585_ace,by = list(year),FUN=sum)
sj_hisa_2000_2014 <- sj_hisa[151:165,]
colnames(sj_hisa) <- c('Year','ACE')
colnames(sj_hisa_2000_2014) <- c('Year','ACE')
sace <- cbind(sj_126a$Group.1,sj_126a$x,sj_245a$x,sj_585a$x)
colnames(sace) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sace <- data.frame(sace)
sace <- sace[-1,]
jsspa <- melt(sace, id.vars ='Year', variable.name = 'Scenarios', value.name = 'ACE')  ##data.frame

ggplot(jsspa, aes(x = Year, y =  ACE, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'ACE_PROD (gC/m3)')+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )
ggplot(sj_hisa_2000_2014 ,aes(x =Year,y = ACE ))+
  geom_line(size =2)+
  theme_base()+
  theme()
# 
# sj_hisa_2000_2014$Scenarios <- rep('History',15)
# sj_hisa_2000_2014 <- sj_hisa_2000_2014[,c(1,3,2)]
# his15a<- matrix(c(2015,'History',91.37),ncol =3)
# colnames(his15a) <- c('Year','Scenarios','ACE')
# 
# his_ssp_ace <- rbind(sj_hisa_2000_2014,his15a,jsspa)
# his_ssp_ace$ACE <- as.double(his_ssp_ace$ACE)
# his_ssp_ace$Year <- as.double(his_ssp_ace$Year)
# ggplot(his_ssp_ace, aes(x = Year, y =  ACE, color = Scenarios))+
#   geom_line(size = 2)+
#   theme_base()+
#   theme(
#     legend.position = c(0.9,0.2)
#   )

####CACES####

jhis_ACE1 <- ncvar_get(nc = sj_his,varid = 'CACES')
jssp126_ACE1 <- ncvar_get(nc = sj_ssp126,varid = 'CACES')
jssp245_ACE1 <- ncvar_get(nc = sj_ssp245,varid = 'CACES')
jssp585_ACE1 <- ncvar_get(nc = sj_ssp585,varid = 'CACES')

jhis_ACE1 <- jhis_ACE1[1:15,1:60225]
jssp126_ACE1 <- jssp126_ACE1[1:15,1:31390]
jssp245_ACE1 <- jssp245_ACE1[1:15,1:31390]
jssp585_ACE1 <- jssp585_ACE1[1:15,1:31390]

jhis_ACE1 <- apply(jhis_ACE1,2,FUN = sum)
jssp126_ACE1 <- apply(jssp126_ACE1,2,FUN = sum)
jssp245_ACE1 <- apply(jssp245_ACE1,2,FUN = sum)
jssp585_ACE1 <- apply(jssp585_ACE1,2,FUN = sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataACE1 <- data.frame(year,jssp126_ACE1,jssp245_ACE1,jssp585_ACE1)
sj_hisACE1 <- aggregate(jhis_ACE1,by = list(Y1),FUN = sum)
sj_126ACE1 <- aggregate(jssp126_ACE1,by = list(year),FUN=sum)
sj_245ACE1 <- aggregate(jssp245_ACE1,by = list(year),FUN=sum)
sj_585ACE1 <- aggregate(jssp585_ACE1,by = list(year),FUN=sum)


sACE1 <- cbind(sj_126ACE1$Group.1,sj_126ACE1$x,sj_245ACE1$x,sj_585ACE1$x)
colnames(sACE1) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sACE1 <- data.frame(sACE1)
sACE1 <- sACE1[-1:-2,]
jsspac <- melt(sACE1, id.vars ='Year', variable.name = 'Scenarios', value.name = 'ACE')  ##data.frame

ggplot(jsspac, aes(x = Year, y =  ACE, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'ACE (molC/m3)')+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )



##### CACEBIOS #####

jhis_ab <- ncvar_get(nc = sj_his,varid = 'CACEBIOS')
jssp126_ab <- ncvar_get(nc = sj_ssp126,varid = 'CACEBIOS')
jssp245_ab <- ncvar_get(nc = sj_ssp245,varid = 'CACEBIOS')
jssp585_ab <- ncvar_get(nc = sj_ssp585,varid = 'CACEBIOS')

jhis_ab <- jhis_ab[1:15,1:60225]*12
jssp126_ab <- jssp126_ab[1:15,1:31390]*12
jssp245_ab <- jssp245_ab[1:15,1:31390]*12
jssp585_ab <- jssp585_ab[1:15,1:31390]*12

jhis_ab <- apply(jhis_ab,2,FUN = mean)
jssp126_ab <- apply(jssp126_ab,2,FUN = mean)
jssp245_ab <- apply(jssp245_ab,2,FUN = mean)
jssp585_ab <- apply(jssp585_ab,2,FUN = mean)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataab <- data.frame(year,jssp126_ab,jssp245_ab,jssp585_ab)
sj_hisab <- aggregate(jhis_ab,by = list(Y1),FUN = sum)
sj_126ab <- aggregate(jssp126_ab,by = list(year),FUN=sum)
sj_245ab <- aggregate(jssp245_ab,by = list(year),FUN=sum)
sj_585ab <- aggregate(jssp585_ab,by = list(year),FUN=sum)
sj_hisab_2000_2014 <- sj_hisab[151:165,]
colnames(sj_hisab) <- c('Year','AM')
colnames(sj_hisab_2000_2014) <- c('Year','AM')
sab <- cbind(sj_126ab$Group.1,sj_126ab$x,sj_245ab$x,sj_585ab$x)
colnames(sab) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sab <- data.frame(sab)
sab <- sab[-1,]
jsspab <- melt(sab, id.vars ='Year', variable.name = 'Scenarios', value.name = 'AM')  ##data.frame

ggplot(jsspab, aes(x = Year, y =  AM, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'AM(gC/m3)')+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
    )
ggplot(sj_hisab_2000_2014 ,aes(x =Year,y = AM ))+
  geom_line(size =2)+
  theme_base()+
  theme()
####HM####

jhis_HM <- ncvar_get(nc = sj_his,varid = 'CCO2BIOS')
jssp126_HM <- ncvar_get(nc = sj_ssp126,varid = 'CCO2BIOS')
jssp245_HM <- ncvar_get(nc = sj_ssp245,varid = 'CCO2BIOS')
jssp585_HM <- ncvar_get(nc = sj_ssp585,varid = 'CCO2BIOS')

jhis_HM <- jhis_HM[1:15,1:60225]*12
jssp126_HM <- jssp126_HM[1:15,1:31390]*12
jssp245_HM <- jssp245_HM[1:15,1:31390]*12
jssp585_HM <- jssp585_HM[1:15,1:31390]*12

jhis_HM <- apply(jhis_HM,2,FUN = sum)
jssp126_HM <- apply(jssp126_HM,2,FUN = sum)
jssp245_HM <- apply(jssp245_HM,2,FUN = sum)
jssp585_HM <- apply(jssp585_HM,2,FUN = sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataHM <- data.frame(year,jssp126_HM,jssp245_HM,jssp585_HM)
sj_hisHM <- aggregate(jhis_HM,by = list(Y1),FUN = sum)
sj_126HM <- aggregate(jssp126_HM,by = list(year),FUN=sum)
sj_245HM <- aggregate(jssp245_HM,by = list(year),FUN=sum)
sj_585HM <- aggregate(jssp585_HM,by = list(year),FUN=sum)
sj_hisHM_2000_2014 <- sj_hisHM[151:165,]
colnames(sj_hisHM) <- c('Year','HM')
colnames(sj_hisHM_2000_2014) <- c('Year','HM')
sHM <- cbind(sj_126HM$Group.1,sj_126HM$x,sj_245HM$x,sj_585HM$x)
colnames(sHM) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sHM <- data.frame(sHM)
sHM <- sHM[-1,]
jsspHM <- melt(sHM, id.vars ='Year', variable.name = 'Scenarios', value.name = 'HM')  ##data.frame

ggplot(jsspHM, aes(x = Year, y =  HM, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'HM (gC/m3)')+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )
ggplot(sj_hisHM_2000_2014 ,aes(x =Year,y = HM ))+
  geom_line(size =2)+
  theme_base()+
  theme()
#### AER####

jhis_AER <- ncvar_get(nc = sj_his,varid = 'CAERCH4BIOS')
jssp126_AER <- ncvar_get(nc = sj_ssp126,varid = 'CAERCH4BIOS')
jssp245_AER <- ncvar_get(nc = sj_ssp245,varid = 'CAERCH4BIOS')
jssp585_AER <- ncvar_get(nc = sj_ssp585,varid = 'CAERCH4BIOS')

jhis_AER <- jhis_AER[1:15,1:60225]*12
jssp126_AER <- jssp126_AER[1:15,1:31390]*12
jssp245_AER <- jssp245_AER[1:15,1:31390]*12
jssp585_AER <- jssp585_AER[1:15,1:31390]*12

jhis_AER <- apply(jhis_AER,2,sum)
jssp126_AER <- apply(jssp126_AER,2,sum)
jssp245_AER <- apply(jssp245_AER,2,sum)
jssp585_AER <- apply(jssp585_AER,2,sum)

Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataAER <- data.frame(year,jssp126_AER,jssp245_AER,jssp585_AER)
sj_hisAER <- aggregate(jhis_AER,by = list(Y1),FUN = sum)
sj_126AER <- aggregate(jssp126_AER,by = list(year),FUN=sum)
sj_245AER <- aggregate(jssp245_AER,by = list(year),FUN=sum)
sj_585AER <- aggregate(jssp585_AER,by = list(year),FUN=sum)
sj_hisAER_2000_2014 <- sj_hisAER[151:165,]
colnames(sj_hisAER) <- c('Year','AER')
colnames(sj_hisAER_2000_2014) <- c('Year','AER')
sAER <- cbind(sj_126AER$Group.1,sj_126AER$x,sj_245AER$x,sj_585AER$x)
colnames(sAER) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sAER <- data.frame(sAER)
sAER <- sAER[-1,]
jsspAER <- melt(sAER, id.vars ='Year', variable.name = 'Scenarios', value.name = 'AER')  ##data.frame

ggplot(jsspAER, aes(x = Year, y =  AER, color = Scenarios))+
  geom_line(size = 2)+
  labs(x = 'Year',y = 'AER(gC/m3)')+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8),
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
  )
 ggplot(sj_hisAER_2000_2014 ,aes(x =Year,y = AER ))+
  geom_line(size =2)+
  theme_base()+
  theme()
 #### ANAER####
 
 jhis_ANAER <- ncvar_get(nc = sj_his,varid = 'CANAERCH4BIOS')
 jssp126_ANAER <- ncvar_get(nc = sj_ssp126,varid = 'CANAERCH4BIOS')
 jssp245_ANAER <- ncvar_get(nc = sj_ssp245,varid = 'CANAERCH4BIOS')
 jssp585_ANAER <- ncvar_get(nc = sj_ssp585,varid = 'CANAERCH4BIOS')
 
 jhis_ANAER <- jhis_ANAER[1:15,1:60225]*12
 jssp126_ANAER <- jssp126_ANAER[1:15,1:31390]*12
 jssp245_ANAER <- jssp245_ANAER[1:15,1:31390]*12
 jssp585_ANAER <- jssp585_ANAER[1:15,1:31390]*12
 
 jhis_ANAER <- apply(jhis_ANAER,2,sum)
 jssp126_ANAER <- apply(jssp126_ANAER,2,sum)
 jssp245_ANAER <- apply(jssp245_ANAER,2,sum)
 jssp585_ANAER <- apply(jssp585_ANAER,2,sum)
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataANAER <- data.frame(year,jssp126_ANAER,jssp245_ANAER,jssp585_ANAER)
 sj_hisANAER <- aggregate(jhis_ANAER,by = list(Y1),FUN = sum)
 sj_126ANAER <- aggregate(jssp126_ANAER,by = list(year),FUN=sum)
 sj_245ANAER <- aggregate(jssp245_ANAER,by = list(year),FUN=sum)
 sj_585ANAER <- aggregate(jssp585_ANAER,by = list(year),FUN=sum)
 sj_hisANAER_2000_2014 <- sj_hisANAER[151:165,]
 colnames(sj_hisANAER) <- c('Year','ANAER')
 colnames(sj_hisANAER_2000_2014) <- c('Year','ANAER')
 sANAER <- cbind(sj_126ANAER$Group.1,sj_126ANAER$x,sj_245ANAER$x,sj_585ANAER$x)
 colnames(sANAER) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sANAER <- data.frame(sANAER)
 sANAER <- sANAER[-1,]
 jsspANAER <- melt(sANAER, id.vars ='Year', variable.name = 'Scenarios', value.name = 'ANAER')  ##data.frame
 
 ggplot(jsspANAER, aes(x = Year, y =  ANAER, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year', y = 'ANAER(gC/m3)')+
   theme_base()+
   theme(
     legend.position = c(0.1,0.8),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 ggplot(sj_hisANAER_2000_2014 ,aes(x =Year,y = ANAER ))+
   geom_line(size =2)+
   theme_base()+
   theme()
 
 ############
 #####COL CH4####
 
 jhis_CCH4 <- ncvar_get(nc = sj_his,varid = 'CCON_CH4S')
 jssp126_CCH4 <- ncvar_get(nc = sj_ssp126,varid = 'CCON_CH4S')
 jssp245_CCH4 <- ncvar_get(nc = sj_ssp245,varid = 'CCON_CH4S')
 jssp585_CCH4 <- ncvar_get(nc = sj_ssp585,varid = 'CCON_CH4S')
 
 jhis_CCH4 <- jhis_CCH4[1:15,1:60225]
 jssp126_CCH4 <- jssp126_CCH4[1:15,1:31390]
 jssp245_CCH4 <- jssp245_CCH4[1:15,1:31390]
 jssp585_CCH4 <- jssp585_CCH4[1:15,1:31390]
 
 jhis_CCH4 <- apply(jhis_CCH4,2,sum)
 jssp126_CCH4 <- apply(jssp126_CCH4,2,sum)
 jssp245_CCH4 <- apply(jssp245_CCH4,2,sum)
 jssp585_CCH4 <- apply(jssp585_CCH4,2,sum)
 
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataCCH4 <- data.frame(year,jssp126_CCH4,jssp245_CCH4,jssp585_CCH4)
 sj_hisCCH4 <- aggregate(jhis_CCH4,by = list(Y1),FUN = sum)
 sj_126CCH4 <- aggregate(jssp126_CCH4,by = list(year),FUN=sum)
 sj_245CCH4 <- aggregate(jssp245_CCH4,by = list(year),FUN=sum)
 sj_585CCH4 <- aggregate(jssp585_CCH4,by = list(year),FUN=sum)
 
 
 sCCH4 <- cbind(sj_126CCH4$Group.1,sj_126CCH4$x,sj_245CCH4$x,sj_585CCH4$x)
 colnames(sCCH4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sCCH4 <- data.frame(sCCH4)
 sCCH4 <- sCCH4[-1:-2,]
 jssp <- melt(sCCH4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame
 
 
 ggplot(jssp, aes(x = Year, y =  Methane, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year', y = 'Column Methane concetration (gC/m3')+
   theme_base()+
   theme(
     legend.position = c(0.9,0.8),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 #####PLANT-MEDIATED#######
 
 jhis_PM <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_AERE')
 jssp126_PM <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_AERE')
 jssp245_PM <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_AERE')
 jssp585_PM <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_AERE')
 
 jhis_PM <- jhis_PM[1:60225]*3600*24*16
 jssp126_PM <- jssp126_PM[1:31390]*3600*24*16
 jssp245_PM <- jssp245_PM[1:31390]*3600*24*16
 jssp585_PM <- jssp585_PM[1:31390]*3600*24*16
 
 # jhis_PM <- apply(jhis_PM,2,sum)
 # jssp126_PM <- apply(jssp126_PM,2,sum)
 # jssp245_PM <- apply(jssp245_PM,2,sum)
 # jssp585_PM <- apply(jssp585_PM,2,sum)
 
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataPM <- data.frame(year,jssp126_PM,jssp245_PM,jssp585_PM)
 sj_hisPM <- aggregate(jhis_PM,by = list(Y1),FUN = sum)
 sj_126PM <- aggregate(jssp126_PM,by = list(year),FUN=sum)
 sj_245PM <- aggregate(jssp245_PM,by = list(year),FUN=sum)
 sj_585PM <- aggregate(jssp585_PM,by = list(year),FUN=sum)
 
 
 sPM <- cbind(sj_126PM$Group.1,sj_126PM$x,sj_245PM$x,sj_585PM$x)
 colnames(sPM) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sPM <- data.frame(sPM)
 sPM <- sPM[-1:-2,]
 jsspPLANT <- melt(sPM, id.vars ='Year', variable.name = 'Scenarios', value.name = 'PLANT')  ##data.frame
 
 ggplot(jsspPLANT, aes(x = Year, y =  PLANT, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'Plant_mediated transport (gC/m2)')+
   theme_base()+
   theme(
     legend.position = c(0.9,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 
 ####DIFFUSIVE######
 
 jhis_DIF <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_DIFF')
 jssp126_DIF <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_DIFF')
 jssp245_DIF <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_DIFF')
 jssp585_DIF <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_DIFF')
 
 jhis_DIF <- jhis_DIF[1:60225]*3600*24*16
 jssp126_DIF <- jssp126_DIF[1:31390]*3600*24*16
 jssp245_DIF <- jssp245_DIF[1:31390]*3600*24*16
 jssp585_DIF <- jssp585_DIF[1:31390]*3600*24*16
 # 
 # jhis_DIF <- apply(jhis_DIF,2,sum)
 # jssp126_DIF <- apply(jssp126_DIF,2,sum)
 # jssp245_DIF <- apply(jssp245_DIF,2,sum)
 # jssp585_DIF <- apply(jssp585_DIF,2,sum)
 
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataDIF <- data.frame(year,jssp126_DIF,jssp245_DIF,jssp585_DIF)
 sj_hisDIF <- aggregate(jhis_DIF,by = list(Y1),FUN = sum)
 sj_126DIF <- aggregate(jssp126_DIF,by = list(year),FUN=sum)
 sj_245DIF <- aggregate(jssp245_DIF,by = list(year),FUN=sum)
 sj_585DIF <- aggregate(jssp585_DIF,by = list(year),FUN=sum)
 
 
 sDIF <- cbind(sj_126DIF$Group.1,sj_126DIF$x,sj_245DIF$x,sj_585DIF$x)
 colnames(sDIF) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sDIF <- data.frame(sDIF)
 sDIF <- sDIF[-1:-2,]
 #lm
 jsspDIF <- melt(sPM, id.vars ='Year', variable.name = 'Scenarios', value.name = 'DIF')  ##data.frame
 
 ggplot(jsspDIF, aes(x = Year, y =  DIF, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'Diffusive transport (gC/m2)')+
   theme_base()+
   theme(
     legend.position = c(0.9,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 
 ####EBUL#####
 
 jhis_EBUL <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_EBUL')
 jssp126_EBUL <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_EBUL')
 jssp245_EBUL <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_EBUL')
 jssp585_EBUL <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_EBUL')
 
 jhis_EBUL <- jhis_EBUL[1:60225]*3600*24*16
 jssp126_EBUL <- jssp126_EBUL[1:31390]*3600*24*16
 jssp245_EBUL <- jssp245_EBUL[1:31390]*3600*24*16
 jssp585_EBUL <- jssp585_EBUL[1:31390]*3600*24*16
 
 # jhis_EBUL <- apply(jhis_EBUL,2,sum)
 # jssp126_EBUL <- apply(jssp126_EBUL,2,sum)
 # jssp245_EBUL <- apply(jssp245_EBUL,2,sum)
 # jssp585_EBUL <- apply(jssp585_EBUL,2,sum)
 
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataEBUL <- data.frame(year,jssp126_EBUL,jssp245_EBUL,jssp585_EBUL)
 sj_hisEBUL <- aggregate(jhis_EBUL,by = list(Y1),FUN = sum)
 sj_126EBUL <- aggregate(jssp126_EBUL,by = list(year),FUN=sum)
 sj_245EBUL <- aggregate(jssp245_EBUL,by = list(year),FUN=sum)
 sj_585EBUL <- aggregate(jssp585_EBUL,by = list(year),FUN=sum)
 
 
 sEBUL <- cbind(sj_126EBUL$Group.1,sj_126EBUL$x,sj_245EBUL$x,sj_585EBUL$x)
 colnames(sEBUL) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sEBUL <- data.frame(sEBUL)
 sEBUL <- sEBUL[-1:-2,]
 #lm
 jsspEBU <- melt(sPM, id.vars ='Year', variable.name = 'Scenarios', value.name = 'EBU')  ##data.frame
 
 ggplot(jsspEBU, aes(x = Year, y =  EBU, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'Ebulition transport (gC/m2)')+
   theme_base()+
   theme(
     legend.position = c(0.9,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
##############
 ######TBOT#####
 
 jhis_t <- ncvar_get(nc = sj_his,varid = 'TBOT')
 jssp126_t <- ncvar_get(nc = sj_ssp126,varid = 'TBOT')
 jssp245_t <- ncvar_get(nc = sj_ssp245,varid = 'TBOT')
 jssp585_t <- ncvar_get(nc = sj_ssp585,varid = 'TBOT')
 
 jhis_t <- jhis_t[1:60225]
 jssp126_t <- jssp126_t[1:31390]
 jssp245_t <- jssp245_t[1:31390]
 jssp585_t <- jssp585_t[1:31390]
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdatat <- data.frame(year,jssp126_t,jssp245_t,jssp585_t)
 sj_hist <- aggregate(jhis_t,by = list(Y1),FUN = mean)
 sj_126t <- aggregate(jssp126_t,by = list(year),FUN=mean)
 sj_245t <- aggregate(jssp245_t,by = list(year),FUN=mean)
 sj_585t <- aggregate(jssp585_t,by = list(year),FUN=mean)
 
 
 st <- cbind(sj_126t$Group.1,sj_126t$x,sj_245t$x,sj_585t$x)
 colnames(st) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 st <- data.frame(st)
 st <- st[-1:-2,]
 #lm
 jssptbo <- melt(st, id.vars ='Year', variable.name = 'Scenarios', value.name = 'tbo')  ##data.frame
 
 ggplot(jssptbo, aes(x = Year, y =  tbo, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'TBOT (K)')+
   theme_base()+
   theme(
     legend.position = c(0.1,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 
 #####RAIN####
 
 jhis_RAIN <- ncvar_get(nc = sj_his,varid = 'RAIN')
 jssp126_RAIN <- ncvar_get(nc = sj_ssp126,varid = 'RAIN')
 jssp245_RAIN <- ncvar_get(nc = sj_ssp245,varid = 'RAIN')
 jssp585_RAIN <- ncvar_get(nc = sj_ssp585,varid = 'RAIN')
 
 jhis_RAIN <- jhis_RAIN[1:60225]*3600*24
 jssp126_RAIN <- jssp126_RAIN[1:31390]*3600*24
 jssp245_RAIN <- jssp245_RAIN[1:31390]*3600*24
 jssp585_RAIN <- jssp585_RAIN[1:31390]*3600*24
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataRAIN <- data.frame(year,jssp126_RAIN,jssp245_RAIN,jssp585_RAIN)
 sj_hisRAIN <- aggregate(jhis_RAIN,by = list(Y1),FUN = sum)
 sj_126RAIN <- aggregate(jssp126_RAIN,by = list(year),FUN=sum)
 sj_245RAIN <- aggregate(jssp245_RAIN,by = list(year),FUN=sum)
 sj_585RAIN <- aggregate(jssp585_RAIN,by = list(year),FUN=sum)
 
 
 sRAIN <- cbind(sj_126RAIN$Group.1,sj_126RAIN$x,sj_245RAIN$x,sj_585RAIN$x)
 colnames(sRAIN) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sRAIN <- data.frame(sRAIN)
 sRAIN <- sRAIN[-1:-2,]
 #lm
 jssprain <- melt(sRAIN, id.vars ='Year', variable.name = 'Scenarios', value.name = 'rain')  ##data.frame
 
 ggplot(jssprain, aes(x = Year, y =  rain, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'Rain (mm)')+
   theme_base()+
   theme(
     legend.position = c(0.1,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 
 ####SWC#####
 
 jhis_swc <- ncvar_get(nc = sj_his,varid = 'H2OSOI')
 jssp126_swc <- ncvar_get(nc = sj_ssp126,varid = 'H2OSOI')
 jssp245_swc <- ncvar_get(nc = sj_ssp245,varid = 'H2OSOI')
 jssp585_swc <- ncvar_get(nc = sj_ssp585,varid = 'H2OSOI')
 
 jhis_swc <- jhis_swc[1,1:60225] 
 jssp126_swc <- jssp126_swc[1,1:31390] 
 jssp245_swc <- jssp245_swc[1,1:31390] 
 jssp585_swc <- jssp585_swc[1,1:31390] 
 
 # jhis_swc <- apply(jhis_swc,2,sum)
 # jssp126_swc <- apply(jssp126_swc,2,sum)
 # jssp245_swc <- apply(jssp245_swc,2,sum)
 # jssp585_swc <- apply(jssp585_swc,2,sum)
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataswc <- data.frame(year,jssp126_swc,jssp245_swc,jssp585_swc)
 sj_hisswc <- aggregate(jhis_swc,by = list(Y1),FUN = mean)
 sj_126swc <- aggregate(jssp126_swc,by = list(year),FUN=mean)
 sj_245swc <- aggregate(jssp245_swc,by = list(year),FUN=mean)
 sj_585swc <- aggregate(jssp585_swc,by = list(year),FUN=mean)
 
 
 sswc <- cbind(sj_126swc$Group.1,sj_126swc$x,sj_245swc$x,sj_585swc$x)
 colnames(sswc) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sswc <- data.frame(sswc)
 sswc <- sswc[-1:-2,]
 #lm
 jsspswc <- melt(sswc, id.vars ='Year', variable.name = 'Scenarios', value.name = 'swc')  ##data.frame
 
 ggplot(jsspswc, aes(x = Year, y =  swc, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'swc (mm)')+
   theme_base()+
   theme(
     legend.position = c(0.1,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 
 ####sT####
 
 jhis_st <- ncvar_get(nc = sj_his,varid = 'TSOI_10CM')
 jssp126_st <- ncvar_get(nc = sj_ssp126,varid = 'TSOI_10CM')
 jssp245_st <- ncvar_get(nc = sj_ssp245,varid = 'TSOI_10CM')
 jssp585_st <- ncvar_get(nc = sj_ssp585,varid = 'TSOI_10CM')
 
 jhis_st <- jhis_st[1:60225] 
 jssp126_st <- jssp126_st[1:31390] 
 jssp245_st <- jssp245_st[1:31390] 
 jssp585_st <- jssp585_st[1:31390] 
 
 # jhis_st <- apply(jhis_st,2,sum)
 # jssp126_st <- apply(jssp126_st,2,sum)
 # jssp245_st <- apply(jssp245_st,2,sum)
 # jssp585_st <- apply(jssp585_st,2,sum)
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdatast <- data.frame(year,jssp126_st,jssp245_st,jssp585_st)
 sj_hisst <- aggregate(jhis_st,by = list(Y1),FUN = mean)
 sj_126st <- aggregate(jssp126_st,by = list(year),FUN=mean)
 sj_245st <- aggregate(jssp245_st,by = list(year),FUN=mean)
 sj_585st <- aggregate(jssp585_st,by = list(year),FUN=mean)
 
 
 sst <- cbind(sj_126st$Group.1,sj_126st$x,sj_245st$x,sj_585st$x)
 colnames(sst) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sst <- data.frame(sst)
 sst <- sst[-1:-2,]
 #lm
 jsspst <- melt(sst, id.vars ='Year', variable.name = 'Scenarios', value.name = 'st')  ##data.frame
 
 ggplot(jsspst, aes(x = Year, y =  st, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'Soil teperature (K)')+
   theme_base()+
   theme(
     legend.position = c(0.1,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )

 ######LAI######
 
 jhis_LAI <- ncvar_get(nc = sj_his,varid = 'TLAI')
 jssp126_LAI <- ncvar_get(nc = sj_ssp126,varid = 'TLAI')
 jssp245_LAI <- ncvar_get(nc = sj_ssp245,varid = 'TLAI')
 jssp585_LAI <- ncvar_get(nc = sj_ssp585,varid = 'TLAI')
 
 jhis_LAI <- jhis_LAI[1:60225] 
 jssp126_LAI <- jssp126_LAI[1:31390] 
 jssp245_LAI <- jssp245_LAI[1:31390] 
 jssp585_LAI <- jssp585_LAI[1:31390] 
 
 # jhis_LAI <- apply(jhis_LAI,2,sum)
 # jssp126_LAI <- apply(jssp126_LAI,2,sum)
 # jssp245_LAI <- apply(jssp245_LAI,2,sum)
 # jssp585_LAI <- apply(jssp585_LAI,2,sum)
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataLAI <- data.frame(year,jssp126_LAI,jssp245_LAI,jssp585_LAI)
 sj_hisLAI <- aggregate(jhis_LAI,by = list(Y1),FUN = mean)
 sj_126LAI <- aggregate(jssp126_LAI,by = list(year),FUN=mean)
 sj_245LAI <- aggregate(jssp245_LAI,by = list(year),FUN=mean)
 sj_585LAI <- aggregate(jssp585_LAI,by = list(year),FUN=mean)
 
 
 sLAI <- cbind(sj_126LAI$Group.1,sj_126LAI$x,sj_245LAI$x,sj_585LAI$x)
 colnames(sLAI) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sLAI <- data.frame(sLAI)
 sLAI <- sLAI[-1:-2,]
 #lm
 jsspLAI <- melt(sLAI, id.vars ='Year', variable.name = 'Scenarios', value.name = 'LAI')  ##data.frame
 
 ggplot(jsspLAI, aes(x = Year, y =  LAI, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'LAI')+
   theme_base()+
   theme(
     legend.position = c(0.1,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
#####QSOIL###### 
 
 jhis_QSOIL <- ncvar_get(nc = sj_his,varid = 'QSOIL')
 jssp126_QSOIL <- ncvar_get(nc = sj_ssp126,varid = 'QSOIL')
 jssp245_QSOIL <- ncvar_get(nc = sj_ssp245,varid = 'QSOIL')
 jssp585_QSOIL <- ncvar_get(nc = sj_ssp585,varid = 'QSOIL')
 
 jhis_QSOIL <- jhis_QSOIL[1:60225]*3600*24
 jssp126_QSOIL <- jssp126_QSOIL[1:31390]*3600*24
 jssp245_QSOIL <- jssp245_QSOIL[1:31390]*3600*24
 jssp585_QSOIL <- jssp585_QSOIL[1:31390]*3600*24
 
 # jhis_QSOIL <- apply(jhis_QSOIL,2,sum)
 # jssp126_QSOIL <- apply(jssp126_QSOIL,2,sum)
 # jssp245_QSOIL <- apply(jssp245_QSOIL,2,sum)
 # jssp585_QSOIL <- apply(jssp585_QSOIL,2,sum)
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataQSOIL <- data.frame(year,jssp126_QSOIL,jssp245_QSOIL,jssp585_QSOIL)
 sj_hisQSOIL <- aggregate(jhis_QSOIL,by = list(Y1),FUN = mean)
 sj_126QSOIL <- aggregate(jssp126_QSOIL,by = list(year),FUN=mean)
 sj_245QSOIL <- aggregate(jssp245_QSOIL,by = list(year),FUN=mean)
 sj_585QSOIL <- aggregate(jssp585_QSOIL,by = list(year),FUN=mean)
 
 
 sQSOIL <- cbind(sj_126QSOIL$Group.1,sj_126QSOIL$x,sj_245QSOIL$x,sj_585QSOIL$x)
 colnames(sQSOIL) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sQSOIL <- data.frame(sQSOIL)
 sQSOIL <- sQSOIL[-1:-2,]
 #lm
 jsspQSOIL <- melt(sQSOIL, id.vars ='Year', variable.name = 'Scenarios', value.name = 'QSOIL')  ##data.frame
 
 ggplot(jsspQSOIL, aes(x = Year, y =  QSOIL, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'Evaporation (mm)')+
   theme_base()+
   theme(
     legend.position = c(0.9,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 ######QVEGE######
 jhis_QVEGE <- ncvar_get(nc = sj_his,varid = 'QVEGE')
 jssp126_QVEGE <- ncvar_get(nc = sj_ssp126,varid = 'QVEGE')
 jssp245_QVEGE <- ncvar_get(nc = sj_ssp245,varid = 'QVEGE')
 jssp585_QVEGE <- ncvar_get(nc = sj_ssp585,varid = 'QVEGE')
 
 jhis_QVEGE <- jhis_QVEGE[1:60225]*3600*24
 jssp126_QVEGE <- jssp126_QVEGE[1:31390]*3600*24
 jssp245_QVEGE <- jssp245_QVEGE[1:31390]*3600*24
 jssp585_QVEGE <- jssp585_QVEGE[1:31390]*3600*24
 
 # jhis_QVEGE <- apply(jhis_QVEGE,2,sum)
 # jssp126_QVEGE <- apply(jssp126_QVEGE,2,sum)
 # jssp245_QVEGE <- apply(jssp245_QVEGE,2,sum)
 # jssp585_QVEGE <- apply(jssp585_QVEGE,2,sum)
 
 Y1 <- rep(1850:2014,each = 365)
 year <- rep(2014:2099,each = 365)
 sspdataQVEGE <- data.frame(year,jssp126_QVEGE,jssp245_QVEGE,jssp585_QVEGE)
 sj_hisQVEGE <- aggregate(jhis_QVEGE,by = list(Y1),FUN = mean)
 sj_126QVEGE <- aggregate(jssp126_QVEGE,by = list(year),FUN=mean)
 sj_245QVEGE <- aggregate(jssp245_QVEGE,by = list(year),FUN=mean)
 sj_585QVEGE <- aggregate(jssp585_QVEGE,by = list(year),FUN=mean)
 
 
 sQVEGE <- cbind(sj_126QVEGE$Group.1,sj_126QVEGE$x,sj_245QVEGE$x,sj_585QVEGE$x)
 colnames(sQVEGE) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
 sQVEGE <- data.frame(sQVEGE)
 sQVEGE <- sQVEGE[-1:-2,]
 #lm
 jsspQVEGE <- melt(sQVEGE, id.vars ='Year', variable.name = 'Scenarios', value.name = 'QVEGE')  ##data.frame
 
 ggplot(jsspQVEGE, aes(x = Year, y =  QVEGE, color = Scenarios))+
   geom_line(size = 2)+
   labs(x = 'Year',y = 'Evaporation (mm)')+
   theme_base()+
   theme(
     legend.position = c(0.1,0.85),
     legend.title = element_blank(),
     legend.text=element_text(colour='black',size=22,face="bold"),
     axis.text.x=element_text(size=22,colour='black' ),
     # axis.text.x.top = element_text(size = 22,face = 'bold'),
     axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
     axis.title.y = element_text(size = 22,face = 'bold'),
     axis.title.x = element_text(size = 22,face = 'bold'),
     # axis.title.x.top = element_text(size = 22,face = 'bold'),
     axis.ticks=element_line(size=0.5),
     plot.background=element_blank()
   )
 
###############
nc_close(sj_his)
nc_close(sj_ssp126)
nc_close(sj_ssp245)
nc_close(sj_ssp585)

####cbs####
cbs_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbs_his_1850-2015.nc')
cbs_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp126-2014-2100.nc')
cbs_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp245-2014-2100.nc')
cbs_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp585-2014-2100.nc')

chis_ch4 <- ncvar_get(nc = cbs_his,varid = 'CH4_SURF_NETFLUX')
cssp126_ch4 <- ncvar_get(nc = cbs_ssp126,varid = 'CH4_SURF_NETFLUX')
cssp245_ch4 <- ncvar_get(nc = cbs_ssp245,varid = 'CH4_SURF_NETFLUX')
cssp585_ch4 <- ncvar_get(nc = cbs_ssp585,varid = 'CH4_SURF_NETFLUX')
chis_ch4 <- chis_ch4[1:60225]*3600*24*16
cssp126_ch4 <- cssp126_ch4[1:31390]*3600*24*16
cssp245_ch4 <- cssp245_ch4[1:31390]*3600*24*16
cssp585_ch4 <- cssp585_ch4[1:31390]*3600*24*16
year <- rep(2014:2099,each = 365)
csspdata <- data.frame(year,cssp126_ch4,cssp245_ch4,cssp585_ch4)
cbs_126 <- aggregate(jssp126_ch4,by = list(year),FUN=sum)
cbs_245 <- aggregate(jssp245_ch4,by = list(year),FUN=sum)
cbs_585 <- aggregate(jssp585_ch4,by = list(year),FUN=sum)

cch4 <- cbind(cbs_126$Group.1,cbs_126$x,cbs_245$x,cbs_585$x)
colnames(cch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
cch4 <- data.frame(cch4)
cch4 <- cch4[-1,]
cssp <- melt(cch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

ggplot(cssp, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  theme_base()+
  theme()





nc_close(cbs_his)
nc_close(cbs_ssp126)
nc_close(cbs_ssp245)
nc_close(cbs_ssp585)




####xxal####
xxal_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxal_his_1850-2015.nc')
xxal_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp126_2014-2100.nc')
xxal_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp245_2014-2100.nc')
xxal_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/5852xxal.nc')








xhis_ch4 <- ncvar_get(nc = xxal_his,varid = 'CH4_SURF_NETFLUX')
xssp126_ch4 <- ncvar_get(nc = xxal_ssp126,varid = 'CH4_SURF_NETFLUX')
xssp245_ch4 <- ncvar_get(nc = xxal_ssp245,varid = 'CH4_SURF_NETFLUX')
xssp585_ch4 <- ncvar_get(nc = xxal_ssp585,varid = 'CH4_SURF_NETFLUX')
xhis_ch4 <- xhis_ch4[1:60225]*3600*24*16
xssp126_ch4 <- xssp126_ch4[1:31390]*3600*24*16
xssp245_ch4 <- xssp245_ch4[1:31390]*3600*24*16
xssp585_ch4 <- xssp585_ch4[1:31390]*3600*24*16
year <- rep(2014:2099,each = 365)
xsspdata <- data.frame(year,xssp126_ch4,xssp245_ch4,xssp585_ch4)
x_126 <- aggregate(xssp126_ch4,by = list(year),FUN=sum)
x_245 <- aggregate(xssp245_ch4,by = list(year),FUN=sum)
x_585 <- aggregate(xssp585_ch4,by = list(year),FUN=sum)


xch4 <- cbind(x_126$Group.1,x_126$x,x_245$x,x_585$x)
colnames(xch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
xch4 <- data.frame(xch4)
xch4 <- xch4[-1,]
xssp <- melt(xch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

ggplot(xssp, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  theme_base()+
  theme()

plot


nc_close(xxal_his)
nc_close(xxal_ssp126)
nc_close(xxal_ssp245)
nc_close(xxal_ssp585)

