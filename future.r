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
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj1261.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj245.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj585.nc')


jhis_npp <- ncvar_get(nc = sj_his,varid = 'NPP')
jssp126_npp <- ncvar_get(nc = sj_ssp126,varid = 'NPP')
jssp245_npp <- ncvar_get(nc = sj_ssp245,varid = 'NPP')
jssp585_npp <- ncvar_get(nc = sj_ssp585,varid = 'NPP')

jhis_npp <- jhis_npp[1:60225]*3600*24*16
jssp126_npp <- jssp126_npp[1:31390]*3600*24*16
jssp245_npp <- jssp245_npp[1:31390]*3600*24*16
jssp585_npp <- jssp585_npp[1:31390]*3600*24*16
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
  theme_base()+
  theme(
    legend.position = c(0.1,0.8)
  )
####DOC####

jhis_doc <- ncvar_get(nc = sj_his,varid = 'CDOCS')
jssp126_doc <- ncvar_get(nc = sj_ssp126,varid = 'CDOCS')
jssp245_doc <- ncvar_get(nc = sj_ssp245,varid = 'CDOCS')
jssp585_doc <- ncvar_get(nc = sj_ssp585,varid = 'CDOCS')

jhis_doc <- jhis_doc[1,1:60225]
jssp126_doc <- jssp126_doc[1,1:31390]
jssp245_doc <- jssp245_doc[1,1:31390]
jssp585_doc <- jssp585_doc[1,1:31390]
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
  theme_base()+
  theme(
    legend.position = c(0.9,0.2)
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
  theme_base()+
  theme(
    legend.position = c(0.9,0.8)
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
# coloured barplot
ggplot(bar_data, aes(x = factor(year), y = mean, fill = Scenarios, colour = Scenarios)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x=" ", y="Methane") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.9, 0.75)) +
  geom_text(aes(label=Tukey), position = position_dodge(0.90), size = 3, 
            vjust=-0.8, hjust=-0.5, colour = "gray25") 
  # ylim(0, 1500) +
  # geom_text(aes(label=Scenarios, y = 25), position = position_dodge(0.90))




####ACE####

jhis_ace <- ncvar_get(nc = sj_his,varid = 'CACES_PROD')
jssp126_ace <- ncvar_get(nc = sj_ssp126,varid = 'CACES_PROD')
jssp245_ace <- ncvar_get(nc = sj_ssp245,varid = 'CACES_PROD')
jssp585_ace <- ncvar_get(nc = sj_ssp585,varid = 'CACES_PROD')

jhis_ace <- jhis_ace[1,1:60225]
jssp126_ace <- jssp126_ace[1,1:31390]
jssp245_ace <- jssp245_ace[1,1:31390]
jssp585_ace <- jssp585_ace[1,1:31390]
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
  theme_base()+
  theme(
    legend.position = c(0.1,0.8)
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

##### CACEBIOS #####

jhis_ab <- ncvar_get(nc = sj_his,varid = 'CACEBIOS')
jssp126_ab <- ncvar_get(nc = sj_ssp126,varid = 'CACEBIOS')
jssp245_ab <- ncvar_get(nc = sj_ssp245,varid = 'CACEBIOS')
jssp585_ab <- ncvar_get(nc = sj_ssp585,varid = 'CACEBIOS')

jhis_ab <- jhis_ab[1,1:60225]
jssp126_ab <- jssp126_ab[1,1:31390]
jssp245_ab <- jssp245_ab[1,1:31390]
jssp585_ab <- jssp585_ab[1,1:31390]
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
  theme_base()+
  theme(
    legend.position = c(0.1,0.8)
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

jhis_HM <- jhis_HM[1,1:60225]
jssp126_HM <- jssp126_HM[1,1:31390]
jssp245_HM <- jssp245_HM[1,1:31390]
jssp585_HM <- jssp585_HM[1,1:31390]
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
  theme_base()+
  theme(
    legend.position = c(0.1,0.8)
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

jhis_AER <- jhis_AER[1:8,1:60225]
jssp126_AER <- jssp126_AER[1:8,1:31390]
jssp245_AER <- jssp245_AER[1:8,1:31390]
jssp585_AER <- jssp585_AER[1:8,1:31390]

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
  theme_base()+
  theme(
    legend.position = c(0.1,0.8)
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
 
 jhis_ANAER <- jhis_ANAER[1:8,1:60225]
 jssp126_ANAER <- jssp126_ANAER[1:8,1:31390]
 jssp245_ANAER <- jssp245_ANAER[1:8,1:31390]
 jssp585_ANAER <- jssp585_ANAER[1:8,1:31390]
 
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
   theme_base()+
   theme(
     legend.position = 'none'
   )
 ggplot(sj_hisANAER_2000_2014 ,aes(x =Year,y = ANAER ))+
   geom_line(size =2)+
   theme_base()+
   theme()
 
 
 
 
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

