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
snpp <- snpp[-1:-2,]

#lm
lm(SSP1.2.6~Year,data = snpp)
lm(SSP2.4.5~Year,data = snpp)
lm(SSP5.8.5~Year,data = snpp)

####SOM####

jhis_som <- ncvar_get(nc = sj_his,varid = 'TOTSOMC')
jssp126_som <- ncvar_get(nc = sj_ssp126,varid = 'TOTSOMC')
jssp245_som <- ncvar_get(nc = sj_ssp245,varid = 'TOTSOMC')
jssp585_som <- ncvar_get(nc = sj_ssp585,varid = 'TOTSOMC')

jhis_som <- jhis_som[1:60225]
jssp126_som <- jssp126_som[1:31390]
jssp245_som <- jssp245_som[1:31390]
jssp585_som <- jssp585_som[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatasom <- data.frame(year,jssp126_som,jssp245_som,jssp585_som)
sj_hissom <- aggregate(jhis_som,by = list(Y1),FUN = mean)
sj_126som <- aggregate(jssp126_som,by = list(year),FUN=mean)
sj_245som <- aggregate(jssp245_som,by = list(year),FUN=mean)
sj_585som <- aggregate(jssp585_som,by = list(year),FUN=mean)


ssom <- cbind(sj_126som$Group.1,sj_126som$x,sj_245som$x,sj_585som$x)
colnames(ssom) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
ssom <- data.frame(ssom)
ssom <- ssom[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = ssom)
lm(SSP2.4.5~Year,data = ssom)
lm(SSP5.8.5~Year,data = ssom)
####DOC####

jhis_doc <- ncvar_get(nc = sj_his,varid = 'CDOCS')
jssp126_doc <- ncvar_get(nc = sj_ssp126,varid = 'CDOCS')
jssp245_doc <- ncvar_get(nc = sj_ssp245,varid = 'CDOCS')
jssp585_doc <- ncvar_get(nc = sj_ssp585,varid = 'CDOCS')

jhis_doc <- jhis_doc[1:60225]
jssp126_doc <- jssp126_doc[1:31390]
jssp245_doc <- jssp245_doc[1:31390]
jssp585_doc <- jssp585_doc[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdatadoc <- data.frame(year,jssp126_doc,jssp245_doc,jssp585_doc)
sj_hisdoc <- aggregate(jhis_doc,by = list(Y1),FUN = mean)
sj_126doc <- aggregate(jssp126_doc,by = list(year),FUN=mean)
sj_245doc <- aggregate(jssp245_doc,by = list(year),FUN=mean)
sj_585doc <- aggregate(jssp585_doc,by = list(year),FUN=mean)


sdoc <- cbind(sj_126doc$Group.1,sj_126doc$x,sj_245doc$x,sj_585doc$x)
colnames(sdoc) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sdoc <- data.frame(sdoc)
sdoc <- sdoc[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sdoc)
lm(SSP2.4.5~Year,data = sdoc)
lm(SSP5.8.5~Year,data = sdoc)
#####SOM->ACE####

jhis_ap <- ncvar_get(nc = sj_his,varid = 'CACES_PROD')
jssp126_ap <- ncvar_get(nc = sj_ssp126,varid = 'CACES_PROD')
jssp245_ap <- ncvar_get(nc = sj_ssp245,varid = 'CACES_PROD')
jssp585_ap <- ncvar_get(nc = sj_ssp585,varid = 'CACES_PROD')

jhis_ap <- jhis_ap[1:60225]*3600*24*16
jssp126_ap <- jssp126_ap[1:31390]*3600*24*16
jssp245_ap <- jssp245_ap[1:31390]*3600*24*16
jssp585_ap <- jssp585_ap[1:31390]*3600*24*16
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataap <- data.frame(year,jssp126_ap,jssp245_ap,jssp585_ap)
sj_hisap <- aggregate(jhis_ap,by = list(Y1),FUN = sum)
sj_126ap <- aggregate(jssp126_ap,by = list(year),FUN=sum)
sj_245ap <- aggregate(jssp245_ap,by = list(year),FUN=sum)
sj_585ap <- aggregate(jssp585_ap,by = list(year),FUN=sum)


sap <- cbind(sj_126ap$Group.1,sj_126ap$x,sj_245ap$x,sj_585ap$x)
colnames(sap) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sap <- data.frame(sap)
sap <- sap[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sap)
lm(SSP2.4.5~Year,data = sap)
lm(SSP5.8.5~Year,data = sap)

####CACES####

jhis_ACE <- ncvar_get(nc = sj_his,varid = 'CACES')
jssp126_ACE <- ncvar_get(nc = sj_ssp126,varid = 'CACES')
jssp245_ACE <- ncvar_get(nc = sj_ssp245,varid = 'CACES')
jssp585_ACE <- ncvar_get(nc = sj_ssp585,varid = 'CACES')

jhis_ACE <- jhis_ACE[1:60225]
jssp126_ACE <- jssp126_ACE[1:31390]
jssp245_ACE <- jssp245_ACE[1:31390]
jssp585_ACE <- jssp585_ACE[1:31390]
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataACE <- data.frame(year,jssp126_ACE,jssp245_ACE,jssp585_ACE)
sj_hisACE <- aggregate(jhis_ACE,by = list(Y1),FUN = sum)
sj_126ACE <- aggregate(jssp126_ACE,by = list(year),FUN=sum)
sj_245ACE <- aggregate(jssp245_ACE,by = list(year),FUN=sum)
sj_585ACE <- aggregate(jssp585_ACE,by = list(year),FUN=sum)


sACE <- cbind(sj_126ACE$Group.1,sj_126ACE$x,sj_245ACE$x,sj_585ACE$x)
colnames(sACE) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sACE <- data.frame(sACE)
sACE <- sACE[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sACE)
lm(SSP2.4.5~Year,data = sACE)
lm(SSP5.8.5~Year,data = sACE)

##### CH4_PROD_ACE_DEPTH ####

jhis_CPA <- ncvar_get(nc = sj_his,varid = 'CH4_PROD_ACE_DEPTH')
jssp126_CPA <- ncvar_get(nc = sj_ssp126,varid = 'CH4_PROD_ACE_DEPTH')
jssp245_CPA <- ncvar_get(nc = sj_ssp245,varid = 'CH4_PROD_ACE_DEPTH')
jssp585_CPA <- ncvar_get(nc = sj_ssp585,varid = 'CH4_PROD_ACE_DEPTH')

jhis_CPA <- jhis_CPA[1:60225]*3600*24*16
jssp126_CPA <- jssp126_CPA[1:31390]*3600*24*16
jssp245_CPA <- jssp245_CPA[1:31390]*3600*24*16
jssp585_CPA <- jssp585_CPA[1:31390]*3600*24*16
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataCPA <- data.frame(year,jssp126_CPA,jssp245_CPA,jssp585_CPA)
sj_hisCPA <- aggregate(jhis_CPA,by = list(Y1),FUN = sum)
sj_126CPA <- aggregate(jssp126_CPA,by = list(year),FUN=sum)
sj_245CPA <- aggregate(jssp245_CPA,by = list(year),FUN=sum)
sj_585CPA <- aggregate(jssp585_CPA,by = list(year),FUN=sum)


sCPA <- cbind(sj_126CPA$Group.1,sj_126CPA$x,sj_245CPA$x,sj_585CPA$x)
colnames(sCPA) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sCPA <- data.frame(sCPA)
sCPA <- sCPA[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sCPA)
lm(SSP2.4.5~Year,data = sCPA)
lm(SSP5.8.5~Year,data = sCPA)

####CH4_PROD_CO2_DEPTH####

jhis_CPC <- ncvar_get(nc = sj_his,varid = 'CH4_PROD_CO2_DEPTH')
jssp126_CPC <- ncvar_get(nc = sj_ssp126,varid = 'CH4_PROD_CO2_DEPTH')
jssp245_CPC <- ncvar_get(nc = sj_ssp245,varid = 'CH4_PROD_CO2_DEPTH')
jssp585_CPC <- ncvar_get(nc = sj_ssp585,varid = 'CH4_PROD_CO2_DEPTH')

jhis_CPC <- jhis_CPC[1:60225]*3600*24*16
jssp126_CPC <- jssp126_CPC[1:31390]*3600*24*16
jssp245_CPC <- jssp245_CPC[1:31390]*3600*24*16
jssp585_CPC <- jssp585_CPC[1:31390]*3600*24*16
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataCPC <- data.frame(year,jssp126_CPC,jssp245_CPC,jssp585_CPC)
sj_hisCPC <- aggregate(jhis_CPC,by = list(Y1),FUN = sum)
sj_126CPC <- aggregate(jssp126_CPC,by = list(year),FUN=sum)
sj_245CPC <- aggregate(jssp245_CPC,by = list(year),FUN=sum)
sj_585CPC <- aggregate(jssp585_CPC,by = list(year),FUN=sum)


sCPC <- cbind(sj_126CPC$Group.1,sj_126CPC$x,sj_245CPC$x,sj_585CPC$x)
colnames(sCPC) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sCPC <- data.frame(sCPC)
sCPC <- sCPC[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sCPC)
lm(SSP2.4.5~Year,data = sCPC)
lm(SSP5.8.5~Year,data = sCPC)

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
#lm
lm(SSP1.2.6~Year,data = sCCH4)
lm(SSP2.4.5~Year,data = sCCH4)
lm(SSP5.8.5~Year,data = sCCH4)

####AM ####

jhis_AM <- ncvar_get(nc = sj_his,varid = 'CACEBIOS')
jssp126_AM <- ncvar_get(nc = sj_ssp126,varid = 'CACEBIOS')
jssp245_AM <- ncvar_get(nc = sj_ssp245,varid = 'CACEBIOS')
jssp585_AM <- ncvar_get(nc = sj_ssp585,varid = 'CACEBIOS')

jhis_AM <- jhis_AM[1:15,1:60225]
jssp126_AM <- jssp126_AM[1:15,1:31390]
jssp245_AM <- jssp245_AM[1:15,1:31390]
jssp585_AM <- jssp585_AM[1:15,1:31390]

jhis_AM <- apply(jhis_AM,2,sum)
jssp126_AM <- apply(jssp126_AM,2,sum)
jssp245_AM <- apply(jssp245_AM,2,sum)
jssp585_AM <- apply(jssp585_AM,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataAM <- data.frame(year,jssp126_AM,jssp245_AM,jssp585_AM)
sj_hisAM <- aggregate(jhis_AM,by = list(Y1),FUN = sum)
sj_126AM <- aggregate(jssp126_AM,by = list(year),FUN=sum)
sj_245AM <- aggregate(jssp245_AM,by = list(year),FUN=sum)
sj_585AM <- aggregate(jssp585_AM,by = list(year),FUN=sum)


sAM <- cbind(sj_126AM$Group.1,sj_126AM$x,sj_245AM$x,sj_585AM$x)
colnames(sAM) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sAM <- data.frame(sAM)
sAM <- sAM[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sAM)
lm(SSP2.4.5~Year,data = sAM)
lm(SSP5.8.5~Year,data = sAM)


####MM####

jhis_MM <- ncvar_get(nc = sj_his,varid = 'CCO2BIOS')
jssp126_MM <- ncvar_get(nc = sj_ssp126,varid = 'CCO2BIOS')
jssp245_MM <- ncvar_get(nc = sj_ssp245,varid = 'CCO2BIOS')
jssp585_MM <- ncvar_get(nc = sj_ssp585,varid = 'CCO2BIOS')

jhis_MM <- jhis_MM[1:15,1:60225]
jssp126_MM <- jssp126_MM[1:15,1:31390]
jssp245_MM <- jssp245_MM[1:15,1:31390]
jssp585_MM <- jssp585_MM[1:15,1:31390]

jhis_MM <- apply(jhis_MM,2,sum)
jssp126_MM <- apply(jssp126_MM,2,sum)
jssp245_MM <- apply(jssp245_MM,2,sum)
jssp585_MM <- apply(jssp585_MM,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataMM <- data.frame(year,jssp126_MM,jssp245_MM,jssp585_MM)
sj_hisMM <- aggregate(jhis_MM,by = list(Y1),FUN = sum)
sj_126MM <- aggregate(jssp126_MM,by = list(year),FUN=sum)
sj_245MM <- aggregate(jssp245_MM,by = list(year),FUN=sum)
sj_585MM <- aggregate(jssp585_MM,by = list(year),FUN=sum)


sMM <- cbind(sj_126MM$Group.1,sj_126MM$x,sj_245MM$x,sj_585MM$x)
colnames(sMM) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sMM <- data.frame(sMM)
sMM <- sMM[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sMM)
lm(SSP2.4.5~Year,data = sMM)
lm(SSP5.8.5~Year,data = sMM)

####AER--CAERCH4BIOS####

jhis_AER <- ncvar_get(nc = sj_his,varid = 'CAERCH4BIOS')
jssp126_AER <- ncvar_get(nc = sj_ssp126,varid = 'CAERCH4BIOS')
jssp245_AER <- ncvar_get(nc = sj_ssp245,varid = 'CAERCH4BIOS')
jssp585_AER <- ncvar_get(nc = sj_ssp585,varid = 'CAERCH4BIOS')

jhis_AER <- jhis_AER[1:15,1:60225]
jssp126_AER <- jssp126_AER[1:15,1:31390]
jssp245_AER <- jssp245_AER[1:15,1:31390]
jssp585_AER <- jssp585_AER[1:15,1:31390]

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


sAER <- cbind(sj_126AER$Group.1,sj_126AER$x,sj_245AER$x,sj_585AER$x)
colnames(sAER) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sAER <- data.frame(sAER)
sAER <- sAER[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sAER)
lm(SSP2.4.5~Year,data = sAER)
lm(SSP5.8.5~Year,data = sAER)

####ANAER####

jhis_ANAER <- ncvar_get(nc = sj_his,varid = 'CANAERCH4BIOS')
jssp126_ANAER <- ncvar_get(nc = sj_ssp126,varid = 'CANAERCH4BIOS')
jssp245_ANAER <- ncvar_get(nc = sj_ssp245,varid = 'CANAERCH4BIOS')
jssp585_ANAER <- ncvar_get(nc = sj_ssp585,varid = 'CANAERCH4BIOS')

jhis_ANAER <- jhis_ANAER[1:15,1:60225]
jssp126_ANAER <- jssp126_ANAER[1:15,1:31390]
jssp245_ANAER <- jssp245_ANAER[1:15,1:31390]
jssp585_ANAER <- jssp585_ANAER[1:15,1:31390]

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


sANAER <- cbind(sj_126ANAER$Group.1,sj_126ANAER$x,sj_245ANAER$x,sj_585ANAER$x)
colnames(sANAER) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sANAER <- data.frame(sANAER)
sANAER <- sANAER[-1:-2,]
#lm
lm(SSP1.2.6~Year,data = sANAER)
lm(SSP2.4.5~Year,data = sANAER)
lm(SSP5.8.5~Year,data = sANAER)

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
#lm
lm(SSP1.2.6~Year,data = sPM)
lm(SSP2.4.5~Year,data = sPM)
lm(SSP5.8.5~Year,data = sPM)

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
lm(SSP1.2.6~Year,data = sDIF)
lm(SSP2.4.5~Year,data = sDIF)
lm(SSP5.8.5~Year,data = sDIF)

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
lm(SSP1.2.6~Year,data = sEBUL)
lm(SSP2.4.5~Year,data = sEBUL)
lm(SSP5.8.5~Year,data = sEBUL)


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
lm(SSP1.2.6~Year,data = st)
lm(SSP2.4.5~Year,data = st)
lm(SSP5.8.5~Year,data = st)

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
lm(SSP1.2.6~Year,data = sRAIN)
lm(SSP2.4.5~Year,data = sRAIN)
lm(SSP5.8.5~Year,data = sRAIN)

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
lm(SSP1.2.6~Year,data = sswc)
lm(SSP2.4.5~Year,data = sswc)
lm(SSP5.8.5~Year,data = sswc)

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
lm(SSP1.2.6~Year,data = sst)
lm(SSP2.4.5~Year,data = sst)
lm(SSP5.8.5~Year,data = sst)


