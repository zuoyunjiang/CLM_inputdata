#### transport ####
library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(multcompView)
library(cowplot)
library(showtext)
####FUN###
# mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
####sanjiang####
sj_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj_his_1850-2015.nc')
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp124-2014-2100.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp245-2014-2100.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp585-2014-2100.nc')


#####PLANT-MEDIATED#######

jhis_PM <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_AERE')
jssp126_PM <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_AERE')
jssp245_PM <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_AERE')
jssp585_PM <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_AERE')
# ncatt_get()

jhis_PM <- jhis_PM[1:60225]*3600*24
jssp126_PM <- jssp126_PM[1:31390]*3600*24
jssp245_PM <- jssp245_PM[1:31390]*3600*24
jssp585_PM <- jssp585_PM[1:31390]*3600*24

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


####DIFFUSIVE######

jhis_DIF <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_DIFF')
jssp126_DIF <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_DIFF')
jssp245_DIF <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_DIFF')
jssp585_DIF <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_DIFF')

jhis_DIF <- jhis_DIF[1:60225]*3600*24
jssp126_DIF <- jssp126_DIF[1:31390]*3600*24
jssp245_DIF <- jssp245_DIF[1:31390]*3600*24
jssp585_DIF <- jssp585_DIF[1:31390]*3600*24
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


####EBUL#####

jhis_EBUL <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_EBUL')
jssp126_EBUL <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_EBUL')
jssp245_EBUL <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_EBUL')
jssp585_EBUL <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_EBUL')

jhis_EBUL <- jhis_EBUL[1:60225]*3600*24
jssp126_EBUL <- jssp126_EBUL[1:31390]*3600*24
jssp245_EBUL <- jssp245_EBUL[1:31390]*3600*24
jssp585_EBUL <- jssp585_EBUL[1:31390]*3600*24

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

####PDE####16-25
# sPM
# sDIF
# sEBUL
SJ_PLANT <- apply(sPM[1:10,],2,FUN = mean)
SJ_DIF <- apply(sDIF[1:10,],2,FUN = mean)
SJ_EBUL<- apply(sEBUL[1:10,],2,FUN = mean)
SJ_TRANS <- rbind(SJ_PLANT,SJ_DIF,SJ_EBUL)

SJ_TRANS <- as.data.frame(SJ_TRANS)
SJ_TRANS$TYPE <- c("Plant","Diffusion", 'Ebuliton')
#####sj-126#####
data <- data.frame(
  group = SJ_TRANS$TYPE,
  value = SJ_TRANS$SSP1.2.6
)
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data$percent <- paste(round(data$prop,2),'%',sep = '')

# Basic piechart
sjpie126 <- ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")
#####sj-245#####
data2 <- data.frame(
  group = SJ_TRANS$TYPE,
  value = SJ_TRANS$SSP2.4.5
)
data2 <- data2 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data2$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data2$percent <- paste(round(data2$prop,2),'%',sep = '')

# Basic piechart
sjpie245 <- ggplot(data2, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

######sj-585######
data3 <- data.frame(
  group = SJ_TRANS$TYPE,
  value = SJ_TRANS$SSP5.8.5
)
data3 <- data3 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data3$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data3$percent <- paste(round(data3$prop,2),'%',sep = '')

# Basic piechart
sjpie585 <- ggplot(data3, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")


nc_close(sj_his)
nc_close(sj_ssp126)
nc_close(sj_ssp245)
nc_close(sj_ssp585)

######cbs#####
cbs_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbs_his_1850-2015.nc')
cbs_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp126-2014-2100.nc')
cbs_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp245-2014-2100.nc')
cbs_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp585-2014-2100.nc')


cbshis_PM <- ncvar_get(nc = cbs_his,varid = 'CH4_SURF_AERE')
cbsssp126_PM <- ncvar_get(nc = cbs_ssp126,varid = 'CH4_SURF_AERE')
cbsssp245_PM <- ncvar_get(nc = cbs_ssp245,varid = 'CH4_SURF_AERE')
cbsssp585_PM <- ncvar_get(nc = cbs_ssp585,varid = 'CH4_SURF_AERE')
# ncatt_get()

cbshis_PM <- cbshis_PM[1:60225]*3600*24
cbsssp126_PM <- cbsssp126_PM[1:31390]*3600*24
cbsssp245_PM <- cbsssp245_PM[1:31390]*3600*24
cbsssp585_PM <- cbsssp585_PM[1:31390]*3600*24

# jhis_PM <- apply(jhis_PM,2,sum)
# jssp126_PM <- apply(jssp126_PM,2,sum)
# jssp245_PM <- apply(jssp245_PM,2,sum)
# jssp585_PM <- apply(jssp585_PM,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataPM <- data.frame(year,cbsssp126_PM,cbsssp245_PM,cbsssp585_PM)
cbs_hisPM <- aggregate(cbshis_PM,by = list(Y1),FUN = sum)
cbs_126PM <- aggregate(cbsssp126_PM,by = list(year),FUN=sum)
cbs_245PM <- aggregate(cbsssp245_PM,by = list(year),FUN=sum)
cbs_585PM <- aggregate(cbsssp585_PM,by = list(year),FUN=sum)


cbsPM <- cbind(cbs_126PM$Group.1,cbs_126PM$x,cbs_245PM$x,cbs_585PM$x)
colnames(cbsPM) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
cbsPM <- data.frame(cbsPM)
cbsPM <- cbsPM[-1:-2,]


####DIFFUSIVE######

cbshis_DIF <- ncvar_get(nc = cbs_his,varid = 'CH4_SURF_DIFF')
cbsssp126_DIF <- ncvar_get(nc = cbs_ssp126,varid = 'CH4_SURF_DIFF')
cbsssp245_DIF <- ncvar_get(nc = cbs_ssp245,varid = 'CH4_SURF_DIFF')
cbsssp585_DIF <- ncvar_get(nc = cbs_ssp585,varid = 'CH4_SURF_DIFF')

cbshis_DIF <- cbshis_DIF[1:60225]*3600*24
cbsssp126_DIF <- cbsssp126_DIF[1:31390]*3600*24
cbsssp245_DIF <- cbsssp245_DIF[1:31390]*3600*24
cbsssp585_DIF <- cbsssp585_DIF[1:31390]*3600*24
# 
# jhis_DIF <- apply(jhis_DIF,2,sum)
# jssp126_DIF <- apply(jssp126_DIF,2,sum)
# jssp245_DIF <- apply(jssp245_DIF,2,sum)
# jssp585_DIF <- apply(jssp585_DIF,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataDIF <- data.frame(year,cbsssp126_DIF,cbsssp245_DIF,cbsssp585_DIF)
cbs_hisDIF <- aggregate(cbshis_DIF,by = list(Y1),FUN = sum)
cbs_126DIF <- aggregate(cbsssp126_DIF,by = list(year),FUN=sum)
cbs_245DIF <- aggregate(cbsssp245_DIF,by = list(year),FUN=sum)
cbs_585DIF <- aggregate(cbsssp585_DIF,by = list(year),FUN=sum)


cbsDIF <- cbind(cbs_126DIF$Group.1,cbs_126DIF$x,cbs_245DIF$x,cbs_585DIF$x)
colnames(cbsDIF) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
cbsDIF <- data.frame(cbsDIF)
cbsDIF <- cbsDIF[-1:-2,]


####EBUL#####

cbshis_EBUL <- ncvar_get(nc = cbs_his,varid = 'CH4_SURF_EBUL')
cbsssp126_EBUL <- ncvar_get(nc = cbs_ssp126,varid = 'CH4_SURF_EBUL')
cbsssp245_EBUL <- ncvar_get(nc = cbs_ssp245,varid = 'CH4_SURF_EBUL')
cbsssp585_EBUL <- ncvar_get(nc = cbs_ssp585,varid = 'CH4_SURF_EBUL')

cbshis_EBUL <- cbshis_EBUL[1:60225]*3600*24
cbsssp126_EBUL <- cbsssp126_EBUL[1:31390]*3600*24
cbsssp245_EBUL <- cbsssp245_EBUL[1:31390]*3600*24
cbsssp585_EBUL <- cbsssp585_EBUL[1:31390]*3600*24

# jhis_EBUL <- apply(jhis_EBUL,2,sum)
# jssp126_EBUL <- apply(jssp126_EBUL,2,sum)
# jssp245_EBUL <- apply(jssp245_EBUL,2,sum)
# jssp585_EBUL <- apply(jssp585_EBUL,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataEBUL <- data.frame(year,cbsssp126_EBUL,cbsssp245_EBUL,cbsssp585_EBUL)
cbs_hisEBUL <- aggregate(cbshis_EBUL,by = list(Y1),FUN = sum)
cbs_126EBUL <- aggregate(cbsssp126_EBUL,by = list(year),FUN=sum)
cbs_245EBUL <- aggregate(cbsssp245_EBUL,by = list(year),FUN=sum)
cbs_585EBUL <- aggregate(cbsssp585_EBUL,by = list(year),FUN=sum)


cbsEBUL <- cbind(cbs_126EBUL$Group.1,cbs_126EBUL$x,cbs_245EBUL$x,cbs_585EBUL$x)
colnames(cbsEBUL) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
cbsEBUL <- data.frame(cbsEBUL)
cbsEBUL <- cbsEBUL[-1:-2,]

####PDE####16-25
# cbsPM
# cbsDIF
# cbsEBUL
cbs_PLANT <- apply(cbsPM[1:10,],2,FUN = mean)
cbs_DIF <- apply(cbsDIF[1:10,],2,FUN = mean)
cbs_EBUL<- apply(cbsEBUL[1:10,],2,FUN = mean)
cbs_TRANS <- rbind(cbs_PLANT,cbs_DIF,cbs_EBUL)


cbs_TRANS <- as.data.frame(cbs_TRANS)
cbs_TRANS$TYPE <- c("Plant","Diffusion", 'Ebuliton')

####cbs-126####
data4 <- data.frame(
  group = cbs_TRANS$TYPE,
  value = cbs_TRANS$SSP1.2.6
)
data4 <- data4 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data4$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data4$percent <- paste(round(data4$prop,2),'%',sep = '')

# Basic piechart
cbspie126 <- ggplot(data4, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

#####cbs-245####
data5 <- data.frame(
  group = cbs_TRANS$TYPE,
  value = cbs_TRANS$SSP2.4.5
)
data5 <- data5 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data5$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data5$percent <- paste(round(data5$prop,2),'%',sep = '')

# Basic piechart
cbspie245 <- ggplot(data5, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

#####cbs-585#####
data6 <- data.frame(
  group = cbs_TRANS$TYPE,
  value = cbs_TRANS$SSP5.8.5
)
data6 <- data6 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data6$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data6$percent <- paste(round(data6$prop,2),'%',sep = '')

# Basic piechart
cbspie585 <- ggplot(data6, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

nc_close(cbs_his)
nc_close(cbs_ssp126)
nc_close(cbs_ssp245)
nc_close(cbs_ssp585)
####XXAL####
xxal_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxal_his_1850-2015.nc')
xxal_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp126_2014-2100.nc')
xxal_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp245_2014-2100.nc')
xxal_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp585_2014-2100.nc')


xxalhis_PM <- ncvar_get(nc = xxal_his,varid = 'CH4_SURF_AERE')
xxalssp126_PM <- ncvar_get(nc = xxal_ssp126,varid = 'CH4_SURF_AERE')
xxalssp245_PM <- ncvar_get(nc = xxal_ssp245,varid = 'CH4_SURF_AERE')
xxalssp585_PM <- ncvar_get(nc = xxal_ssp585,varid = 'CH4_SURF_AERE')
# ncatt_get()

xxalhis_PM <- xxalhis_PM[1:60225]*3600*24
xxalssp126_PM <- xxalssp126_PM[1:31390]*3600*24
xxalssp245_PM <- xxalssp245_PM[1:31390]*3600*24
xxalssp585_PM <- xxalssp585_PM[1:31390]*3600*24

# jhis_PM <- apply(jhis_PM,2,sum)
# jssp126_PM <- apply(jssp126_PM,2,sum)
# jssp245_PM <- apply(jssp245_PM,2,sum)
# jssp585_PM <- apply(jssp585_PM,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataPM <- data.frame(year,xxalssp126_PM,xxalssp245_PM,xxalssp585_PM)
xxal_hisPM <- aggregate(xxalhis_PM,by = list(Y1),FUN = sum)
xxal_126PM <- aggregate(xxalssp126_PM,by = list(year),FUN=sum)
xxal_245PM <- aggregate(xxalssp245_PM,by = list(year),FUN=sum)
xxal_585PM <- aggregate(xxalssp585_PM,by = list(year),FUN=sum)


xxalPM <- cbind(xxal_126PM$Group.1,xxal_126PM$x,xxal_245PM$x,xxal_585PM$x)
colnames(xxalPM) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
xxalPM <- data.frame(xxalPM)
xxalPM <- xxalPM[-1:-2,]


####DIFFUSIVE######

xxalhis_DIF <- ncvar_get(nc = xxal_his,varid = 'CH4_SURF_DIFF')
xxalssp126_DIF <- ncvar_get(nc = xxal_ssp126,varid = 'CH4_SURF_DIFF')
xxalssp245_DIF <- ncvar_get(nc = xxal_ssp245,varid = 'CH4_SURF_DIFF')
xxalssp585_DIF <- ncvar_get(nc = xxal_ssp585,varid = 'CH4_SURF_DIFF')

xxalhis_DIF <- xxalhis_DIF[1:60225]*3600*24
xxalssp126_DIF <- xxalssp126_DIF[1:31390]*3600*24
xxalssp245_DIF <- xxalssp245_DIF[1:31390]*3600*24
xxalssp585_DIF <- xxalssp585_DIF[1:31390]*3600*24
# 
# jhis_DIF <- apply(jhis_DIF,2,sum)
# jssp126_DIF <- apply(jssp126_DIF,2,sum)
# jssp245_DIF <- apply(jssp245_DIF,2,sum)
# jssp585_DIF <- apply(jssp585_DIF,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataDIF <- data.frame(year,xxalssp126_DIF,xxalssp245_DIF,xxalssp585_DIF)
xxal_hisDIF <- aggregate(xxalhis_DIF,by = list(Y1),FUN = sum)
xxal_126DIF <- aggregate(xxalssp126_DIF,by = list(year),FUN=sum)
xxal_245DIF <- aggregate(xxalssp245_DIF,by = list(year),FUN=sum)
xxal_585DIF <- aggregate(xxalssp585_DIF,by = list(year),FUN=sum)


xxalDIF <- cbind(xxal_126DIF$Group.1,xxal_126DIF$x,xxal_245DIF$x,xxal_585DIF$x)
colnames(xxalDIF) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
xxalDIF <- data.frame(xxalDIF)
xxalDIF <- xxalDIF[-1:-2,]


####EBUL#####

xxalhis_EBUL <- ncvar_get(nc = xxal_his,varid = 'CH4_SURF_EBUL')
xxalssp126_EBUL <- ncvar_get(nc = xxal_ssp126,varid = 'CH4_SURF_EBUL')
xxalssp245_EBUL <- ncvar_get(nc = xxal_ssp245,varid = 'CH4_SURF_EBUL')
xxalssp585_EBUL <- ncvar_get(nc = xxal_ssp585,varid = 'CH4_SURF_EBUL')

xxalhis_EBUL <- xxalhis_EBUL[1:60225]*3600*24
xxalssp126_EBUL <- xxalssp126_EBUL[1:31390]*3600*24
xxalssp245_EBUL <- xxalssp245_EBUL[1:31390]*3600*24
xxalssp585_EBUL <- xxalssp585_EBUL[1:31390]*3600*24

# jhis_EBUL <- apply(jhis_EBUL,2,sum)
# jssp126_EBUL <- apply(jssp126_EBUL,2,sum)
# jssp245_EBUL <- apply(jssp245_EBUL,2,sum)
# jssp585_EBUL <- apply(jssp585_EBUL,2,sum)


Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdataEBUL <- data.frame(year,xxalssp126_EBUL,xxalssp245_EBUL,xxalssp585_EBUL)
xxal_hisEBUL <- aggregate(xxalhis_EBUL,by = list(Y1),FUN = sum)
xxal_126EBUL <- aggregate(xxalssp126_EBUL,by = list(year),FUN=sum)
xxal_245EBUL <- aggregate(xxalssp245_EBUL,by = list(year),FUN=sum)
xxal_585EBUL <- aggregate(xxalssp585_EBUL,by = list(year),FUN=sum)


xxalEBUL <- cbind(xxal_126EBUL$Group.1,xxal_126EBUL$x,xxal_245EBUL$x,xxal_585EBUL$x)
colnames(xxalEBUL) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
xxalEBUL <- data.frame(xxalEBUL)
xxalEBUL <- xxalEBUL[-1:-2,]

####PDE####16-25
# xxalPM
# xxalDIF
# xxalEBUL
xxal_PLANT <- apply(xxalPM[1:10,],2,FUN = mean)
xxal_DIF <- apply(xxalDIF[1:10,],2,FUN = mean)
xxal_EBUL<- apply(xxalEBUL[1:10,],2,FUN = mean)
xxal_TRANS <- rbind(xxal_PLANT,xxal_DIF,xxal_EBUL)


xxal_TRANS <- as.data.frame(xxal_TRANS)
xxal_TRANS$TYPE <- c("Plant","Diffusion", 'Ebuliton')


####xxal-126####
data7 <- data.frame(
  group = xxal_TRANS$TYPE,
  value = xxal_TRANS$SSP1.2.6
)
data7 <- data7 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data7$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data7$percent <- paste(round(data7$prop,2),'%',sep = '')

# Basic piechart
xxalpie126 <- ggplot(data7, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

#####xxal-245####
data8 <- data.frame(
  group = xxal_TRANS$TYPE,
  value = xxal_TRANS$SSP2.4.5
)
data8 <- data8 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data8$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data8$percent <- paste(round(data8$prop,2),'%',sep = '')

# Basic piechart
xxalpie245 <- ggplot(data8, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

#####xxal-585#####
data9 <- data.frame(
  group = xxal_TRANS$TYPE,
  value = xxal_TRANS$SSP5.8.5
)
data9 <- data9 %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data9$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data9$percent <- paste(round(data9$prop,2),'%',sep = '')

# Basic piechart
xxalpie585 <- ggplot(data9, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

nc_close(xxal_his)
nc_close(xxal_ssp126)
nc_close(xxal_ssp245)
nc_close(xxal_ssp585)

#####combine#####
# library(customLayout)
# library(gridExtra)# 创建排版画布
# lay1 <- lay_new(matrix(1:6,ncol = 2),widths = c(1,1))
# lay2 <- lay_new(matrix(1:3))
# cl <- lay_bind_col(lay1, lay2, widths = c(2, 1))
# lay_show(cl)
# lay_set(cl)
# 
# sjpie126
# sjpie245
# sjpie585
# cbspie126
# cbspie245
# cbspie585
# xxalpie126
# xxalpie245
# xxalpie585

gg <- ggdraw() +     
  draw_plot(sjpie126, 0, 0.66, 0.33, 0.33) + 
  draw_plot(sjpie245, 0, 0.33, 0.33, 0.33) + 
  draw_plot(sjpie585, 0, 0, 0.33, 0.33) + 
  draw_plot(cbspie126, 0.33, 0.66, 0.33, 0.33) + 
  draw_plot(cbspie245, 0.33,0.33, 0.33, 0.33) + 
  draw_plot(cbspie585, 0.33,0, 0.33, 0.33) + 
  draw_plot(xxalpie126, 0.66, 0.66, 0.33, 0.33) + 
  draw_plot(xxalpie245, 0.66, 0.33, 0.33, 0.33) + 
  draw_plot(xxalpie585, 0.66, 0, 0.33, 0.33)
  # draw_plot_label(c("A", "B", "C"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 15, colour = "cyan", family = "Dancing") # 加上标签，
#showtext_begin()
#print(gg)
#showtext_end()

