#### future ####
library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(zoo)
library(multcompView)


####sanjiang##
sj_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sj_his_1850-2015.nc')
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp124-2014-2100.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp245-2014-2100.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/sj/sjssp585-2014-2100.nc')


jhis_ch4 <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_NETFLUX')
jssp126_ch4 <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_NETFLUX')
jssp245_ch4 <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_NETFLUX')
jssp585_ch4 <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_NETFLUX')
jhis_ch4 <- jhis_ch4[1:60225]*3600*24*16
jssp126_ch4 <- jssp126_ch4[1:31390]*3600*24*16
jssp245_ch4 <- jssp245_ch4[1:31390]*3600*24*16
jssp585_ch4 <- jssp585_ch4[1:31390]*3600*24*16
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdata <- data.frame(year,jssp126_ch4,jssp245_ch4,jssp585_ch4)
sj_hisd <- aggregate(jhis_ch4,by = list(Y1),FUN = sum)
sj_126 <- aggregate(jssp126_ch4,by = list(year),FUN=sum)
sj_245 <- aggregate(jssp245_ch4,by = list(year),FUN=sum)
sj_585 <- aggregate(jssp585_ch4,by = list(year),FUN=sum)

sch4 <- cbind(sj_126$Group.1,sj_126$x,sj_245$x,sj_585$x)
colnames(sch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sch4 <- data.frame(sch4)
sch4 <- sch4[-1,]
jssp <- melt(sch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

sj_line <- ggplot(jssp, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  labs(x=" ", y=" ") +
  theme_base()+
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
    
  )
line_bar1 <- 
  ggplot(jssp, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  labs(x=" ", y=" ") +
  theme_base()+
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
    
  )

###bar plot###
sj_2006 <- data.frame(
  Year <- 2006:2014,
  Scenarios <- rep('Histoical',9),
  Methane <- sj_hisd[157:165,2]
)
sj_16_25 <-  jssp[which(jssp$Year%in% c(2016:2025) ),]
sj_50s <-  jssp[which(jssp$Year%in% c(2050:2059) ),]
sj_90s <-  jssp[which(jssp$Year%in% c(2090:2099) ),]

a <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = mean)
b <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = sd)
c <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = mean)
d <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = sd)
e <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = mean)
f <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = sd)
j <- mean(sj_2006$Methane....sj_hisd.157.165..2.)
k <- sd(sj_2006$Methane....sj_hisd.157.165..2.)
g <- cbind(a,b$x)
h <- cbind(c,d$x)
i <- cbind(e,f$x)
l <- data.frame(
  Scenarios <- 'Historical',
  mean <- j,
  sd <- k)
colnames(g) <- c('Scenarios','mean','sd')
colnames(h) <- c('Scenarios','mean','sd')
colnames(i) <- c('Scenarios','mean','sd')
colnames(l) <- c('Scenarios','mean','sd')
bar_data <- rbind(l,g,h,i)
bar_data$year <- c(rep('2006-2015',1),rep(c('2016-2025','2050s','2090s'),each =3))
colnames(sj_2006) <- c('Year','Scenarios','Methane')
anovadata <- rbind(sj_2006,sj_16_25,sj_50s,sj_90s)
anovadata$date <- c(rep('0615',9),rep(c('1625','50s','90s'),each =30))
anovadata$Scenarios <- factor(anovadata$Scenarios)
anovadata$date <- factor(anovadata$date)
anovadata$let <- c(rep("A",9),rep(LETTERS[2:10],each = 10))
anovadata$let <- factor(anovadata$let)
anova <- aov(Methane~let, data = anovadata)
summary(anova)
tukey <- TukeyHSD(anova)
tukey.cld <- multcompLetters4(anova, tukey)
cld <- as.data.frame.list(tukey.cld$let)
bar_data$Tukey <- cld$Letters
bar_data$se <- bar_data$sd/sqrt(10)
# coloured barplot

sj_bar <-   
ggplot(bar_data,aes(x = factor(year), y = mean)) +
  geom_col(aes(fill = Scenarios), position = position_dodge2(preserve = 'single')) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge2(preserve = 'single', padding = 0.5))+
  geom_text(aes(y=mean+se+1,label=Tukey), position = position_dodge2(0.9), size = 17, 
            hjust=0.5, colour = "black")+
  labs(x=" ", y="Sanjiang Plain") +
  scale_y_continuous(position = "right")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = 'none', 
        legend.title = element_blank(),
        legend.text=element_text(colour='black',size=22,face="bold"),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        # axis.text.x.top = element_text(size = 22,face = 'bold'),
        axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
        axis.title.y = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size = 22,face = 'bold'),
        # axis.title.x.top = element_text(size = 22,face = 'bold'),
        axis.ticks=element_line(size=0.5),
        plot.background=element_blank()) 
line_bar <-   
  ggplot(bar_data,aes(x = factor(year), y = mean)) +
  geom_col(aes(fill = Scenarios), position = position_dodge2(preserve = 'single')) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge2(preserve = 'single', padding = 0.5))+
  geom_text(aes(y=mean+se+1,label=Tukey), position = position_dodge2(0.9), size = 17, 
            hjust=0.5, colour = "black")+
  labs(x=" ", y="Sanjiang Plain") +
  scale_y_continuous(position = "right")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = 'top', 
        legend.title = element_blank(),
        legend.text=element_text(colour='black',size=22,face="bold"),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        # axis.text.x.top = element_text(size = 22,face = 'bold'),
        axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
        axis.title.y = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size = 22,face = 'bold'),
        # axis.title.x.top = element_text(size = 22,face = 'bold'),
        axis.ticks=element_line(size=0.5),
        plot.background=element_blank()) 
####cbs####
sj_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbs_his_1850-2015.nc')
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp126-2014-2100.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp245-2014-2100.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/cbs/cbsssp585-2014-2100.nc')


jhis_ch4 <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_NETFLUX')
jssp126_ch4 <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_NETFLUX')
jssp245_ch4 <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_NETFLUX')
jssp585_ch4 <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_NETFLUX')
jhis_ch4 <- jhis_ch4[1:60225]*3600*24*16
jssp126_ch4 <- jssp126_ch4[1:31390]*3600*24*16
jssp245_ch4 <- jssp245_ch4[1:31390]*3600*24*16
jssp585_ch4 <- jssp585_ch4[1:31390]*3600*24*16
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdata <- data.frame(year,jssp126_ch4,jssp245_ch4,jssp585_ch4)
sj_hisd <- aggregate(jhis_ch4,by = list(Y1),FUN = sum)
sj_126 <- aggregate(jssp126_ch4,by = list(year),FUN=sum)
sj_245 <- aggregate(jssp245_ch4,by = list(year),FUN=sum)
sj_585 <- aggregate(jssp585_ch4,by = list(year),FUN=sum)

sch4 <- cbind(sj_126$Group.1,sj_126$x,sj_245$x,sj_585$x)
colnames(sch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sch4 <- data.frame(sch4)
sch4 <- sch4[-1,]
jssp1 <- melt(sch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

cbs_line <- ggplot(jssp1, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  labs(x=" ", y="Methane flux (gC m-2)") +
  theme_base()+
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    axis.text.x=element_blank(),
    axis.ticks.x = element_blank(),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank()
    
  )

###bar plot###
sj_2006 <- data.frame(
  Year <- 2006:2014,
  Scenarios <- rep('Histoical',9),
  Methane <- sj_hisd[157:165,2]
)
sj_16_25 <-  jssp1[which(jssp1$Year%in% c(2016:2025) ),]
sj_50s <-  jssp1[which(jssp1$Year%in% c(2050:2059) ),]
sj_90s <-  jssp1[which(jssp1$Year%in% c(2090:2099) ),]

a <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = mean)
b <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = sd)
c <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = mean)
d <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = sd)
e <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = mean)
f <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = sd)
j <- mean(sj_2006$Methane....sj_hisd.157.165..2.)
k <- sd(sj_2006$Methane....sj_hisd.157.165..2.)
g <- cbind(a,b$x)
h <- cbind(c,d$x)
i <- cbind(e,f$x)
l <- data.frame(
  Scenarios <- 'Historical',
  mean <- j,
  sd <- k)
colnames(g) <- c('Scenarios','mean','sd')
colnames(h) <- c('Scenarios','mean','sd')
colnames(i) <- c('Scenarios','mean','sd')
colnames(l) <- c('Scenarios','mean','sd')
bar_data1 <- rbind(l,g,h,i)
bar_data1$year <- c(rep('2006-2015',1),rep(c('2016-2025','2050s','2090s'),each =3))
colnames(sj_2006) <- c('Year','Scenarios','Methane')
anovadata <- rbind(sj_2006,sj_16_25,sj_50s,sj_90s)
anovadata$date <- c(rep('0615',9),rep(c('1625','50s','90s'),each =30))
anovadata$Scenarios <- factor(anovadata$Scenarios)
anovadata$date <- factor(anovadata$date)
anovadata$date <- factor(anovadata$date)
anovadata$let <- c(rep("A",9),rep(LETTERS[2:10],each = 10))
anovadata$let <- factor(anovadata$let)
anova <- aov(Methane~let, data = anovadata)
summary(anova)
tukey <- TukeyHSD(anova)
tukey.cld <- multcompLetters4(anova, tukey)
cld <- as.data.frame.list(tukey.cld$let)
bar_data1$Tukey <- cld$Letters
bar_data1$se <- bar_data1$sd/sqrt(10)
# coloured barplot

cbs_bar <-   
  ggplot(bar_data1,aes(x = factor(year), y = mean)) +
  geom_col(aes(fill = Scenarios), position = position_dodge2(preserve = 'single')) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge2(preserve = 'single', padding = 0.5))+
  geom_text(aes(y=mean+se+0.5,label=Tukey), position = position_dodge2(0.9), size = 17, 
            hjust=0.5, colour = "black")+
  labs(x=" ", y="Changbai Mountain") +
  scale_y_continuous(position = "right")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = 'none', 
        legend.title = element_blank(),
        legend.text=element_text(colour='black',size=22,face="bold"),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        # axis.text.x.top = element_text(size = 22,face = 'bold'),
        axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
        axis.title.y = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size = 22,face = 'bold'),
        # axis.title.x.top = element_text(size = 22,face = 'bold'),
        axis.ticks=element_line(size=0.5),
        plot.background=element_blank()) 


######xxal#####
sj_his <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxal_his_1850-2015.nc')
sj_ssp126 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp126_2014-2100.nc')
sj_ssp245 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/xxalssp245_2014-2100.nc')
sj_ssp585 <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/ssp/history/xxal/5852xxal.nc')

jhis_ch4 <- ncvar_get(nc = sj_his,varid = 'CH4_SURF_NETFLUX')
jssp126_ch4 <- ncvar_get(nc = sj_ssp126,varid = 'CH4_SURF_NETFLUX')
jssp245_ch4 <- ncvar_get(nc = sj_ssp245,varid = 'CH4_SURF_NETFLUX')
jssp585_ch4 <- ncvar_get(nc = sj_ssp585,varid = 'CH4_SURF_NETFLUX')
jhis_ch4 <- jhis_ch4[1:60225]*3600*24*16
jssp126_ch4 <- jssp126_ch4[1:31390]*3600*24*16
jssp245_ch4 <- jssp245_ch4[1:31390]*3600*24*16
jssp585_ch4 <- jssp585_ch4[1:31390]*3600*24*16
Y1 <- rep(1850:2014,each = 365)
year <- rep(2014:2099,each = 365)
sspdata <- data.frame(year,jssp126_ch4,jssp245_ch4,jssp585_ch4)
sj_hisd <- aggregate(jhis_ch4,by = list(Y1),FUN = sum)
sj_126 <- aggregate(jssp126_ch4,by = list(year),FUN=sum)
sj_245 <- aggregate(jssp245_ch4,by = list(year),FUN=sum)
sj_585 <- aggregate(jssp585_ch4,by = list(year),FUN=sum)

sch4 <- cbind(sj_126$Group.1,sj_126$x,sj_245$x,sj_585$x)
colnames(sch4) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
sch4 <- data.frame(sch4)
sch4 <- sch4[-1,]
jssp2 <- melt(sch4, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Methane')  ##data.frame

xxal_line <- ggplot(jssp2, aes(x = Year, y =  Methane, color = Scenarios))+
  geom_line(size = 2)+
  labs(x=" ", y=" ") +
  theme_base()+
  theme(
    legend.position = 'none',
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

###bar plot###
sj_2006 <- data.frame(
  Year <- 2006:2014,
  Scenarios <- rep('Histoical',9),
  Methane <- sj_hisd[157:165,2]
)
sj_16_25 <-  jssp2[which(jssp2$Year%in% c(2016:2025) ),]
sj_50s <-  jssp2[which(jssp2$Year%in% c(2050:2059) ),]
sj_90s <-  jssp2[which(jssp2$Year%in% c(2090:2099) ),]

a <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = mean)
b <- aggregate(sj_16_25$Methane,by = list(sj_16_25$Scenarios),FUN = sd)
c <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = mean)
d <- aggregate(sj_50s$Methane,by = list(sj_50s$Scenarios),FUN = sd)
e <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = mean)
f <- aggregate(sj_90s$Methane,by = list(sj_90s$Scenarios),FUN = sd)
j <- mean(sj_2006$Methane....sj_hisd.157.165..2.)
k <- sd(sj_2006$Methane....sj_hisd.157.165..2.)
g <- cbind(a,b$x)
h <- cbind(c,d$x)
i <- cbind(e,f$x)
l <- data.frame(
  Scenarios <- 'Historical',
  mean <- j,
  sd <- k)
colnames(g) <- c('Scenarios','mean','sd')
colnames(h) <- c('Scenarios','mean','sd')
colnames(i) <- c('Scenarios','mean','sd')
colnames(l) <- c('Scenarios','mean','sd')
bar_data2 <- rbind(l,g,h,i)
bar_data2$year <- c(rep('2006-2015',1),rep(c('2016-2025','2050s','2090s'),each =3))
colnames(sj_2006) <- c('Year','Scenarios','Methane')
anovadata <- rbind(sj_2006,sj_16_25,sj_50s,sj_90s)
anovadata$date <- c(rep('0615',9),rep(c('1625','50s','90s'),each =30))
anovadata$Scenarios <- factor(anovadata$Scenarios)
anovadata$date <- factor(anovadata$date)
anovadata$let <- c(rep("A",9),rep(LETTERS[2:10],each = 10))
anovadata$let <- factor(anovadata$let)
anova <- aov(Methane~ let, data = anovadata)
summary(anova)
tukey <- TukeyHSD(anova)
tukey.cld <- multcompLetters4(anova, tukey)
cld <- as.data.frame.list(tukey.cld$let)
bar_data2$Tukey <- cld$Letters
bar_data2$se <- bar_data$sd/sqrt(10)
# coloured barplot

xxal_bar <-   
  ggplot(bar_data2,aes(x = factor(year), y = mean)) +
  geom_col(aes(fill = Scenarios), position = position_dodge2(preserve = 'single')) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge2(preserve = 'single', padding = 0.5))+
  geom_text(aes(y=mean+se+1,label=Tukey), position = position_dodge2(0.9), size = 17, 
            hjust=0.5, colour = "black")+
  labs(x=" ", y="Lesser Khingan Mountain") +
  scale_y_continuous(position = "right")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = 'none', 
        legend.title = element_blank(),
        legend.text=element_text(colour='black',size=22,face="bold"),
        axis.text.x=element_text(size=22,colour='black' ),
        # axis.text.x.top = element_text(size = 22,face = 'bold'),
        axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
        axis.title.y = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size = 22,face = 'bold'),
        # axis.title.x.top = element_text(size = 22,face = 'bold'),
        axis.ticks=element_line(size=0.5),
        plot.background=element_blank()) 

###########
plot_grid(sj_line,sj_bar,cbs_line,cbs_bar,xxal_line,xxal_bar,
          labels = c('a','b','c','d','e','f'),
          label_size = 22,
          label_x = 0.1,
          ncol =2,
          align = 'v')
