
sj_ssp126 <- read.csv('D:/Rtest/future/ssp126/sj126-2015-2100.csv',header = T)
sj_ssp245 <- read.csv('D:/Rtest/future/ssp245/sj245-2015-2100.csv',header = T)
sj_ssp585 <- read.csv('D:/Rtest/future/ssp585/sj585-2015-2100.csv',header = T)

cbs_ssp126 <- read.csv('D:/Rtest/future/ssp126/cbs126_2015-2100.csv',header = T)
cbs_ssp245 <- read.csv('D:/Rtest/future/ssp245/cbs245_2015-2100.csv',header = T)
cbs_ssp585 <- read.csv('D:/Rtest/future/ssp585/cbs585_2015-2100.csv',header = T)

xxal_ssp126 <- read.csv('D:/Rtest/future/ssp126/xxal126_2015-2100.csv',header = T)
xxal_ssp245 <- read.csv('D:/Rtest/future/ssp245/xxal245_2015-2100.csv',header = T)
xxal_ssp585 <- read.csv('D:/Rtest/future/ssp585/xxal585_2015-2100.csv',header = T)

year <- rep(2015:2100,each = 1460)
####TBOT#####

tbot <- data.frame(year,sj_ssp126$v4,sj_ssp245$v4,sj_ssp585$v4)
t126 <- aggregate(tbot$sj_ssp126.v4,by = list(year), FUN=mean)
t245 <- aggregate(tbot$sj_ssp245.v4,by = list(year), FUN=mean)
t585 <- aggregate(tbot$sj_ssp585.v4,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
sj_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Temperature')  ##data.frame
sj_jt$Site <- rep('Sanjiang Plain',258)

tbot <- data.frame(year,cbs_ssp126$v4,cbs_ssp245$v4,cbs_ssp585$v4)
t126 <- aggregate(tbot$cbs_ssp126.v4,by = list(year), FUN=mean)
t245 <- aggregate(tbot$cbs_ssp245.v4,by = list(year), FUN=mean)
t585 <- aggregate(tbot$cbs_ssp585.v4,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
cbs_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Temperature')  ##data.frame
cbs_jt$Site <- rep('Changbai Mountains',258)

tbot <- data.frame(year,xxal_ssp126$v4,xxal_ssp245$v4,xxal_ssp585$v4)
t126 <- aggregate(tbot$xxal_ssp126.v4,by = list(year), FUN=mean)
t245 <- aggregate(tbot$xxal_ssp245.v4,by = list(year), FUN=mean)
t585 <- aggregate(tbot$xxal_ssp585.v4,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
xxal_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Temperature')  ##data.frame
xxal_jt$Site <- rep('Lesser Khingan Mountains',258)

sdata <- rbind(sj_jt,cbs_jt,xxal_jt)
sdata$Site <- factor(sdata$Site,levels = c('Sanjiang Plain','Changbai Mountains','Lesser Khingan Mountains'))

ggplot(sdata, aes(x = Year, y =  Temperature, color = Scenarios))+
  geom_line(size = 2)+
  facet_wrap(~Site)+
  scale_colour_manual(values = c('#619CFF','#00BA38','#F8766D'))+
  labs(x = '')+
  theme_base()+
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    # axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    # axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    strip.text.x = element_text(size = 22,face = 'bold'),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    plot.background=element_blank()
  )

######RAIN######

tbot <- data.frame(year,sj_ssp126$v6,sj_ssp245$v6,sj_ssp585$v6)
t126 <- aggregate(tbot$sj_ssp126.v6,by = list(year), FUN=mean)
t245 <- aggregate(tbot$sj_ssp245.v6,by = list(year), FUN=mean)
t585 <- aggregate(tbot$sj_ssp585.v6,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
sj_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'PRECTmms')  ##data.frame
sj_jt$Site <- rep('Sanjiang Plain',258)

tbot <- data.frame(year,cbs_ssp126$v6,cbs_ssp245$v6,cbs_ssp585$v6)
t126 <- aggregate(tbot$cbs_ssp126.v6,by = list(year), FUN=mean)
t245 <- aggregate(tbot$cbs_ssp245.v6,by = list(year), FUN=mean)
t585 <- aggregate(tbot$cbs_ssp585.v6,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
cbs_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'PRECTmms')  ##data.frame
cbs_jt$Site <- rep('Changbai Mountains',258)

tbot <- data.frame(year,xxal_ssp126$v6,xxal_ssp245$v6,xxal_ssp585$v6)
t126 <- aggregate(tbot$xxal_ssp126.v6,by = list(year), FUN=mean)
t245 <- aggregate(tbot$xxal_ssp245.v6,by = list(year), FUN=mean)
t585 <- aggregate(tbot$xxal_ssp585.v6,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
xxal_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'PRECTmms')  ##data.frame
xxal_jt$Site <- rep('Lesser Khingan Mountains',258)

sdata1 <- rbind(sj_jt,cbs_jt,xxal_jt)
sdata1$Site <- factor(sdata1$Site,levels = c('Sanjiang Plain','Changbai Mountains','Lesser Khingan Mountains'))
sdata1$Value <- sdata1$Value*31536000
ggplot(sdata1, aes(x = Year, y =  Value, color = Scenarios))+
  geom_line(size = 2)+
  facet_wrap(~Site)+
  scale_colour_manual(values = c('#619CFF','#00BA38','#F8766D'))+
  labs(x = '')+
  theme_base()+
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    # axis.text.x=element_text(size=22,colour='black' ),
    # axis.text.x.top = element_text(size = 22,face = 'bold'),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    # axis.title.x = element_text(size = 22,face = 'bold'),
    # axis.title.x.top = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    strip.text.x = element_text(size = 22,face = 'bold'),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    plot.background=element_blank()
  )

####wind####

tbot <- data.frame(year,sj_ssp126$v2,sj_ssp245$v2,sj_ssp585$v2)
t126 <- aggregate(tbot$sj_ssp126.v2,by = list(year), FUN=mean)
t245 <- aggregate(tbot$sj_ssp245.v2,by = list(year), FUN=mean)
t585 <- aggregate(tbot$sj_ssp585.v2,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
sj_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
sj_jt$Site <- rep('Sanjiang Plain',258)

tbot <- data.frame(year,cbs_ssp126$v2,cbs_ssp245$v2,cbs_ssp585$v2)
t126 <- aggregate(tbot$cbs_ssp126.v2,by = list(year), FUN=mean)
t245 <- aggregate(tbot$cbs_ssp245.v2,by = list(year), FUN=mean)
t585 <- aggregate(tbot$cbs_ssp585.v2,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
cbs_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
cbs_jt$Site <- rep('Changbai Mountains',258)

tbot <- data.frame(year,xxal_ssp126$v2,xxal_ssp245$v2,xxal_ssp585$v2)
t126 <- aggregate(tbot$xxal_ssp126.v2,by = list(year), FUN=mean)
t245 <- aggregate(tbot$xxal_ssp245.v2,by = list(year), FUN=mean)
t585 <- aggregate(tbot$xxal_ssp585.v2,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
xxal_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
xxal_jt$Site <- rep('Lesser Khingan Mountains',258)

sdata2 <- rbind(sj_jt,cbs_jt,xxal_jt)
sdata2$Site <- factor(sdata2$Site,levels = c('Sanjiang Plain','Changbai Mountains','Lesser Khingan Mountains'))
sdata2$Var <- rep('Wind',774)
####PSRF####

tbot <- data.frame(year,sj_ssp126$v3,sj_ssp245$v3,sj_ssp585$v3)
t126 <- aggregate(tbot$sj_ssp126.v3,by = list(year), FUN=mean)
t245 <- aggregate(tbot$sj_ssp245.v3,by = list(year), FUN=mean)
t585 <- aggregate(tbot$sj_ssp585.v3,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
sj_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
sj_jt$Site <- rep('Sanjiang Plain',258)

tbot <- data.frame(year,cbs_ssp126$v3,cbs_ssp245$v3,cbs_ssp585$v3)
t126 <- aggregate(tbot$cbs_ssp126.v3,by = list(year), FUN=mean)
t245 <- aggregate(tbot$cbs_ssp245.v3,by = list(year), FUN=mean)
t585 <- aggregate(tbot$cbs_ssp585.v3,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
cbs_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
cbs_jt$Site <- rep('Changbai Mountains',258)

tbot <- data.frame(year,xxal_ssp126$v3,xxal_ssp245$v3,xxal_ssp585$v3)
t126 <- aggregate(tbot$xxal_ssp126.v3,by = list(year), FUN=mean)
t245 <- aggregate(tbot$xxal_ssp245.v3,by = list(year), FUN=mean)
t585 <- aggregate(tbot$xxal_ssp585.v3,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
xxal_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
xxal_jt$Site <- rep('Lesser Khingan Mountains',258)

sdata3 <- rbind(sj_jt,cbs_jt,xxal_jt)
sdata3$Site <- factor(sdata3$Site,levels = c('Sanjiang Plain','Changbai Mountains','Lesser Khingan Mountains'))
sdata3$Var <- rep('PSRF',774)
####RH####

tbot <- data.frame(year,sj_ssp126$v5,sj_ssp245$v5,sj_ssp585$v5)
t126 <- aggregate(tbot$sj_ssp126.v5,by = list(year), FUN=mean)
t245 <- aggregate(tbot$sj_ssp245.v5,by = list(year), FUN=mean)
t585 <- aggregate(tbot$sj_ssp585.v5,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
sj_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
sj_jt$Site <- rep('Sanjiang Plain',258)

tbot <- data.frame(year,cbs_ssp126$v5,cbs_ssp245$v5,cbs_ssp585$v5)
t126 <- aggregate(tbot$cbs_ssp126.v5,by = list(year), FUN=mean)
t245 <- aggregate(tbot$cbs_ssp245.v5,by = list(year), FUN=mean)
t585 <- aggregate(tbot$cbs_ssp585.v5,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
cbs_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
cbs_jt$Site <- rep('Changbai Mountains',258)

tbot <- data.frame(year,xxal_ssp126$v5,xxal_ssp245$v5,xxal_ssp585$v5)
t126 <- aggregate(tbot$xxal_ssp126.v5,by = list(year), FUN=mean)
t245 <- aggregate(tbot$xxal_ssp245.v5,by = list(year), FUN=mean)
t585 <- aggregate(tbot$xxal_ssp585.v5,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
xxal_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
xxal_jt$Site <- rep('Lesser Khingan Mountains',258)

sdata4 <- rbind(sj_jt,cbs_jt,xxal_jt)
sdata4$Site <- factor(sdata4$Site,levels = c('Sanjiang Plain','Changbai Mountains','Lesser Khingan Mountains'))
sdata4$Var <- rep('RH',774)
####FSDS####

tbot <- data.frame(year,sj_ssp126$v7,sj_ssp245$v7,sj_ssp585$v7)
t126 <- aggregate(tbot$sj_ssp126.v7,by = list(year), FUN=mean)
t245 <- aggregate(tbot$sj_ssp245.v7,by = list(year), FUN=mean)
t585 <- aggregate(tbot$sj_ssp585.v7,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
sj_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
sj_jt$Site <- rep('Sanjiang Plain',258)

tbot <- data.frame(year,cbs_ssp126$v7,cbs_ssp245$v7,cbs_ssp585$v7)
t126 <- aggregate(tbot$cbs_ssp126.v7,by = list(year), FUN=mean)
t245 <- aggregate(tbot$cbs_ssp245.v7,by = list(year), FUN=mean)
t585 <- aggregate(tbot$cbs_ssp585.v7,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
cbs_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
cbs_jt$Site <- rep('Changbai Mountains',258)

tbot <- data.frame(year,xxal_ssp126$v7,xxal_ssp245$v7,xxal_ssp585$v7)
t126 <- aggregate(tbot$xxal_ssp126.v7,by = list(year), FUN=mean)
t245 <- aggregate(tbot$xxal_ssp245.v7,by = list(year), FUN=mean)
t585 <- aggregate(tbot$xxal_ssp585.v7,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
xxal_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
xxal_jt$Site <- rep('Lesser Khingan Mountains',258)

sdata5 <- rbind(sj_jt,cbs_jt,xxal_jt)
sdata5$Site <- factor(sdata5$Site,levels = c('Sanjiang Plain','Changbai Mountains','Lesser Khingan Mountains'))
sdata5$Var <- rep('FSDS',774)
####FLDS####

tbot <- data.frame(year,sj_ssp126$v8,sj_ssp245$v8,sj_ssp585$v8)
t126 <- aggregate(tbot$sj_ssp126.v8,by = list(year), FUN=mean)
t245 <- aggregate(tbot$sj_ssp245.v8,by = list(year), FUN=mean)
t585 <- aggregate(tbot$sj_ssp585.v8,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
sj_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
sj_jt$Site <- rep('Sanjiang Plain',258)

tbot <- data.frame(year,cbs_ssp126$v8,cbs_ssp245$v8,cbs_ssp585$v8)
t126 <- aggregate(tbot$cbs_ssp126.v8,by = list(year), FUN=mean)
t245 <- aggregate(tbot$cbs_ssp245.v8,by = list(year), FUN=mean)
t585 <- aggregate(tbot$cbs_ssp585.v8,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
cbs_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
cbs_jt$Site <- rep('Changbai Mountains',258)

tbot <- data.frame(year,xxal_ssp126$v8,xxal_ssp245$v8,xxal_ssp585$v8)
t126 <- aggregate(tbot$xxal_ssp126.v8,by = list(year), FUN=mean)
t245 <- aggregate(tbot$xxal_ssp245.v8,by = list(year), FUN=mean)
t585 <- aggregate(tbot$xxal_ssp585.v8,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)
xxal_jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Value')  ##data.frame
xxal_jt$Site <- rep('Lesser Khingan Mountains',258)

sdata6 <- rbind(sj_jt,cbs_jt,xxal_jt)
sdata6$Site <- factor(sdata6$Site,levels = c('Sanjiang Plain','Changbai Mountains','Lesser Khingan Mountains'))
sdata6$Var <- rep('FLDS',774)

###########
###########
sdata$Var <- rep('TBOT',774)
colnames(sdata) <- c('Year','Scenarios','Value','Site','Var')
sdata1$Var <- rep('PRECTmms',774)
colnames(sdata1) <- c('Year','Scenarios','Value','Site','Var')
cdata <- rbind(sdata,sdata1,sdata2,sdata3,sdata4,sdata5,sdata6)

ggplot(cdata, aes(x = Year, y =  Value, color = Scenarios))+
  geom_line(size = 2)+
  facet_grid(Var ~ Site,scales = 'free')+
  scale_colour_manual(values = c('#619CFF','#00BA38','#F8766D'))+
  labs(y = '')+
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
    strip.text.x = element_text(size = 22,face = 'bold'),
    strip.text.y = element_text(size = 22,face = 'bold'),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_blank(),
    plot.background=element_blank()
  )
