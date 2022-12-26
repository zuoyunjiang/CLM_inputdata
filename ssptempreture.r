
ssp126 <- read.csv('D:/Rtest/future/ssp126/sj126-2015-2100.csv',header = T)
ssp245 <- read.csv('D:/Rtest/future/ssp245/sj245-2015-2100.csv',header = T)
ssp585 <- read.csv('D:/Rtest/future/ssp585/sj585-2015-2100.csv',header = T)
year <- rep(2015:2100,each = 1460)
tbot <- data.frame(year,ssp126$v4,ssp245$v4,ssp585$v4)
t126 <- aggregate(tbot$ssp126.v4,by = list(year), FUN=mean)
t245 <- aggregate(tbot$ssp245.v4,by = list(year), FUN=mean)
t585 <- aggregate(tbot$ssp585.v4,by = list(year), FUN=mean)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)

jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Temperature')  ##data.frame

ggplot(jt, aes(x = Year, y =  Temperature, color = Scenarios))+
  geom_line(size = 2)+
  theme_base()+
  theme()

tbot <- data.frame(year,ssp126$v6*21600,ssp245$v6*21600,ssp585$v6*21600)
t126 <- aggregate(tbot$ssp126.v6,by = list(year), FUN=sum)
t245 <- aggregate(tbot$ssp245.v6,by = list(year), FUN=sum)
t585 <- aggregate(tbot$ssp585.v6,by = list(year), FUN=sum)

t <- cbind(t126$Group.1,t126$x,t245$x,t585$x)
colnames(t) <- c('Year','SSP1-2.6','SSP2-4.5','SSP5-8.5')
t <- data.frame(t)

jt <- melt(t, id.vars ='Year', variable.name = 'Scenarios', value.name = 'Precipitation')  ##data.frame

ggplot(jt, aes(x = Year, y =  Precipitation, color = Scenarios))+
  geom_line(size = 2)+
  theme_base()+
  theme(
    legend.position = c(0.1,0.8)
  )
