
##上接future.r代码
jsspab
jsspHM 
jsspAER
jsspANAER

a <- aggregate(jsspab$AM,by = list(jsspab$Scenarios),FUN = mean)
b <- aggregate(jsspab$AM,by = list(jsspab$Scenarios),FUN = sd)
c <- aggregate(jsspHM$HM,by = list(jsspHM$Scenarios),FUN = mean)
d <- aggregate(jsspHM$HM,by = list(jsspHM$Scenarios),FUN = sd)
e <- aggregate(jsspAER$AER,by = list(jsspAER$Scenarios),FUN = mean)
f <- aggregate(jsspAER$AER,by = list(jsspAER$Scenarios),FUN = sd)
g <- aggregate(jsspANAER$ANAER,by = list(jsspANAER$Scenarios),FUN = mean)
h <- aggregate(jsspANAER$ANAER,by = list(jsspANAER$Scenarios),FUN = sd)

i <- cbind(a,b$x)
j <- cbind(c,d$x)
k <- cbind(e,f$x)
l <- cbind(g,h$x)

colnames(i) <- c('Scenarios','mean','sd')
colnames(j) <- c('Scenarios','mean','sd')
colnames(k) <- c('Scenarios','mean','sd')
colnames(l) <- c('Scenarios','mean','sd')
bar_data3 <- rbind(i,j,k,l)
bar_data3$type <- rep(c('AM','MM','AER','ANAER'),each = 3)
bar_data3$se <- bar_data3$sd/sqrt(85)
bar_data4 <- bar_data3[1:6,]
bar_data5 <- bar_data3[7:12,]
ggplot(bar_data4,aes(x = factor(type), y = mean)) +
  geom_col(aes(fill = Scenarios), position = position_dodge2(preserve = 'single')) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge2(preserve = 'single', padding = 0.5))+
  # geom_text(aes(y=mean+se+1,label=Tukey), position = position_dodge2(0.9), size = 12, 
  #           hjust=0.5, colour = "black")+
  labs(x=" ", y="methanogens Biomass (mol/m3)") +
  # scale_y_continuous(position = "right")+
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
ggplot(bar_data5,aes(x = factor(type), y = mean)) +
  geom_col(aes(fill = Scenarios), position = position_dodge2(preserve = 'single')) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge2(preserve = 'single', padding = 0.5))+
  # geom_text(aes(y=mean+se+1,label=Tukey), position = position_dodge2(0.9), size = 12, 
  #           hjust=0.5, colour = "black")+
  labs(x=" ", y="methanotrophs Biomass (mol/m3)") +
  # scale_y_continuous(position = "right")+
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
