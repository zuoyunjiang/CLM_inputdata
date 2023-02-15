try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/test_t/test_t.clm2.h0.2019-01-01-00000 (119).nc')
nogen_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/sindex/origin/nogens.clm2.h0.2019-01-01-00000 (3).nc')
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

# library(RColorBrewer)
# coul <- brewer.pal(2, "Set2") 
# data <- data.frame(
#   name= c('GEN','NOGEN'),
#   value= c(mean(DOC_10),mean(DOC_101))
# )
# 
# barplot(height=data$value, names=data$name, col=coul )
# 
# ggplot(data, aes(x=name, y=value, fill=name)) + 
#   geom_bar(stat="identity")
# 
# DOCDATA <- cbind(DOC_10,DOC_101)
# DOCDATA1 <- melt(DOCDATA)
# DOCDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")
# 
# ggplot() +
#   geom_bar(aes(y = value, x = name, fill = name), size=1.5, data = data,
#            stat="identity") +
#   theme(legend.position="bottom", legend.direction="horizontal",
#         legend.title = element_blank()) +
#   # scale_x_continuous(breaks=seq(2006,2014,1)) +
#   labs(x="DOY", y="DOC(g/m3)") +
#   ggtitle("(a) Sanjiang Plain") +
#   # scale_color_manual(values=fill) +
#   theme_base()+
#   theme(axis.line = element_line(size=1, colour = "black"), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), panel.border = element_blank(),
#         panel.background = element_blank()) +
#   theme(plot.title=element_text(family="xkcd-Regular"), text=element_text(family="xkcd-Regular"),
#         axis.text.x=element_text(colour="black", size = 10),
#         axis.text.y=element_text(colour="black", size = 10),
#         legend.key=element_rect(fill="white", colour="white"))

ACEDATA <- cbind(CO2PROCH4_10,CO2PROCH4_101)
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^8, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x=" ", y=" ") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=5.3,label="(a)",size = 20,face = 'bold')+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = c(0.9,0.9),
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    axis.text.x = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    # axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_blank(),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^7, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x=" ", y=" ") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=6.3,label="(b)",size = 20,face = 'bold')+
  # annotate("text",x=-1,y=6.3,label="x10^-7",size = 20,angle = -90)+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = 'none',
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    axis.text.x = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    # axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_blank(),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))
####cbs
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/cbs_t.clm2.h0.2011-01-01-00000 (110).nc')
nogen_data<- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/cbs/mic-gen/cbs_nogens.clm2.h0.2011-01-01-00000 (3).nc')
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
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^8, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x=" ", y=" ") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=11,label="(c)",size = 20,face = 'bold')+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = 'none',
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    axis.text.x = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    # axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_blank(),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^7, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x=" ", y=" ") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=14.2,label="(d)",size = 20,face = 'bold')+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = 'none',
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    axis.text.x = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    # axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_blank(),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))
###djl
try_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_t.clm2.h0.2019-01-01-00000 (231).nc')
nogen_data <- nc_open(filename = 'D:/AAAA-资料E盘/CLM/microbe/djh/djh_nogens.clm2.h0.2019-01-01-00000 (11).nc')
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
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^7, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x=" ", y="Methane Production (mol/m3/s)") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=41,label="(e)",size = 20,face = 'bold')+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = 'none',
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    axis.text.x = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    # axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_blank(),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^5, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x="DOY", y="Methane Production (mol/m3/s)") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=11.5,label="(f)",size = 20,face = 'bold')+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = 'none',
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    axis.text.x = element_blank(),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    # axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_blank(),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))
#####sallie’s fen
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
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^7, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x="DOY", y=" ") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=5,label="(g)",size = 20,face = 'bold')+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = 'none',
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))


ACEDATA <- cbind(ACEPROCH4_10,ACEPROCH4_101)
ACEDATA1 <- melt(ACEDATA)
ACEDATA1$Group <- rep(c('GEN','NOGEN'), each = 365)
# fill <- c("#56B4E9", "#ff69b4")

ggplot() +
  geom_line(aes(y = value*10^6, x = Var1, colour = Group), size=1.5, data = ACEDATA1,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  # scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x="DOY", y=" ") + # 10-8
  # ggtitle("(a) Sanjiang Plain") +
  annotate("text",x=0,y=6.2,label="(h)",size = 20,face = 'bold')+
  # scale_color_manual() +
  theme_base()+
  theme(
    strip.text.x = element_text(size = 22),
    legend.position = 'none',
    legend.title = element_blank(),
    # legend.title=element_text(colour='black'),
    legend.margin=margin(grid::unit(0,"cm")),
    legend.text=element_text(colour='black',size=22,face="bold"),
    legend.key.height=grid::unit(0.5,"cm"),
    legend.key.width=grid::unit(0.5,"cm"),
    axis.text.x=element_text(size=22,colour='black',vjust = 1,hjust = 1),
    axis.text.y=element_text(size=22,vjust=0.2,colour='black'),
    axis.title.y = element_text(size = 22,face = 'bold'),
    axis.title.x = element_text(size = 22,face = 'bold'),
    axis.ticks=element_line(size=0.5),
    plot.background=element_blank(),
    # panel.border=element_blank(), # tianjia bianjie
    plot.margin=margin(0.5,1.5,0.5,1.3,"cm"),
    plot.title=element_text(colour='black',hjust=0.5,size=24,face="bold"))
