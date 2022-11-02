####sindex#####
library(ncdf4)
library(ggplot2)
library(caret)
library(cowplot)
library(reshape2)
library(plot3D)
library(RColorBrewer)
library(dplyr)
library(scales)
library(ggpmisc)
# install.packages("coolbutuseless/threed")
# devtools::install_github('coolbutuseless/ggthreed')
library(ggthreed)
install.packages('plotrix')
library(plotrix)
library(grid)

#####################
origin <- nc_open('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/test_t.clm2.h0.2012-01-01-00000 (428).nc')
CH4 <- ncvar_get(nc = origin,varid = 'CH4_SURF_NETFLUX')
AM <- ncvar_get(nc = origin,varid = 'CACEBIOS')
MM <- ncvar_get(nc = origin,varid = 'CCO2BIOS')
AER <- ncvar_get(nc = origin,varid = 'CAERCH4BIOS')
ANAER <- ncvar_get(nc = origin,varid = 'CANAERCH4BIOS')

CH4_R <- sum(CH4[121:273])
AM_R <- sum(AM[1:8,121:273])
MM_R <- sum(MM[1:8,121:273])
AER_R <- sum(AER[1:8,121:273])
ANAER_R <- sum(ANAER[1:8,121:273])

posfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/POS/')
posdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/POS/',posfile,sep = '')
Psin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(posdir[i])
  pCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  pAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  pMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  pAER <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  pANAER <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(pCH4[121:273])
  AM_A <- sum(pAM[1:8,121:273])
  MM_A <- sum(pMM[1:8,121:273])
  AER_A <- sum(pAER[1:8,121:273])
  ANAER_A <- sum(pANAER[1:8,121:273])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/0.2
  SAM <- ((AM_A - AM_R)/AM_R)/0.2
  SMM <- ((MM_A - MM_R)/MM_R)/0.2
  SAER <- ((AER_A - AER_R)/AER_R)/0.2
  SANAER <- ((ANAER_A - ANAER_R)/ANAER_R)/0.2
  
  Psin[i,1] <- SCH4
  Psin[i,2] <- SAM
  Psin[i,3] <- SMM 
  Psin[i,4] <- SAER 
  Psin[i,5] <- SANAER
}
negfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/NEG//')
negdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/NEG/',negfile,sep = '')
Nsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(negdir[i])
  nCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  nAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  nMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  nAER <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  nANAER <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(nCH4[121:273])
  AM_A <- sum(nAM[1:8,121:273])
  MM_A <- sum(nMM[1:8,121:273])
  AER_A <- sum(nAER[1:8,121:273])
  ANAER_A <- sum(nANAER[1:8,121:273])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/0.2
  SAM <- ((AM_A - AM_R)/AM_R)/0.2
  SMM <- ((MM_A - MM_R)/MM_R)/0.2
  SAER <- ((AER_A - AER_R)/AER_R)/0.2
  SANAER <- ((ANAER_A - ANAER_R)/ANAER_R)/0.2
  
  Nsin[i,1] <- SCH4
  Nsin[i,2] <- SAM
  Nsin[i,3] <- SMM 
  Nsin[i,4] <- SAER 
  Nsin[i,5] <- SANAER
}
colnames(Psin) <- c('PCH4','PAM','PMM','PAER','PANAER')
colnames(Nsin) <- c('NCH4','NAM','NMM','NAER','NANAER')
Psin <- Psin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
Nsin <- Nsin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
rownames(Psin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                    'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                    'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                    'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
                    )
rownames(Nsin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                    'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                    'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                    'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
####heng####
sindex <- cbind(Psin,Nsin)
sindex <- sindex[,c(1,6,2,7,3,8,4,9,5,10)]
sindex1 <- melt(sindex)
sindex2 <- sindex1 %>%
  # convert state to factor and reverse order of levels
  mutate(variable=factor(Var2,levels=rev(sort(unique(Var2))))) %>%
  # create a new variable from count
  mutate(valuefactor=cut(value,breaks=c(-20,-0.1,-0.005,0,0.005,0.1,max(value,na.rm=T)),
                         labels=c('< -0.1',"-0.1 ~ -0.05","-0.05~0","0~0.05","0.05 ~ 0.1", '>0.1'))) %>%
  # change level order
  mutate(valuefactor=factor(as.character(valuefactor),levels=rev(levels(valuefactor))))

textcol <- "black"
# color <- colorRampPalette(colors = c("red",'white','blue'))(6)
# color <- c("#99000D", "#FC9272", "#FFF5F0", "#F7FBFF","#9ECAE1", "#084594" )
color <- c("#FF0000", "#FF6666", "#FFCCCC", "#CCCCFF","#6699FF", "#0000FF" )
# color <- c("#084594", "#9ECAE1", "#F7FBFF", "#FFF5F0", "#FC9272", "#99000D")
breakset1<-seq(1,14,2)
# labelset1<-c('AceProdACmax','bdnr','br_mr','DeadH2Methanogens', 'k_dom', 'dom_diffus','flnr','GrowACEMethanogens','GrowH2Methanogens', 'grperc', 'KACE', 'froot_leaf',
             # 'YAceMethanogens','YH2Methanogens')
# txtx<-c(1:7)*2-1.2
# txt<-c("-    +")
xinterceptSet<-c(1:7)*2+0.5
ggplot(sindex2,aes(x=Var1,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(a) Sanjiang Plain")+
  # scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+

  # scale_x_discrete(labels=c('AceProdACmax','','bdnr','','br_mr','','DeadACEMethanogens','','DeadH2Methanogens','', 'DeadMethanotrophs','','k_dom',
  #                           '','dom_diffus','','flnr','','GrowACEMethanogens','','GrowH2Methanogens','','GrowRmethanotrophs',
  #                           '','grperc', '','KACE', '','froot_leaf', '','YAceMethanogens','','YH2Methanogens','','YMethanotrophs',''))+
  # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4"),na.value = "grey90")+  #"#ddf1da"
  # annotate(geom="text",x=txtx,y=0.3,label=txt,size =18,hjust ="bottom",vjust ="bottom")+
  coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=16)+
  # theme_bw()
  theme(legend.position="none",legend.direction="vertical",
        # legend.box = 'horizontal',
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        # legend.box.just = ,
        axis.text.x=element_text(size=16,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=16,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=48,face="bold"))
##########
######vertical#####
sindex <- rbind(Psin,Nsin)
colnames(sindex) <- c('CH4','AM','MM','AER','ANAER')
sindex3 <- matrix(nrow = 28,ncol = 5)
sindex3[c(seq(1,28,2)),] <- sindex[1:14,]
sindex3[c(seq(2,28,2)),] <- sindex[15:28,]
colnames(sindex3) <- c('CH4','AM','MM','AER','ANAER')

sindex4 <- melt(sindex3)
sindex5 <- sindex4 %>%
  # convert state to factor and reverse order of levels
  mutate(variable=factor(Var2,levels=rev(sort(unique(Var2))))) %>%
  # create a new variable from count
  mutate(valuefactor=cut(value,breaks=c(-20,-0.1,-0.005,0,0.005,0.1,max(value,na.rm=T)),
                         labels=c('< -0.1',"-0.1 ~ -0.05","-0.05~0","0~0.05","0.05 ~ 0.1", '>0.1'))) %>%
  # change level order
  mutate(valuefactor=factor(as.character(valuefactor),levels=rev(levels(valuefactor))))

textcol <- "black"
# color <- colorRampPalette(colors = c("red",'white','blue'))(6)
# color <- c("#99000D", "#FC9272", "#FFF5F0", "#F7FBFF","#9ECAE1", "#084594" )
color <- c("#FF0000", "#FF6666", "#FFCCCC", "#CCCCFF","#6699FF", "#0000FF" )
# color <- c("#084594", "#9ECAE1", "#F7FBFF", "#FFF5F0", "#FC9272", "#99000D")
breakset1<-seq(1,28,2)
labelset1<-c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
             'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
             'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
             'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
txtx<-c(1:14)*2-1.2
txt<-c("+ -")
xinterceptSet<-c(1:14)*2+0.5
ggplot(sindex5,aes(x=Var1,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(a) Sanjiang Plain")+
  # scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  
  scale_x_discrete(labels=c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                            'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                            'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                            'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
  ))+
  # scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4"),na.value = "grey90")+  #"#ddf1da"
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =18,hjust ="bottom",vjust ="bottom")+
  coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=16)+
  theme_bw()+
  theme(legend.position="none",legend.direction="vertical",
        # legend.box = 'horizontal',
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        # legend.box.just = ,
        axis.text.x=element_text(size=16,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=16,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=48,face="bold"))



