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
OM <- ncvar_get(nc = origin,varid = 'CAERCH4BIOS')
AOM <- ncvar_get(nc = origin,varid = 'CANAERCH4BIOS')

CH4_R <- sum(CH4[121:273])
AM_R <- sum(AM[1:8,121:273])
MM_R <- sum(MM[1:8,121:273])
OM_R <- sum(OM[1:8,121:273])
AOM_R <- sum(AOM[1:8,121:273])

posfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/POS/')
posdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/POS/',posfile,sep = '')
Psin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(posdir[i])
  pCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  pAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  pMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  pOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  pAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(pCH4[121:273])
  AM_A <- sum(pAM[1:8,121:273])
  MM_A <- sum(pMM[1:8,121:273])
  OM_A <- sum(pOM[1:8,121:273])
  AOM_A <- sum(pAOM[1:8,121:273])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/0.2
  SAM <- ((AM_A - AM_R)/AM_R)/0.2
  SMM <- ((MM_A - MM_R)/MM_R)/0.2
  SOM <- ((OM_A - OM_R)/OM_R)/0.2
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/0.2
  
  Psin[i,1] <- SCH4
  Psin[i,2] <- SAM
  Psin[i,3] <- SMM 
  Psin[i,4] <- SOM 
  Psin[i,5] <- SAOM
}
negfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/NEG//')
negdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SJ/NEG/',negfile,sep = '')
Nsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(negdir[i])
  nCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  nAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  nMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  nOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  nAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(nCH4[121:273])
  AM_A <- sum(nAM[1:8,121:273])
  MM_A <- sum(nMM[1:8,121:273])
  OM_A <- sum(nOM[1:8,121:273])
  AOM_A <- sum(nAOM[1:8,121:273])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/(-0.2)
  SAM <- ((AM_A - AM_R)/AM_R)/(-0.2)
  SMM <- ((MM_A - MM_R)/MM_R)/(-0.2)
  SOM <- ((OM_A - OM_R)/OM_R)/(-0.2)
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/(-0.2)
  
  Nsin[i,1] <- SCH4
  Nsin[i,2] <- SAM
  Nsin[i,3] <- SMM 
  Nsin[i,4] <- SOM 
  Nsin[i,5] <- SAOM
}
colnames(Psin) <- c('PCH4','PAM','PMM','POM','PAOM')
colnames(Nsin) <- c('NCH4','NAM','NMM','NOM','NAOM')
Psin <- Psin[c(1,2,8,6,7,5,4,3,11,9,10,12,14,13),]
Nsin <- Nsin[c(1,2,8,6,7,5,4,3,11,9,10,12,14,13),]
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
colnames(sindex) <- c('CH4','AM','MM','OM','AOM')
sindex3 <- matrix(nrow = 28,ncol = 5)
sindex3[c(seq(1,28,2)),] <- sindex[1:14,]
sindex3[c(seq(2,28,2)),] <- sindex[15:28,]
colnames(sindex3) <- c('CH4','AM','MM','OM','AOM')

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
txt<-c("+   -")
xinterceptSet<-c(1:14)*2+0.5
# sindex5$Vname <- c(rep(c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
#                        'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
#                        'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
#                        'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
# ),each = 2))
ggplot(sindex5,aes(x=Var1,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(a) Sanjiang Plain")+
  # scale_x_discrete(breaks=breakset1,labels=labelset1)+
  # scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  
  scale_x_discrete(labels=c('m_dAceProdACmax','','KAce','','m_dGrowRAceMethanogens','','m_dDeadRAceMethanogens','','m_dYAceMethanogens',
                            '','m_dGrowRH2Methanogens','','m_dDeadRH2Methanogens','','m_dYH2Methanogens','',
                            'm_dGrowRMethanotrophs','','m_dDeadRMethanotrophs','','m_dYMethanotrophs','',
                            'm_dGrowRAOMMethanotrophs','','m_dDeadRAOMMethanotrophs','','m_dYAOMMethanotrophs',''))+
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =12,hjust ="bottom",vjust ="bottom")+
  coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=16)+
  theme_bw()+
  theme(legend.position="none",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        axis.text.x=element_text(size=16,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=16,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=36,face="bold"))

########cbs################

origin <- nc_open('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/CBS/cbs_t.clm2.h0.2011-01-01-00000.nc')
CH4 <- ncvar_get(nc = origin,varid = 'CH4_SURF_NETFLUX')
AM <- ncvar_get(nc = origin,varid = 'CACEBIOS')
MM <- ncvar_get(nc = origin,varid = 'CCO2BIOS')
OM <- ncvar_get(nc = origin,varid = 'CAERCH4BIOS')
AOM <- ncvar_get(nc = origin,varid = 'CANAERCH4BIOS')

CH4_R <- sum(CH4[121:273])
AM_R <- sum(AM[1:8,121:273])
MM_R <- sum(MM[1:8,121:273])
OM_R <- sum(OM[1:8,121:273])
AOM_R <- sum(AOM[1:8,121:273])

posfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/CBS/POS/')
posdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/CBS/POS/',posfile,sep = '')
CPsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(posdir[i])
  pCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  pAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  pMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  pOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  pAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(pCH4[121:273])
  AM_A <- sum(pAM[1:8,121:273])
  MM_A <- sum(pMM[1:8,121:273])
  OM_A <- sum(pOM[1:8,121:273])
  AOM_A <- sum(pAOM[1:8,121:273])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/0.2
  SAM <- ((AM_A - AM_R)/AM_R)/0.2
  SMM <- ((MM_A - MM_R)/MM_R)/0.2
  SOM <- ((OM_A - OM_R)/OM_R)/0.2
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/0.2
  
  colnum <- as.double(substr(posfile[i],2,3))
  
  CPsin[colnum,1] <- SCH4
  CPsin[colnum,2] <- SAM
  CPsin[colnum,3] <- SMM 
  CPsin[colnum,4] <- SOM 
  CPsin[colnum,5] <- SAOM
}
negfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/CBS/NEG/')
negdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/CBS/NEG/',negfile,sep = '')
CNsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(negdir[i])
  nCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  nAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  nMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  nOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  nAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(nCH4[121:273])
  AM_A <- sum(nAM[1:8,121:273])
  MM_A <- sum(nMM[1:8,121:273])
  OM_A <- sum(nOM[1:8,121:273])
  AOM_A <- sum(nAOM[1:8,121:273])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/(-0.2)
  SAM <- ((AM_A - AM_R)/AM_R)/(-0.2)
  SMM <- ((MM_A - MM_R)/MM_R)/(-0.2)
  SOM <- ((OM_A - OM_R)/OM_R)/(-0.2)
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/(-0.2)
  
  colnum <- as.double(substr(negfile[i],2,3))
  
  CNsin[colnum,1] <- SCH4
  CNsin[colnum,2] <- SAM
  CNsin[colnum,3] <- SMM 
  CNsin[colnum,4] <- SOM 
  CNsin[colnum,5] <- SAOM
}
colnames(CPsin) <- c('PCH4','PAM','PMM','POM','PAOM')
colnames(CNsin) <- c('NCH4','NAM','NMM','NOM','NAOM')
# CPsin <- CPsin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
# CNsin <- CNsin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
rownames(CPsin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                    'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                    'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                    'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
rownames(CNsin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                    'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                    'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                    'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
csindex <- rbind(CPsin,CNsin)
colnames(csindex) <- c('CH4','AM','MM','OM','AOM')
csindex3 <- matrix(nrow = 28,ncol = 5)
csindex3[c(seq(1,28,2)),] <- csindex[1:14,]
csindex3[c(seq(2,28,2)),] <- csindex[15:28,]
colnames(csindex3) <- c('CH4','AM','MM','OM','AOM')

csindex4 <- melt(csindex3)
csindex5 <- csindex4 %>%
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
txt<-c("+   -")
xinterceptSet<-c(1:14)*2+0.5
# sindex5$Vname <- c(rep(c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
#                        'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
#                        'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
#                        'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
# ),each = 2))
ggplot(csindex5,aes(x=Var1,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(b) Changbai Mountains")+
  # scale_x_discrete(breaks=breakset1,labels=labelset1)+
  # scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  
  scale_x_discrete(labels=c('m_dAceProdACmax','','KAce','','m_dGrowRAceMethanogens','','m_dDeadRAceMethanogens','','m_dYAceMethanogens',
                            '','m_dGrowRH2Methanogens','','m_dDeadRH2Methanogens','','m_dYH2Methanogens','',
                            'm_dGrowRMethanotrophs','','m_dDeadRMethanotrophs','','m_dYMethanotrophs','',
                            'm_dGrowRAOMMethanotrophs','','m_dDeadRAOMMethanotrophs','','m_dYAOMMethanotrophs',''))+
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =12,hjust ="bottom",vjust ="bottom")+
  coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=16)+
  theme_bw()+
  theme(legend.position="none",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        axis.text.x=element_text(size=16,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=16,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=36,face="bold"))

######djh#######

origin <- nc_open('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/DJH/djh_nogens.clm2.h0.2019-01-01-00000 (1).nc')
CH4 <- ncvar_get(nc = origin,varid = 'CH4_SURF_NETFLUX')
AM <- ncvar_get(nc = origin,varid = 'CACEBIOS')
MM <- ncvar_get(nc = origin,varid = 'CCO2BIOS')
OM <- ncvar_get(nc = origin,varid = 'CAERCH4BIOS')
AOM <- ncvar_get(nc = origin,varid = 'CANAERCH4BIOS')

CH4_R <- sum(CH4)
AM_R <- sum(AM[1:4,])
MM_R <- sum(MM[1:4,])
OM_R <- sum(OM[1:4,])
AOM_R <- sum(AOM[1:4,])

posfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/DJH/POS2/')
posdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/DJH/POS2/',posfile,sep = '')
DPsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(posdir[i])
  pCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  pAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  pMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  pOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  pAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(pCH4)
  AM_A <- sum(pAM[1:4,])
  MM_A <- sum(pMM[1:4,])
  OM_A <- sum(pOM[1:4,])
  AOM_A <- sum(pAOM[1:4,])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/0.2
  SAM <- ((AM_A - AM_R)/AM_R)/0.2
  SMM <- ((MM_A - MM_R)/MM_R)/0.2
  SOM <- ((OM_A - OM_R)/OM_R)/0.2
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/0.2
  
  colnum <- as.double(substr(posfile[i],2,3))
  
  DPsin[colnum,1] <- SCH4
  DPsin[colnum,2] <- SAM
  DPsin[colnum,3] <- SMM 
  DPsin[colnum,4] <- SOM 
  DPsin[colnum,5] <- SAOM
}
negfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/DJH/NEG2/')
negdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/DJH/NEG2/',negfile,sep = '')
DNsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(negdir[i])
  nCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  nAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  nMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  nOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  nAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(nCH4)
  AM_A <- sum(nAM[1:4,])
  MM_A <- sum(nMM[1:4,])
  OM_A <- sum(nOM[1:4,])
  AOM_A <- sum(nAOM[1:4,])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/(-0.2)
  SAM <- ((AM_A - AM_R)/AM_R)/(-0.2)
  SMM <- ((MM_A - MM_R)/MM_R)/(-0.2)
  SOM <- ((OM_A - OM_R)/OM_R)/(-0.2)
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/(-0.2)
  
  colnum <- as.double(substr(negfile[i],2,3))
  
  DNsin[colnum,1] <- SCH4
  DNsin[colnum,2] <- SAM
  DNsin[colnum,3] <- SMM 
  DNsin[colnum,4] <- SOM 
  DNsin[colnum,5] <- SAOM
}
colnames(DPsin) <- c('PCH4','PAM','PMM','POM','PAOM')
colnames(DNsin) <- c('NCH4','NAM','NMM','NOM','NAOM')
# DPsin <- DPsin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
# DNsin <- DNsin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
rownames(DPsin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                     'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                     'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                     'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
rownames(DNsin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                     'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                     'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                     'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
dsindex <- rbind(DPsin,DNsin)
colnames(dsindex) <- c('CH4','AM','MM','OM','AOM')
dsindex3 <- matrix(nrow = 28,ncol = 5)
dsindex3[c(seq(1,28,2)),] <- dsindex[1:14,]
dsindex3[c(seq(2,28,2)),] <- dsindex[15:28,]
colnames(dsindex3) <- c('CH4','AM','MM','OM','AOM')

dsindex4 <- melt(dsindex3)
dsindex5 <- dsindex4 %>%
  # convert state to factor and reverse order of levels
  mutate(variable=factor(Var2,levels=rev(sort(unique(Var2))))) %>%
  # create a new variable from count
  mutate(valuefactor=cut(value,breaks=c(-200000,-0.5,-0.05,0,0.05,0.5,max(value,na.rm=T)),
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
txt<-c("+   -")
xinterceptSet<-c(1:14)*2+0.5
# sindex5$Vname <- c(rep(c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
#                        'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
#                        'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
#                        'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
# ),each = 2))
ggplot(dsindex5,aes(x=Var1,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(c) Dajiuhu Peatland")+
  # scale_x_discrete(breaks=breakset1,labels=labelset1)+
  # scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  
  scale_x_discrete(labels=c('m_dAceProdACmax','','KAce','','m_dGrowRAceMethanogens','','m_dDeadRAceMethanogens','','m_dYAceMethanogens',
                            '','m_dGrowRH2Methanogens','','m_dDeadRH2Methanogens','','m_dYH2Methanogens','',
                            'm_dGrowRMethanotrophs','','m_dDeadRMethanotrophs','','m_dYMethanotrophs','',
                            'm_dGrowRAOMMethanotrophs','','m_dDeadRAOMMethanotrophs','','m_dYAOMMethanotrophs',''))+
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =12,hjust ="bottom",vjust ="bottom")+
  coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=16)+
  theme_bw()+
  theme(legend.position="none",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        axis.text.x=element_text(size=16,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=16,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=36,face="bold"))

#######sallie fen######

origin <- nc_open('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SAL/t_sal.clm2.h0.2010-01-01-00000.nc')
CH4 <- ncvar_get(nc = origin,varid = 'CH4_SURF_NETFLUX')
AM <- ncvar_get(nc = origin,varid = 'CACEBIOS')
MM <- ncvar_get(nc = origin,varid = 'CCO2BIOS')
OM <- ncvar_get(nc = origin,varid = 'CAERCH4BIOS')
AOM <- ncvar_get(nc = origin,varid = 'CANAERCH4BIOS')

CH4_R <- sum(CH4)
AM_R <- sum(AM[1:4,])
MM_R <- sum(MM[1:4,])
OM_R <- sum(OM[1:4,])
AOM_R <- sum(AOM[1:4,])

posfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SAL/POS/')
posdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SAL/POS/',posfile,sep = '')
SPsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(posdir[i])
  pCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  pAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  pMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  pOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  pAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(pCH4)
  AM_A <- sum(pAM[1:4,])
  MM_A <- sum(pMM[1:4,])
  OM_A <- sum(pOM[1:4,])
  AOM_A <- sum(pAOM[1:4,])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/0.2
  SAM <- ((AM_A - AM_R)/AM_R)/0.2
  SMM <- ((MM_A - MM_R)/MM_R)/0.2
  SOM <- ((OM_A - OM_R)/OM_R)/0.2
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/0.2
  
  colnum <- as.double(substr(posfile[i],2,3))
  
  SPsin[colnum,1] <- SCH4
  SPsin[colnum,2] <- SAM
  SPsin[colnum,3] <- SMM 
  SPsin[colnum,4] <- SOM 
  SPsin[colnum,5] <- SAOM
}
negfile <- list.files('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SAL/NEG/')
negdir <- paste('D:/AAAA-资料E盘/CLM/microbe/SCDS-sindex/SAL/NEG/',negfile,sep = '')
SNsin <- matrix(ncol = 5,nrow = 14)
for(i in 1:14){
  sdata <- nc_open(negdir[i])
  nCH4 <- ncvar_get(nc = sdata,varid = 'CH4_SURF_NETFLUX')
  nAM <- ncvar_get(nc = sdata,varid = 'CACEBIOS')
  nMM <- ncvar_get(nc = sdata,varid = 'CCO2BIOS')
  nOM <- ncvar_get(nc = sdata,varid = 'CAERCH4BIOS')
  nAOM <- ncvar_get(nc = sdata,varid = 'CANAERCH4BIOS')
  
  CH4_A <- sum(nCH4)
  AM_A <- sum(nAM[1:4,])
  MM_A <- sum(nMM[1:4,])
  OM_A <- sum(nOM[1:4,])
  AOM_A <- sum(nAOM[1:4,])
  
  SCH4 <- ((CH4_A - CH4_R)/CH4_R)/(-0.2)
  SAM <- ((AM_A - AM_R)/AM_R)/(-0.2)
  SMM <- ((MM_A - MM_R)/MM_R)/(-0.2)
  SOM <- ((OM_A - OM_R)/OM_R)/(-0.2)
  SAOM <- ((AOM_A - AOM_R)/AOM_R)/(-0.2)
  
  colnum <- as.double(substr(negfile[i],2,3))
  
  SNsin[colnum,1] <- SCH4
  SNsin[colnum,2] <- SAM
  SNsin[colnum,3] <- SMM 
  SNsin[colnum,4] <- SOM 
  SNsin[colnum,5] <- SAOM
}
colnames(SPsin) <- c('PCH4','PAM','PMM','POM','PAOM')
colnames(SNsin) <- c('NCH4','NAM','NMM','NOM','NAOM')
# DPsin <- DPsin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
# DNsin <- DNsin[c(1,2,5,4,3,8,6,7,11,9,10,12,14,13),]
rownames(SPsin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                     'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                     'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                     'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
rownames(SNsin) <- c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
                     'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
                     'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
                     'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
)
ssindex <- rbind(SPsin,SNsin)
colnames(ssindex) <- c('CH4','AM','MM','OM','AOM')
ssindex3 <- matrix(nrow = 28,ncol = 5)
ssindex3[c(seq(1,28,2)),] <- ssindex[1:14,]
ssindex3[c(seq(2,28,2)),] <- ssindex[15:28,]
colnames(ssindex3) <- c('CH4','AM','MM','OM','AOM')

ssindex4 <- melt(ssindex3)
ssindex5 <- ssindex4 %>%
  # convert state to factor and reverse order of levels
  mutate(variable=factor(Var2,levels=rev(sort(unique(Var2))))) %>%
  # create a new variable from count
  mutate(valuefactor=cut(value,breaks=c(-20,-0.1,-0.05,0,0.05,0.1,max(value,na.rm=T)),
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
txt<-c("+   -")
xinterceptSet<-c(1:14)*2+0.5
# sindex5$Vname <- c(rep(c('m_dAceProdACmax','KAce','m_dGrowRAceMethanogens','m_dDeadRAceMethanogens','m_dYAceMethanogens',
#                        'm_dGrowRH2Methanogens','m_dDeadRH2Methanogens','m_dYH2Methanogens',
#                        'm_dGrowRMethanotrophs','m_dDeadRMethanotrophs','m_dYMethanotrophs',
#                        'm_dGrowRAOMMethanotrophs','m_dDeadRAOMMethanotrophs','m_dYAOMMethanotrophs'
# ),each = 2))
ggplot(ssindex5,aes(x=Var1,y=variable,fill=valuefactor))+
  geom_tile(colour="white",size=1)+
  guides(fill=guide_legend(title="Sensitivity \nIndex"))+
  labs(x="",y="",title="(d) Sallie's fen")+
  # scale_x_discrete(breaks=breakset1,labels=labelset1)+
  # scale_y_discrete(labels=c('CH4','NPP','ER','NEE','GPP'))+
  # scale_x_discrete(labels=c('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',''))+
  
  scale_x_discrete(labels=c('m_dAceProdACmax','','KAce','','m_dGrowRAceMethanogens','','m_dDeadRAceMethanogens','','m_dYAceMethanogens',
                            '','m_dGrowRH2Methanogens','','m_dDeadRH2Methanogens','','m_dYH2Methanogens','',
                            'm_dGrowRMethanotrophs','','m_dDeadRMethanotrophs','','m_dYMethanotrophs','',
                            'm_dGrowRAOMMethanotrophs','','m_dDeadRAOMMethanotrophs','','m_dYAOMMethanotrophs',''))+
  annotate(geom="text",x=txtx,y=0.3,label=txt,size =12,hjust ="bottom",vjust ="bottom")+
  coord_fixed()+
  geom_vline(xintercept=xinterceptSet, color = "grey",size=0.5)+
  scale_fill_manual(values = color,na.value = "grey90")+
  theme_grey(base_size=16)+
  theme_bw()+
  theme(legend.position="none",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=24,face="bold"),
        legend.key.height=grid::unit(0.7,"cm"),
        legend.key.width=grid::unit(0.3,"cm"),
        axis.text.x=element_text(size=16,face = 'bold',colour=textcol,vjust = 1,hjust = 1,angle = 60),
        axis.text.y=element_text(size=16,face = 'bold',vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=1),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.2,2,0,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=36,face="bold"))

