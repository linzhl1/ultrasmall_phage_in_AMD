library(ggplot2)
library(vegan)
setwd("C:\\Users\\lzl\\OneDrive - 中山大学\\AMD博士课题\\超微型微生物\\分析\\virus\\all_vh\\without_ref")
data2<-read.table('abundance\\CPR_relate_vir_abun.tab',header = T,row.names = 1,check.names = FALSE)
data2=t(data2)
sp <- specaccum(data2, method = 'random', permutations = 10000)
#d=data.frame(sp$sites,sp$richness,sp$sd)
#d$sp.richness=d$sp.richness*100
#d$sp.sd=d$sp.sd*100

data3=read.csv('abundance\\DPANN_relate_vir_abun.tab',sep = "\t",header = T,row.names = 1,check.names = FALSE)
data3=t(data3)
sp3=specaccum(data3, method = 'random', permutations = 10000)
d=data.frame(sp$sites,sp$richness,sp$sd,sp3$sites,sp3$richness,sp3$sd)

p1=ggplot(d)+geom_line(aes(x=sp.sites,y=sp.richness),size=1.1,col = '#94B9E5')+
  geom_errorbar(aes(sp.sites,ymin=sp.richness-sp.sd,ymax=sp.richness+sp.sd),lwd=0.8,width=1,position=position_dodge(0),
                color='#94B9E5',alpha = 0.6)+
  labs(x="Number of Samples",y="Cumulative number")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size=0.5),axis.text.x = element_text(size=18,color = "black"),axis.text.y = element_text(size=18,color="black"),axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),legend.text = element_text(size=18))+
  scale_x_continuous(limits =c(0,100),breaks = seq(0,100,20))+scale_y_continuous(limits =c(0,5500))+
  coord_cartesian(ylim=c(0,300),xlim = c(0,100))
p2=ggplot(d)+
  geom_line(aes(x=sp3.sites,y=sp3.richness),size=1.1,col = '#d45959')+
  geom_errorbar(aes(sp3.sites,ymin=sp3.richness-sp3.sd,ymax=sp3.richness+sp3.sd),lwd=0.8,width=1,position=position_dodge(0),
                color='#d45959',alpha = 0.6)+
  labs(x="Number of Samples",y="Cumulative number")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),axis.line.x = element_line(size = 0.5),
        axis.line.y = element_line(size=0.5),axis.text.x = element_text(size=18,color = "black"),axis.text.y = element_text(size=18,color="black"),axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),legend.text = element_text(size=18))+
  scale_x_continuous(limits =c(0,100),breaks = seq(0,100,20))+scale_y_continuous(limits =c(0,120),breaks = seq(0,120,40))
ggarrange(p1, p2, ncol = 2, nrow = 1,
          labels = c("a","b"), # 添加标签
          font.label = list(size = 24, face = "bold")) # 设置标签字体样式
# 12.5x5.4