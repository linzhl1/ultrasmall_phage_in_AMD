library(vegan)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(multcompView)
library(ggsci)

setwd("C:\\Users\\lzl\\OneDrive - 中山大学\\AMD博士课题\\超微型微生物\\分析\\virus\\all_vh\\without_ref")

par(mfrow = c(1, 2))
# CPR
{
  cpi=read.table('fig1\\c_hallmark.csv',sep = ',', stringsAsFactors = FALSE,row.names = 1,header = T,check.names = F)
  wcpi=read.table('fig1\\cpr_g_hallmark.csv',sep = ',', stringsAsFactors = FALSE,row.names = 1,header = T,check.names = F)
  #分析cpi和wcpi第一列的组间差异t检验以及秩和检验
  cpi = cpi[,4]
  wcpi = wcpi[,4]
  cpi = as.numeric(cpi)
  wcpi = as.numeric(wcpi)
  wilcox.test(cpi,wcpi,alternative = c('less'))
  boxplot(cpi, wcpi, names = c("AMD", "Groundwater"), main = "CPR", ylab = "PI", col = c("lightblue", "lightgreen"))
  amd = data.frame(group='AMD', crate=cpi)
  ground = data.frame(group='Groundwater', crate=wcpi)
  nfactor = rbind(amd, ground)
  p1=ggboxplot(nfactor, x = "group", y = "crate",
            color = "group", palette = "jco")+ stat_compare_means(comparisons = list( c("AMD", "Groundwater")),
                                                                  label="p.signif", method = "wilcox.test", size = 10)+
    labs(x="",y="pI of CPR virus")+
    theme_classic()+
    theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size=0.5),axis.text.x = element_text(size=18,color = "black"),axis.text.y = element_text(size=18,color="black"),axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),legend.text = element_text(size=18))+
    scale_y_continuous(limits =c(2,14),breaks = seq(2,14,2))+
    theme(legend.position = "none")

p1
}

#DPANN
{
  dpi=read.table('fig1\\d_hallmark.csv',sep = ',', stringsAsFactors = FALSE,row.names = 1,header = T,check.names = F)
  wdpi=read.table('fig1\\dpann_g_hallmark.csv',sep = ',', stringsAsFactors = FALSE,row.names = 1,header = T,check.names = F)
  #分析dpi和wdpi第一列的组间差异t检验以及秩和检验
  dpi=dpi[,4]
  wdpi=wdpi[,4]
  dpi=as.numeric(dpi)
  wdpi=as.numeric(wdpi)
  t.test(dpi,wdpi)
  wilcox.test(dpi,wdpi,alternative = c('less'))
  amd = data.frame(group='AMD', crate=dpi)
  ground = data.frame(group='Groundwater', crate=wdpi)
  nfactor = rbind(amd, ground)
  p2=ggboxplot(nfactor, x = "group", y = "crate",
               color = "group", palette = "jco")+ stat_compare_means(comparisons = list( c("AMD", "Groundwater")),
                                                                     label="p.signif", method = "wilcox.test",size = 10)+
    labs(x="",y="pI of DPANN virus")+
    theme_classic()+
    theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size=0.5),axis.text.x = element_text(size=18,color = "black"),axis.text.y = element_text(size=18,color="black"),axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),legend.text = element_text(size=18))+
    scale_y_continuous(limits =c(2,14),breaks = seq(2,14,2))+
    theme(legend.position = "none")
  
  p2
}
ggarrange(p1, p2, ncol = 2, nrow = 1,
          labels = c("a","b"), # 添加标签
          font.label = list(size = 24, face = "bold")) # 设置标签字体样式
