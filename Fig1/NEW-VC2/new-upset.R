setwd("C:\\Users\\lzl\\OneDrive - 中山大学\\AMD博士课题\\超微型微生物\\分析\\virus\\all_vh\\without_ref\\fig1\\NEW-VC2")

cpr=read.csv("CPR.txt")
dpann=read.csv("DPANN.txt")
gwvc=read.csv("GWVC.txt")
ref=read.csv("REF.txt")
clist=unique(cpr$VC.name)
dlist=unique(dpann$VC.name)
glist=unique(gwvc$name)
rlist=unique(ref$name)
gulist <- glist[glist %in%clist | glist %in% dlist]
rulist <- rlist[rlist %in%clist | rlist %in% dlist]

ldata = list(Ref=rulist,GWVC=gulist,CPR=clist,DPANN=dlist)

library(ggVennDiagram)
ggVennDiagram(ldata)


library(UpSetR)
data2<-fromList(ldata)
upset(data2) # 要先加载UpSetR
