dir.create('scripts')
dir.create('results')
rm(list = ls())
options(stringsAsFactors = F)
source('/pub1/data/mg_projects/projects/codes/mg_base.R')
# source('z:/projects/codes/mg_base.R')
ggplotKMCox=function(dat,title='Groups',labs=NULL,add_text=NULL, col = mycolor){
  library(ggplot2)
  colnames(dat)=c('time','status','groups')
  #sdf<-survdiff(Surv(time,status) ~ groups,data=dat)
  #print((sdf))
  #summary(sdf)
  #p<-pchisq(sdf$chisq,length(sdf$n)-1,lower.tail=FALSE)
  sf<-survfit(Surv(time,status) ~ groups,data=dat)
  surv=survminer::ggsurvplot(sf, data = dat, palette = col, #jco palette 
                             pval = TRUE, surv.median.line='hv'
                             # ,conf.int = T
                             ,conf.int.style ='step'
                             , pval.coord=c(0, 0.2), #Add p-value 
                             risk.table = TRUE, 
                             legend.title = title
                             ,legend.labs = labs
  )
  p1=surv$plot+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                                ,axis.text.x=element_blank()
                                ,axis.title.x=element_blank()
                                ,plot.margin=unit(c(0.2, 0.2, 0, 0.1), "inches")
                                #,axis.title.y=element_blank()
                                ,legend.position=c(1,1), legend.justification=c(1,1)
                                ,legend.background = element_rect(fill = NA, colour = NA)
                                ,legend.title = element_text(family="Times",face="plain")
                                ,legend.text = element_text(family="Times",face="plain"))
  #p1=p1+text()
  #tms=data.frame(Group=tms.gp,value=tms.tps,Attribute=rep(data_m[1,1],length(tms.gp))
  #               ,ymax=rep(max(ylim),length(tms.gp)))
  #p4=p4+geom_text(data=tms,aes(x=Group, y=ymax, label=value),color="black")
  if(!is.null(add_text)){
    text.tb=surv$data.survplot[1,]
    text.tb[1,1]=0
    text.tb[1,5]=0
    text.tb$Text=add_text
    p1=p1+geom_text(data=text.tb,aes(x=time, y=surv, label=Text),color="black",hjust =0)
  }
  
  p2=surv$table+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                                 #,axis.text.x=element_blank()
                                 #,axis.title.x=element_blank()
                                 #,axis.title.y=element_blank()
                                 ,plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches")
                                 ,plot.title=element_blank()
                                 ,legend.position=c(1,1), legend.justification=c(1,1)
                                 #,legend.background = element_rect(fill = NA, colour = NA)
                                 ,legend.title = element_text(family="Times",face="plain")
                                 ,legend.text = element_text(family="Times",face="plain"))
  
  g2=ggpubr::ggarrange(p1,p2, ncol = 1, nrow = 2,heights = c(1,0.3),align = "v")
  return(g2)
}
mg_violin <- function(data,xangle=0,ylab='value',xlab='',leg.title='Group',test_method='anova',cmp_test_method='t.test',legend.pos='r',melt=F,jitter=T,ylim=NULL,show_compare=NULL,point_size=NULL,mycolor = mycolor){
  library(ggplot2)
  if(is.null(ylim)){
    
  }
  if(melt){
    data_m=data
    colnames(data_m)=c('Group','value')
  }else{
    data_m=reshape2::melt(data)
    colnames(data_m)=c('Group','value')
  }
  if(!is.null(ylim)){
    data_m$value[data_m$value>ylim[2]]<-NA
  }
  data_m=data_m[which(!is.na(data_m[,1])),]
  if(xangle==0){
    tx=element_text(colour="black",family="Times")
  }else{
    tx=element_text(angle=xangle,hjust = 1,colour="black",family="Times")
  }
  
  pos='right'
  if(is.null(legend.pos)){
    pos='none'
  }else if(legend.pos=='tr'){
    pos=c(1,1)
  }else if(legend.pos=='br'){
    pos=c(1,0)
  }else if(legend.pos=='tl'){
    pos=c(0,1)
  }else if(legend.pos=='bl'){
    pos=c(0,0)
  }else if(legend.pos=='t'){
    pos='top'
  }else if(legend.pos=='r'){
    pos='right'
  }else if(legend.pos=='b'){
    pos='bottom'
  }
  uni.group=unique(data_m[,1])
  ct=length(uni.group)
  
  p1<-ggplot(data_m,aes(x=Group,y=value))+geom_violin(alpha=1)+scale_color_manual(values = mycolor)
  if(ct<=4){
    p1=p1+ggsci::scale_fill_igv()
  }else if(ct<=10){
    p1=p1+ggsci::scale_fill_jco(name=leg.title)
  }else if(ct<=20){
    p1=p1+ggsci::scale_fill_d3(palette = "category20",name=leg.title)
  }else if(ct<=30){
    cbPalette=c(ggsci::pal_jco("nrc", alpha = 0.6)(10),ggsci::pal_jco("category20", alpha = 0.6)(20))
    p1=p1+scale_fill_manual(values=cbPalette[1:ct])
  }else if(ct<=38){
    cbPalette=c(ggsci::pal_jco()(10)
                ,ggsci::pal_jco("nrc", alpha = 0.6)(10)
                ,ggsci::pal_d3("category20", alpha = 0.6)(20)
                ,ggsci::pal_jco("default", alpha = 0.6)(8))
    p1=p1+scale_fill_manual(values=cbPalette[1:ct])
  }
  
  if(jitter){
    if(is.null(point_size)){
      p1<-p1+geom_jitter(alpha=0.3,col='black',show.legend=FALSE,width = 0.2)
    }else{
      p1<-p1+geom_jitter(alpha=0.3,col='black',show.legend=FALSE,width = 0.2,size=point_size)
    }
  }
  
  p1=p1+theme_bw()+geom_boxplot(width=0.2,aes(fill=Group),outlier.shape = NA)
  p1=p1+theme(axis.text.x=tx, 
              axis.text.y=element_text(family="Times",face="plain"), 
              axis.title.y=element_text(family="Times",face="plain"), 
              legend.text=element_text(face="plain", family="Times", colour="black" 
              ),
              legend.title=element_text(face="plain", family="Times", colour="black"
              ),
              legend.justification=pos, legend.position=pos
              ,legend.background = element_rect(fill = NA, colour = NA)
  )+ylab(ylab)+xlab(xlab)
  til=''
  if(test_method=='anova'){
    if(length(unique(data_m[,1]))<3){
      x1=data_m[,2][which(data_m[,1]==unique(data_m[,1])[1])]
      x2=data_m[,2][which(data_m[,1]==unique(data_m[,1])[2])]
      pv=t.test(x1,x2)$p.value 
      til=paste0('t-tests p=',signif(pv,2))
    }else{
      fit <- aov(value~Group, data = data_m)
      pv=summary(fit)[[1]][5][[1]]
      fv=summary(fit)[[1]][4][[1]]
      til=paste0('ANOVA tests p=',signif(pv,2))
    }
  }else{
    if(length(unique(data_m[,1]))<3){
      x1=data_m[,2][which(data_m[,1]==unique(data_m[,1])[1])]
      x2=data_m[,2][which(data_m[,1]==unique(data_m[,1])[2])]
      pv=wilcox.test(x1,x2)$p.value 
      til=paste0('wilcox.tests p=',signif(pv,2))
    }else{
      fit=kruskal.test(value~Group, data = data_m)
      pv=fit$p.value
      til=paste0('Kruskal-Wallis test p=',signif(pv,2))
    }
  }
  p1=p1+ggtitle(til) 
  if(!is.null(ylim)){
    p1=p1+ylim(ylim)
  }
  if(is.null(show_compare)){
    if(length(uni.group)>5){
      show_compare=F
    }else{
      show_compare=T
    }
  }
  if(show_compare){
    comps=list()
    for(i in 1:(length(uni.group)-1)){
      for(j in (i+1):length(uni.group)){
        comps=c(comps,list(c(uni.group[i],uni.group[j])))
      }
    }
    p1=p1+ggpubr::stat_compare_means(comparisons = comps,method = cmp_test_method,label= "p.signif", step_increase = 0.0)
  }
  return(p1)
}
ggplotTimeROC=function(time,status,score,mks=c(1,3,5), col = mycolor){
  #time=g.os
  #status=g.ev
  #score=as.numeric(cpm.score)
  #cx=coxRun(data.frame(time,status,score))
  #if(cx[1]<=1){
  #  score=-1*score
  #}
  roc.tm=mg_surv_pROC(time,status,score,mks)
  print('roc.tm')
  print((roc.tm))
  library(survival)
  library(ggplot2)
  mks=mg_predict_time_ymd(time,mks)
  print(mks)  
  ROC.DSST=timeROC::timeROC(T=time,
                            delta=status
                            ,marker=score,
                            cause=1,weighting="marginal",
                            times=mks,
                            iid=TRUE)
  print(ROC.DSST)
  mks=mks[which(!is.na(ROC.DSST$AUC)&ROC.DSST$AUC>0)]
  print(mks)
  if(length(mks)>0){
    if(max(ROC.DSST$AUC)<0.5){
      score=-1*score
    }
    ROC.DSST=timeROC::timeROC(T=time,
                              delta=status
                              ,marker=score,
                              cause=1,weighting="marginal",
                              times=mks,
                              iid=TRUE)
    print(ROC.DSST$times)
    if(max(ROC.DSST$times)<20){
      lb=paste0(ROC.DSST$times,'-Years')
    }else if(max(ROC.DSST$times)<365){
      lb=paste0(round(ROC.DSST$times/12,0),'-Years')
    }else{
      lb=paste0(round(ROC.DSST$times/365,0),'-Years')
    }
    
    lbs=paste0(lb,',AUC=',round(ROC.DSST$AUC,2),',95%CI(',paste0(round(confint(ROC.DSST,level = 0.95,na.rm=T)$CI_AUC[,1]/100,2),'-',
                                                                 round(confint(ROC.DSST,level = 0.95,na.rm=T)$CI_AUC[,2]/100,2)),')')
    #roc.tm=ROC.DSST$times[which(ROC.DSST$times>0)]
    
    #p.dat=rbind()
    #for(i in which(ROC.DSST$times>0)){
    #los=lowess(ROC.DSST$FP[,i], y=ROC.DSST$TP[,i], f = 1/3, iter = 100)
    #los$x=c(0,los$x,1)
    #los$y=c(0,los$y,1)
    # p.dat=rbind(p.dat,data.frame(los$x, y=los$y,rep(lbs[i],length(los$y)),stringsAsFactors = F))
    #}
    
    p.dat=rbind()
    print(length(roc.tm))
    for(i in 1:length(roc.tm)){
      #print(i)
      r1=roc.tm[[i]]
      x1=1-r1$specificities
      y1=r1$sensitivities
      #print(cbind(1-r1$specificities,r1$sensitivities))
      nx1=unique(x1)
      ny1=c()
      for(x in unique(x1)){
        x.inds=which(x1==x)
        if(length(x.inds)>0&x<0.5){
          ny1=c(ny1,min(y1[x.inds]))
        }else if(length(x.inds)>0){
          ny1=c(ny1,max(y1[x.inds]))
        }else{
          ny1=c(ny1,y1[x.inds][1])
        }
      }
      #print(cbind(nx1,ny1))
      p.dat=rbind(p.dat,data.frame(x=nx1, y=ny1,rep(lbs[i],length(nx1)),stringsAsFactors = F))
    }
    colnames(p.dat)=c('V1','V2','Type')
    p.dat=as.data.frame(p.dat)
    
    p1=ggplot(p.dat, aes(x=V1,y=V2, fill=Type))
    p1=p1+geom_line(aes(colour=Type),lwd=1.1)+
      theme_bw()+
      xlab('False positive fraction')+
      ylab('True positive fraction') +
      scale_color_manual(values = col)
    #p1=p1+stat_smooth(aes(colour=Type),se = FALSE, size = 1)+theme_bw()+xlab('False positive fraction')+ylab('True positive fraction') 
    
    p1=p1+theme(axis.text.y=element_text(family="Times",face="plain"),axis.text.x=element_text(family="Times",face="plain")
                ,axis.title.x=element_text(family="Times",face="plain"),axis.title.y=element_text(family="Times",face="plain")
                ,plot.title=element_blank()
                ,plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "inches")
                ,legend.position=c(1,0)
                ,legend.justification=c(1,0)
                ,legend.background = element_rect(fill = NA, colour = NA)
                ,legend.title = element_text(family="Times",face="plain")
                ,legend.text = element_text(family="Times",face="plain"))
    return(p1)
  }else{
    return(mg_getplot_bank('No data plot by ROC!'))
  }
}
mg_PlotMutiBoxplot=function(data,group,group_cols='jco'
                            ,test_method=c('t.test','wilcox.test','paired_t.test','paired_wilcox.test','anova','kruskal.test')[1]
                            ,order=NULL,size=1,fill=F,outlier.shape=NA,yscale=c('none','log2','log10')[1]
                            ,xangle=45,ylab='Value',xlab='',box_span=0.7
                            ,orientation = c("vertical", "horizontal", "reverse")[1]
                            ,legend.pos=NULL,melt=F,ylim=NULL,binwidth=0.05
                            ,add=c("none", "dotplot", "jitter", "boxplot", "point", "mean"
                                   , "mean_se", "mean_sd", "mean_ci", "mean_range", "median"
                                   , "median_iqr", "median_mad", "median_range")[3]){
  paired=FALSE
  if(test_method=='paired_t.test'|test_method=='paired_wilcox.test'){
    test_method=gsub('paired_','',test_method)
    paired=TRUE
  }
  print(class(data))
  if(add=='jitter'){
    fill=F
  }
  
  library(ggplot2)
  if(!melt){
    #print(class(data))
    if(class(data)=='numeric'|class(data)=='integer'){
      data=as.numeric(data)
      vd1.sbs=data.frame(group,rep('Tag',length(group)),data)
      #print(vd1.sbs)
    }else{
      data=as.data.frame(data)
      data$ID=group
      vd1.sbs <- reshape2::melt(data, id.vars=c("ID"))
    }
    colnames(vd1.sbs)=c('category','type','Score')
    Data=vd1.sbs
  }else{
    vd1.sbs=data
    colnames(vd1.sbs)=c('category','type','Score')
    Data=vd1.sbs
  }
  
  #vd1.sbs[,2]=paste0('C',as.numeric(as.character(vd1.sbs[,2])))
  if(is.null(order)){
    order=unique(vd1.sbs[,2])
  }
  
  if(xangle==0){
    tx=element_text(colour="black",family="Times")
  }else{
    tx=element_text(angle=xangle,hjust = 1,colour="black",family="Times")
  }
  
  pos=c(0,0)
  if(is.null(legend.pos)){
    pos='none'
  }else if(legend.pos=='tr'){
    pos=c(1,1)
  }else if(legend.pos=='br'){
    pos=c(1,0)
  }else if(legend.pos=='tl'){
    pos=c(0,1)
  }else if(legend.pos=='bl'){
    pos=c(0,0)
  }else if(legend.pos=='top'){
    pos='top'
  }else if(legend.pos=='buttom'){
    pos='buttom'
  }else{
    pos='right'
  }
  print(pos)
  if(fill){
    p <- ggpubr::ggboxplot(vd1.sbs, x="type", y="Score", fill = "category", yscale = yscale
                           ,palette = group_cols,width = box_span,size = size,order = order,outlier.shape=outlier.shape
                           ,orientation=orientation,add=add,add.params = list(binwidth=binwidth)
                           ,short.panel.labs = T)#按dose进
  }else{
    p <- ggpubr::ggboxplot(vd1.sbs, x="type", y="Score", color = "category", yscale = yscale
                           ,palette = group_cols,width = box_span,size = size,order = order,outlier.shape=outlier.shape
                           ,orientation=orientation,add=add,add.params = list(binwidth=binwidth)
                           ,short.panel.labs = T)#按dose进
  }
  
  p=p+ggpubr::stat_compare_means(aes(group=category), label = "p.signif", method = test_method,paired=paired
                                 #,label.y = max(vd1.sbs[,1])
  )
  #p=p+ylab(ylab)+xlab(xlab)
  #p=p+theme(axis.text.x = element_text(angle = xangle, hjust = 1))
  p=p+theme_bw()+theme(axis.text.x=tx, #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                       axis.text.y=element_text(family="Times",face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                       axis.title.y=element_text(family="Times",face="plain"), #设置y轴标题的字体属性
                       #panel.border = element_blank(),axis.line = element_line(colour = "black"), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                       legend.text=element_text(face="plain", family="Times", colour="black"  #设置图例的子标题的字体属性
                       ),
                       legend.title=element_text(face="plain", family="Times", colour="black" #设置图例的总标题的字体属性
                       ),
                       legend.justification=pos, legend.position=pos
                       ,legend.background = element_rect(fill = NA, colour = NA)
                       #,panel.grid.major = element_blank(),   #不显示网格线
                       #panel.grid.minor = element_blank()
  )+ylab(ylab)+xlab(xlab) #设置x轴和y轴的标题
  if(!is.null(ylim)){
    p=p+ylim(ylim)
  }
  return(p)
}

plotMutiBar=function(dat,ist=F,margin=T,xlb='',ylb='',lineCol='black',lineW=0.5,legTitle='Group',showValue=F,showLine=T,xangle=0,isAuto=T){
  library(ggplot2)
  #library(tidyverse)
  #library(reshape2)
  #library(optparse)
  if(ist){
    dat=t(dat)
  }
  lbc=colnames(dat)
  lbr=row.names(dat)
  bk_dat=dat
  if(margin){
    dat=dat%*%diag(1/c(apply(t(dat), 1, sum)))
  }
  row.names(dat)=paste0('R',1:(nrow(dat)))
  colnames(dat)=paste0('C',1:(ncol(dat)))
  row.names(bk_dat)=paste0('R',1:(nrow(bk_dat)))
  colnames(bk_dat)=paste0('C',1:(ncol(bk_dat)))
  #df=cbind(bg=paste0('R',1:nrow(dat)),dat)
  #colnames(df)=c('bg',paste0('C',1:(ncol(dat))))
  tp.dat=as.data.frame(cbind(bg=row.names(dat),dat))
  tp.dat[,1]=as.character(tp.dat[,1])
  for(i in 2:ncol(tp.dat)){
    tp.dat[,i]=as.numeric(as.character(tp.dat[,i]))
  }
  mt.df=reshape2::melt(tp.dat)
  colnames(mt.df)=c('bg','variable','value')
  
  pg=ggplot(mt.df, aes(x=variable, y=value, fill=bg))+geom_bar(stat = "identity", width=lineW, col=lineCol) + theme_bw()
  if(showLine){
    for (i in 2:(ncol(tp.dat)-1)) {
      tmp=tp.dat[order(tp.dat[,1],decreasing = T),]
      tmp[,i]=base::cumsum(tmp[,i])
      tmp[,i+1]=base::cumsum(tmp[,i+1])
      colnames(tmp)[c(i,i+1)]=c('STY','ED')
      tmp1=cbind(tmp,STX=rep(i-1+lineW/2,nrow(tmp))
                 ,EDX=rep(i-lineW/2,nrow(tmp)))
      pg=pg+geom_segment(data=tmp1,aes(x=STX, xend=EDX, y=STY, yend=ED))
    }
  }
  
  if(showValue){
    pg=pg+geom_text(data=mt.df,aes(label=sprintf("%0.2f", round(value, digits = 2))),position=position_stack(vjust=0.5))
  }
  pg=pg+scale_x_discrete(breaks = paste0('C',1:(ncol(dat))),label = lbc)
  pg=pg+labs(x=xlb, y=ylb)+ggsci::scale_fill_jco()+theme(legend.position = "bottom")
  pg=pg+ggsci::scale_fill_jco()+scale_fill_discrete(breaks = paste0('R',1:nrow(dat)),label = lbr,name=legTitle)
  if(xangle>0){
    pg=pg+theme(axis.text.x = element_text(angle = xangle, hjust = 1),legend.position = "bottom")
  }
  
  g.tb=matrix(0,nrow=ncol(dat),ncol=ncol(dat))
  for(i in 1:(ncol(dat))){
    for(j in 1:ncol(dat)){
      if(i!=j){
        g.tb[i,j]=round(-log10((chisq.test(bk_dat[,c(i,j)])$p.value)),2)
      }
    }
  }
  colnames(g.tb)=lbc
  row.names(g.tb)=lbc
  g.tb=reshape2::melt(g.tb) 
  colnames(g.tb)=c('A1','A2','A3')
  g.tb$A4=paste0(g.tb[,3],ifelse(g.tb[,3]>-log10(0.05),'(*)',''))
  stable.p=ggplot(g.tb, aes(A1, A2)) + geom_tile(aes(fill = A3),colour = "white") + theme_bw()+xlab('')+ylab('')+ scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(x=A1,y=A2,label=A4))+theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
  stable.p=stable.p+ggtitle('-log10(anova p value)')
  if(isAuto){
    g1=ggpubr::ggarrange(stable.p,pg, ncol = 1, nrow = 2,heights = c(0.5,1),align = "hv")
    return(g1)
  }else{
    return(list(Bar=pg,Table=stable.p))
  }
}

mg_plot_lasso <- function(fit,cv_fit,lambda=NULL,show_text=T,figLabels=c('A','B')){
  if(is.null(lambda)){
    lmda=cv_fit$lambda.min
  }else{
    lmda=lambda
  }
  fit.coef=fit$beta[(apply(fit$beta,1,function(x){
    return(sum(x!=0))
  })>0),]
  
  fit.coef=as.matrix(fit.coef)
  colnames(fit.coef)=fit$lambda
  #fit$lambda==cv_fit$lambda
  library(ggplot2)
  dat=data.table::melt(t(as.matrix(fit.coef)))
  dat_z=dat[which(dat$value==0),]
  dat=dat[which(dat$value!=0),]
  dat.sv=rbind()
  for (u in unique(dat_z[,2])) {
    t.z=dat_z[which(dat_z[,2]==u),1]
    t.zx=max(t.z)
    dat.sv=rbind(dat.sv,c(t.zx,u,0))
    t.zn=min(t.z)
    if(t.zx!=t.zn){
      dat.sv=rbind(dat.sv,c(t.zn,u,0))
    }
  }
  colnames(dat.sv)=colnames(dat_z)
  #dat_z=dat_z[dat_z[,2]%in%names(which(fit.coef[,which(fit$lambda==lmda)]!=0)),]
  dat=crbind2DataFrame(rbind(dat,dat.sv))
  mn=min(-log(dat$Var1))
  mx=max(-log(dat$Var1))
  if(show_text){
    mx=(mx-mn)*0.1+mx
  }
  p=ggplot(dat, aes(x=-log(Var1), y=value,colour=Var2))+geom_line()+theme_bw()+theme(legend.position = "none")
  p=p+coord_cartesian(xlim=c(mn, mx))+xlab('-ln(lambda)')+ylab('Coefficients')
  if(show_text){
    fl=fit.coef[which(fit.coef[,which(fit$lambda==lmda)]!=0),ncol(fit.coef)]
    for_label=data.frame(Var1=rep(min(dat$Var1),length(fl)),Var2=names(fl),value=fl)
    p=p+ggrepel::geom_label_repel(
      aes(label = Var2,color=Var2),
      data = for_label,hjust = 0
    )
  }
  p=p+geom_vline(aes(xintercept=-log(lmda)), colour="#BB0000", linetype="dashed")
  p=p+annotate('text',x=-log(lmda),y=min(dat[,3]),label=paste0('lambda=',round(lmda,4)))
  tgc=data.frame(lambda=cv_fit$lambda,cvm=cv_fit$cvm,cvup=cv_fit$cvup,cvlo=cv_fit$cvlo,cvsd=cv_fit$cvsd
                 ,col=ifelse(cv_fit$lambda>=cv_fit$lambda.min&cv_fit$lambda<=cv_fit$lambda.1se,ifelse(cv_fit$lambda==lmda,'A','C'),'B'))
  p1=ggplot(tgc, aes(x=log(lambda), y=cvm)) + xlab('ln(lambda)')+ ylab('Parial Likelihood Deviance')+
    geom_errorbar(aes(ymin=cvm-cvsd, ymax=cvm+cvsd)) +
    geom_point(aes(colour=col))
  p1=p1+theme_bw()+theme(legend.position = "none")
  gal=ggpubr::ggarrange(p,p1, ncol = 2, nrow = 1
                        #,align = "hv"
                        ,labels = figLabels)
  return(gal)
}

calculate_mRNAsi <- function(exp_file, genes = c('')) {
  library(gelnet)
  library(dplyr)
  # 使用PCBC干细胞训练基因集
  norm <- read.delim("d:/public/PCBC/syn2701943_rnaseq_norm_gene_symbol.tsv") %>% tibble::column_to_rownames( "tracking_id" ) %>% as.matrix
  sam_type <- read.delim("d:/public/PCBC/syn2701943_rnaseq_norm_samples_type.tsv", 
                         header = T,stringsAsFactors = F, 
                         check.names = F)
  same_genes1 <- intersect(rownames(exp_file), rownames(norm))
  if (length(genes) > 1) {
    same_genes <- intersect(same_genes1, genes)
  } else {
    same_genes <- same_genes1
  }
  print(setdiff(genes, same_genes1))
  norm <- norm[same_genes, ]
  mean_norm <- apply(norm, 1, mean)
  norm <- norm - mean_norm
  norm.tr <- norm[,sam_type[sam_type$type == 'SC',]$Samples]
  norm.bk <- norm[,sam_type[sam_type$type != 'SC',]$Samples]
  mm <- gelnet(t(norm.tr), NULL, 0, 1)
  # 使用训练得到的基因数据计算肿瘤样本的 mRNAsi 指数
  exp_file <- exp_file[same_genes, ]
  mRNAsi <- apply(exp_file, 2, function(z) {cor(z, mm$w, 
                                                method="sp", 
                                                use="complete.obs")})
  mRNAsi <- mRNAsi - min(mRNAsi)
  mRNAsi <- mRNAsi / max(mRNAsi)
  mRNAsi <- as.data.frame(cbind(mRNAsi), stringsAsFactors = FALSE)
  mRNAsi$samples <- rownames(mRNAsi)
  return(list(mRNAsi_index = mRNAsi,train = mm$w))
}


cor_point <- function(x,y,method='Pearson',top_col='red',right_col='blue',ylab='y expression',xlab='x expression'){
  #x=rnorm(200)
  #y=rnorm(200)
  library(ggplot2)
  data=data.frame(x,y)  
  colnames(data)=c('wt','mpg')
  til=''
  if(method=='Pearson'){
    cr=cor.test(x,y)
    p=cr$p.value
    r=cr$estimate
    til=paste0('Pearson\'s correlation\nR=',round(r,3),'\nP=',signif(p,3))
  }else{
    cr=cor.test(x,y,method = "spearman")
    p=cr$p.value
    r=cr$estimate
    til=paste0('spearman correlation\nR=',round(r,3),'\nP=',signif(p,3))
  }
  
  empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme(                              
      plot.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
      ,plot.margin=unit(c(0.1, 0.1, 0, 0), "inches")
    )
  empty=empty+geom_text(aes(x=1, y=1, label=til),color="black")
  
  plot_top <- ggplot(data, aes(wt, fill=top_col)) + 
    geom_density(alpha=.5,fill=top_col) +theme_bw()+ 
    theme(legend.position = "none",                           
          #plot.background = element_blank(), 
          #panel.grid.major = element_blank(), 
          #panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          #panel.background = element_blank(),
          axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.ticks.x = element_blank()
          ,plot.margin=unit(c(0.1, 0, 0, 0.1), "inches")
    )
  
  plot_right <- ggplot(data, aes(mpg, fill=right_col)) + 
    geom_density(alpha=.5,fill=right_col) +coord_flip()+theme_bw()+ 
    theme(legend.position = "none",                           
          #plot.background = element_blank(), 
          #panel.grid.major = element_blank(), 
          #panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          #panel.background = element_blank(),
          #axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          #axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
          ,plot.margin=unit(c(0.01, 0.1, 0.1, 0), "inches")
    )
  #scale_fill_manual(values = c("orange", "purple")) + 
  
  p1=ggplot(data=data, aes(x=wt, y=mpg))+geom_point()+stat_smooth(method="lm")
  p1=p1+theme_bw()
  p1=p1+theme(axis.text.x=element_text(family="Times",face="plain"), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
              axis.text.y=element_text(family="Times",face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
              axis.title.y=element_text(family="Times",face="plain"), #设置y轴标题的字体属性
              #panel.border = element_blank(),
              axis.line = element_line(colour = "black"), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
              legend.text=element_text(face="plain", family="Times", colour="black"),  #设置图例的子标题的字体属性
              legend.title=element_text(face="plain", family="Times", colour="black"), #设置图例的总标题的字体属性
              plot.margin=unit(c(0.01, 0.01, 0.1, 0.1), "inches")
              #,panel.grid.major = element_blank(),   #不显示网格线
              #panel.grid.minor = element_blank()
  )+ylab(ylab)+xlab(xlab)
  
  pg1=ggpubr::ggarrange(plot_top,p1, ncol = 1, nrow = 2,heights = c(0.3,1),align = "v")
  pg2=ggpubr::ggarrange(empty,plot_right, ncol = 1, nrow = 2,heights = c(0.3,1),align = "v")
  
  pgal=ggpubr::ggarrange(pg1,pg2, ncol = 2, nrow = 1,widths = c(1,0.3),align = "h")
  return(pgal)
}

library(scales)
library(ggsci)
mycolor <- pal_igv(alpha =1)(9)
show_col(mycolor)
dir.create('scripts')

library(data.table)
library(stringr)
dir.create('00_origin_datas')


gene_type <- gene_type[!duplicated(gene_type$ENSGID), ]
rownames(gene_type) <- gene_type$ENSGID
table(gene_type$TYPE)
gene_type <- crbind2DataFrame(gene_type)
genes_protein <- gene_type[gene_type$TYPE == 'protein_coding', ]$SYMBOL
genes_lncRNA <- gene_type[gene_type$TYPE == 'lncRNA', ]$SYMBOL
str(genes_protein)
str(genes_lncRNA)







# 数据预处理 #############
# TCGA

tcga_cli2 <- subset(tcga_cli2,
                    type == 'THCA')
table(tcga_cli2$new_tumor_event_type)
tcga_cli2 <- tcga_cli2[, c("bcr_patient_barcode", "type", "OS.time", "OS", 
                           "PFI.time", "PFI", "DFI.time", "DFI", "DSS.time", "DSS")]
colnames(tcga_cli2) <- c("Samples", "type", "OS.time", "OS", 
                         "PFI.time", "PFI", "DFI.time", "DFI", "DSS.time", "DSS")
tcga_cli2$Samples <- paste0(tcga_cli2$Samples, '-01')


tcga_cli <- read.delim('00_origin_datas/TCGA/Clinical BCR XML.merge.txt',
                       header = T, stringsAsFactors = F)
tcga_cli$A0_Samples <- paste0(tcga_cli$A0_Samples, '-01')
tcga_cli <- tcga_cli[, c("A0_Samples", 
                         "age_at_initial_pathologic_diagnosis",
                         "A18_Sex",
                         "A3_T", "A4_N", "A5_M",
                         "A6_Stage")]
colnames(tcga_cli) <- c("Samples",
                        "Age",
                        "Gender",
                        "A3_T", "A4_N", "A5_M",
                        "Stage")

tcga_cli$A3_T <- gsub('[ab]', '', tcga_cli$A3_T)
tcga_cli$A3_T[tcga_cli$A3_T == ''] <- NA
tcga_cli$A3_T[tcga_cli$A3_T == 'TX'] <- NA
tcga_cli$A4_N[tcga_cli$A4_N == ''] <- NA
tcga_cli$Stage <- gsub('[ABC]', '', tcga_cli$Stage)
tcga_cli$Stage <- gsub('Stage ', '', tcga_cli$Stage)
tcga_cli$Stage[tcga_cli$Stage == ''] <- NA
# tcga_cli$Grade[tcga_cli$Grade == 'Not Available'] <- NA
tcga_cli$Gender <- ifelse(tcga_cli$Gender == 'MALE', 'Male', 'Female')



tcga_cli <- merge(tcga_cli2, tcga_cli, by = 'Samples', all=T)
# tcga_cli <- merge(tcga_cli, tcga_cli1, by = 'Samples', all=T)

rownames(tcga_cli) <- tcga_cli$Samples

tcga_cli <- subset(tcga_cli,
                   !is.na(OS.time) & OS.time > 0)
table(tcga_cli$OS)
tcga_cli <- crbind2DataFrame(tcga_cli)
table(tcga_cli$Stage)


# 1.1.2、表达谱信息 #########
tcga_exp <- read.table('00_origin_datas/TCGA/TCGA-THCA-Symbol.txt',
                       sep = '\t', header = T, check.names = F, row.names = 1)
boxplot(tcga_exp[, 1:5])
table(substr(colnames(tcga_exp), 14, 17))
which(substr(colnames(tcga_exp), 14, 17) == '01')

tcga_group <- data.frame(row.names = colnames(tcga_exp),
                         Samples = colnames(tcga_exp),
                         Groups  = colnames(tcga_exp),
                         stringsAsFactors = F)
tcga_group$Groups <- substr(colnames(tcga_exp), 14, 17)
tcga_group <- tcga_group[tcga_group$Groups != '06', ]
table(tcga_group$Groups)
tcga_group$Groups <- ifelse(tcga_group$Groups == '01', 'Tumor', 'Adjacent')



tcga_tpm_log2 <- log2(tcga_exp[, tcga_group$Samples] + 1)
tcga_tpm_log2 <- tcga_tpm_log2[rowSums(tcga_tpm_log2) > 0, ]

dim(tcga_tpm_log2)
boxplot(tcga_tpm_log2[, 1:5])

tmr_samples <- intersect(tcga_cli$Samples, colnames(tcga_tpm_log2))
tcga_cli <- tcga_cli[tmr_samples, ]



# mRNAsi 的计算 ###########################
save(tcga_cli,
     tcga_tpm_log2,
     tcga_group,
     tcga_exp,
     file = 'cal_mRNAsi.Rdata')
load('cal_mRNAsi.Rdata')


tcga_mRNAsi <- calculate_mRNAsi(exp_file = log2(tcga_exp + 1))
save(tcga_mRNAsi,
     file = 'mRNAsi.Rdata')
load('mRNAsi.Rdata')

tcga_mRNAsi_res <- tcga_mRNAsi$mRNAsi_index



mg_violin(data.frame(tcga_group$Groups,
                     tcga_mRNAsi_res[tcga_group$Samples, ]$mRNAsi), 
          melt=TRUE, ylab='mRNAsi',
          leg.title='Gender', 
          legend.pos='tl')

tcga_cli$mRNAsi <- tcga_mRNAsi_res[tcga_cli$Samples, ]$mRNAsi



# mRNAsi 在临床特征中的分布 ###########################
dir.create('01_mRNAsi_cli')
colnames(tcga_cli)

tcga_Gender_mRNAsi_plot <- mg_violin(tcga_cli[, c("Gender", "mRNAsi")], 
                                     melt=TRUE, ylab='mRNAsi',
                                     jitter = F,
                                     leg.title='Gender', 
                                     legend.pos='tl')
tcga_Gender_mRNAsi_plot

tcga_A3_T_mRNAsi_plot <- mg_violin(tcga_cli[, c("A3_T", "mRNAsi")], 
                                     melt=TRUE, ylab='mRNAsi',
                                     jitter = F,
                                     leg.title='A3_T', 
                                     legend.pos='tl')
tcga_A3_T_mRNAsi_plot


tcga_cli$A4_N <- gsub('[ab]', '', tcga_cli$A4_N)
tcga_cli$A4_N[tcga_cli$A4_N == 'NX'] <- NA
tcga_A4_N_mRNAsi_plot <- mg_violin(tcga_cli[, c("A4_N", "mRNAsi")], 
                                     melt=TRUE, ylab='mRNAsi',
                                     jitter = F,
                                     leg.title='A4_N', 
                                     legend.pos='tl')
tcga_A4_N_mRNAsi_plot

tcga_cli$A5_M[tcga_cli$A5_M == 'MX'] <- NA
tcga_cli$A5_M[tcga_cli$A5_M == ''] <- NA
tcga_A5_M_mRNAsi_plot <- mg_violin(tcga_cli[, c("A5_M", "mRNAsi")], 
                                     melt=TRUE, ylab='mRNAsi',
                                     jitter = F,
                                     leg.title='A5_M', 
                                     legend.pos='tl')
tcga_A5_M_mRNAsi_plot

tcga_Stage_mRNAsi_plot <- mg_violin(tcga_cli[, c("Stage", "mRNAsi")], 
                                     melt=TRUE, ylab='mRNAsi',
                                     jitter = F,
                                     leg.title='Stage', 
                                     legend.pos='tl')
tcga_Stage_mRNAsi_plot


fivenum(tcga_cli$Age)
tcga_cli$Age1 <- ifelse(tcga_cli$Age > 45, '>45', '<=45')
tcga_Age_mRNAsi_plot <- mg_violin(tcga_cli[, c("Age1", "mRNAsi")], 
                                     melt=TRUE, ylab='mRNAsi',
                                     jitter = F,
                                     leg.title='Age', 
                                     legend.pos='tl')
tcga_Age_mRNAsi_plot

tcga_mRNAsi_cli_plot <- cowplot::plot_grid(tcga_Age_mRNAsi_plot,
                                      tcga_Gender_mRNAsi_plot,
                                      tcga_A3_T_mRNAsi_plot,
                                      tcga_A4_N_mRNAsi_plot,
                                      tcga_A5_M_mRNAsi_plot,
                                      tcga_Stage_mRNAsi_plot,
                                      ncol = 3,
                                      labels = LETTERS[1:6])
tcga_mRNAsi_cli_plot
ggsave(plot = tcga_mRNAsi_cli_plot,
       filename = '01_mRNAsi_cli/tcga_mRNAsi_cli_plot.pdf',
       width = 12, height = 10)



# mRNAsi 在亚型中的差异 ###############
tcga_other_subtype <- read.csv('00_origin_datas/TCGA/TCGA_immune_subtype_mmc2.csv',
                               check.names = F, stringsAsFactors = F)
tcga_other_subtype <- tcga_other_subtype[tcga_other_subtype$`TCGA Study` == 'THCA', ]
tcga_other_subtype$`TCGA Participant Barcode` <- paste0(tcga_other_subtype$`TCGA Participant Barcode`, '-01')

tcga_cli <- merge(tcga_cli,
                  tcga_other_subtype[, c("TCGA Participant Barcode", "Immune Subtype")],
                  by.x = 'Samples', by.y = 'TCGA Participant Barcode', all = T)
tcga_cli <- tcga_cli[!is.na(tcga_cli$OS.time), ]
rownames(tcga_cli) <- tcga_cli$Samples

tcga_imsubtype_mRNAsi_plot <- mg_violin(tcga_cli[, c("Immune Subtype", "mRNAsi")], 
                                  melt=TRUE, ylab='mRNAsi',
                                  jitter = F,
                                  leg.title='Immune Subtype', 
                                  legend.pos='tl')
tcga_imsubtype_mRNAsi_plot
ggsave(plot = tcga_imsubtype_mRNAsi_plot,
       filename = '01_mRNAsi_cli/tcga_imsubtype_mRNAsi_plot.pdf',
       width = 6, height = 5)



# mRNAsi 在肿瘤驱动基因突变中的分布差异 #############
# 突变分析 ######################
dir.create('02_mRNAsi_mut')
tcga.mut.dat <- getTCGAMAFByCode('THCA')
tcga.mut.dat <- as.data.frame(tcga.mut.dat@data)
tcga.mut.dat <- tcga.mut.dat[, c("Hugo_Symbol", "Tumor_Sample_Barcode", "Variant_Classification")]

tcga.mut.dat$Variant_Classification <- 1
tcga.mut.dat <- reshape2::dcast(data = tcga.mut.dat, Hugo_Symbol ~ Tumor_Sample_Barcode)
class(tcga.mut.dat)
rownames(tcga.mut.dat) <- tcga.mut.dat$Hugo_Symbol
tcga.mut.dat <- tcga.mut.dat[, -1]



colnames(tcga.mut.dat) <- paste0(colnames(tcga.mut.dat), '-01')
mut.samples <- intersect(colnames(tcga.mut.dat), tcga_cli$Samples)


tcga.mut.dat <- tcga.mut.dat[, mut.samples]
tcga_mut_cli <- tcga_cli[mut.samples, ]
table(as.numeric(tcga.mut.dat['ADCY2', ]))
tcga.mut.dat <- ifelse(tcga.mut.dat == 0, 0, 1)


# mut.genes <- openxlsx::read.xlsx('00_origin_datas/cancer.driver.genes.PMC4160307.xlsx', sheet = 1)[, 1]
# mut.genes <- intersect(mut.genes, rownames(tcga.mut.dat))
# tcga.mut.dat<- tcga.mut.dat[mut.genes, ]

gene.freq <- as.data.frame(rowSums(tcga.mut.dat))
head(gene.freq)
write.csv(gene.freq,
          file = 'results/tcga.subtype.mut.gene.csv',
          quote = F)

snv.genes <- rownames(gene.freq)[gene.freq$`rowSums(tcga.mut.dat)` > 2]

tcga.mut.dat <- tcga.mut.dat[snv.genes, ]
tcga.mut.dat <- ifelse(tcga.mut.dat > 0, 'Mutant', 'WildType')



tcga_mut_cli <- cbind(tcga_mut_cli,
                      t(tcga.mut.dat[, tcga_mut_cli$Samples]))




# table(tcga_mut_cli$ADCY2)
# x1 = tcga_mut_cli$mRNAsi[tcga_mut_cli[, 'ADCY2'] == 'WildType']
# x2 = tcga_mut_cli$mRNAsi[tcga_mut_cli[, 'ADCY2'] == 'Mutant']
# t.test(x1,x2)$p.value 

snv_mRNAsi_p <- c()
for (ge in snv.genes) {
  print(ge)
  x1 = tcga_mut_cli$mRNAsi[tcga_mut_cli[, ge] == 'WildType']
  x2 = tcga_mut_cli$mRNAsi[tcga_mut_cli[, ge] == 'Mutant']
  tmp.p <- t.test(x1,x2)$p.value 
  snv_mRNAsi_p <- c(snv_mRNAsi_p, tmp.p)
}

snv_mRNAsi_p_dat <- data.frame(Genes = snv.genes,
                               P = snv_mRNAsi_p,
                               stringsAsFactors = F)
snv.genes.filter <- snv_mRNAsi_p_dat$Genes[snv_mRNAsi_p_dat$P < 0.05]



snv_mRNAsi_list <- list()
for (ge in snv.genes.filter) {
  print(ge)
  tmp <- mg_violin(tcga_mut_cli[, c(ge, "mRNAsi")], 
                   melt=TRUE, ylab='mRNAsi',
                   jitter = F,
                   leg.title=ge, 
                   xlab = ge,
                   legend.pos='tl')
  snv_mRNAsi_list[[ge]] <- tmp
}

colnames(tcga_mut_cli)
tcga_mut_cli[, c("BRAF", "mRNAsi")]

tcga_mRNAsi_mut_plot <- cowplot::plot_grid(plotlist = snv_mRNAsi_list,
                                           ncol = 5)
tcga_mRNAsi_mut_plot
ggsave(plot = tcga_mRNAsi_mut_plot,
       filename = '02_mRNAsi_mut/tcga_mRNAsi_mut_plot.pdf',
       width = 12, height = 10)



# 突变与临床结合的 mRNAsi 趋势瀑布图绘制 ###############
mut.plot.dat <- tcga.mut.dat[snv.genes.filter, ]
mut.plot.dat[mut.plot.dat == 'WildType'] <- NA

library(scales)
library(ggsci)
library(ComplexHeatmap)
alter_graphic <- function (graphic = c("rect", "point"), width = 1, 
                           height = 1, horiz_margin = unit(1, "pt"),
                           vertical_margin = unit(1, "pt"), 
                           fill = "red", col = NA, pch = 16, 
                           ...) 
{
  graphic = match.arg(graphic)[1]
  if (graphic == "rect") {
    if (!is.numeric(width)) {
      stop_wrap("`width` should be nummeric.")
    }
    if (!is.numeric(height)) {
      stop_wrap("`height` should be nummeric.")
    }
    if (width != 1) {
      if (missing(horiz_margin)) {
        horiz_margin = unit(0, "pt")
      }
    }
    if (height != 1) {
      if (missing(vertical_margin)) {
        vertical_margin = unit(0, "pt")
      }
    }
    fun = function(x, y, w, h) {
      w = w * width
      h = h * height
      grid.rect(x, y, w - horiz_margin * 2, h - vertical_margin * 
                  2, gp = gpar(fill = fill, col = col, ...))
    }
  }
  else if (graphic == "point") {
    fun = function(x, y, w, h) {
      grid.points(x, y, pch = pch, gp = gpar(fill = fill, 
                                             col = col, ...))
    }
  }
  return(fun)
}

# 定义突变颜色 #########
col = c("Mutant" = 'black')

alter_fun = list(
  background = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "#CCCCCC", col = NA))
  },
  
  Mutant = function(x, y, w, h) {
    grid.rect(x, y, w-unit(3, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = col["Mutant"], col = NA))
  }
)

alter_fun = list(
  background = alter_graphic("rect", fill = "#CCCCCC"),	
  Mutant = alter_graphic("rect", fill = col["Mutant"])
)
heatmap_legend_param = list(title = "Alterations", 
                            at = c("Mutant"), 
                            labels = c("Mutant"))

# color_cluster = mycolor[1:3]
# names(color_cluster) = c('Immune-D', 'Immune-E', 'Stromal-E')

tcga_mut_cli <- dplyr::arrange(tcga_mut_cli, mRNAsi)

Gender_color <- mycolor[1:2]
names(Gender_color) <- na.omit(unique(tcga_mut_cli$Gender))

A3_T_color <- mycolor[1:4]
names(A3_T_color) <- na.omit(unique(tcga_mut_cli$A3_T))

A4_N_color <- mycolor[1:2]
names(A4_N_color) <- na.omit(unique(tcga_mut_cli$A4_N))

A5_M_color <- mycolor[1:2]
names(A5_M_color) <- na.omit(unique(tcga_mut_cli$A5_M))

Stage_color <- mycolor[1:4]
names(Stage_color) <- na.omit(unique(tcga_mut_cli$Stage))



mut1 <- oncoPrint(as.matrix(mut.plot.dat[, tcga_mut_cli$Samples]),
                  # row_order = rownames(c1.mut.plot.dat),
                  column_order = tcga_mut_cli$Samples,
                  alter_fun = alter_fun, 
                  col = col, 
                  column_title = "mRNAsi",
                  heatmap_legend_param = heatmap_legend_param,
                  pct_digits = 2,
                  # top_annotation = HeatmapAnnotation(mRNAsi=anno_barplot(tcga_mut_cli$mRNAsi),
                  #                                    show_annotation_name = TRUE,
                  #                                    gap=unit(1, "mm"),
                  #                                    na_col="grey"),
                  top_annotation = HeatmapAnnotation(mRNAsi=anno_barplot(tcga_mut_cli$mRNAsi,
                                                                         border = F,
                                                                         gp = gpar(fill = "black"),
                                                                         bar_width = 1,
                                                                         height = unit(3, "cm")),
                                                        df=tcga_mut_cli[, c("Gender", "A3_T", "A4_N",
                                                                            "A5_M", "Stage")],
                                                        col=list("Gender"=Gender_color,
                                                                 "A3_T"=A3_T_color, 
                                                                 "A4_N"=A4_N_color,
                                                                 "A5_M"=A5_M_color, 
                                                                 "Stage"=Stage_color),
                                                        show_annotation_name = TRUE,
                                                        gap=unit(1, "mm"),
                                                        na_col="grey"),
                  pct_side = "right", row_names_side = "left")
mut1
dev.off()
pdf('02_mRNAsi_mut/tcga_mRNAsi_mut_pheatmap.pdf', width = 12, height = 6)
mut1
dev.off()



# mRNAsi 与免疫评分，通路评分的相关性 ################
dir.create('03_mRNAsi_immune')
tcga_estimate <- immu_estimate(tcga_tpm_log2, isTCGA=T, platform='illumina')
class(tcga_estimate)


tcga_estimate1 <- as.data.frame(tcga_estimate)
tcga_estimate1$samples <- rownames(tcga_estimate1)
tcga_estimate1 <- merge(tcga_estimate1, tcga_mRNAsi_res, by.x = 'samples', by.y = 'samples')

tcga_estimate_mRNAsi <- ggpubr::ggarrange(cor_point(tcga_estimate1$StromalScore,
                                                  tcga_estimate1$mRNAsi,
                                                  method='Pearson',
                                                  top_col='red',
                                                  right_col='blue',
                                                  ylab='mRNAsi',
                                                  xlab='StromalScore'),
                                        cor_point(tcga_estimate1$ImmuneScore,
                                                  tcga_estimate1$mRNAsi,
                                                  method='Pearson',
                                                  top_col='red',
                                                  right_col='blue',
                                                  ylab='mRNAsi',
                                                  xlab='ImmuneScore'),
                                        cor_point(tcga_estimate1$ESTIMATEScore,
                                                  tcga_estimate1$mRNAsi,
                                                  method='Pearson',
                                                  top_col='red',
                                                  right_col='blue',
                                                  ylab='mRNAsi',
                                                  xlab='ESTIMATEScore'),
                                        ncol = 3,nrow = 1,
                                        labels = toupper(letters)[1:3],align = "hv")
tcga_estimate_mRNAsi
ggsave(plot = tcga_estimate_mRNAsi,
       filename = '03_mRNAsi_immune/tcga_estimate_mRNAsi.pdf',
       width = 15, height = 5, device = cairo_pdf)


tcga_ssgsea <- immu_ssgsea(exp = tcga_tpm_log2, isTCGA = T)
class(tcga_ssgsea)
tcga_ssgsea <- as.data.frame(tcga_ssgsea)

tcga_ssgsea1 <- cbind(tcga_cli[, c("Samples", "mRNAsi")],
                      tcga_ssgsea[tcga_cli$Samples, ])


tcga_ssgsea_mRNAsi_cor_list <- list()
for (ce in colnames(tcga_ssgsea)) {
  print(ce)
  
  tmp.dat <- tcga_ssgsea1[, c('mRNAsi', ce)]
  colnames(tmp.dat) <- c('mRNAsi', 'Cell')
  
  tmp <- cor_point(as.numeric(tmp.dat[, 'Cell']),
                   as.numeric(tmp.dat[, 'mRNAsi']),
                   method='Pearson',
                   top_col='red',
                   right_col='blue',
                   ylab= 'mRNAsi',
                   xlab= ce)
  tcga_ssgsea_mRNAsi_cor_list[[ce]] <- tmp
}

tcga_ssgsea_mRNAsi_cor_plot <- cowplot::plot_grid(plotlist = tcga_ssgsea_mRNAsi_cor_list,
                                                  ncol = 5)
tcga_ssgsea_mRNAsi_cor_plot
ggsave(plot = tcga_ssgsea_mRNAsi_cor_plot,
       filename = '03_mRNAsi_immune/tcga_ssgsea_mRNAsi_cor_plot.pdf',
       width = 25, height = 25, device = cairo_pdf)




# mRNAsi 与预后的关系 #####################
dir.create('04_mRNAsi_km')
colnames(tcga_cli)
library(survminer)
library(survival)
# OS
tcga.OS.point <- surv_cutpoint(tcga_cli, time = "OS.time", event = "OS",
                                 variables = 'mRNAsi')
tcga.OS.cutoff <- as.numeric(summary(tcga.OS.point)[1])
tcga.OS.cutoff
tcga.OS.mRNAsi.km <- ggplotKMCox(data.frame(tcga_cli$OS.time / 365,
                                            tcga_cli$OS,
                                  ifelse(tcga_cli$mRNAsi>=tcga.OS.cutoff,'High','Low')),
                       title = 'TCGA OS',
                       labs = c('High','Low'))
tcga.OS.mRNAsi.km

# PFI
tcga.PFI.point <- surv_cutpoint(tcga_cli, time = "PFI.time", event = "PFI",
                               variables = 'mRNAsi')
tcga.PFI.cutoff <- as.numeric(summary(tcga.PFI.point)[1])
tcga.PFI.cutoff
tcga.PFI.mRNAsi.km <- ggplotKMCox(data.frame(tcga_cli$PFI.time / 365,
                                            tcga_cli$PFI,
                                            ifelse(tcga_cli$mRNAsi>=tcga.PFI.cutoff,'High','Low')),
                                 title = 'TCGA PFI',
                                 labs = c('High','Low'))
tcga.PFI.mRNAsi.km

# DFI
tcga.DFI.point <- surv_cutpoint(tcga_cli, time = "DFI.time", event = "DFI",
                               variables = 'mRNAsi')
tcga.DFI.cutoff <- as.numeric(summary(tcga.DFI.point)[1])
tcga.DFI.cutoff
tcga.DFI.mRNAsi.km <- ggplotKMCox(data.frame(tcga_cli$DFI.time / 365,
                                            tcga_cli$DFI,
                                            ifelse(tcga_cli$mRNAsi>=tcga.DFI.cutoff,'High','Low')),
                                 title = 'TCGA DFI',
                                 labs = c('High','Low'))
tcga.DFI.mRNAsi.km

# DSS
tcga.DSS.point <- surv_cutpoint(tcga_cli, time = "DSS.time", event = "DSS",
                               variables = 'mRNAsi')
tcga.DSS.cutoff <- as.numeric(summary(tcga.DSS.point)[1])
tcga.DSS.cutoff
tcga.DSS.mRNAsi.km <- ggplotKMCox(data.frame(tcga_cli$DSS.time / 365,
                                            tcga_cli$DSS,
                                            ifelse(tcga_cli$mRNAsi>=tcga.DSS.cutoff,'High','Low')),
                                 title = 'TCGA DSS',
                                 labs = c('High','Low'))
tcga.DSS.mRNAsi.km



tcga_mRNAsi_km <- cowplot::plot_grid(tcga.OS.mRNAsi.km,
                                     tcga.PFI.mRNAsi.km,
                                     tcga.DFI.mRNAsi.km,
                                     tcga.DSS.mRNAsi.km,
                                     ncol = 4,
                                     labels = LETTERS[1:4])
tcga_mRNAsi_km

ggsave(plot = tcga_mRNAsi_km,
       filename = '04_mRNAsi_km//tcga_mRNAsi_km.pdf',
       width = 16, height = 5, device = cairo_pdf)


# # WGCNA 分析 ############
# dir.create('05_WGCNA')
# 
# # cluster gene
# library(WGCNA)
# allowWGCNAThreads(nThreads = 36)#允许R语言程序最大线程运行
# enableWGCNAThreads(nThreads = 36)# 打开多线程
# 
# 
# # boxplot(tcga_tpm_log2[, 1:4])
# # tpm_T2 <- tcga_tpm_log2[, tcga_cli$Samples]
# # tcga.mads=apply(tpm_T2, 1, mad)
# # tpm_T2=tpm_T2[which(tcga.mads>0.1),]
# # tpm_T2=tpm_T2[rownames(tpm_T2) %in% genes_protein,]
# # dim(tpm_T2)
# # # tpm_T2=(2^tpm_T2-1)
# # tpm_T2=t(tpm_T2)
# # dim(tpm_T2)
# 
# tpm_T2=tcga_tpm_log2[rownames(tcga_tpm_log2) %in% genes_protein, tcga_cli$Samples]
# dim(tpm_T2)
# tpm_T2 <- tpm_T2[rowSums(tpm_T2) > 0, ]
# tpm_T2=(2^tpm_T2-1)
# tpm_T2=t(tpm_T2)
# range(tpm_T2)
# 
# pdf('05_WGCNA/Fig4ABC.pdf',width = 8,height = 8)
# tpm_T2.power=mg_wgcna_get_power(tpm_T2)
# dev.off()
# 
# tpm_T2.power$cutPower
# tpm_T2.module=mg_WGCNA_getModule(tpm_T2,
#                                  power = tpm_T2.power$cutPower,
#                                  deepSplit=2,
#                                  mergeCutHeight=0.25,
#                                  minModuleSize=100)
# 
# table(tpm_T2.module$Modules[,2])
# length(table(tpm_T2.module$Modules[,2]))
# 
# pdf('05_WGCNA/Fig4D.pdf',height = 6,width = 10)
# plotDendroAndColors(tpm_T2.module$Tree, tpm_T2.module$Modules,
#                     c("Dynamic Module",'Merged Module'),
#                     dendroLabels = FALSE, hang = 0.03,
#                     addGuide = TRUE, guideHang = 0.05)
# dev.off()
# 
# writeMatrix(tpm_T2.module$Modules,outpath = 'results/tcga.wgcna.module.genes.txt')
# 
# fig4e=mg_barplot_point(labels = names(table(tpm_T2.module$Modules[,2]))
#                        ,values = as.numeric(table(tpm_T2.module$Modules[,2]))
#                        ,point_sizes = 2
#                        ,point_cols = names(table(tpm_T2.module$Modules[,2]))
#                        ,xlab = 'Number of Genes',legend.pos = NULL)
# fig4e
# savePDF('05_WGCNA/Fig4E.pdf',fig4e,height = 6,width = 6)
# 
# #### 模块特征向量聚类
# # Calculate eigengenes
# MEs = tpm_T2.module$MEs
# # Calculate dissimilarity of module eigengenes
# MEDiss = 1-cor(MEs);
# # Cluster module eigengenes
# METree = hclust(as.dist(MEDiss), method = "average");
# # Plot the result
# pdf('05_WGCNA/Fig4F.pdf',height = 6,width = 12,onefile = T)
# plot(METree, main = "Clustering of module eigengenes",xlab = "", sub = "")
# dev.off()
# 
# 
# 
# # design <- model.matrix(~0+factor(tcga_cli$Cluster))
# # colnames(design)=c('Immune-D', 'Immune-E', 'Stromal-E')
# 
# spms <- tcga_cli[, c("mRNAsi", "OS", "PFI", "DFI", "DSS")]
# 
# MEs_col<-tpm_T2.module$MEs
# dim(MEs_col)
# 
# modTraitCor = cor(MEs_col[,rownames(MEDiss)[METree$order]]
#                   , spms
#                   ,use = 'pairwise.complete.obs')
# modTraitP = corPvalueStudent(modTraitCor, dim(spms)[1])
# 
# textMatrix = paste(signif(modTraitCor, 2), " (", format(modTraitP,scientific =TRUE,digits = 3), ")", sep = "")
# dim(textMatrix) = dim(modTraitCor)
# dim(textMatrix)
# 
# rownames(modTraitCor)=gsub("ME","",rownames(modTraitCor))
# rownames(textMatrix)=gsub("ME","",rownames(textMatrix))
# colnames(modTraitCor)
# 
# pdf('05_WGCNA/Fig4F-2.pdf',width = 6,height = 12)
# labeledHeatmap(Matrix = data.frame(modTraitCor),
#                xLabels = colnames(modTraitCor),
#                yLabels = rownames(modTraitCor),
#                cex.lab = 1,
#                ySymbols = rownames(modTraitCor), colorLabels = FALSE,
#                colors = blueWhiteRed(50),
#                textMatrix = data.frame(textMatrix),
#                setStdMargins = FALSE,
#                cex.text = 0.8, zlim = c(-1,1),
#                main = paste("Module-trait relationships"))
# dev.off()
# 
# 
# #基因的模块成员度（module membership）计算
# #即各基因表达值与相应模块特征基因的相关性，其衡量了基因在全局网络中的位置
# ## Notes: signedKME函数的列名是根据输入数据的列名称从第3个字母开始截取，所以如果前面的模块
# ## 的名称前面的ME如果已经去除，记得在前面加上两个字母
# geneModuleMembership <- signedKME(tpm_T2
#                                   , data.frame(tpm_T2.module$MEs)
#                                   , outputColumnName = "")
# head(geneModuleMembership)
# MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership)
#                                            , nrow(tpm_T2.module$MEs)))
# #各基因表达值与临床表型的相关性分析
# geneTraitSignificance <- as.data.frame(cor(tpm_T2
#                                            , spms
#                                            , use = 'pairwise.complete.obs'))
# 
# GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance)
#                                            , nrow(spms)))
# 
# modNames<-colnames(geneModuleMembership)
# modNames
# 
# get_module_hub_genes=function(df_module,df_ME,df_exp,module){
#   inds=which(df_module[,2]==module)
#   df_exp=df_exp[,match(row.names(df_module)[inds],colnames(df_exp))]
#   colnames(df_ME)
#   grep(paste0("ME",module),colnames(df_ME))
#   df_cor=cor(df_exp,df_ME[,grep(paste0("ME",module),colnames(df_ME))],method = 'pearson')
#   df_cor=data.frame(df_cor)
#   colnames(df_cor)='Correlation'
#   return(df_cor)
# }
# 
# 
# for (na in modNames) {
#   print(na)
#   tmp.genes <- get_module_hub_genes(df_module=tpm_T2.module$Modules
#                                     ,df_ME=tpm_T2.module$MEs
#                                     ,df_exp=tpm_T2.power$Exp
#                                     ,module = na)
#   tmp.genes <- rownames(tmp.genes)
#   print(tmp.genes)
#   tmp_genes_res <- enrichmentORA(tmp.genes,
#                                  mp_dbs=c('pathway_KEGG'))
#   
#   if (!is.null(tmp_genes_res)) {
#     tmp_genes_res <- tmp_genes_res[!duplicated(tmp_genes_res$description), ]
#     write.table(tmp_genes_res,
#                 file = paste0('05_WGCNA/', na, '_genes_GO_KEGG.txt'),
#                 sep = '\t', quote = F)
#     
#     tmp.genes.plot <- dotplot_batch(tmp_genes_res, 
#                                     dbs =c('pathway_KEGG'),
#                                     top=10,
#                                     FDR = T)
#     ggsave(plot = tmp.genes.plot,
#            filename = paste0('05_WGCNA/', na, '_genes_GO_KEGG.pdf'),
#            width = 5, height = 4)
#   }
# }



# mRNAsi 相关基因的鉴定 ###########
dir.create('05_subtype')
library(psych)
tpm_T2=tcga_tpm_log2[rownames(tcga_tpm_log2) %in% genes_protein, tcga_cli$Samples]
dim(tpm_T2)
tpm_T2 <- tpm_T2[rowSums(tpm_T2) > 0, ]


tcga_mRNAsi_gene_cor <- corr.test(t(tpm_T2),
                                  as.matrix(tcga_cli[, "mRNAsi"]))

tcga_mRNAsi_gene_cor$r

tcga_mRNAsi_gene_cor_res <- cbind(tcga_mRNAsi_gene_cor$r,
                                  tcga_mRNAsi_gene_cor$p)
colnames(tcga_mRNAsi_gene_cor_res) <- c('Corr', 'P')
tcga_mRNAsi_gene_cor_res <- as.data.frame(tcga_mRNAsi_gene_cor_res)
table(tcga_mRNAsi_gene_cor_res$Corr > 0.2, tcga_mRNAsi_gene_cor_res$P < 0.05)
table(tcga_mRNAsi_gene_cor_res$Corr < -0.3, tcga_mRNAsi_gene_cor_res$P < 0.05)
table(abs(tcga_mRNAsi_gene_cor_res$Corr) > 0.4, tcga_mRNAsi_gene_cor_res$P < 0.05)


tcga_mRNAsi_gene_cor_filtered <- tcga_mRNAsi_gene_cor_res[abs(tcga_mRNAsi_gene_cor_res$Corr) > 0.4 & tcga_mRNAsi_gene_cor_res$P < 0.05, ]
dim(tcga_mRNAsi_gene_cor_filtered)
head(tcga_mRNAsi_gene_cor_filtered)
write.csv(tcga_mRNAsi_gene_cor_filtered,
          file = 'results/tcga_mRNAsi_gene_cor_filtered.csv',
          quote = F)


mRNAsi_genes <- rownames(tcga_mRNAsi_gene_cor_res)[abs(tcga_mRNAsi_gene_cor_res$Corr) > 0.4 & tcga_mRNAsi_gene_cor_res$P < 0.05]

# 功能富集分析 ######################
get_top <- function(data, top = 10) {
  tpm.top <- data.frame()
  for (pa in c('geneontology_Biological_Process',
               'geneontology_Cellular_Component',
               'geneontology_Molecular_Function',
               'pathway_KEGG')) {
    print(pa)
    tmp <- subset(data,
                  DB == pa)
    # print(tmp)
    if (nrow(tmp) >= top) {
      tpm.top <- rbind(tpm.top, tmp[1:top, ])
    } else {
      tpm.top <- rbind(tpm.top, tmp)
    }
  }
  return(tpm.top)
}
go_kegg_barplot <- function(data, col = mycolor) {
  library(patchwork)
  tmp <- data[, c("description", "FDR", "overlap", "DB", "userId")]
  tmp$FDR[tmp$FDR < 1e-16] <- 1e-16
  colnames(tmp) <- c("Description", "FDR", "Count", "DB", "Symbol")
  tmp$FDR_log10_neg <- -log10(tmp$FDR)
  
  annodata <- data.frame(Description = unique(tmp$DB),
                         FDR = NA,
                         Count = NA,
                         DB = unique(tmp$DB),
                         Symbol = NA,
                         FDR_log10_neg = NA,
                         stringsAsFactors = F)
  
  tmp <- tmp %>% rbind(annodata)
  tmp <- tmp %>% arrange(DB, FDR_log10_neg)
  
  
  annos <- sort(unique(tmp$DB), decreasing = T)
  color_anno <- c(col[1:length(annos)],"black")
  names(color_anno) <- c(annos,"Other")
  
  tmp$Description <- factor(tmp$Description, levels = tmp$Description)
  
  pa <- ggplot()+
    geom_bar(data = tmp, aes(x=FDR_log10_neg, y=Description, fill=DB), stat = "identity")+
    geom_text(data = tmp, aes(x=FDR_log10_neg+0.5, y=Description, label=Count))+
    scale_x_continuous("-log10(FDR)",expand = c(0.01,0),limits = c(0,20))+
    scale_fill_manual(values = color_anno)+
    labs(title = "GO and KEGG")+
    theme_bw()+
    theme(axis.title.x.bottom = element_text(size = 14),
          axis.text.x.bottom = element_text(size = 10,color="black"),
          axis.ticks.length.x.bottom = unit(0.2,"cm"),
          axis.title.y.left = element_blank(),
          axis.text.y.left = element_blank(),
          axis.ticks.length.y.left = unit(0.1,"cm"),
          plot.title = element_text(size = 16,hjust = 0.5),
          legend.position = "none")
  # pa
  
  size_anno=c(rep(4.3,4),4.8)
  names(size_anno)=c(annos,"Other")
  
  tmp2 <- tmp
  tmp2$DB[tmp2$Description %in% unique(tmp2$DB)] = "Other"
  pb <- ggplot(tmp2, aes(x=0, y=Description, label=Description, color=DB, size=DB))+
    geom_text(hjust=1)+
    scale_color_manual(values = color_anno)+
    scale_size_manual(values = size_anno)+
    scale_x_continuous(limits = c(-0.05,0),expand = c(0,0))+
    theme_void()+
    theme(
      legend.position = "none"
    )
  
  
  p <- pb+pa+plot_layout(widths = c(1.2,1))
  return(p)
}

mRNAsi_go_kegg <- enrichmentORA(mRNAsi_genes,
                             mp_dbs=c('pathway_KEGG',
                                      'geneontology_Biological_Process',
                                      'geneontology_Cellular_Component',
                                      'geneontology_Molecular_Function'))
mRNAsi_go_kegg_filtered <- mRNAsi_go_kegg[mRNAsi_go_kegg$FDR < 0.05, ]
table(mRNAsi_go_kegg_filtered$DB)
write.table(mRNAsi_go_kegg_filtered,
            file = 'results/mRNAsi_go_kegg_filtered.txt',
            quote = F, sep = '\t')


library(dplyr)
mRNAsi_go_kegg_filtered_1 <- mRNAsi_go_kegg_filtered[c(3,4,5,6,7,25,30,35,36,26,12,
                                                 45:1022), ]

mRNAsi_go_kegg_filtered_1.top <- get_top(data = mRNAsi_go_kegg_filtered_1)
mRNAsi_go_kegg_filtered_1.top

mRNAsi_go_kegg_filtered_1.plot <- go_kegg_barplot(mRNAsi_go_kegg_filtered_1.top,
                                                  col = mycolor[c(1,2,3,5)])
mRNAsi_go_kegg_filtered_1.plot

ggsave(plot = mRNAsi_go_kegg_filtered_1.plot,
       filename = '05_subtype/mRNAsi_go_kegg_filtered_1.plot.pdf',
       width = 8, height = 8)


## 识别亚型 #################
# 单因素分析 ##########
coxFun <- function(dat){
  library(survival)
  colnames(dat)=c('time','status','gene')
  fmla=as.formula("Surv(time,status)~gene")
  cox=coxph(fmla,data=dat)
  p=summary(cox)[[7]][5]
  result=c(p,summary(cox)[[8]][1],summary(cox)[[8]][3],summary(cox)[[8]][4])
  return(result)
}
p.cutoff <- 0.05
# 单因素分析
tcga_sig_cox <- t(apply(t(tcga_tpm_log2[mRNAsi_genes, tcga_cli$Samples]),2,function(x){
  vl=as.numeric(x)
  tm=tcga_cli$PFI.time
  ev=tcga_cli$PFI
  #ev=ifelse(ev=='Alive',0,1)
  dat=data.frame(tm,ev,vl)[which(tm > 0 & !is.na(vl)),]
  return(coxFun(dat))
}))
colnames(tcga_sig_cox)=c('p.value','HR','Low 95%CI','High 95%CI')
length(which(tcga_sig_cox[,1]<p.cutoff))
# write.table(tcga_sig_cox,"files/tcga_OS_train_single_HR_TPM_pvalue_1.txt",quote=F,sep="\t")
tcga_sig_cox <- na.omit(tcga_sig_cox)
tcga_sig_cox <- as.data.frame(tcga_sig_cox)

mRNAsi_sigcox_genes <- rownames(tcga_sig_cox)[tcga_sig_cox$p.value < 0.01]


tcga_sig_cox_filtered <- tcga_sig_cox[tcga_sig_cox$p.value < 0.01, ]
table(tcga_sig_cox_filtered$HR > 1)


tcga_sig_cox_filtered$Genes <- rownames(tcga_sig_cox_filtered)
tcga_sig_cox_filtered <- tcga_sig_cox_filtered[, c(5,1,2,3,4)]
head(tcga_sig_cox_filtered)
tcga_sig_cox_filtered$p.value <- signif(tcga_sig_cox_filtered$p.value, 2)
tcga_sig_cox_filtered <- dplyr::arrange(tcga_sig_cox_filtered, desc(HR))
pdf('05_subtype/tcga_sig_cox.pdf', width = 6, height = 15, onefile = F)
mg_forestplot_v2(tcga_sig_cox_filtered)
dev.off()





library(ConsensusClusterPlus)
#  tcga 
tcga.sig.dat <- as.matrix(tcga_tpm_log2[mRNAsi_sigcox_genes, tcga_cli$Samples])
tcga.sig.dat <- sweep(tcga.sig.dat,1, apply(tcga.sig.dat,1,median))
dim(tcga.sig.dat)
?ConsensusClusterPlus
getwd()
{
  setwd('05_subtype/')
  clust_subtype_tcga <- ConsensusClusterPlus(tcga.sig.dat
                                             , maxK = 10, reps = 500, pItem = 0.8
                                             , pFeature = 1
                                             , title = "tcga_THCA_subtype"
                                             , clusterAlg = "pam"
                                             , distance = "pearson"
                                             # , finalLinkage = 'centroid'
                                             , plot = "pdf"
                                             , writeTable = T
                                             , seed = 123)
  setwd('..')
}


k=3
tcga.subtype <- data.frame(Sample = names(clust_subtype_tcga[[k]]$consensusClass),
                           Cluster = paste0('C', clust_subtype_tcga[[k]]$consensusClass),
                           stringsAsFactors = F)
rownames(tcga.subtype) <- tcga.subtype$Sample


# writeMatrix(tcga.subtype,row=T,header=T
#             ,outpath = 'files/tcga_THCA_subtype.txt')
writeMatrix(tcga.subtype,row=T,header=T
            ,outpath = 'results/tcga_THCA_subtype.txt')
tcga.subtype.cli <- cbind(tcga_cli, tcga.subtype[tcga_cli$Samples, ])
library(survcomp)
tcga_cluster_os.km <- ggplotKMCox(data.frame(tcga.subtype.cli$OS.time/365
                                             , event = tcga.subtype.cli$OS
                                             , tcga.subtype.cli$Cluster)
                                  ,labs = c('C1','C2','C3')
                                  ,title = 'TCGA OS'
                                  , add_text = '')
tcga_cluster_os.km


tcga_cluster_pfi.km <- ggplotKMCox(data.frame(tcga.subtype.cli$PFI.time/365
                       , event = tcga.subtype.cli$PFI
                       , tcga.subtype.cli$Cluster)
                       ,labs = c('C1','C2','C3')
                       ,title = 'TCGA PFI'
            , add_text = '')
tcga_cluster_pfi.km

tcga_cluster_dfi.km <- ggplotKMCox(data.frame(tcga.subtype.cli$DFI.time/365
                       , event = tcga.subtype.cli$DFI
                       , tcga.subtype.cli$Cluster)
                       ,labs = c('C1','C2','C3')
                       ,title = 'TCGA DFI'
            , add_text = '')
tcga_cluster_dfi.km

tcga_cluster_dss.km <- ggplotKMCox(data.frame(tcga.subtype.cli$DSS.time/365
                       , event = tcga.subtype.cli$DSS
                       , tcga.subtype.cli$Cluster)
                       ,labs = c('C1','C2','C3')
                       ,title = 'TCGA DSS'
            , add_text = '')
tcga_cluster_dss.km



tcga_cluster_km <- cowplot::plot_grid(tcga_cluster_os.km,
                                      tcga_cluster_pfi.km,
                                      tcga_cluster_dfi.km,
                                      tcga_cluster_dss.km,
                                      ncol = 4,
                                      labels = c('A', '', '', ''))
tcga_cluster_km
ggsave(plot = tcga_cluster_km,
       filename = '05_subtype/tcga_cluster_km.pdf',
       width = 18, height = 5)



dim(tcga.sig.dat)
cor_res <- psych::corr.test(t(tcga.sig.dat))
library(corrplot)
corrplot(cor_res$r, type="upper", order="hclust", 
         p.mat = cor_res$p, sig.level = 0.05)
#不显著的为空白,筛选的标准为0.01
pdf('05_subtype/sig.gene.cor.pdf', width = 10, height = 10)
corrplot(cor_res$r, 
         # type="upper", 
         # method = c("square"),
         order="hclust", 
         hclust.method = "median",
         p.mat = cor_res$p, 
         sig.level = 0.05, 
         insig = "blank")
dev.off()




tcga_imsubtype_mRNAsi_plot <- mg_violin(tcga_cli[, c("Immune Subtype", "mRNAsi")], 
                                        melt=TRUE, ylab='mRNAsi',
                                        jitter = F,
                                        leg.title='Immune Subtype', 
                                        legend.pos='tl')
tcga_imsubtype_mRNAsi_plot

tcga_cluster_mRNAsi_plot <- mg_violin(tcga.subtype.cli[, c("Cluster", "mRNAsi")], 
                                        melt=TRUE, ylab='mRNAsi',
                                        jitter = F,
                                        leg.title='Cluster', 
                                        legend.pos='tl')
tcga_cluster_mRNAsi_plot

tcga_subtype_mRNAsi_plot <- cowplot::plot_grid(tcga_cluster_mRNAsi_plot,
                                               tcga_imsubtype_mRNAsi_plot,
                                               ncol = 2)
tcga_subtype_mRNAsi_plot
ggsave(plot = tcga_subtype_mRNAsi_plot,
       filename = '05_subtype/tcga_subtype_mRNAsi_plot.pdf',
       width = 10, height = 5)




# SNV 分析 #############
# 突变分析 ######################
dir.create('06_cluster_SNV')
tcga.mut.dat <- getTCGAMAFByCode('THCA')
tcga.mut.dat <- as.data.frame(tcga.mut.dat@data)
tcga.mut.dat <- tcga.mut.dat[, c("Hugo_Symbol", "Tumor_Sample_Barcode", "Variant_Classification")]

tcga.mut.dat$Variant_Classification <- 1
tcga.mut.dat <- reshape2::dcast(data = tcga.mut.dat, Hugo_Symbol ~ Tumor_Sample_Barcode)
class(tcga.mut.dat)
rownames(tcga.mut.dat) <- tcga.mut.dat$Hugo_Symbol
tcga.mut.dat <- tcga.mut.dat[, -1]
tcga.mut.dat <- ifelse(tcga.mut.dat == 0, 0, 1)


colnames(tcga.mut.dat) <- paste0(colnames(tcga.mut.dat), '-01')
mut.samples <- intersect(colnames(tcga.mut.dat), tcga.subtype.cli$Samples)


tcga.mut.dat <- tcga.mut.dat[, mut.samples]
tcga_mut_cli <- tcga.subtype.cli[mut.samples, ]

tcga.mut.dat.freq <- as.data.frame(rowSums(tcga.mut.dat))
colnames(tcga.mut.dat.freq) <- 'Freq'
tcga.mut.dat.freq$Genes <- rownames(tcga.mut.dat.freq)
library(dplyr)
str(tcga.mut.dat.freq)
head(tcga.mut.dat.freq)
tcga.mut.dat.freq <- dplyr::arrange(tcga.mut.dat.freq, desc(Freq))
head(tcga.mut.dat.freq)
dim(tcga.mut.dat.freq)
write.csv(tcga.mut.dat.freq,
          file = '06_cluster_SNV/tcga.subtype.mut.gene.csv')

# mut.genes <- openxlsx::read.xlsx('00_origin_datas/cancer.driver.genes.PMC4160307.xlsx', sheet = 1)[, 1]
# 
# tcga.mut.dat <- ifelse(tcga.mut.dat > 0, 'Mutant', 'WildType')
# 
# dim(tcga.mut.dat)

mut.genes <- rownames(tcga.mut.dat.freq)[tcga.mut.dat.freq$Freq > 2]
# 
# 
# which(mut.genes == 'TSC22D1')
# tcga.mut.dat.freq['TSC22D1', ]
# table(tcga.mut.dat['TSC22D1', ])

mut.res <- data.frame(`C1` = NA,
                      `C2` = NA,
                      `C3` = NA)
table(tcga.mut.dat[mut.genes[1], ], tcga_mut_cli$Cluster)
table(tcga.mut.dat[mut.genes[10], ], tcga_mut_cli$Cluster)
table(tcga.mut.dat["KCTD9", ], tcga_mut_cli$Cluster)
dim(table(tcga.mut.dat[mut.genes[10], ], tcga_mut_cli$Cluster))

mut.p <- c()
tmp.ge <- c()
for (ge in mut.genes) {
  print(ge)
  tmp <- table(tcga.mut.dat[ge, ], tcga_mut_cli$Cluster)
  if (dim(tmp)[1] > 1) {
    pvalue <- fisher.test(tmp)
    mut.p <- c(mut.p, pvalue$p.value)
    mut.res <- rbind(mut.res, tmp[2, ])
    tmp.ge <- c(tmp.ge, ge)
  }
}
mut.res <- na.omit(mut.res)
rownames(mut.res) <- tmp.ge
class(mut.res)
mut.res$P.value <- mut.p

table(mut.res$P.value < 0.05)
mut.res.filtered <- mut.res[mut.res$P.value < 0.05, ]
mut.res.filtered
dim(mut.res.filtered)
# write.csv(mut.res.filtered,
#           file = 'results/tcga.mut.dat.csv')
# write.csv(mut.res.filtered,
#           file = '06_cluster_SNV/tcga.genes.csv')

mut.plot.dat <- tcga.mut.dat[rownames(mut.res.filtered), ]
mut.plot.dat <- ifelse(mut.plot.dat == 1, 'Mutant', NA)
# rownames(mut.plot.dat) <- paste(rownames(mut.plot.dat), signif(mut.res.filtered$P.value[1:15], 1), sep = ' ')
library(scales)
library(ggsci)
library(ComplexHeatmap)
alter_graphic <- function (graphic = c("rect", "point"), width = 1, 
                           height = 1, horiz_margin = unit(1, "pt"), vertical_margin = unit(1, 
                                                                                            "pt"), fill = "red", col = NA, pch = 16, 
                           ...) 
{
  graphic = match.arg(graphic)[1]
  if (graphic == "rect") {
    if (!is.numeric(width)) {
      stop_wrap("`width` should be nummeric.")
    }
    if (!is.numeric(height)) {
      stop_wrap("`height` should be nummeric.")
    }
    if (width != 1) {
      if (missing(horiz_margin)) {
        horiz_margin = unit(0, "pt")
      }
    }
    if (height != 1) {
      if (missing(vertical_margin)) {
        vertical_margin = unit(0, "pt")
      }
    }
    fun = function(x, y, w, h) {
      w = w * width
      h = h * height
      grid.rect(x, y, w - horiz_margin * 2, h - vertical_margin * 
                  2, gp = gpar(fill = fill, col = col, ...))
    }
  }
  else if (graphic == "point") {
    fun = function(x, y, w, h) {
      grid.points(x, y, pch = pch, gp = gpar(fill = fill, 
                                             col = col, ...))
    }
  }
  return(fun)
}

# 定义突变颜色 #########
col = c("Mutant" = 'black')

alter_fun = list(
  background = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "#CCCCCC", col = NA))
  },
  
  Mutant = function(x, y, w, h) {
    grid.rect(x, y, w-unit(3, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = col["Mutant"], col = NA))
  }
)

alter_fun = list(
  background = alter_graphic("rect", fill = "#CCCCCC"),	
  Mutant = alter_graphic("rect", fill = col["Mutant"])
)
heatmap_legend_param = list(title = "Alterations", 
                            at = c("Mutant"), 
                            labels = c("Mutant"))

color_cluster = mycolor[1:3]
names(color_cluster) = c('C1', 'C2', 'C3')

c1.tcga_mut_cli <- tcga_mut_cli[tcga_mut_cli$Cluster == 'C1', ]
c1.mut.plot.dat <- mut.plot.dat[, c1.tcga_mut_cli$Samples]
# pdf('PDFs/c1.mut.plot.dat.pdf', width = 8, height = 6, onefile = F)
table(tcga_mut_cli$Cluster)
dim(c1.mut.plot.dat)
mut1 <- oncoPrint(as.matrix(c1.mut.plot.dat),
                  row_order = rownames(c1.mut.plot.dat),
                  # column_order = tcga_mut_cli$Samples,
                  alter_fun = alter_fun, 
                  col = col, 
                  column_title = "C1",
                  heatmap_legend_param = heatmap_legend_param,
                  bottom_annotation = HeatmapAnnotation(Cluster=c1.tcga_mut_cli[, c("Cluster")],
                                                        col=list("Cluster"=color_cluster),
                                                        show_annotation_name = TRUE,
                                                        gap=unit(1, "mm"),
                                                        na_col="grey"),
                  pct_side = "right", row_names_side = "left")
mut1
dev.off()


c2.tcga_mut_cli <- tcga_mut_cli[tcga_mut_cli$Cluster == 'C2', ]
c2.mut.plot.dat <- mut.plot.dat[, c2.tcga_mut_cli$Samples]
# pdf('PDFs/c2.mut.plot.dat.pdf', width = 8, height = 6, onefile = F)
table(tcga_mut_cli$Cluster)
mut2 <- oncoPrint(as.matrix(c2.mut.plot.dat),
                  row_order = rownames(c2.mut.plot.dat),
                  # column_order = tcga_mut_cli$Samples,
                  alter_fun = alter_fun, 
                  col = col, 
                  column_title = "C2",
                  heatmap_legend_param = heatmap_legend_param,
                  bottom_annotation = HeatmapAnnotation(Cluster=c2.tcga_mut_cli[, c("Cluster")],
                                                        col=list("Cluster"=color_cluster),
                                                        show_annotation_name = TRUE,
                                                        gap=unit(1, "mm"),
                                                        na_col="grey"),
                  pct_side = "right", row_names_side = "left")
mut2
dev.off()


c3.tcga_mut_cli <- tcga_mut_cli[tcga_mut_cli$Cluster == 'C3', ]
c3.mut.plot.dat <- mut.plot.dat[, c3.tcga_mut_cli$Samples]
# pdf('PDFs/c3.mut.plot.dat.pdf', width = 8, height = 6, onefile = F)
table(tcga_mut_cli$Cluster)
mut3 <- oncoPrint(as.matrix(c3.mut.plot.dat),
                  row_order = rownames(c3.mut.plot.dat),
                  # column_order = tcga_mut_cli$Samples,
                  alter_fun = alter_fun,
                  col = col,
                  column_title = "C3",
                  heatmap_legend_param = heatmap_legend_param,
                  bottom_annotation = HeatmapAnnotation(Cluster=c3.tcga_mut_cli[, c("Cluster")],
                                                        col=list("Cluster"=color_cluster),
                                                        show_annotation_name = TRUE,
                                                        gap=unit(1, "mm"),
                                                        na_col="grey"),
                  pct_side = "right", row_names_side = "left")
mut3
dev.off()




pdf('06_cluster_SNV/tcga.mut.plot.dat.pdf', width = 15, height = 5, onefile = F)
mut1 + mut2 + mut3 
dev.off()


tcga_tmb <- mg_getTCGATMBByCode('THCA')
tcga_tmb <- as.data.frame(tcga_tmb)
head(tcga_tmb)
tcga_tmb$Sample <- paste0(tcga_tmb$Sample, '-01')

head(tcga.subtype)
tcga_tmb <- merge(tcga_tmb, tcga.subtype.cli[, c("Samples", "Cluster", "Cluster")], 
                  by.x = 'Sample', by.y = 'Samples')

tcga_tmb_plot <- mg_violin(tcga_tmb[, c("Cluster", "TMB")]
                           ,melt = T
                           ,ylab = 'Tumor mutation burden'
                           # ,ylim = c(0,100)
                           ,test_method = 'wilcox.test'
                           ,cmp_test_method = 'wilcox.test'
                           ,legend.pos = NULL
                           ,jitter=F
                           # ,group.col = pal_aaas()(9)[c(2,4,3)]
                           ,show_compare = T)
tcga_tmb_plot
ggsave(plot = tcga_tmb_plot,
       filename = '06_cluster_SNV/tcga_tmb_plot.pdf',
       width = 5, height = 5)


####
tcga.immu.lands.p1=readMatrix(paste0(MG_Grobal_baseFolder,'/source/PMC5982584_supplement_2.txt'))
colnames(tcga.immu.lands.p1)
tcga.immu.lands.p1<-tcga.immu.lands.p1[tcga.immu.lands.p1$`TCGA Study`=='THCA',]
rownames(tcga.immu.lands.p1)=paste0(rownames(tcga.immu.lands.p1),'-01')

tcga.subtype_forAlt=tcga.subtype
rownames(tcga.subtype_forAlt)=substr(rownames(tcga.subtype_forAlt),1,15)

table(is.na(match(row.names(tcga.subtype_forAlt),row.names(tcga.immu.lands.p1))))

tcga.immu.lands.p1=tcga.immu.lands.p1[intersect(row.names(tcga.subtype_forAlt),row.names(tcga.immu.lands.p1)),]
dim(tcga.immu.lands.p1)
colnames(tcga.immu.lands.p1)

table(tcga.immu.lands.p1$`TCGA Subtype`,tcga.subtype_forAlt[rownames(tcga.immu.lands.p1),1])
plotMutiBar(table(tcga.immu.lands.p1$`TCGA Subtype`
                  ,tcga.subtype_forAlt[rownames(tcga.immu.lands.p1),2]))

table(tcga.immu.lands.p1$`Immune Subtype`,tcga.subtype_forAlt[rownames(tcga.immu.lands.p1),2])
plotMutiBar(table(tcga.immu.lands.p1$`Immune Subtype`
                  ,tcga.subtype_forAlt[rownames(tcga.immu.lands.p1),2]))

colnames(tcga.immu.lands.p1)

col.selected=c('Homologous Recombination Defects','Fraction Altered','Number of Segments','Nonsilent Mutation Rate')
length(col.selected)

pal_aaas()(10)[c(2,1,3,4:9)]

tcga.geneAlt.p.all=list()
for(val in col.selected){
  if(val=='Nonsilent Mutation Rate'){
    p=mg_violin(data.frame(tcga.subtype_forAlt[rownames(tcga.immu.lands.p1),2]
                           ,tcga.immu.lands.p1[,val])
                ,melt = T
                ,ylab = 'Tumor mutation burden'
                # ,ylim = c(0,100)
                ,test_method = 'wilcox.test'
                ,cmp_test_method = 'wilcox.test'
                ,legend.pos = 'tr'
                ,jitter=T
                # ,group.col = pal_aaas()(9)[c(2,4,3)]
                ,show_compare = T)
  }else{
    p=mg_violin(data.frame(tcga.subtype_forAlt[rownames(tcga.immu.lands.p1),2]
                           ,tcga.immu.lands.p1[,val])
                ,melt = T
                ,ylab = val
                ,jitter=T
                # ,group.col = pal_d3()(9)
                ,test_method = 'wilcox.test'
                ,cmp_test_method = 'wilcox.test'
                ,legend.pos = 'tr'
                ,show_compare = T)
  }
  
  tcga.geneAlt.p.all=c(tcga.geneAlt.p.all,list(p))
}

fig3a=mg_merge_plot(tcga.geneAlt.p.all,nrow = 1,ncol = 4,common.legend = T)
fig3a
savePDF('06_cluster_SNV/Fig3A.pdf',fig3a,height = 5,width = 12)
tcga.geneAlt.p.all[[5]]




# 分子亚型的临床特征比较  ##############
# TCGA
colnames(tcga.subtype.cli)
tcga.subtype.cli$Age <- as.numeric(tcga.subtype.cli$Age)
fivenum(tcga.subtype.cli$Age)
tcga.subtype.cli$Age1 <- ifelse(tcga.subtype.cli$Age > 45, '>45', '<=45')

Age1_compare <- table(tcga.subtype.cli$Age1, tcga.subtype.cli$Cluster)
Age1_compare
Age1_compare_bar <- plotMutiBar(Age1_compare, legTitle = 'Age')
Age1_compare_bar


T.Stage_compare <- table(tcga.subtype.cli$A3_T, tcga.subtype.cli$Cluster)
T.Stage_compare
T.Stage_compare_bar <- plotMutiBar(T.Stage_compare, legTitle = 'T.Stage')
T.Stage_compare_bar

tcga.subtype.cli$A4_N <- gsub('[ab]', '', tcga.subtype.cli$A4_N)
tcga.subtype.cli$A4_N[tcga.subtype.cli$A4_N == 'NX'] <- NA
N.Stage_compare <- table(tcga.subtype.cli$A4_N, tcga.subtype.cli$Cluster)
N.Stage_compare
N.Stage_compare_bar <- plotMutiBar(N.Stage_compare, legTitle = 'N.Stage')
N.Stage_compare_bar

tcga.subtype.cli$A5_M[tcga.subtype.cli$A5_M == 'MX'] <- NA
tcga.subtype.cli$A5_M[tcga.subtype.cli$A5_M == ''] <- NA
M.Stage_compare <- table(tcga.subtype.cli$A5_M, tcga.subtype.cli$Cluster)
M.Stage_compare
M.Stage_compare_bar <- plotMutiBar(M.Stage_compare, legTitle = 'M.Stage')
M.Stage_compare_bar

Stage_compare <- table(tcga.subtype.cli$Stage, tcga.subtype.cli$Cluster)
Stage_compare
Stage_compare_bar <- plotMutiBar(Stage_compare, legTitle = 'Stage')
Stage_compare_bar

Gender_compare <- table(tcga.subtype.cli$Gender, tcga.subtype.cli$Cluster)
Gender_compare
Gender_compare_bar <- plotMutiBar(Gender_compare, legTitle = 'Gender')
Gender_compare_bar

tcga_os <- table(tcga.subtype.cli$PFI, tcga.subtype.cli$Cluster)
tcga_os
# rownames(tcga_os) <- c('Alive', 'Dead')
tcga_os_bar <- plotMutiBar(tcga_os, legTitle = 'PFI')
tcga_os_bar




# tcga_other_subtype <- read.csv('00_origin_datas/TCGA/TCGA_immune_subtype_mmc2.csv',
#                                check.names = F, stringsAsFactors = F)
# tcga_other_subtype <- tcga_other_subtype[tcga_other_subtype$`TCGA Study` == 'THCA', ]
# tcga_other_subtype$`TCGA Participant Barcode` <- paste0(tcga_other_subtype$`TCGA Participant Barcode`, '-01')
# 
# 
# tcga.subtype.cli.1 <- merge(tcga.subtype.cli, tcga_other_subtype[, c("TCGA Participant Barcode",
#                                                                      "Immune Subtype")],
#                             by.x = 'Samples', by.y = 'TCGA Participant Barcode', all = T)
# tcga.subtype.cli.1 <- tcga.subtype.cli.1[!is.na(tcga.subtype.cli.1$Cluster), ]
# # library(TCGAbiolinks)
# # tcga_other_subtype1 <- TCGAquery_subtype(tumor = "THCA")
# # rm(tcga_other_subtype1)
# # tcga_other_subtype1$patient <- paste0(tcga_other_subtype1$patient, '-01')
# 
# 
# tcga.subtype.cli.1 <- crbind2DataFrame(tcga.subtype.cli.1)
Immune_compare <- table(tcga.subtype.cli$`Immune Subtype`, tcga.subtype.cli$Cluster)
Immune_compare
Immune_compare_bar <- plotMutiBar(Immune_compare, legTitle = 'Immune Subtype')
Immune_compare_bar

library(survival)
Immune_km <- ggplotKMCox(data.frame(tcga.subtype.cli$OS.time/365
                                    , event = tcga.subtype.cli$OS
                                    , tcga.subtype.cli$`Immune Subtype`)
                         ,labs = c('C1','C2','C3','C4','C6')
                         ,title = 'Immune Subtype'
                         , add_text = '')
Immune_km
ggsave(plot = Immune_km,
       filename = '06_cluster_SNV/Immune_km.pdf',
       width = 6, height = 6)


tcga.subtype.cli_bar <- cowplot::plot_grid(tcga_os_bar,
                                           Age1_compare_bar,
                                           Gender_compare_bar,
                                           T.Stage_compare_bar,
                                           N.Stage_compare_bar,
                                           M.Stage_compare_bar,
                                           Stage_compare_bar,
                                           Immune_compare_bar,
                                           align = 'hv',
                                           ncol = 4,
                                           labels = LETTERS[1:8])
tcga.subtype.cli_bar
ggsave(plot = tcga.subtype.cli_bar,
       filename = '06_cluster_SNV/tcga.subtype.cli_bar.pdf',
       width = 20, height = 10)





tcga_imsubtype_mRNAsi_plot <- mg_violin(tcga_cli[, c("Immune Subtype", "mRNAsi")], 
                                        melt=TRUE, ylab='mRNAsi',
                                        jitter = F,
                                        leg.title='Immune Subtype', 
                                        legend.pos='tl')
tcga_imsubtype_mRNAsi_plot
ggsave(plot = tcga_imsubtype_mRNAsi_plot,
       filename = '01_mRNAsi_cli/tcga_imsubtype_mRNAsi_plot.pdf',
       width = 6, height = 5)


tcga_cluster_mRNAsi_plot <- mg_violin(tcga.subtype.cli[, c("Cluster", "mRNAsi")], 
                                        melt=TRUE, ylab='mRNAsi',
                                        jitter = F,
                                        leg.title='Cluster', 
                                        legend.pos='tl')
tcga_cluster_mRNAsi_plot

tcga_subtype_mRNAsi_plot <- cowplot::plot_grid(tcga_imsubtype_mRNAsi_plot,
                                               tcga_cluster_mRNAsi_plot,
                                               ncol = 2, 
                                               labels = LETTERS[1:2])
tcga_subtype_mRNAsi_plot


ggsave(plot = tcga_subtype_mRNAsi_plot,
       filename = '05_subtype/tcga_subtype_mRNAsi_plot.pdf',
       width = 10, height = 5)





# 亚型免疫分析 ####################
dir.create('07_cluster_immune')
head(tcga_estimate)
estimate_list <- list()
i <- 1
for (ty in c("StromalScore","ImmuneScore","ESTIMATEScore")) {
  
  tmp <- mg_violin(data.frame(tcga.subtype.cli$Cluster,
                              tcga_estimate[tcga.subtype.cli$Samples, ty]), 
                   melt=TRUE, ylab=ty,
                   leg.title='', test_method='rank',
                   jitter = F,
                   xlab = 'TCGA',
                   legend.pos='br',
                   show_compare = T)
  estimate_list[[i]] <- tmp
  i <- i + 1
}

tcga_cluster_estimate_plot <- cowplot::plot_grid(plotlist = estimate_list,
                                                 ncol = 3)
tcga_cluster_estimate_plot


tcga_cluster_ssgsea_plot <- mg_PlotMutiBoxplot(tcga_ssgsea[tcga.subtype.cli$Samples, ]
                                               , group = tcga.subtype.cli$Cluster
                                               , legend.pos = 'tl'
                                               , add = 'boxplot'
                                               , xangle = 60
                                               , ylab = 'ssGSEA Score'
                                               # , xlab = 'drug'
                                               , group_cols = ggsci::pal_igv()(9)
                                               , test_method = 'anova')
tcga_cluster_ssgsea_plot




immunosuppression.genes=c('ADORA2A','ARHGEF5','BTLA','CD160','CD244','CD27','CD274','CD276','CD47','CD80','CEACAM1','CTLA4','GEM','HAVCR2','ICOS','IDO1','LAG3','PDCD1','TNFSF4','VISTA','VTCN1')
length(immunosuppression.genes)

######### tcga
dim(tcga_tpm_log2)
boxplot(tcga_tpm_log2[, 1:5])
setdiff(immunosuppression.genes,rownames(tcga_tpm_log2))
rownames(tcga_tpm_log2)[which(rownames(tcga_tpm_log2)=='VSIR')]='VISTA'

tcga.t.exp.icg=tcga_tpm_log2[rownames(tcga_tpm_log2) %in% immunosuppression.genes,]
tcga.t.exp.icg=t(tcga.t.exp.icg)
dim(tcga.t.exp.icg)

tcga_cluster_icg_plot <- mg_PlotMutiBoxplot(tcga.t.exp.icg[rownames(tcga.subtype.cli),]
                          , group = tcga.subtype.cli$Cluster
                          , legend.pos = 'tl'
                          , add = 'boxplot'
                          , xangle = 60
                          , ylab = 'EXP'
                          # , xlab = 'drug'
                          , group_cols = ggsci::pal_igv()(9)
                          , test_method = 'anova')
tcga_cluster_icg_plot

tcga_cluster_immune_plot <- cowplot::plot_grid(tcga_cluster_estimate_plot,
                                               tcga_cluster_ssgsea_plot,
                                               tcga_cluster_icg_plot,
                                               ncol = 1,
                                               labels = LETTERS[1:3])
tcga_cluster_immune_plot

ggsave(plot = tcga_cluster_immune_plot,
       filename = '07_cluster_immune/tcga_cluster_immune_plot.pdf',
       width = 12, height = 13.5)


# TIDE 分析 ################
dir.create('08_TIDE_drug')

# tcga 的计算 ######
dir.create('08_TIDE_drug/TCGA')
tcga_tide_dat <- t(scale(t(tcga_tpm_log2[, tcga.subtype.cli$Samples]),scale = F))
boxplot(tcga_tide_dat[, 1:5])
tcga_tide_dat[1:5, 1:5]

write.table(tcga_tide_dat,
            file = '08_TIDE_drug/TCGA/tcga_tide_dat.txt',
            quote = F, sep = '\t')

tcga_tide_dat<-read.csv('08_TIDE_drug/TCGA/tcga_tide_res.csv',row.names = 1,stringsAsFactors = F)
colnames(tcga_tide_dat)



tcga_tide_list <- list()
for (fea in c("TIDE","Dysfunction","Exclusion")) {
  print(fea)
  tmp_plot <- mg_violin(data.frame(tcga.subtype.cli$Cluster
                                   ,tcga_tide_dat[tcga.subtype.cli$Samples, fea])
                        ,melt = T
                        ,ylab = fea
                        ,jitter=F
                        , xlab = 'TCGA'
                        # ,group.col = pal_jco()(9)[1:2]
                        ,test_method = 'wilcox.test'
                        ,cmp_test_method = 'wilcox.test'
                        ,legend.pos = 'tr'
                        ,show_compare = T)
  tcga_tide_list[[fea]] <- tmp_plot
}
tcga_tide_plot1 <- cowplot::plot_grid(plotlist = tcga_tide_list,
                                      ncol = 3)
tcga_tide_plot1
# ggsave(plot = tcga_tide_plot1,
#        filename = '05_tide/tcga_tide_plot.pdf',
#        width = 15, height = 5)


tcga_tide_bar_dat <- table(tcga.subtype.cli$Cluster, 
                           tcga_tide_dat[tcga.subtype.cli$Samples, "Responder"])
tcga_tide_bar_dat
tcga_tide_bar <- plotMutiBar(t(tcga_tide_bar_dat), showValue = T, showLine = F, legTitle='Responder')
tcga_tide_bar


tcga_tide_plot <- cowplot::plot_grid(tcga_tide_plot1,
                                     tcga_tide_bar,
                                     rel_widths = c(3,1))
tcga_tide_plot

ggsave(plot = tcga_tide_plot,
       filename = '08_TIDE_drug/tcga_tide_plot.pdf',
       width = 15, height = 4.5)




# 药物敏感性 ###########################
library(pRRophetic)
library(ggplot2)

# tcga 
## Cisplatin,顺铂
set.seed(12345)
tcga_Cisplatin <- pRRopheticPredict(as.matrix(tcga_tpm_log2[, tcga.subtype.cli$Samples])
                                    , "Cisplatin"
                                    , "all"
                                    , selection=1
                                    ,dataset = "cgp2016")
tcga_Cisplatin <- data.frame(tcga_Cisplatin)

tcga_durg_ic50_res <- tcga_Cisplatin

drugs <- c("Cisplatin","Erlotinib","Rapamycin","Sunitinib","PHA-665752","MG-132","Paclitaxel",
           "Cyclopamine","AZ628","Sorafenib","VX-680","Imatinib","TAE684","Crizotinib","Saracatinib",
           "S-Trityl-L-cysteine","Z-LLNle-CHO","Dasatinib","GNF-2","CGP-60474","CGP-082996","A-770041",
           "WH-4-023","WZ-1-84","BI-2536","BMS-509744","CMK","Pyrimethamine","JW-7-52-1","A-443654",
           "GW843682X","MS-275","Parthenolide","KIN001-135","TGX221","Bortezomib","XMD8-85",
           "Roscovitine","Salubrinal","Lapatinib","Vinorelbine","NSC-87877","QS11","CP466722",
           "Midostaurin","Shikonin","AKT inhibitor VIII","Embelin","Bexarotene","Bleomycin","Phenformin")
dim(tcga_durg_ic50_res)
for (drug in drugs) {
  print(drug)
  set.seed(12345)
  tmpic50 <- pRRopheticPredict(as.matrix(tcga_tpm_log2[, tcga.subtype.cli$Samples])
                               , drug
                               , "all"
                               , selection=1
                               , dataset = "cgp2016")
  tmpic50 <- data.frame(tmpic50)
  colnames(tmpic50) <- drug
  tcga_durg_ic50_res <- cbind(tcga_durg_ic50_res, tmpic50)
}

colnames(tcga_durg_ic50_res)
tcga_durg_ic50_res <- tcga_durg_ic50_res[, -1]
class(tcga_durg_ic50_res)







num <- 41:51
mg_PlotMutiBoxplot(tcga_durg_ic50_res[tcga.subtype.cli$Samples, num]
                   , group = tcga.subtype.cli$Cluster
                   , legend.pos = 'top'
                   , add = 'boxplot'
                   , xangle = 0
                   , ylab = 'Estimated IC50'
                   # , xlab = 'drug'
                   , group_cols = ggsci::pal_jama()(9)
                   , test_method = 'kruskal.test')




tcga_durg_ic50_res[, c("Cisplatin", "Erlotinib", "Paclitaxel", "Lapatinib",
                       
                       "AKT inhibitor VIII", "Sorafenib", "Imatinib", "Crizotinib")]

tcga_drug_plot_list <- list()
for (dr in c("Cisplatin", "Erlotinib", "Paclitaxel", "Lapatinib",
             "AKT inhibitor VIII", "Sorafenib", "Imatinib", "Crizotinib")) {
  tmp <- mg_violin(data.frame(tcga.subtype.cli$Cluster,
                              tcga_durg_ic50_res[tcga.subtype.cli$Samples, dr])
                   ,melt = T
                   ,ylab = 'Estimated IC50'
                   ,xlab = dr
                   # ,ylab = 'TCGA'
                   # ,ylim = c(0,100)
                   ,test_method = 'wilcox.test'
                   ,cmp_test_method = 'wilcox.test'
                   ,legend.pos = 'br'
                   ,jitter=F
                   # ,Cluster.col = pal_aaas()(9)[c(2,4,3)]
                   ,show_compare = T)
  tcga_drug_plot_list[[dr]] <- tmp
}


tcga_drug_plot <- cowplot::plot_grid(plotlist = tcga_drug_plot_list,
                                     ncol = 4)
tcga_drug_plot
ggsave(plot = tcga_drug_plot,
       filename = '08_TIDE_drug/tcga_drug_plot.pdf',
       width = 16, height = 10)


# ggsave(plot = drug_plot,
#        filename = '08_TIDE_drug/drug_plot.pdf',
#        width = 20, height = 5)






# 预后模型分析 ################
dir.create('09_model')

model_genes <- mRNAsi_genes

tcga_model_data <- cbind(tcga.subtype.cli[, c("PFI.time", "PFI")],
                         t(tcga_tpm_log2[model_genes, tcga.subtype.cli$Samples]))
colnames(tcga_model_data) <- gsub('-', '_', colnames(tcga_model_data))
colnames(tcga_model_data)[1:2] <- c('OS.time', 'OS')



save(tcga_model_data,
     file = 'model.Rdata')

coxFun <- function(dat){
  library(survival)
  colnames(dat)=c('time','status','gene')
  fmla=as.formula("Surv(time,status)~gene")
  cox=coxph(fmla,data=dat)
  p=summary(cox)[[7]][5]
  result=c(p,summary(cox)[[8]][1],summary(cox)[[8]][3],summary(cox)[[8]][4])
  return(result)
}
plotKMCox_1=function(dat,n){
  library(survival)
  library(ggsci)
  mypal = pal_jama(alpha = 0.7)(7)
  colnames(dat)=c('time','status','groups')
  sdf<-survdiff(Surv(time,status) ~ groups,data=dat)
  #print((sdf))
  p<-pchisq(sdf$chisq,length(sdf$n)-1,lower.tail=FALSE)
  sf<-survfit(Surv(time,status) ~ groups,data=dat)
  colKm=c(mypal[2],mypal[1],mypal[3],mypal[4])
  plot(sf, mark.time = TRUE,col=colKm,xlab=paste("Survival time in day","\np=",round(p,5)),ylab = "Survival probabilities",main=n)
  legend('topright',paste0(gsub('groups=','',names(sf$strata)),'(N=',sdf$n,')'), col = colKm,
         lty = c(1,1, 1, 1),lwd=c(1,1,1,1),merge = TRUE,cex = 0.8)
  return(p)
}
select_gene_median <- function(dat1, 
                               dat2 = NULL, 
                               dat3 = NULL, 
                               a = 1, 
                               n = 100, 
                               ratio = 0.5, 
                               cut_p = 0.05, 
                               years = c(1,3,5)){
  library(timeROC)
  library(survival)
  library(glmnet)
  library(mosaic)
  sample.index <- data.frame()
  SampleingTime <- c()
  SamplingSeed <- c()
  tra.auc <- c()
  test.auc <- c()
  geo1.auc <- c()
  geo2.auc <- c()
  all.auc <- c()
  tra.p <- c()
  test.p <- c()
  geo1.p <- c()
  geo2.p <- c()
  all.p <- c()
  tcga.dat <- dat1
  geo1.dat <- dat2
  geo2.dat <- dat3
  gene.list <- c()
  GeneNums <- c()
  for (i in a:n) {
    set.seed(i)
    par(mfrow = c(2, 3))
    myd.index <- seq(1,nrow(tcga.dat),1)
    tra.index <- sample(myd.index,size = round(nrow(tcga.dat)*ratio))
    tra.dat <- tcga.dat[tra.index,]
    test.dat <- tcga.dat[-tra.index,]
    dir.create('files/model_select/', recursive = T, showWarnings = F)
    write.table(rownames(tra.dat), file = paste0('files/model_select/tra.dat_median_', i, '.txt'), sep = '\t', quote = F, row.names = F)
    write.table(rownames(test.dat), file = paste0('files/model_select/test.dat_median_', i, '.txt'), sep = '\t', quote = F, row.names = F)
    tra.cox <- t(apply(tra.dat[,3:c(ncol(tra.dat))],2,function(x){
      vl=as.numeric(x)
      tm=tra.dat$OS.time
      ev=tra.dat$OS
      #ev=ifelse(ev=='Alive',0,1)
      dat=data.frame(tm,ev,vl)[which(tm > 0 & !is.na(vl)),]
      return(coxFun(dat))
    }))
    colnames(tra.cox) <- c('p.value','HR','Low 95%CI','High 95%CI')
    tra.cox <- as.data.frame(tra.cox)
    write.table(tra.cox, file = paste0('files/model_select/sig_cox_median_', i, '.txt'), sep = '\t', quote = F)
    cut.cox=tra.cox[which(tra.cox[,1]<cut_p),] 
    if (nrow(cut.cox) > 1) {
      print(paste("Processing....: ",i," resampling",sep=""))
      flush.console()
      geneid=rownames(cut.cox)
      geneid=geneid[which(!is.na(geneid))]
      if (length(geneid)>1) {
        sample.index<-c(sample.index,data.frame(tra.index))
        set.seed(i)
        cv.fit <- cv.glmnet(as.matrix(tra.dat[,geneid]), cbind(time=tra.dat$OS.time, 
                                                               status=tra.dat$OS)
                            ,family="cox", nlambda=100, alpha=1) 
        sig.coef <- coefficients(cv.fit,s=cv.fit$lambda.min)[which(coefficients(cv.fit,s=cv.fit$lambda.min)[,1]!=0),1]
        write.table(sig.coef, file = paste0('files/model_select/sig.coef_median_', i, '.txt'), sep = '\t', quote = F)
        if (length(names(sig.coef)>1)) {
          dat1 <- cbind(time=tra.dat$OS.time,
                        status=tra.dat$OS,
                        tra.dat[,match(names(sig.coef),
                                       colnames(tra.dat))])
          fmla <- as.formula(paste0("Surv(time, status) ~"
                                    ,paste0(names(sig.coef),collapse = '+')))
          cox <- coxph(fmla, data =as.data.frame(dat1))
          # lan=coef(cox)
          # print(lan)
          cox1=step(cox, trace = 0)
          lan=coef(cox1)
          print(lan)
          write.table(lan, file = paste0('files/model_select/lan_median_', i, '.txt'), sep = '\t', quote = F)
          final_gene=names(lan)
          GeneNums <-c(GeneNums, length(final_gene))
          
          risk.tra=as.numeric(lan%*%as.matrix(t(tra.dat[,final_gene])))
          # risk.tra=zscore(risk.tra)
          ROC.DSST <- timeROC(T=tra.dat$OS.time,
                              delta=tra.dat$OS,
                              marker=risk.tra,
                              cause=1,
                              weighting="marginal",
                              times=c(365*years[1],365*years[2],365*years[3]),
                              iid=TRUE)
          tra.auc=c(tra.auc,max(ROC.DSST$AUC))
          trap=plotKMCox(data.frame(tra.dat$OS.time,tra.dat$OS,ifelse(risk.tra>=median(risk.tra),'H','L')))
          tra.p=c(tra.p,trap)
          risk.test=as.numeric(lan%*%as.matrix(t(test.dat[,final_gene])))
          # risk.test=zscore(risk.test)
          ROC.DSST1=timeROC(T=test.dat$OS.time,delta=test.dat$OS,marker=risk.test,cause=1,weighting="marginal",
                            times=c(365*years[1],365*years[2],365*years[3]),iid=TRUE)
          test.auc=c(test.auc,max(ROC.DSST1$AUC))
          testp=plotKMCox(data.frame(test.dat$OS.time,test.dat$OS,ifelse(risk.test>=median(risk.test),'H','L')))
          test.p=c(test.p,testp)
          risk.all=as.numeric(lan%*%as.matrix(t(tcga.dat[,final_gene])))
          # risk.all=zscore(risk.all)
          ROC.DSST3=timeROC(T=tcga.dat$OS.time,delta=tcga.dat$OS,marker=risk.all,cause=1,weighting="marginal",
                            times=c(365*years[1],365*years[2],365*years[3]),iid=TRUE)
          all.auc=c(all.auc,max(ROC.DSST3$AUC))
          allp=plotKMCox(data.frame(tcga.dat$OS.time,tcga.dat$OS,ifelse(risk.all>=median(risk.all),'H','L')))
          all.p=c(all.p,allp)
          # final_gene1=as.character(gene.type[final_gene,1])
          
          if (length(intersect(final_gene,colnames(geo1.dat)))==length(final_gene)) {
            risk.geo1=as.numeric(lan%*%as.matrix(t(geo1.dat[,intersect(final_gene,colnames(geo1.dat))])))
            # risk.geo1=zscore(risk.geo1)
            ROC.DSST2=timeROC(T=geo1.dat$OS.time,delta=geo1.dat$OS,marker=risk.geo1,cause=1,weighting="marginal",
                              times=c(365*years[1],365*years[2],365*years[3]),iid=TRUE)
            geo1.auc=c(geo1.auc,max(ROC.DSST2$AUC))
            geop=plotKMCox(data.frame(geo1.dat$OS.time,geo1.dat$OS,ifelse(risk.geo1>=median(risk.geo1),'H','L')))
            geo1.p=c(geo1.p,geop)
          }else{
            geo1.auc=c(geo1.auc,NA)
            geo1.p=c(geo1.p,NA)
          }
          
          if (length(intersect(final_gene,colnames(geo2.dat)))==length(final_gene)) {
            risk.geo2=as.numeric(lan%*%as.matrix(t(geo2.dat[,intersect(final_gene,colnames(geo2.dat))])))
            # risk.geo2=zscore(risk.geo2)
            ROC.DSST4=timeROC(T=geo2.dat$OS.time,delta=geo2.dat$OS,marker=risk.geo2,cause=1,weighting="marginal",
                              times=c(365*years[1],365*years[2],365*years[3]),iid=TRUE)
            geo2.auc=c(geo2.auc,max(ROC.DSST4$AUC))
            geop=plotKMCox(data.frame(geo2.dat$OS.time,geo2.dat$OS,ifelse(risk.geo2>=median(risk.geo2),'H','L')))
            geo2.p=c(geo2.p,geop)
          }else{
            geo2.auc=c(geo2.auc,NA)
            geo2.p=c(geo2.p,NA)
          }
          print(c('AUC',max(ROC.DSST$AUC),max(ROC.DSST1$AUC),max(ROC.DSST3$AUC)))
          print(c('P',trap,testp,allp))
          print(c('num',length(final_gene)))
          print("........")
          SampleingTime=c(SampleingTime,i)
          gene.list=c(gene.list,as.character(final_gene))
          SamplingSeed=c(SamplingSeed,rep(i,length(final_gene)))
        }
      }
    }
  }
  myd.clustering.df=data.frame("SamplingTime"=SampleingTime,
                               "TrainRiskP"=tra.p,"TrainRiskAUC"=tra.auc,
                               "TestRiskP"=test.p,"TestRiskAUC"=test.auc,
                               "TCGARiskP"=all.p,"TCGARiskAUC"=all.auc,
                               "GEO1RiskP"=geo1.p,"GEO1RiskAUC"=geo1.auc,
                               "GEO2RiskP"=geo2.p,"GEO2RiskAUC"=geo2.auc,
                               "GeneNums"=GeneNums)
  sample.index=as.data.frame(sample.index)
  colnames(sample.index)=paste("seed",SampleingTime,sep="")
  myd.clustering.genes=data.frame("SamplingSeed"=SamplingSeed,"Genes"=gene.list)
  return(list(myd.clustering.df,sample.index,myd.clustering.genes))
}






# load('model.Rdata')

table(tcga_model_data$OS.time > 365 * 3)



load('model.Rdata')
getwd()
{
  setwd('09_model/')
  myd_exp_resampling <- select_gene_median(dat1 = tcga_model_data, 
                                           a = 101,
                                           n = 1000,
                                           ratio = 0.5,
                                           cut_p = 0.01, 
                                           years = c(1, 3, 5))
  setwd('..')
}

myd_exp_resampling[[1]]
write.csv(myd_exp_resampling[[1]],
          file = '09_model/tcga-PFI-101-1000-0.5-0.01.csv',
          quote = F, row.names = F)




num = 234
load('model.Rdata')
myd_exp_resampling <- select_gene_median(dat1 = tcga_model_data, 
                                         a = num,
                                         n = num,
                                         ratio = 0.5,
                                         cut_p = 0.01, 
                                         years = c(1, 3, 5))
myd_exp_resampling[[1]]





# ####################
load('model.Rdata')
tra.samples <- read.delim(paste0('files/model_select/tra.dat_median_', num, '.txt'),
                          stringsAsFactors = F)[, 1]
tra.data <- tcga_model_data[tra.samples, ]
dim(tra.data)
test.samples <- read.delim(paste0('files/model_select/test.dat_median_', num, '.txt'),
                           stringsAsFactors = F)[, 1]
test.data <- tcga_model_data[test.samples, ]
dim(test.data)






coxFun <- function(dat){
  library(survival)
  colnames(dat)=c('time','status','gene')
  fmla=as.formula("Surv(time,status)~gene")
  cox=coxph(fmla,data=dat)
  p=summary(cox)[[7]][5]
  result=c(p,summary(cox)[[8]][1],summary(cox)[[8]][3],summary(cox)[[8]][4])
  return(result)
}
p.cutoff <- 0.01
# 单因素分析
tra.cox <- t(apply(tra.data[,3:c(ncol(tra.data))],2,function(x){
  vl=as.numeric(x)
  tm=tra.data$OS.time
  ev=tra.data$OS
  #ev=ifelse(ev=='Alive',0,1)
  dat=data.frame(tm,ev,vl)[which(tm > 0 & !is.na(vl)),]
  return(coxFun(dat))
}))
colnames(tra.cox)=c('p.value','HR','Low 95%CI','High 95%CI')
length(which(tra.cox[,1]<p.cutoff))
# write.table(tra.cox,"files/tcga_OS_train_single_HR_TPM_pvalue_1.txt",quote=F,sep="\t")
tra.cox <- na.omit(tra.cox)

dim(tra.cox)

tra.cox1 <- as.data.frame(tra.cox)
# tra.cox1 <- tra.cox1[tra.cox1$HR < 10, ]
# tra.cox1 <- tra.cox1[tra.cox1$HR > 1e-5, ]
dim(tra.cox1)
# pdf('09_model/sig.cox.pdf', width = 5, height = 5)
fig5a <- mg_volcano(log2(tra.cox1$HR),
                    tra.cox1$p.value,
                    cutFC = 0,
                    legend.pos = 'tl',
                    cutPvalue = p.cutoff,
                    xlab = 'Cox coefficient',
                    ylab = '-log10(P.value)')
fig5a
filter_genes <- rownames(tra.cox[tra.cox[,1]<p.cutoff, ])
length(filter_genes)
write.csv(tra.cox[tra.cox[,1]<0.01, ],
          file = '09_model/sig.genes.csv',
          quote = F)
tra.cox.filtered <- as.data.frame(tra.cox[tra.cox[,1]<p.cutoff, ])
table(tra.cox.filtered$HR > 1)
dir.create('results')
# write.csv(tra.cox[tra.cox[,1]<p.cutoff, ],
#           file = 'results/S2.csv',
#           quote = F)



library(glmnet)
# set.seed(2022)
set.seed(num)
fit1 <- glmnet(as.matrix(tra.data[,filter_genes])
            #,factor(samps)
            ,cbind(time=tra.data$OS.time,
                   status=tra.data$OS)
            ,family="cox"
            #,family="binomial"
            #,type.measure="deviance"
            ,nlambda=100
            , alpha=1)

cv.fit <-cv.glmnet(as.matrix(tra.data[,filter_genes])
                  #,factor(samps)
                  ,cbind(time=tra.data$OS.time,
                         status=tra.data$OS)
                  ,family="cox"
                  #,family="binomial"
                  #,type.measure="deviance"
                  ,nlambda=100
                  , alpha=1)
sig.coef <- coefficients(cv.fit,s=cv.fit$lambda.min)[which(coefficients(cv.fit,s=cv.fit$lambda.min)[,1]!=0),1]
length(sig.coef)
cv.fit$lambda.min

lasso.pdf <- mg_plot_lasso(fit1,
                           cv.fit,
                           # lambda = cv.fit$lambda.min,
                           show_text=T,
                           figLabels=c('B','C'))
lasso.pdf
ggsave(plot = lasso.pdf,
       filename = '09_model/lasso.pdf',
       width = 10, height = 5)
pdf('09_model/lasso1.pdf',width = 10,height = 5)
par(mfrow=c(1,2))
plot(fit1, xvar="lambda")
plot(cv.fit)
dev.off()

lasso_genes <- names(sig.coef)

# 基因的多因素
tcga_dat1 <- cbind(time=tra.data$OS.time,
                   status=tra.data$OS,
                   tra.data[, lasso_genes])

fmla <- as.formula(paste0("Surv(time, status) ~"
                          ,paste0(lasso_genes, collapse = '+')))


cox <- coxph(fmla, data =as.data.frame(tcga_dat1))
# lan <- coef(cox)
# round(lan, 3)
# genes <- names(cox$coefficients)
# # tra.cox[genes,]
# summary(cox)
# # pdf('12_model/multi_cox.pdf', width = 6, height = 6, onefile = F)
# survminer::ggforest(cox,data=tcga_dat1)
# dev.off()

cox1 <- step(cox, trace = 0)
lan <- coef(cox1)
round(lan, 3)
genes <- names(cox1$coefficients)
tra.cox[genes,]
summary(cox1)
pdf('09_model/multi_cox.pdf', width = 8, height = 5, onefile = F)
survminer::ggforest(cox1,data=tcga_dat1)
dev.off()


dim(tcga_dat1)
lst.modl=createCoxModel(as.matrix(tcga_dat1[, lasso_genes])
                        ,time = tcga_dat1$time
                        ,event = tcga_dat1$status,
                        isStep = T)
lst.modl$Genes
lst.modl$fmla

gene.coef=data.frame(Gene=lst.modl$Genes,Coef=lst.modl$Coef)
gene.coef$Type=ifelse(lst.modl$Coef>0,'Risk','Protective')
gene.coef$Type=factor(gene.coef$Type,levels=c('Risk','Protective'))
table(gene.coef$Type)
library(dplyr)
library(ggsci)
fig5d=gene.coef %>% 
  ggplot(aes(reorder(Gene, Coef), Coef)) +
  geom_col(aes(fill = Type)) +
  scale_fill_jco() +
  coord_flip() +
  labs(x = "") +
  labs(y = "LASSO Cox coefficient") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1),legend.position = 'top')
fig5d
ggsave(plot = fig5d,
       filename = '09_model/lasso_cox.pdf',
       width = 5, height = 5)


fig5ad <- cowplot::plot_grid(fig5a,
                             fig5d,
                             ncol = 2,
                             labels = c('A', 'D'))
fig5ad

fig5 <- cowplot::plot_grid(fig5ad,
                           lasso.pdf,
                           ncol = 1)
fig5
ggsave(plot = fig5,
       filename = '09_model/Fig7.pdf',
       width = 10, height = 10)

# 6、验证集与测试机的KM曲线 #####
library(survcomp)
library(survminer)
# 训练集
risk.tr <- as.numeric(lan%*%as.matrix(t(tra.data[,genes])))
tra.data$RS <- risk.tr
tra.data.point <- surv_cutpoint(tra.data, time = "OS.time", event = "OS",
                                variables = 'RS')
tra.cutoff <- as.numeric(summary(tra.data.point)[1])
tra.cutoff



tr.roc <- ggplotTimeROC(tra.data$OS.time / 365,
                        tra.data$OS,
                        risk.tr,
                        mks = c(1,3,5))
tr.km <- ggplotKMCox(data.frame(tra.data$OS.time / 365,
                                tra.data$OS,
                                ifelse(risk.tr>=tra.cutoff,'High','Low')),
                     title = 'Train',
                     labs = c('High','Low'))

tr.roc.km <- cowplot::plot_grid(tr.km,
                                tr.roc,
                                ncol = 2,
                                # align = 'v',
                                labels = LETTERS[1:2])
tr.roc.km
ggsave(plot = tr.roc.km,
       filename = '09_model/train.roc.km.pdf',
       width = 10, height = 5)



# 验证集
# te.genes <- intersect(genes, colnames(test.data))
# fmla.te <- as.formula(paste0("Surv(OS.time, OS) ~"
#                              ,paste0(te.genes,collapse = '+')))
# cox.te <- coxph(fmla.te, data =as.data.frame(test.data))
# te_lan <- coef(cox.te)
# risk.te=as.numeric(te_lan%*%as.matrix(t(test.data[,names(te_lan)])))


risk.te <- as.numeric(lan%*%as.matrix(t(test.data[,genes])))
test.data$RS <- risk.te
test.data.point <- surv_cutpoint(test.data, time = "OS.time", event = "OS",
                                 variables = 'RS')
tra.cutoff <- as.numeric(summary(test.data.point)[1])
tra.cutoff



te.roc <- ggplotTimeROC(test.data$OS.time / 365,
                        test.data$OS,
                        risk.te,
                        mks = c(1,3,5))
te.km <- ggplotKMCox(data.frame(test.data$OS.time / 365,
                                test.data$OS,
                                ifelse(risk.te>=tra.cutoff,'High','Low')),
                     title = 'Test',
                     labs = c('High','Low'))

te.roc.km <- cowplot::plot_grid(te.km,
                                te.roc,
                                ncol = 2,
                                # align = 'v',
                                labels = LETTERS[1:2])
te.roc.km
ggsave(plot = te.roc.km,
       filename = '09_model/test.roc.km.pdf',
       width = 10, height = 5)

# 全部 tcga
# tcga.genes <- intersect(genes, colnames(tcga_model_data))
# fmla.tcga <- as.formula(paste0("Surv(OS.time, OS) ~"
#                                ,paste0(tcga.genes,collapse = '+')))
# cox.tcga <- coxph(fmla.tcga, data =as.data.frame(tcga_model_data))
# tcga_lan <- coef(cox.tcga)
# 
# risk.tcga=as.numeric(tcga_lan%*%as.matrix(t(tcga_model_data[,names(tcga_lan)])))

library(survminer)
risk.tcga=as.numeric(lan%*%as.matrix(t(tcga_model_data[,names(lan)])))

tcga_model_data$RS <- risk.tcga

# tcga_model_data$RS <- predict(cox.tcga)
tcga.data.point <- surv_cutpoint(tcga_model_data, time = "OS.time", event = "OS",
                                 variables = 'RS')
tcga.cutoff <- as.numeric(summary(tcga.data.point)[1])
tcga.cutoff



tcga.roc <- ggplotTimeROC(tcga_model_data$OS.time / 365,
                          tcga_model_data$OS,
                          risk.tcga,
                          mks = c(1,3,5))
tcga.km <- ggplotKMCox(data.frame(tcga_model_data$OS.time / 365,
                                  tcga_model_data$OS,
                                  ifelse(risk.tcga>=tcga.cutoff,'High','Low')),
                       title = 'TCGA PFI',
                       labs = c('High','Low'))
tcga.km

tcga.roc.km <- cowplot::plot_grid(tcga.km,
                                  tcga.roc,
                                  ncol = 2,
                                  # align = 'v',
                                  labels = LETTERS[1:2])
tcga.roc.km
ggsave(plot = tcga.roc.km,
       filename = '09_model/tcga.roc.km.pdf',
       width = 10, height = 5)



# 风险评分在亚型中的比较 ###############
tcga.subtype.cli <- tcga.subtype.cli[rownames(tcga_model_data), ]
tcga.subtype.cli$RiskScore <- risk.tcga
tcga.subtype.cli$RiskType <- ifelse(risk.tcga >= tcga.cutoff, 'High', 'Low')
tcga_RiskScore_cluster_plot <- mg_violin(tcga.subtype.cli[, c("Cluster", "RiskScore")]
                                       ,melt = T
                                       ,ylab = 'RiskScore'
                                       , xlab = 'TCGA'
                                       # ,ylim = c(0,100)
                                       ,test_method = 'wilcox.test'
                                       ,cmp_test_method = 'wilcox.test'
                                       ,legend.pos = NULL
                                       ,jitter=F
                                       # ,group.col = pal_aaas()(9)[c(2,4,3)]
                                       ,show_compare = T)
tcga_RiskScore_cluster_plot

ggsave(plot = tcga_RiskScore_cluster_plot,
       filename = '09_model/tcga_RiskScore_cluster_plot.pdf',
       width = 5, height = 5)


# tcga.PFI.roc <- ggplotTimeROC(tcga.subtype.cli$OS.time / 365,
#                           tcga.subtype.cli$PFI,
#                           tcga.subtype.cli$RiskScore,
#                           mks = c(1,3,5))

tcga.OS.km <- ggplotKMCox(data.frame(tcga.subtype.cli$OS.time / 365,
                                      tcga.subtype.cli$OS,
                                      tcga.subtype.cli$RiskType),
                           title = 'TCGA OS',
                           labs = c('High','Low'))
tcga.OS.km

tcga.PFI.km <- ggplotKMCox(data.frame(tcga.subtype.cli$PFI.time / 365,
                                      tcga.subtype.cli$PFI,
                                      tcga.subtype.cli$RiskType),
                           title = 'TCGA PFI',
                           labs = c('High','Low'))
tcga.PFI.km

tcga.DFI.km <- ggplotKMCox(data.frame(tcga.subtype.cli$DFI.time / 365,
                                      tcga.subtype.cli$DFI,
                                      tcga.subtype.cli$RiskType),
                           title = 'TCGA DFI',
                           labs = c('High','Low'))
tcga.DFI.km

tcga.DSS.km <- ggplotKMCox(data.frame(tcga.subtype.cli$DSS.time / 365,
                                      tcga.subtype.cli$DSS,
                                      tcga.subtype.cli$RiskType),
                           title = 'TCGA DSS',
                           labs = c('High','Low'))
tcga.DSS.km



tcha.km.cluster <- cowplot::plot_grid(tr.km,
                                      te.km,
                                      tcga.km,
                                      tcga.OS.km,
                                      tcga.DFI.km,
                                      tcga.DSS.km,
                                      tcga_RiskScore_cluster_plot,
                                      ncol = 3,
                                      labels = LETTERS[1:7])
tcha.km.cluster
ggsave(plot = tcha.km.cluster,
       filename = '09_model/tcga.km.cluster.pdf',
       width = 15, height = 15)





# 泛癌分析 ###############
dir.create('10_pancan')
library(data.table)
library(stringr)

tcga_pancan_cli <- read.delim('00_origin_datas/TCGA/PMC6066282-TCGA-CDR-clinical.txt',
                              header = T, check.names = F, stringsAsFactors = F)
tcga_pancan_cli <- tcga_pancan_cli[!duplicated(tcga_pancan_cli$bcr_patient_barcode), ]
colnames(tcga_pancan_cli)
tcga_pancan_cli <- subset(tcga_pancan_cli,
                          !is.na(PFI.time) &
                            PFI.time > 0 & 
                            !is.na(PFI))
tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type == 'LAML'] <- paste0(tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type == 'LAML'], '-03')
tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type != 'LAML'] <- paste0(tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type != 'LAML'], '-01')
rownames(tcga_pancan_cli) <- tcga_pancan_cli$bcr_patient_barcode
colnames(tcga_pancan_cli)[1] <- 'Samples'
tcga_types <- as.character(unique(tcga_pancan_cli$type))


tcga_pancan_exp <- fread(paste0('00_origin_datas/TCGA/TCGA-', "ACC", '-Symbol.txt'),
                         data.table = F)
class(tcga_pancan_exp)
dim(tcga_pancan_exp)
tcga_pancan_exp[1:5, 1:5]

tcga_groups <- data.frame(row.names = colnames(tcga_pancan_exp)[2:ncol(tcga_pancan_exp)],
                          Samples = colnames(tcga_pancan_exp)[2:ncol(tcga_pancan_exp)],
                          Type = 'ACC',
                          stringsAsFactors = F)

for (ty in tcga_types[2:32]) {
  print(ty)
  tmp <- fread(paste0('00_origin_datas/TCGA/TCGA-', ty, '-Symbol.txt'),
               data.table = F)
  colnames(tmp)[1] <- 'Tags'
  
  tmp_groups <- data.frame(row.names = colnames(tmp)[2:ncol(tmp)],
                           Samples = colnames(tmp)[2:ncol(tmp)],
                           Type = ty,
                           stringsAsFactors = F)
  tcga_pancan_exp <- merge(tcga_pancan_exp, tmp, by = 'Tags', all= T)
  tcga_groups <- rbind(tcga_groups, tmp_groups)
}

tcga_groups$Type1 <- as.numeric(substr(tcga_groups$Samples, 14, 16))
table(tcga_groups$Type1)
tcga_groups <- subset(tcga_groups,
                      Type1 == 1 | 
                        Type1 == 3 |
                        Type1 == 11)
tcga_groups$Type1 <- ifelse(tcga_groups$Type1 < 10, 'Tumor', 'Adjacent')
tcga_groups$Sample <- substr(tcga_groups$Samples, 1, 12)


tcga_pancan_exp[1:5, 1:3]
rownames(tcga_pancan_exp) <- tcga_pancan_exp$Tags
tcga_pancan_exp <- tcga_pancan_exp[, -1]
boxplot(tcga_pancan_exp[, 1:10])
tcga_pancan_exp[1:5, 1:3]
tcga_pancan_exp_log2 <- log2(tcga_pancan_exp + 1)
tcga_pancan_exp_log2 <- tcga_pancan_exp_log2[rowSums(tcga_pancan_exp_log2) > 0, ]
dim(tcga_pancan_exp_log2)
tcga_pancan_exp_log2[1:5, 1:3]
boxplot(tcga_pancan_exp_log2[, 1:5])


intersect(genes, rownames(tcga_pancan_exp_log2))


table(tcga_pancan_cli$type)

tcga_types1 <- tcga_types[which(tcga_types != 'THCA')]

tcga_pancan_km_list <- list()
tcga_pancan_roc_list <- list()
for (ty in tcga_types1) {
  print(ty)
  if (ty == 'GBM' | ty == "SKCM") {
    mks <- c(1,2,3)
  } else {
    mks <- c(1,2,3)
  }
  tmp.cli <- tcga_pancan_cli[tcga_pancan_cli$type == ty, ]
  tmp.samples <- intersect(tmp.cli$Samples, colnames(tcga_pancan_exp_log2))
  tmp_model_dat <- cbind(tmp.cli[tmp.samples, c("PFI.time", "PFI")],
                         t(tcga_pancan_exp_log2[genes, tmp.samples]))
  
  fmla.tmp <- as.formula(paste0("Surv(PFI.time, PFI) ~"
                                ,paste0(genes,collapse = '+')))
  cox.tmp <- coxph(fmla.tmp, data =as.data.frame(tmp_model_dat))
  tmp_lan <- coef(cox.tmp)
  
  risk.tmp=as.numeric(tmp_lan%*%as.matrix(t(tmp_model_dat[,names(tmp_lan)])))
  
  library(survminer)
  # risk.tmp=as.numeric(lan%*%as.matrix(t(tmp_model_dat[,names(lan)])))
  
  tmp_model_dat$RiskScore <- risk.tmp
  tmp.data.point <- surv_cutpoint(tmp_model_dat, time = "PFI.time", event = "PFI",
                                  variables = 'RiskScore')
  tmp.cutoff <- as.numeric(summary(tmp.data.point)[1])
  tmp_model_dat$Group <- ifelse(risk.tmp >= tmp.cutoff, 'High', 'Low')
  write.csv(tmp_model_dat,
            file = paste0(ty, '_model_dat.csv'),
            quote = F)
  
  tmp.roc <- ggplotTimeROC(tmp_model_dat$PFI.time / 365,
                           tmp_model_dat$PFI,
                           risk.tmp,
                           mks = mks)
  tcga_pancan_roc_list[[ty]] <- tmp.roc
  tmp.km <- ggplotKMCox(data.frame(tmp_model_dat$PFI.time / 365,
                                   tmp_model_dat$PFI,
                                   ifelse(risk.tmp>=tmp.cutoff,'High','Low')),
                        title = ty,
                        labs = c('High','Low'))
  tcga_pancan_km_list[[ty]] <- tmp.km
}

tcga_pamcan_km_plot <- cowplot::plot_grid(plotlist = tcga_pancan_km_list,
                                          ncol = 6)
ggsave(plot = tcga_pamcan_km_plot,
       filename = '10_pancan/tcga_pamcan_km_plot.pdf',
       width = 30, height = 30)



# TIDE_immune ################
dir.create('11_immune_data')

# IMvigor210
dir.create('11_immune_data/IMvigor210')
IMvigor210_cli <- read.delim('00_origin_datas/IMvigor210/IMvigor210_cli.txt', header = T)
IMvigor210_cli <- IMvigor210_cli[, c("os", "censOS",
                                     "Best.Confirmed.Overall.Response",
                                     "IC.Level", "TC.Level", "Immune.phenotype",
                                     "FMOne.mutation.burden.per.MB",
                                     "Neoantigen.burden.per.MB")]
colnames(IMvigor210_cli) <- c('OS.time', 'OS', 'Response', 
                              "IC.Level", "TC.Level", "Immune.phenotype",
                              'TMB', 'NEO')
table(IMvigor210_cli$Response)
# IMvigor210_cli <- IMvigor210_cli[IMvigor210_cli$Response != 'NE', ]


IMvigor210_anno <- read.delim('00_origin_datas/IMvigor210/IMvigor210_entrez2gene.txt', header = T)
IMvigor210_exp <- read.delim('00_origin_datas/IMvigor210/IMvigor210_Counts2TPM.txt', 
                             header = T, row.names = 1)
com_enid <- intersect(IMvigor210_anno$entrez_id, rownames(IMvigor210_exp))

IMvigor210_exp <- IMvigor210_exp[com_enid, ]
IMvigor210_anno <- IMvigor210_anno[com_enid, ]


IMvigor210_exp <- log2(IMvigor210_exp + 1)
IMvigor210_exp$genes <- IMvigor210_anno$symbol
boxplot(IMvigor210_exp[, 1:5])

IMvigor210_exp <- aggregate(.~genes,
                            data = IMvigor210_exp, 
                            FUN = median)
rownames(IMvigor210_exp) <- IMvigor210_exp$genes
IMvigor210_exp <- IMvigor210_exp[-1, -1]
colnames(IMvigor210_exp)
boxplot(IMvigor210_exp[, 1:5])
IMvigor210_exp <- IMvigor210_exp[, -349]
# IMvigor210_exp <- log2(IMvigor210_exp + 1)

intersect(rownames(IMvigor210_cli), colnames(IMvigor210_exp))




IMvigor210_model_data <- cbind(IMvigor210_cli[, c("OS.time", "OS")],
                               t(IMvigor210_exp[, rownames(IMvigor210_cli)]))

IMvigor210.genes <- intersect(genes, colnames(IMvigor210_model_data))
IMvigor210.genes

IMvigor210_cli1 <- IMvigor210_cli[IMvigor210_cli$Response != 'NE', ]
colnames(IMvigor210_cli1)
IMvigor210_model_data <- IMvigor210_model_data[rownames(IMvigor210_cli1), ]
fmla.IMvigor210 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                     ,paste0(IMvigor210.genes,collapse = '+')))
cox.IMvigor210 <- coxph(fmla.IMvigor210, data =as.data.frame(IMvigor210_model_data))
IMvigor210_lan <- coef(cox.IMvigor210)

risk.IMvigor210=as.numeric(IMvigor210_lan%*%as.matrix(t(IMvigor210_model_data[,names(IMvigor210_lan)])))
# risk.IMvigor210=as.numeric(lan%*%as.matrix(t(IMvigor210_model_data[,names(lan)])))

IMvigor210_model_data$RS <- risk.IMvigor210
IMvigor210.data.point <- surv_cutpoint(IMvigor210_model_data, time = "OS.time", event = "OS",
                                       variables = 'RS')
IMvigor210.cutoff <- as.numeric(summary(IMvigor210.data.point)[1])
IMvigor210.cutoff


IMvigor210.roc <- ggplotTimeROC(IMvigor210_model_data$OS.time / 365,
                                IMvigor210_model_data$OS,
                                risk.IMvigor210,
                                mks = c(0.5,1,1.5))
IMvigor210.km <- ggplotKMCox(data.frame(IMvigor210_model_data$OS.time / 365,
                                        IMvigor210_model_data$OS,
                                        ifelse(risk.IMvigor210>=IMvigor210.cutoff,'High','Low')),
                             title = 'RiskScore',
                             labs = c('High','Low'))

IMvigor210.roc.km <- cowplot::plot_grid(IMvigor210.km, 
                                        IMvigor210.roc,
                                        ncol = 2,
                                        # align = 'v',
                                        labels = LETTERS[1])
IMvigor210.roc.km
# ggsave(plot = IMvigor210.roc.km,
#        filename = '11_immune_data/IMvigor210/IMvigor210.RiskScore.roc.km.pdf',
#        width = 10, height = 5)


IMvigor210_tide_dat <- t(scale(t(IMvigor210_exp[, rownames(IMvigor210_cli1)]),scale = F))
boxplot(IMvigor210_tide_dat[, 1:5])
dim(IMvigor210_tide_dat)
# dir.create('files/TIDE')
write.table(IMvigor210_tide_dat,
            file = '11_immune_data/IMvigor210/IMvigor210_tide_dat.txt',
            quote = F, sep = '\t')

IMvigor210_tide_dat <- read.csv('11_immune_data/IMvigor210/IMvigor210_tide_res.csv',
                                row.names = 1,stringsAsFactors = F)
IMvigor210_tide_dat <- cbind(IMvigor210_tide_dat[rownames(IMvigor210_model_data), ],
                             IMvigor210_model_data[, c("OS.time", "OS")])
IMvigor210_tide_dat$RiskScore <- risk.IMvigor210
IMvigor210_tide_dat$Response <- IMvigor210_cli1$Response
colnames(IMvigor210_tide_dat)
str(IMvigor210_tide_dat)

IMvigor210.tide.point <- surv_cutpoint(IMvigor210_tide_dat, time = "OS.time", event = "OS",
                                       variables = 'TIDE')
IMvigor210.tide.cutoff <- as.numeric(summary(IMvigor210.tide.point)[1])
IMvigor210.tide.cutoff
IMvigor210_tide_dat$TIDE.group <- ifelse(IMvigor210_tide_dat$TIDE >= IMvigor210.tide.cutoff, 'High', 'Low')




IMvigor210.tide.roc <- ggplotTimeROC(IMvigor210_tide_dat$OS.time / 365,
                                     IMvigor210_tide_dat$OS,
                                     IMvigor210_tide_dat$TIDE,
                                     mks = c(0.5,1,1.5))
IMvigor210.tide.km <- ggplotKMCox(data.frame(IMvigor210_tide_dat$OS.time / 365,
                                             IMvigor210_tide_dat$OS,
                                             IMvigor210_tide_dat$TIDE.group),
                                  title = 'TIDE',
                                  labs = c('High','Low'))

IMvigor210.tide.roc.km <- cowplot::plot_grid(IMvigor210.tide.km, 
                                             IMvigor210.tide.roc,
                                             ncol = 2,
                                             # align = 'v',
                                             labels = LETTERS[2])
IMvigor210.tide.roc.km
ggsave(plot = IMvigor210.tide.roc.km,
       filename = '11_immune_data/IMvigor210/IMvigor210.tide.roc.km.pdf',
       width = 10, height = 5)

IMvigor210_tide_dat <- crbind2DataFrame(IMvigor210_tide_dat)
head(IMvigor210_tide_dat[, c("Response", "TIDE", "RiskScore")])
table(IMvigor210_tide_dat$Response)
IMvigor210_tide_dat$Response[IMvigor210_tide_dat$Response == 'PD'] <- 0
IMvigor210_tide_dat$Response[IMvigor210_tide_dat$Response == 'SD'] <- 0
IMvigor210_tide_dat$Response[IMvigor210_tide_dat$Response == 'CR'] <- 1
IMvigor210_tide_dat$Response[IMvigor210_tide_dat$Response == 'PR'] <- 1

library(pROC)
library(dplyr)
IMvigor210.tide_roc_dat <- data.frame()
for (ty in c("TIDE", "RiskScore")) {
  print(ty)
  ty_dat <- IMvigor210_tide_dat[, c(ty, "Response")]
  # ty_dat <- ty_dat[ty_dat[, ty] == '0' | ty_dat[, ty] == '1', ]
  ty_dat <- crbind2DataFrame(ty_dat)
  
  ty_roc <-  roc(as.formula(paste0('Response ~ ', ty)), data = ty_dat, smooth = T)
  tmp_roc_dat <- data.frame(TP = ty_roc$sensitivities,
                            FP = 1 - ty_roc$specificities,
                            Methods =paste0(ty, ' AUC = ', round(ty_roc$auc, 2)))
  IMvigor210.tide_roc_dat <- rbind(IMvigor210.tide_roc_dat, tmp_roc_dat)
}
head(IMvigor210.tide_roc_dat)

# pdf('11_immune_data/IMvigor210/Response_roc.pdf', width = 5, height = 5)
ggplot(IMvigor210.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
  geom_line(aes(colour=Methods),lwd=0.75)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  xlab('False positive rate')+ylab('True positive rate')+ 
  scale_colour_manual(values = mycolor[1:7]) +
  labs(title = 'Response') +
  theme_bw() +
  theme(legend.position=c(1,0),legend.justification=c(1,0),
        legend.background = element_rect(fill = NA, colour = NA))
dev.off()

IMvigor210.Response_roc <- ggplot(IMvigor210.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
  geom_line(aes(colour=Methods),lwd=0.75)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  xlab('False positive rate')+ylab('True positive rate')+ 
  scale_colour_manual(values = mycolor[1:7]) +
  labs(title = 'Response') +
  theme_bw() +
  theme(legend.position=c(1,0),legend.justification=c(1,0),
        legend.background = element_rect(fill = NA, colour = NA))
IMvigor210.Response_roc

IMvigor210.plot <- cowplot::plot_grid(IMvigor210.km, 
                                      IMvigor210.roc,
                                      IMvigor210.tide.km, 
                                      IMvigor210.tide.roc,
                                      IMvigor210.Response_roc,
                                      ncol = 5,
                                      # align = 'v',
                                      labels = c('A', '', 'B', '', 'C'))
IMvigor210.plot




# GSE91061 
GSE91061_cli <- read.delim('00_origin_datas/GSE91061/GSE91061.cli.txt',
                           row.names = 1, check.names = F, stringsAsFactors = F)
GSE91061_cli$Title <- str_split_fixed(GSE91061_cli$Title, '_', 2)[, 1]
GSE91061_cli$Samples <- rownames(GSE91061_cli)
table(GSE91061_cli$`visit (pre or on treatment)`)
GSE91061_cli <- GSE91061_cli[GSE91061_cli$`visit (pre or on treatment)` == 'Pre', ]
rownames(GSE91061_cli) <- GSE91061_cli$Title
GSE91061_cli <- GSE91061_cli[!duplicated(GSE91061_cli$Title), ]

GSE91061_cli1 <- read.delim('00_origin_datas/GSE91061/GSE91061.cli1.txt',
                            row.names = 1, check.names = F)

GSE91061_com_samples <- intersect(GSE91061_cli$Title, rownames(GSE91061_cli1))

GSE91061_cli1 <- GSE91061_cli1[GSE91061_com_samples, ]
GSE91061_cli <- GSE91061_cli[GSE91061_com_samples, ]
rownames(GSE91061_cli1) <- GSE91061_cli$Samples
colnames(GSE91061_cli1)[3:4] <- c('OS', 'OS.time')
table(GSE91061_cli1$OS)
GSE91061_cli1$OS <- ifelse(GSE91061_cli1$OS == 'TRUE', 1, 0)
GSE91061_cli1$OS.time <- as.numeric(GSE91061_cli1$OS.time) * 7

GSE91061_exp <- read.delim('00_origin_datas/GSE91061/GSE91061.exp.txt',
                           row.names = 1, check.names = F, stringsAsFactors = F)
GSE91061_exp <- GSE91061_exp[, rownames(GSE91061_cli1)]
intersect(colnames(GSE91061_exp), rownames(GSE91061_cli1))

boxplot(GSE91061_exp[, 1:5])
GSE91061_exp <- log2(GSE91061_exp + 1)


GSE91061_model_data <- cbind(GSE91061_cli1[, c("OS.time", 'OS')],
                             t(GSE91061_exp[, rownames(GSE91061_cli1)]))
GSE91061.genes <- intersect(genes, colnames(GSE91061_model_data))


fmla.GSE91061 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                   ,paste0(GSE91061.genes,collapse = '+')))
cox.GSE91061 <- coxph(fmla.GSE91061, data =as.data.frame(GSE91061_model_data))
GSE91061_lan <- coef(cox.GSE91061)

risk.GSE91061=as.numeric(GSE91061_lan%*%as.matrix(t(GSE91061_model_data[,names(GSE91061_lan)])))

GSE91061_model_data$RS <- risk.GSE91061
GSE91061.data.point <- surv_cutpoint(GSE91061_model_data, time = "OS.time", event = "OS",
                                     variables = 'RS')
GSE91061.cutoff <- as.numeric(summary(GSE91061.data.point)[1])
GSE91061.cutoff



GSE91061.roc <- ggplotTimeROC(GSE91061_model_data$OS.time / 365,
                              GSE91061_model_data$OS,
                              risk.GSE91061,
                              mks = c(1,2,2.5))
GSE91061.km <- ggplotKMCox(data.frame(GSE91061_model_data$OS.time / 365,
                                      GSE91061_model_data$OS,
                                      ifelse(risk.GSE91061>=GSE91061.cutoff,'High','Low')),
                           title = 'RiskScore',
                           labs = c('High','Low'))

GSE91061.roc.km <- cowplot::plot_grid(GSE91061.km, 
                                      GSE91061.roc,
                                      ncol = 2,
                                      # align = 'v',
                                      labels = LETTERS[1:2])
GSE91061.roc.km
dir.create('11_immune_data/GSE91061')
# ggsave(plot = GSE91061.roc.km,
#        filename = '11_immune_data/GSE91061/GSE91061.RiskScore.roc.km.pdf',
#        width = 10, height = 5)


GSE91061_tide_dat <- t(scale(t(GSE91061_exp[, rownames(GSE91061_cli1)]),scale = F))
boxplot(GSE91061_tide_dat[, 1:5])
dim(GSE91061_tide_dat)
dir.create('11_immune_data/GSE91061')
write.table(GSE91061_tide_dat,
            file = '11_immune_data/GSE91061/GSE91061_tide_dat.txt',
            quote = F, sep = '\t')

GSE91061_tide_dat <- read.csv('11_immune_data/GSE91061/GSE91061_tide_res.csv',row.names = 1,stringsAsFactors = F)
GSE91061_tide_dat <- cbind(GSE91061_tide_dat[rownames(GSE91061_model_data), ],
                           GSE91061_model_data[, c("OS.time", "OS")])
GSE91061_tide_dat$RiskScore <- risk.GSE91061
GSE91061_tide_dat$Response <- GSE91061_cli1$Response
colnames(GSE91061_tide_dat)
str(GSE91061_tide_dat)

GSE91061.tide.point <- surv_cutpoint(GSE91061_tide_dat, time = "OS.time", event = "OS",
                                     variables = 'TIDE')
GSE91061.tide.cutoff <- as.numeric(summary(GSE91061.tide.point)[1])
GSE91061.tide.cutoff
GSE91061_tide_dat$TIDE.group <- ifelse(GSE91061_tide_dat$TIDE >= GSE91061.tide.cutoff, 'High', 'Low')




GSE91061.tide.roc <- ggplotTimeROC(GSE91061_tide_dat$OS.time / 365,
                                   GSE91061_tide_dat$OS,
                                   GSE91061_tide_dat$TIDE,
                                   mks = c(1,2,2.5))
GSE91061.tide.km <- ggplotKMCox(data.frame(GSE91061_tide_dat$OS.time / 365,
                                           GSE91061_tide_dat$OS,
                                           GSE91061_tide_dat$TIDE.group),
                                title = 'TIDE', 
                                labs = c('High','Low'))

GSE91061.tide.roc.km <- cowplot::plot_grid(GSE91061.tide.roc, 
                                           GSE91061.tide.km,
                                           ncol = 2,
                                           # align = 'v',
                                           labels = LETTERS[1:2])
GSE91061.tide.roc.km
# ggsave(plot = GSE91061.tide.roc.km,
#        filename = '11_immune_data/GSE91061/GSE91061.tide.roc.km.pdf',
#        width = 10, height = 5)


head(GSE91061_tide_dat[, c("Response", "TIDE", "RiskScore")])
GSE91061_tide_dat <- crbind2DataFrame(GSE91061_tide_dat)
table(GSE91061_tide_dat$Response)
GSE91061_tide_dat$Response[GSE91061_tide_dat$Response == 'PD'] <- 0
GSE91061_tide_dat$Response[GSE91061_tide_dat$Response == 'SD'] <- 0
GSE91061_tide_dat$Response[GSE91061_tide_dat$Response == 'CR'] <- 1
GSE91061_tide_dat$Response[GSE91061_tide_dat$Response == 'PR'] <- 1

library(pROC)
library(dplyr)
GSE91061.tide_roc_dat <- data.frame()
for (ty in c("TIDE", "RiskScore")) {
  print(ty)
  ty_dat <- GSE91061_tide_dat[, c(ty, "Response")]
  # ty_dat <- ty_dat[ty_dat[, ty] == '0' | ty_dat[, ty] == '1', ]
  ty_dat <- crbind2DataFrame(ty_dat)
  
  ty_roc <-  roc(as.formula(paste0('Response ~ ', ty)), data = ty_dat, smooth = T)
  tmp_roc_dat <- data.frame(TP = ty_roc$sensitivities,
                            FP = 1 - ty_roc$specificities,
                            Methods =paste0(ty, ' AUC = ', round(ty_roc$auc, 2)))
  GSE91061.tide_roc_dat <- rbind(GSE91061.tide_roc_dat, tmp_roc_dat)
}
head(GSE91061.tide_roc_dat)

# pdf('11_immune_data/GSE91061/Response_roc.pdf', width = 5, height = 5)
ggplot(GSE91061.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
  geom_line(aes(colour=Methods),lwd=0.75)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  xlab('False positive rate')+ylab('True positive rate')+ 
  scale_colour_manual(values = mycolor[1:7]) +
  labs(title = 'Response') +
  theme_bw() +
  theme(legend.position=c(1,0),legend.justification=c(1,0),
        legend.background = element_rect(fill = NA, colour = NA))
dev.off()

GSE91061.Response_roc <- ggplot(GSE91061.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
  geom_line(aes(colour=Methods),lwd=0.75)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  xlab('False positive rate')+ylab('True positive rate')+ 
  scale_colour_manual(values = mycolor[1:7]) +
  labs(title = 'Response') +
  theme_bw() +
  theme(legend.position=c(1,0),legend.justification=c(1,0),
        legend.background = element_rect(fill = NA, colour = NA))
GSE91061.Response_roc

GSE91061.plot <- cowplot::plot_grid(GSE91061.km, 
                                    GSE91061.roc,
                                    GSE91061.tide.km, 
                                    GSE91061.tide.roc,
                                    GSE91061.Response_roc,
                                    ncol = 5,
                                    # align = 'v',
                                    labels = c('D', '', 'E', '', 'F'))
GSE91061.plot



# # GSE78220
# GSE78220_cli <- getGEOSampleData('GSE78220')
# GSE78220_cli1 <- GSE78220_cli[, c("Acc", "Title", "anti-pd-1 response", 
#                                   "overall survival (days)", "vital status")]
# colnames(GSE78220_cli1) <- c(c("Samples", "Title", "Rresponse", 
#                                "OS.time", "OS"))
# GSE78220_cli1 <- na.omit(GSE78220_cli1)
# table(GSE78220_cli1$OS)
# GSE78220_cli1$OS <- ifelse(GSE78220_cli1$OS == 'Alive', 0, 1)
# rownames(GSE78220_cli1) <- GSE78220_cli1$Title
# 
# GSE78220_exp <- openxlsx::read.xlsx('00_origin_datas/GSE78220/GSE78220_PatientFPKM.xlsx',
#                                     sheet = 1)
# rownames(GSE78220_exp) <- GSE78220_exp$Gene
# GSE78220_exp <- GSE78220_exp[, -1]
# colnames(GSE78220_exp) <- str_split_fixed(colnames(GSE78220_exp), '\\.', 3)[, 1]
# boxplot(GSE78220_exp[, 1:5])
# 
# GSE78220_exp <- log2(GSE78220_exp + 1)
# boxplot(GSE78220_exp[, 1:5])
# 
# GSE78220_model_data <- cbind(GSE78220_cli1[, c("OS.time", 'OS')],
#                              t(GSE78220_exp[, rownames(GSE78220_cli1)]))
# GSE78220.genes <- intersect(genes, colnames(GSE78220_model_data))
# 
# 
# fmla.GSE78220 <- as.formula(paste0("Surv(OS.time, OS) ~"
#                                    ,paste0(GSE78220.genes,collapse = '+')))
# cox.GSE78220 <- coxph(fmla.GSE78220, data =as.data.frame(GSE78220_model_data))
# GSE78220_lan <- coef(cox.GSE78220)
# 
# risk.GSE78220=as.numeric(GSE78220_lan%*%as.matrix(t(GSE78220_model_data[,names(GSE78220_lan)])))
# 
# GSE78220_model_data$RS <- risk.GSE78220
# GSE78220.data.point <- surv_cutpoint(GSE78220_model_data, time = "OS.time", event = "OS",
#                                      variables = 'RS')
# GSE78220.cutoff <- as.numeric(summary(GSE78220.data.point)[1])
# GSE78220.cutoff
# 
# 
# GSE78220.roc <- ggplotTimeROC(GSE78220_model_data$OS.time / 365,
#                               GSE78220_model_data$OS,
#                               risk.GSE78220,
#                               mks = c(1,2,2.5))
# GSE78220.km <- ggplotKMCox(data.frame(GSE78220_model_data$OS.time / 365,
#                                       GSE78220_model_data$OS,
#                                       ifelse(risk.GSE78220>=GSE78220.cutoff,'High','Low')),
#                            title = 'RiskScore',
#                            labs = c('High','Low'))
# 
# GSE78220.roc.km <- cowplot::plot_grid(GSE78220.km, 
#                                       GSE78220.roc,
#                                       ncol = 2,
#                                       # align = 'v',
#                                       labels = LETTERS[1:2])
# GSE78220.roc.km
# dir.create('11_immune_data/GSE78220')
# # ggsave(plot = GSE78220.roc.km,
# #        filename = '11_immune_data/GSE78220/GSE78220.RiskScore.roc.km.pdf',
# #        width = 10, height = 5)
# 
# 
# GSE78220_tide_dat <- t(scale(t(GSE78220_exp[, rownames(GSE78220_cli1)]),scale = F))
# boxplot(GSE78220_tide_dat[, 1:5])
# dim(GSE78220_tide_dat)
# dir.create('11_immune_data/GSE78220')
# write.table(GSE78220_tide_dat,
#             file = '11_immune_data/GSE78220/GSE78220_tide_dat.txt',
#             quote = F, sep = '\t')
# 
# GSE78220_tide_dat <- read.csv('11_immune_data/GSE78220/GSE78220_tide_res.csv',row.names = 1,stringsAsFactors = F)
# GSE78220_tide_dat <- cbind(GSE78220_tide_dat[rownames(GSE78220_model_data), ],
#                            GSE78220_model_data[, c("OS.time", "OS")])
# GSE78220_tide_dat$RiskScore <- risk.GSE78220
# GSE78220_tide_dat$Response <- GSE78220_cli1$Rresponse
# colnames(GSE78220_tide_dat)
# str(GSE78220_tide_dat)
# 
# 
# 
# GSE78220.tide.point <- surv_cutpoint(GSE78220_tide_dat, time = "OS.time", event = "OS",
#                                      variables = 'TIDE')
# GSE78220.tide.cutoff <- as.numeric(summary(GSE78220.tide.point)[1])
# GSE78220.tide.cutoff
# GSE78220_tide_dat$TIDE.group <- ifelse(GSE78220_tide_dat$TIDE >= GSE78220.tide.cutoff, 'High', 'Low')
# 
# 
# 
# 
# GSE78220.tide.roc <- ggplotTimeROC(GSE78220_tide_dat$OS.time / 365,
#                                    GSE78220_tide_dat$OS,
#                                    GSE78220_tide_dat$TIDE,
#                                    mks = c(1,2,2.5))
# GSE78220.tide.km <- ggplotKMCox(data.frame(GSE78220_tide_dat$OS.time / 365,
#                                            GSE78220_tide_dat$OS,
#                                            GSE78220_tide_dat$TIDE.group),
#                                 title = 'TIDE',
#                                 labs = c('High','Low'))
# 
# 
# GSE78220.tide.roc.km <- cowplot::plot_grid(GSE78220.tide.roc, 
#                                            GSE78220.tide.km,
#                                            ncol = 2,
#                                            # align = 'v',
#                                            labels = LETTERS[1:2])
# GSE78220.tide.roc.km
# # ggsave(plot = GSE78220.tide.roc.km,
# #        filename = '11_immune_data/GSE78220/GSE78220.tide.roc.km.pdf',
# #        width = 10, height = 5)
# 
# colnames(GSE78220_tide_dat)
# head(GSE78220_tide_dat[, c("Response", "TIDE", "RiskScore")])
# table(GSE78220_tide_dat$Response)
# GSE78220_tide_dat$Response[GSE78220_tide_dat$Response == 'Progressive Disease'] <- 0
# # GSE78220_tide_dat$Response[GSE78220_tide_dat$Response == 'SD'] <- 0
# GSE78220_tide_dat$Response[GSE78220_tide_dat$Response == 'Complete Response'] <- 1
# GSE78220_tide_dat$Response[GSE78220_tide_dat$Response == 'Partial Response'] <- 1
# 
# 
# library(pROC)
# library(dplyr)
# GSE78220.tide_roc_dat <- data.frame()
# for (ty in c("TIDE", "RiskScore")) {
#   print(ty)
#   ty_dat <- GSE78220_tide_dat[, c(ty, "Response")]
#   # ty_dat <- ty_dat[ty_dat[, ty] == '0' | ty_dat[, ty] == '1', ]
#   ty_dat <- crbind2DataFrame(ty_dat)
#   
#   ty_roc <-  roc(as.formula(paste0('Response ~ ', ty)), data = ty_dat, smooth = T)
#   tmp_roc_dat <- data.frame(TP = ty_roc$sensitivities,
#                             FP = 1 - ty_roc$specificities,
#                             Methods =paste0(ty, ' AUC = ', round(ty_roc$auc, 2)))
#   GSE78220.tide_roc_dat <- rbind(GSE78220.tide_roc_dat, tmp_roc_dat)
# }
# head(GSE78220.tide_roc_dat)
# 
# # pdf('11_immune_data/GSE78220/Response_roc.pdf', width = 5, height = 5)
# ggplot(GSE78220.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
#   geom_line(aes(colour=Methods),lwd=0.75)+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
#   xlab('False positive rate')+ylab('True positive rate')+ 
#   scale_colour_manual(values = mycolor[1:7]) +
#   labs(title = 'Response') +
#   theme_bw() +
#   theme(legend.position=c(1,0),legend.justification=c(1,0),
#         legend.background = element_rect(fill = NA, colour = NA))
# dev.off()
# 
# GSE78220.Response_roc <- ggplot(GSE78220.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
#   geom_line(aes(colour=Methods),lwd=0.75)+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
#   xlab('False positive rate')+ylab('True positive rate')+ 
#   scale_colour_manual(values = mycolor[1:7]) +
#   labs(title = 'Response') +
#   theme_bw() +
#   theme(legend.position=c(1,0),legend.justification=c(1,0),
#         legend.background = element_rect(fill = NA, colour = NA))
# GSE78220.Response_roc
# 
# 
# 
# GSE78220.plot <- cowplot::plot_grid(GSE78220.km, 
#                                     GSE78220.roc,
#                                     GSE78220.tide.km, 
#                                     GSE78220.tide.roc,
#                                     GSE78220.Response_roc,
#                                     ncol = 5,
#                                     # align = 'v',
#                                     labels = c('G', '', 'H', '', 'I'))
# GSE78220.plot
# 
# 
# 
# # RiskScore.TIDE.plot <- cowplot::plot_grid(IMvigor210.plot,
# #                                     GSE91061.plot,
# #                                     GSE78220.plot,
# #                                     ncol = 1)
# # RiskScore.TIDE.plot
# # ggsave(plot = RiskScore.TIDE.plot,
# #        filename = '11_immune_data/RiskScore.TIDE.plot.pdf',
# #        width = 22.5, height = 13.5)



# # GSE135222
# GSE135222_cli <- getGEOSampleData('GSE135222')
# GSE135222_cli1 <- GSE135222_cli[, c("Acc", "Title", "pfs.time", "progression-free survival (pfs)")]
# colnames(GSE135222_cli1) <- c('Samples', "Title", 'OS.time', 'OS')
# 
# GSE135222_cli1 <- GSE135222_cli1[GSE135222_cli1$OS.time != 'NULL', ]
# table(GSE135222_cli1$OS)
# GSE135222_cli1$Title <- gsub(' ', '', GSE135222_cli1$Title)
# 
# GSE135222_cli2 <- openxlsx::read.xlsx('00_origin_datas/GSE135222/GSE135222_cli.xlsx', sheet = 1)
# GSE135222_cli2$Patient.ID <- paste0('NSCLC', GSE135222_cli2$Patient.ID)
# rownames(GSE135222_cli2) <- GSE135222_cli2$Patient.ID
# intersect(GSE135222_cli2$Patient.ID, GSE135222_cli1$Title)
# 
# colnames(GSE135222_cli1)
# colnames(GSE135222_cli2)
# 
# GSE135222_cli <- cbind(GSE135222_cli1,
#                        GSE135222_cli2[GSE135222_cli1$Title, ])
# 
# 
# 
# GSE135222_exp <- read.delim('00_origin_datas/GSE135222/GSE135222_GEO_RNA-seq_omicslab_exp.tsv',
#                             header = T, row.names = 1, check.names = F, stringsAsFactors = F)
# rownames(GSE135222_exp) <- str_split_fixed(rownames(GSE135222_exp), '\\.', 2)[, 1]
# 
# GSE135222_exp <- exp_probe2symbol_v2(GSE135222_exp,
#                                      gene_type[,c(1,2)])
# GSE135222_exp <- log2(GSE135222_exp + 1)
# 
# boxplot(GSE135222_exp[, 1:2])
# 
# colnames(GSE135222_exp)
# rownames(GSE135222_cli) <- GSE135222_cli$Title
# GSE135222_model_data <- cbind(GSE135222_cli[, c("OS.time", "OS")],
#                               t(GSE135222_exp[, GSE135222_cli$Title]))
# rownames(GSE135222_model_data)
# 
# GSE135222.genes <- intersect(genes, colnames(GSE135222_model_data))
# GSE135222.genes
# fmla.GSE135222 <- as.formula(paste0("Surv(OS.time, OS) ~"
#                                     ,paste0(GSE135222.genes,collapse = '+')))
# cox.GSE135222 <- coxph(fmla.GSE135222, data =as.data.frame(GSE135222_model_data))
# GSE135222_lan <- coef(cox.GSE135222)
# 
# risk.GSE135222=as.numeric(GSE135222_lan%*%as.matrix(t(GSE135222_model_data[,names(GSE135222_lan)])))
# 
# GSE135222_model_data$RS <- risk.GSE135222
# GSE135222.data.point <- surv_cutpoint(GSE135222_model_data, time = "OS.time", event = "OS",
#                                       variables = 'RS')
# GSE135222.cutoff <- as.numeric(summary(GSE135222.data.point)[1])
# GSE135222.cutoff
# 
# 
# 
# GSE135222.roc <- ggplotTimeROC(GSE135222_model_data$OS.time / 365,
#                                GSE135222_model_data$OS,
#                                risk.GSE135222,
#                                mks = c(0.5, 1))
# GSE135222.km <- ggplotKMCox(data.frame(GSE135222_model_data$OS.time / 365,
#                                        GSE135222_model_data$OS,
#                                        ifelse(risk.GSE135222>=GSE135222.cutoff,'High','Low')),
#                             title = 'RiskScore',
#                             labs = c('High','Low'))
# 
# GSE135222.roc.km <- cowplot::plot_grid(GSE135222.km,
#                                        GSE135222.roc,
#                                        ncol = 2,
#                                        # align = 'v',
#                                        labels = LETTERS[1:2])
# GSE135222.roc.km
# # ggsave(plot = GSE135222.roc.km,
# #        filename = '07_model/GSE135222.roc.km.pdf',
# #        width = 10, height = 5)
# #
# #
# #
# # GSE135222.roc.km
# dir.create('11_immune_data/GSE135222')
# # ggsave(plot = GSE135222.roc.km,
# #        filename = '11_immune_data/GSE135222/GSE135222.RiskScore.roc.km.pdf',
# #        width = 10, height = 5)
# 
# dim(GSE135222_exp)
# GSE135222_tide_dat <- t(scale(t(GSE135222_exp),scale = F))
# boxplot(GSE135222_tide_dat[, 1:5])
# dim(GSE135222_tide_dat)
# dir.create('11_immune_data/GSE135222')
# write.table(GSE135222_tide_dat,
#             file = '11_immune_data/GSE135222/GSE135222_tide_dat.txt',
#             quote = F, sep = '\t')
# 
# GSE135222_tide_dat <- read.csv('11_immune_data/GSE135222/GSE135222_tide_res.csv',row.names = 1,stringsAsFactors = F)
# GSE135222_tide_dat <- cbind(GSE135222_tide_dat[rownames(GSE135222_model_data), ],
#                             GSE135222_model_data[, c("OS.time", "OS")])
# GSE135222_tide_dat$RiskScore <- risk.GSE135222
# GSE135222_tide_dat$Response <- GSE135222_cli$benefit
# colnames(GSE135222_tide_dat)
# str(GSE135222_tide_dat)
# 
# GSE135222.tide.point <- surv_cutpoint(GSE135222_tide_dat, time = "OS.time", event = "OS",
#                                       variables = 'TIDE')
# GSE135222.tide.cutoff <- as.numeric(summary(GSE135222.tide.point)[1])
# GSE135222.tide.cutoff
# GSE135222_tide_dat$TIDE.group <- ifelse(GSE135222_tide_dat$TIDE >= GSE135222.tide.cutoff, 'High', 'Low')
# 
# 
# 
# 
# GSE135222.tide.roc <- ggplotTimeROC(GSE135222_tide_dat$OS.time / 365,
#                                     GSE135222_tide_dat$OS,
#                                     GSE135222_tide_dat$TIDE,
#                                     mks = c(0.5, 1))
# GSE135222.tide.km <- ggplotKMCox(data.frame(GSE135222_tide_dat$OS.time / 365,
#                                             GSE135222_tide_dat$OS,
#                                             GSE135222_tide_dat$TIDE.group),
#                                  title = 'TIDE',
#                                  labs = c('High','Low'))
# 
# 
# GSE135222.tide.roc.km <- cowplot::plot_grid(GSE135222.tide.roc,
#                                             GSE135222.tide.km,
#                                             ncol = 2,
#                                             # align = 'v',
#                                             labels = LETTERS[1:2])
# GSE135222.tide.roc.km
# # ggsave(plot = GSE135222.tide.roc.km,
# #        filename = '11_immune_data/GSE135222/GSE135222.tide.roc.km.pdf',
# #        width = 10, height = 5)
# 
# colnames(GSE135222_tide_dat)
# head(GSE135222_tide_dat[, c("Response", "TIDE", "RiskScore")])
# table(GSE135222_tide_dat$Response)
# GSE135222_tide_dat$Response[GSE135222_tide_dat$Response == 'N'] <- 0
# GSE135222_tide_dat$Response[GSE135222_tide_dat$Response == 'Y'] <- 1
# 
# 
# colnames(GSE135222_tide_dat)
# library(pROC)
# library(dplyr)
# GSE135222.tide_roc_dat <- data.frame()
# for (ty in c("TIDE", "RiskScore")) {
#   print(ty)
#   ty_dat <- GSE135222_tide_dat[, c(ty, "Response")]
#   # ty_dat <- ty_dat[ty_dat[, ty] == '0' | ty_dat[, ty] == '1', ]
#   ty_dat <- crbind2DataFrame(ty_dat)
# 
#   ty_roc <-  roc(as.formula(paste0('Response ~ ', ty)), data = ty_dat, smooth = T)
#   tmp_roc_dat <- data.frame(TP = ty_roc$sensitivities,
#                             FP = 1 - ty_roc$specificities,
#                             Methods =paste0(ty, ' AUC = ', round(ty_roc$auc, 2)))
#   GSE135222.tide_roc_dat <- rbind(GSE135222.tide_roc_dat, tmp_roc_dat)
# }
# head(GSE135222.tide_roc_dat)
# 
# # pdf('11_immune_data/GSE135222/Response_roc.pdf', width = 5, height = 5)
# ggplot(GSE135222.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
#   geom_line(aes(colour=Methods),lwd=0.75)+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
#   xlab('False positive rate')+ylab('True positive rate')+
#   scale_colour_manual(values = mycolor[1:7]) +
#   labs(title = 'Response') +
#   theme_bw() +
#   theme(legend.position=c(1,0),legend.justification=c(1,0),
#         legend.background = element_rect(fill = NA, colour = NA))
# dev.off()
# 
# GSE135222.Response_roc <- ggplot(GSE135222.tide_roc_dat, aes(x=FP,y=TP, fill=Methods))+
#   geom_line(aes(colour=Methods),lwd=0.75)+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
#   xlab('False positive rate')+ylab('True positive rate')+
#   scale_colour_manual(values = mycolor[1:7]) +
#   labs(title = 'Response') +
#   theme_bw() +
#   theme(legend.position=c(1,0),legend.justification=c(1,0),
#         legend.background = element_rect(fill = NA, colour = NA))
# GSE135222.Response_roc
# 
# 
# 
# GSE135222.plot <- cowplot::plot_grid(GSE135222.km,
#                                      GSE135222.roc,
#                                      GSE135222.tide.km,
#                                      GSE135222.tide.roc,
#                                      GSE135222.Response_roc,
#                                      ncol = 5,
#                                      # align = 'v',
#                                      labels = c('J', '', 'K', '', 'L'))
# GSE135222.plot



RiskScore.TIDE.plot <- cowplot::plot_grid(IMvigor210.plot,
                                        GSE91061.plot,
                                        ncol = 1)
RiskScore.TIDE.plot
ggsave(plot = RiskScore.TIDE.plot,
       filename = '11_immune_data/RiskScore.TIDE.plot.pdf',
       width = 22.5, height = 9) 


# 保存数据 ############
save.image('THCA_mRNAsi.Rdata')
load('THCA_mRNAsi.Rdata')



tcga.subtype.cli
write.csv(tcga.subtype.cli,
          file = 'results/tcga.cli.csv',
          row.names = F, quote = F)



