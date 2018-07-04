######基因型对比######
#输出文件名F:/1.tif
#读入源文件"F:/test2.txt"
args<-commandArgs(T)
x<-args[3]
y<-args[4]
if(!is.integer(args[5])){
   m_color<-as.integer(args[5])
}
if(!is.integer(args[6])){
   n_color<-as.integer(args[6])
}
if(!is.integer(args[7])){
   o_color<-as.integer(args[7])
}
assign("colors", c(m_color,n_color,o_color))
assign("labs",c(args[8],args[9],args[10]))
assign("picturetype",args[11])
width<-as.integer(args[12])
height<-as.integer(args[13])
assign("legs",args[14])
if(legs=="T"){
   legs<-T
}else{
   legs<-F
}
#legs
ck1<-as.integer(x)
list<-strsplit(y,",")
if(is.na(ck1)){
   ck<-x
}else{
   ck<-ck1
}
len<-length(list[[1]])
#analyseType=args[5]
#sql=args[6]
#sql=gsub("%"," ",sql)
#library(RODBC)
plot.SQ<-function(x,ck=3,test=4,gb24,cl,labs,legs,picturetype,gb24flag){
#print(gb24$pos)
  if(picturetype=="tif"){
          tiff(file=paste(args[2],"_",test,".tif",sep=""),width=width,height=height)
	  geno<-x[,c(ck,test)];
	  geno[is.na(geno)]<-"NC";
	  geno<-geno[(geno[,1])!="NC" & geno[,2]!="NC",];
	  geno<-geno[geno[,1]!=geno[,2],]; #去重
	  if (nrow(geno)>0)  {
	    for (i in 1:nrow(geno)) {
	       if (geno[i,1]=="AB") {
		  geno[i,1]<-"H";
		  geno[i,2]<-"A";
	       }else {
		  geno[i,1]<-"A";
		  geno[i,2]<-ifelse(geno[i,2]=="AB","H","B")
	       }# end if
	    }# end for
	  }# end if
	  geno<-cbind(x[rownames(geno),1:2],geno);
	  path<-paste(args[2],"_",test,".csv",sep="")
	  ##write.table(geno, file = path, row.names = T, quote = F, sep="\t")
	  write.csv(geno, file = path, row.names = T)
	  ##source("D:/ftp/R/plot.gmap.R")
	  #x11(height=5,width=8.5)
	  par(mfrow=c(1,2),mar=c(1.0,4.0,3,1.2)+0.1)
	  if(is.na(ck1)){
	      main_name<-strsplit(ck,"_")
	      main_names<-main_name[[1]][2];
		  if(nrow(geno)==0){geno[1,1:4]=c(1,1,NA,NA)}
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }else{
	      main_name<-strsplit(colnames(x)[ck],"_")
	      main_names<-main_name[[1]][2];
		  if(nrow(geno)==0){geno[1,1:4]=c(1,1,NA,NA)}
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }
	  if(is.na(test1)){
	      main_name<-strsplit(test,"_")
	      main_names<-main_name[[1]][2];
		  if(nrow(geno)==0){geno[1,1:4]=c(1,1,NA,NA)}
          plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }else{
	      main_name<-strsplit(colnames(x)[test],"_")
	      main_names<-main_name[[1]][2];
		  if(nrow(geno)==0){geno[1,1:4]=c(1,1,NA,NA)}
          plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }	  
	  dev.off()
  }
  if(picturetype=="jpg"){
          jpeg(file=paste(args[2],"_",test,".jpg",sep=""),width=width,height=height)
	  geno<-x[,c(ck,test)];
	  geno[is.na(geno)]<-"NC";
	  geno<-geno[(geno[,1])!="NC" & geno[,2]!="NC",];
	  geno<-geno[geno[,1]!=geno[,2],]; #去重
	  if (nrow(geno)>0)  {
	    for (i in 1:nrow(geno)) {
	       if (geno[i,1]=="AB") {
		  geno[i,1]<-"H";
		  geno[i,2]<-"A";
	       }else {
		  geno[i,1]<-"A";
		  geno[i,2]<-ifelse(geno[i,2]=="AB","H","B")
	       }# end if
	    }# end for
	  }# end if
	  geno<-cbind(x[rownames(geno),1:2],geno);
	  path<-paste(args[2],"_",test,".csv",sep="")
	  ##write.table(geno, file = path, row.names = T, quote = F, sep="\t")
	  write.csv(geno, file = path, row.names = T)
	  ##source("D:/ftp/R/plot.gmap.R")
	  #x11(height=5,width=8.5)
	  par(mfrow=c(1,2),mar=c(1.0,4.0,3,1.2)+0.1)
	  if(is.na(ck1)){
	      main_name<-strsplit(ck,"_")
	      main_names<-main_name[[1]][2];
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }else{
	      main_name<-strsplit(colnames(x)[ck],"_")
	      main_names<-main_name[[1]][2];
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }
	  if(is.na(test1)){
	      main_name<-strsplit(test,"_")
	      main_names<-main_name[[1]][2];
              plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }else{
	      main_name<-strsplit(colnames(x)[test],"_")
	      main_names<-main_name[[1]][2];
              plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }	  
	  dev.off()
  }
  if(picturetype=="png"){
          png(file=paste(args[2],"_",test,".png",sep=""),width=width,height=height)
	  geno<-x[,c(ck,test)];
	  geno[is.na(geno)]<-"NC";
	  geno<-geno[(geno[,1])!="NC" & geno[,2]!="NC",];
	  geno<-geno[geno[,1]!=geno[,2],]; #去重
	  if (nrow(geno)>0)  {
	    for (i in 1:nrow(geno)) {
	       if (geno[i,1]=="AB") {
		  geno[i,1]<-"H";
		  geno[i,2]<-"A";
	       }else {
		  geno[i,1]<-"A";
		  geno[i,2]<-ifelse(geno[i,2]=="AB","H","B")
	       }# end if
	    }# end for
	  }# end if
	  geno<-cbind(x[rownames(geno),1:2],geno);
	  path<-paste(args[2],"_",test,".csv",sep="")
	  ##write.table(geno, file = path, row.names = T, quote = F, sep="\t")
	  write.csv(geno, file = path, row.names = T)
	  ##source("D:/ftp/R/plot.gmap.R")
	  #x11(height=5,width=8.5)
	  par(mfrow=c(1,2),mar=c(1.0,4.0,3,1.2)+0.1)
	  if(is.na(ck1)){
	      main_name<-strsplit(ck,"_")
	      main_names<-main_name[[1]][2];
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }else{
	      main_name<-strsplit(colnames(x)[ck],"_")
	      main_names<-main_name[[1]][2];
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }
	  if(is.na(test1)){
	      main_name<-strsplit(test,"_")
	      main_names<-main_name[[1]][2];
              plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }else{
	      main_name<-strsplit(colnames(x)[test],"_")
	      main_names<-main_name[[1]][2];
              plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }	  
	  dev.off()
  }
  if(picturetype=="pdf"){
          pdf(file=paste(args[2],"_",test,".pdf",sep=""),family="GB1")
	  geno<-x[,c(ck,test)];
	  geno[is.na(geno)]<-"NC";
	  geno<-geno[(geno[,1])!="NC" & geno[,2]!="NC",];
	  geno<-geno[geno[,1]!=geno[,2],]; #去重
	  if (nrow(geno)>0)  {
	    for (i in 1:nrow(geno)) {
	       if (geno[i,1]=="AB") {
		  geno[i,1]<-"H";
		  geno[i,2]<-"A";
	       }else {
		  geno[i,1]<-"A";
		  geno[i,2]<-ifelse(geno[i,2]=="AB","H","B")
	       }# end if
	    }# end for
	  }# end if
	  geno<-cbind(x[rownames(geno),1:2],geno);
	  path<-paste(args[2],"_",test,".csv",sep="")
	  ##write.table(geno, file = path, row.names = T, quote = F, sep="\t")
	  write.csv(geno, file = path, row.names = T)
	  ##source("D:/ftp/R/plot.gmap.R")
	  #x11(height=5,width=8.5)
	  par(mfrow=c(1,2),mar=c(1.0,4.0,3,1.2)+0.1,family="serif")
	  if(is.na(ck1)){
	      main_name<-strsplit(ck,"_")
	      main_names<-main_name[[1]][2];
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }else{
	      main_name<-strsplit(colnames(x)[ck],"_")
	      main_names<-main_name[[1]][2];
	      plot.gmap(geno[,1:3],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag);
	  }
	  if(is.na(test1)){
	      main_name<-strsplit(test,"_")
	      main_names<-main_name[[1]][2];
              plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }else{
	      main_name<-strsplit(colnames(x)[test],"_")
	      main_names<-main_name[[1]][2];
              plot.gmap(geno[,c(1,2,4)],cl=cl,main=main_names,gb24=gb24,lab=labs,legend=legs,gbflag=gb24flag)
          }	  
	  dev.off()
  }
}
#############################
##plot.gmap
  plot.gmap<-function(x,gene,legend,genotypes=c("A","B","H"),na=c("-","NC","NA"),na.color=NA,
                     cl=c(NA,2,4),main="Genotyping map",lab,cen=F,gb24,gbflag) {
   chr.len<-c(43.268879,35.930381,36.406689,35.278225,29.894789,31.246789,
              29.696629,28.439308,23.011239,23.134759,28.512666,27.497214)+0.1
   centro<-c(16.8,13.6,19.4,9.7,12.5,15.3,12.1,12.9,2.8,8.1,12,11.9);
   chrpos<-1:12
#### map
   x<-as.data.frame(x,stringsAsFactors =F);
   x[x==0]<-NA;
   x<-na.omit(x);
   map<-x[,2];
   if (max(map,na.rm=T)>1e6) map<-map/1e6
   map <- split(map,x[,1]);
   if (ncol(x)==2) x[,3]<-rep("-",nrow(x))
   geno<-as.character(x[,3]);
   geno[is.na(geno)]<-"-";
   geno[geno=="-"]<-na.color;
   geno[geno=="NC"]<-na.color;
   geno[geno==genotypes[1]]<-cl[1];
   geno[geno==genotypes[2]]<-cl[2];
   geno[geno==genotypes[3]]<-cl[3];
   geno<-as.numeric(geno);
   geno<-split(geno,x[,1]);
   chr<-as.numeric(names(map))
#### plot axis
   thelim <- range(chrpos)+c(-0.1,0.2)
   plot(0, 0, type = "n", xlim = thelim,ylim = rev(c(-1,44)), las=1,bty="n",
         yaxt="n",xaxt="n", xlab = NA, ylab ="Location (Mb)" ,main=main)
   text(x=chrpos,y=-2,labels=chrpos)
   axis(2,seq(0,45,5),las=1)
#### plot map
   for (i in chrpos) {
        rect(chrpos[i]-0.2,0,chrpos[i]+0.2,chr.len[i],border="green")     # chromosmes
        if (cen) polygon(i+c(0.3,0,0.3)+0.2,centro[i]+c(-0.4,0,+0.4),col="yellow",border=1)  # centromeres 
   }  
   for (i in chr) {   
        segments(chrpos[i]-0.18,map[[match(i,chr)]],chrpos[i]+0.18,map[[match(i,chr)]],col=geno[[match(i,chr)]],lwd=0.5)  
        rect(chrpos[i]-0.2,0,chrpos[i]+0.2,chr.len[i],border="green")     # chromosmes
   } 
   if (gbflag) {
     if (max(gb24$pos,na.rm=T)>1e6) gb24$pos<-gb24$pos/1e6
     for (i in 1:nrow(gb24)) {
         polygon(gb24[i,1]+c(0.3,0,0.3)+0.2,gb24[i,2]+c(-0.4,0,+0.4),col=gb24[i,3]*2,border=1)
     }
   }
        
#### gene
   if (!missing(gene)){
      for(i in 1:(length(gene)/2) ) {
        points(gene[i*2-1],gene[i*2],pch=16,col=2)
      }
   }     
#### legend
 if (legend){
   if (is.na(cl[1])) cl[1]<-8
   legend(9,33,legend=lab,lty=1,col=cl,xpd=T)
   # polygon(9.6+c(0.3,0,0.3),43+c(-0.5,0,+0.5),col="yellow",border=1,xpd=T)
 }
}
#################
if(args[15]=="F"){
   gb24flag<-F
   gb<-F
}else{
   gb24flag<-T
   gb<-read.csv(args[15],head=T,row.names=1)
}

x<-read.table(args[1],head=T,as.is=T,row.names=1)
checkchr<-is.numeric(x[,2])
checkchr2<-paste("CHR:",checkchr,sep="")
checkchr2
for(i in 1:len){
      test1=as.integer(list[[1]][i])
      if(is.na(test1)){
         test<-list[[1]][i]
      }else{
         test<-test1
      }
      plot.SQ(x,ck,test,gb24=gb,cl=colors,labs,legs,picturetype,gb24flag)
}

