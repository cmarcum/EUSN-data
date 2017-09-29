library(sna)
library(network)

selc<-read.csv("eusn.manual.csv",colClasses="character",head=FALSE)
selc<-apply(selc,2,function(x) gsub("( )|[[:punct:]]","",x))
selc[which(selc=="")]<-NA
actors<-na.omit(unique(c(selc)))
opm<-matrix(0,nrow=length(actors),ncol=length(actors),dimnames=list(actors,actors))

for(i in 1:nrow(selc)){
  opm[na.omit(selc[i,]),na.omit(selc[i,])]<-1+opm[na.omit(selc[i,]),na.omit(selc[i,])]
}


actors.nomb<-enc2native(iconv(actors,"latin1","ASCII",sub="z"))
opm2<-opm
colnames(opm2)<-1:ncol(opm)
rownames(opm2)<-1:ncol(opm)
eusn<-as.network(opm2,directed=FALSE)
set.vertex.attribute(eusn,"appear",diag(opm))
set.edge.value(eusn,"freq",opm2)

#TheIsos<-(eusn%v%"vertex.names")[isolates(eusn)]
#delete.vertices(eusn,isolates(eusn))
	
eusn.comps<-list()
nc<-components(eusn)
eusn.cd<-component.dist(eusn)
eusn%v%"color"<-rainbow(nc)[eusn.cd$membership]

for(i in 1:nc){
 eusn.comps[[i]]<-get.inducedSubgraph(eusn,v=which(eusn.cd$membership%in%i))
}

eusn.ceusn<-eusn.comps[order(sapply(eusn.comps,network.size),decreasing=TRUE)]

#sapply(eusn.ceusn,network.size)
set.seed(29917)
png("EUSNNet.png",width=1200,height=1000,pointsize=12)
par(mar=c(1.75,1.75,1.75,1.75),xpd=TRUE)
gplot(eusn,vertex.col=eusn%v%"color",usearrows=FALSE,main="EUSN III 2017 Author Collaboration Network")
dev.off()

png("EUSNNetComp.png",width=1200,height=1000,pointsize=12)
par(mfrow=c(mfrow=c(2,2)))
gplot(eusn.ceusn[[1]],vertex.col=eusn.ceusn[[1]]%v%"color",usearrows=FALSE,displaylabels=TRUE,edge.col="gray",edge.lwd=eusn.ceusn[[1]]%e%"freq",label=actors[as.numeric(eusn.ceusn[[1]]%v%"vertex.names")],vertex.cex=sqrt(eusn.ceusn[[1]]%v%"appear"))
gplot(eusn.ceusn[[2]],vertex.col=eusn.ceusn[[2]]%v%"color",usearrows=FALSE,displaylabels=TRUE,edge.col="gray",edge.lwd=eusn.ceusn[[2]]%e%"freq",label=actors[as.numeric(eusn.ceusn[[2]]%v%"vertex.names")],vertex.cex=sqrt(eusn.ceusn[[2]]%v%"appear"))
gplot(eusn.ceusn[[3]],vertex.col=eusn.ceusn[[3]]%v%"color",usearrows=FALSE,displaylabels=TRUE,edge.col="gray",edge.lwd=eusn.ceusn[[3]]%e%"freq",label=actors[as.numeric(eusn.ceusn[[3]]%v%"vertex.names")],vertex.cex=sqrt(eusn.ceusn[[3]]%v%"appear"))
gplot(eusn.ceusn[[4]],vertex.col=eusn.ceusn[[4]]%v%"color",usearrows=FALSE,displaylabels=TRUE,edge.col="gray",edge.lwd=eusn.ceusn[[4]]%e%"freq",label=actors[as.numeric(eusn.ceusn[[4]]%v%"vertex.names")],vertex.cex=sqrt(eusn.ceusn[[4]]%v%"appear"))
dev.off()

#Now orgfile

melc<-read.csv("eusnorg.manual.csv",colClasses="character",head=FALSE)
melc<-apply(melc,2,function(x) gsub("( )|[[:punct:]]","",x))
melc[which(melc=="")]<-NA
actorsorg<-na.omit(unique(c(melc)))
gpm<-matrix(0,nrow=length(actorsorg),ncol=length(actorsorg),dimnames=list(actorsorg,actorsorg))

for(i in 1:nrow(melc)){
  gpm[na.omit(melc[i,]),na.omit(melc[i,])]<-1+gpm[na.omit(melc[i,]),na.omit(melc[i,])]
}


actorsorg.nomb<-enc2native(iconv(actorsorg,"latin1","ASCII",sub="z"))
gpm2<-gpm
colnames(gpm2)<-1:ncol(gpm)
rownames(gpm2)<-1:ncol(gpm)
eusnorg<-as.network(gpm2,directed=FALSE)
set.vertex.attribute(eusnorg,"appear",diag(gpm))
set.edge.value(eusnorg,"freq",gpm2)

set.seed(299)
png("EUSNOrg.png",1200,1000,pointsize=12)
par(mar=c(1.75,1.75,1.75,1.75),xpd=TRUE)
gplot(eusnorg,usearrows=FALSE,vertex.cex=diag(gpm)^(1/2.75),displaylabels=TRUE,edge.col="gray",edge.lwd=eusnorg%e%"freq",label=actorsorg[as.numeric(eusnorg%v%"vertex.names")],main="EUSN III Session Co-Organizer Network")
dev.off()


save.image("EUSN.Rdata")

