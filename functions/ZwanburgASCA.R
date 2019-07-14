GZ.ASCA=function(Data,Factor,interactions,SCale=FALSE){
  
  # Data is a matrix 
  # Factor is a data.frame : as.factor
  # interactions is a matrix
  # Data are assumed to be centered
  
  
N=nrow(Data)             	      #------- number of individuals 
n.factor=ncol(Factor)    		#------- number of factors 
n.interactions=nrow(interactions)   #------- number of interactions 

#------------ as.factor 
for(f in 1:n.factor){
  Factor=as.data.frame(Factor)
  Factor[,f]=as.factor(Factor[,f]) 
}


#------------ standardizing data: # Data are assumed to be centered
Data=scale(Data,center=TRUE,scale=SCale)
SSQ.Data=sum(Data * Data)

##-------------------------------------------------------------------------
##                                  Results
##-------------------------------------------------------------------------
res.ASCA=list()

res.ASCA=Data

res.ASCA$Factor=Factor
res.ASCA$name.Factor=paste("Factor",1:n.factor,sep="")
  
res.ASCA$factors.scores=list()
res.ASCA$factors.loadings=list()
res.ASCA$factors.singular=list()
res.ASCA$factors.projected=list()


res.ASCA$interactions.scores=list()
res.ASCA$interactions.loadings=list()
res.ASCA$interactions.singular=list()
res.ASCA$interactions.projected=list()

res.ASCA$Data.residual=0
res.ASCA$SSQ.residual=0

res.ASCA$mean.factor=list()
res.ASCA$SSQ.factor=list()

res.ASCA$mean.interactions=list()
res.ASCA$SSQ.interactions=list()

##-------------------------------------------------------------------------
##                            Decomposition of Data
##-------------------------------------------------------------------------
#---------------------- calculation of levels mean in each factor
mean.factor=list()

# GZ ----> Data.residuals should start with the data matrix
Data.residual=Data
proj=list()
for(f in 1:n.factor){
  dummay.matrix=model.matrix(~-1+Factor[,f])            # design matrix for this factor
  proj[[f]]=dummay.matrix %*% solve(t(dummay.matrix) %*% dummay.matrix) %*% t(dummay.matrix) # proj matrix to calculate mean in each level
  mean.factor[[f]]= proj[[f]] %*% Data
  res.ASCA$mean.factor[[f]]=mean.factor[[f]]
  res.ASCA$SSQ.factor[[f]]= sum(mean.factor[[f]] * mean.factor[[f]])
  Data.residual=Data.residual-mean.factor[[f]]
}


#---------------------- calculation of levels mean in each interaction
mean.interactions=list()
for(i in 1:n.interactions){
  interaction.design=interaction(Factor[,interactions[i,]],lex.order = TRUE)
  dummay.matrix=model.matrix(~-1+interaction.design)
  proj=dummay.matrix %*% solve(t(dummay.matrix) %*% dummay.matrix) %*% t(dummay.matrix)
 
  # ------>  GZ subtract factor means from interaction means
  mean.interactions[[i]]= proj %*% Data - res.ASCA$mean.factor[[interactions[i,1]]] - 
                                           res.ASCA$mean.factor[[interactions[i,2]]]
 
  
  res.ASCA$mean.interactions[[i]]= mean.interactions[[i]]
  res.ASCA$SSQ.interactions[[i]]= sum(mean.interactions[[i]] * mean.interactions[[i]])
  Data.residual=Data.residual-mean.interactions[[i]]  
}

res.ASCA$Data.residual=Data.residual
res.ASCA$SSQ.residual=sum(Data.residual * Data.residual)

##-------------------------------------------------------------------------
##                            PCA on each mean.factor
##-------------------------------------------------------------------------
#-------------------------- PCA on factors mean
for(f in 1:n.factor){
  res.pca=svd(mean.factor[[f]])                                  # SVD
  res.ASCA$factors.scores[[f]]=res.pca$u %*% diag(res.pca$d)     # scores
  rownames(res.ASCA$factors.scores[[f]])=Factor[,f]     
  
  res.ASCA$factors.loadings[[f]]=res.pca$v                       # loadings
  rownames(res.ASCA$factors.loadings[[f]])=colnames(Data)
  
  res.ASCA$factors.singular[[f]]=res.pca$d
  
  res.ASCA$factors.projected[[f]]=Data.residual %*% res.pca$v   # project residuals on loadings
  rownames(res.ASCA$factors.projected[[f]])=Factor[,f]
}

#---------------------------- PCA on interactions mean
for(i in 1:n.interactions){
  interaction.design=interaction(Factor[,interactions[i,]],lex.order = TRUE) # make interaction matrix
  res.pca=svd(mean.interactions[[i]])                                # SVD
  res.ASCA$interactions.scores[[i]]=res.pca$u %*% diag(res.pca$d)    # scores
  rownames(res.ASCA$interactions.scores[[i]])=interaction.design
  
  res.ASCA$interactions.loadings[[i]]=res.pca$v                      # loadings
  rownames(res.ASCA$interactions.loadings[[i]])=colnames(Data)
  
  res.ASCA$interactions.singular[[i]]=res.pca$d
  
  res.ASCA$interactions.projected[[i]]=Data.residual %*% res.pca$v   # project residuals on loadings
  rownames(res.ASCA$interactions.projected[[i]])=interaction.design
}

#----------------------- Plots
for(f in 1:n.factor){
  plot.ASCA(res.ASCA,f,axes=c(1,2))
   if(f<n.factor)windows()
}

windows()
  for(f in 1:n.interactions){
    plot.ASCA.interaction(res.ASCA,f,axes=c(1,2))
    if(f<n.interactions)windows()
  }

return(res.ASCA)

}





#----------------------------------------------
plot.ASCA.interaction=function(res.ASCA,f,axes=c(1,2)){
  xax=axes[1]
  yax=axes[2]
  

  #--------------
  lab.x <- paste("Dim ", axes[1],sep = "")
  lab.y <- paste("Dim ", axes[2],sep = "")
  #--------------
  groups=interaction(Factor[,1],Factor[,2],lex.order = TRUE) # make interaction matrix

  cooll=rainbow(length(levels(groups)))
  title1=paste("Loadings plot", "Interaction" , sep=" \n ")
  title2=paste("Scores plot", "Interaction" , sep=" \n ")
  
#-------------- variables plot: common loadings plot
  rownames(res.ASCA$interactions.loadings[[f]])=colnames(res.ASCA$Data)
  plot(res.ASCA$interactions.loadings[[f]][,1:2], type="n",main=title1, xlab =lab.x,ylab=lab.y)
  text(res.ASCA$interactions.loadings[[f]][,1:2],labels=colnames(res.ASCA$Data),cex=1.5,font.lab=2)
  abline(h=c(0,0),v=c(0,0))
#---------- individuals plot
  windows()

  scores=res.ASCA$interactions.scores[[f]][,c(xax,yax)]
  
  # GZ -----> Scores are the center (average) of the projected points
  project= scores + res.ASCA$interactions.projected[[f]][,c(xax,yax)]
  pervar=res.ASCA$interactions.singular[[f]]
  cp1 <- 100*round(pervar[xax]/sum(pervar), digits = 2)
  cp2 <- 100*round(pervar[yax]/sum(pervar), digits = 2)
  lab.x <- paste("Dim ", xax, " (", cp1, "%)", sep = "")
  lab.y <- paste("Dim ", yax, " (", cp2, "%)", sep = "")
  #xxlim=c(min(project[,1],scores[,1]),max(project[,1],scores[,1]))
  #yylim=c(min(project[,2],scores[,2]),max(project[,2],scores[,2]))
  xxlim=c(min(scores[,1],scores[,1]),max(scores[,1],scores[,1]))
  yylim=c(min(scores[,2],scores[,2]),max(scores[,2],scores[,2]))

  plot(scores[,xax],scores[,yax], xlab = lab.x, ylab = lab.y,type="n",xlim=xxlim,ylim=yylim,main=title2)  
  #text(scores,labels=groups,cex=1,font.lab=2,col=cooll[as.factor((groups))])
  rownames(scores)=groups
  points.center=aggregate(scores[,1:2],list(groups),mean)[,-1]
  text(points.center,labels=levels(groups),cex=1.9,col=cooll)
  abline(h=c(0,0),v=c(0,0))
} 








#----------------------------------------------
plot.ASCA=function(res.ASCA,f,axes=c(1,2)){
  xax=axes[1]
  yax=axes[2]
  
  cooll=palette(c("black", "red", "green3","blue","cyan","magenta", 
                  "darkgray", "darkgoldenrod", "darkgreen", "violet", 
                  "turquoise", "orange", "lightpink", "lavender", "yellow", 
                  "lightgreen", "lightgrey", "lightblue", "darkkhaki", 
                  "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", 
                  "darkorchid", "darkred", "darksalmon", "darkseagreen", 
                  "darkslateblue", "darkslategray", "darkslategrey", 
                  "darkturquoise", "darkviolet", "lightgray", "lightsalmon", 
                  "lightyellow", "maroon"))
  #--------------
  lab.x <- paste("Dim ", axes[1],sep = "")
  lab.y <- paste("Dim ", axes[2],sep = "")
  #--------------
  groups=res.ASCA$Factor[,f]
  cooll=rainbow(length(levels(groups)))
  title1=paste("Loadings plot", res.ASCA$name.Factor[f] , sep=" \n ")
  title2=paste("Scores plot", res.ASCA$name.Factor[f] , sep=" \n ")
  
  #-------------- variables plot: common loadings plot
  rownames(res.ASCA$factors.loadings[[f]])=colnames(res.ASCA$Data)
  plot(res.ASCA$factors.loadings[[f]][,1:2], type="n",main=title1, xlab =lab.x,ylab=lab.y)
  text(res.ASCA$factors.loadings[[f]][,1:2],labels=colnames(res.ASCA$Data),cex=1.5,font.lab=2)
  abline(h=c(0,0),v=c(0,0))
  #---------- individuals plot
  windows()
  scores=res.ASCA$factors.scores[[f]][,c(xax,yax)]
 
  # GZ -----> Scores are the center (average) of the projected points
  project= scores + res.ASCA$factors.projected[[f]][,c(xax,yax)]
  pervar=res.ASCA$factors.singular[[f]]
  cp1 <- 100*round(pervar[xax]/sum(pervar), digits = 2)
  cp2 <- 100*round(pervar[yax]/sum(pervar), digits = 2)
  lab.x <- paste("Dim ", xax, " (", cp1, "%)", sep = "")
  lab.y <- paste("Dim ", yax, " (", cp2, "%)", sep = "")
  xxlim=c(min(project[,1],scores[,1]),max(project[,1],scores[,1]))
  yylim=c(min(project[,2],scores[,2]),max(project[,2],scores[,2]))

  plot(scores[,xax],scores[,yax], xlab = lab.x, ylab = lab.y,type="n",xlim=xxlim,ylim=yylim,main=title2)  
  text(scores,labels=groups,cex=1.9,font.lab=2,col=cooll[(as.factor((groups)))])
  text(project,labels=groups,cex=.9,font.lab=2,col=cooll[(as.factor((groups)))])
  abline(h=c(0,0),v=c(0,0))
}
