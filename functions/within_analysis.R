

#-----------------------------------------------------------------------------
#                         Within analysis without defaltion
#-----------------------------------------------------------------------------
#---------------- design matrix and projection matrix
dummay.matrix=model.matrix(~-1+groups)
proj.g=dummay.matrix %*% solve(t(dummay.matrix) %*% dummay.matrix) %*% t(dummay.matrix)

#---------------- data centered globaly
Data=scale(Data,center=TRUE,scale=FALSE)
#---------------- Between and within group dataset
data.B= proj.g %*%  Data
data.W= (diag(1,nrow(Data))-proj.g )%*%  Data
load=eigen(solve(var(Data)) %*% var(data.W))$vectors
scores=Data %*% load
t(scores) %*% scores


#---------------------------------------------------- withn analysis with deflation
library(MASS)
within_analysis=function(Data,groups,H){
  
res=list()
#---------------- design matrix and projection matrix
dummay.matrix=model.matrix(~-1+groups)
proj.g=dummay.matrix %*% solve(t(dummay.matrix) %*% dummay.matrix) %*% t(dummay.matrix)

#---------------- data centered globaly
Data=scale(Data,center=TRUE,scale=FALSE)
Data.h=Data
res$load=matrix(0,nrow=ncol(Data),ncol=H)
res$scores=matrix(0,nrow=nrow(Data),ncol=H)
res$var.x=rep(0,H)
res$var.xw=rep(0,H)
for(h in 1:H){
#---------------- Between and within group dataset
data.B= proj.g %*%  Data.h
data.W= (diag(1,nrow(Data.h))-proj.g )%*%  Data.h
res$load[,h]=matrix(eigen(ginv(var(Data.h)) %*% var(data.W))$vectors[,1],ncol=1)
res$scores[,h]=Data.h %*% res$load[,h]
res$var.x[h]= t(res$load[,h]) %*% t(Data.h) %*% Data.h %*% res$load[,h]
res$var.xw[h]= t(res$load[,h]) %*% t(data.W) %*% data.W %*% res$load[,h]
Data.h=Data.h- (res$scores[,h] %*% t(res$scores[,h]) %*% Data.h)/ as.vector(t(res$scores[,h]) %*% (res$scores[,h]))
}
Data.h=Data
data.B= proj.g %*%  Data.h
data.W= (diag(1,nrow(Data.h))-proj.g )%*%  Data.h
for(h in 1:H){
res$var.x[h]= t(res$load[,h]) %*% t(Data.h) %*% Data.h %*% res$load[,h]
res$var.xw[h]= t(res$load[,h]) %*% t(data.W) %*% data.W %*% res$load[,h]
}

axes=c(1,2)
  lab.x <- paste("Dim ", axes[1],sep = "")
  lab.y <- paste("Dim ", axes[2],sep = "")
rownames(res$load)=colnames(Data.h)
plot(res$scores, xlab = lab.x, ylab = lab.y,type="n",main="Within variation")  
text(res$scores,labels=groups2,cex=1,font.lab=2,col=c(as.factor((groups))))
rownames(res$scores)=groups
points.center=aggregate(res$scores[,1:2],list(groups),mean)[,-1]
text(points.center,labels=levels(groups),cex=1.9,col=c(as.factor(levels(groups))))
abline(h=c(0,0),v=c(0,0))

windows()
plot(res$load[,1:2], type="n",main="Within variation-loadings", xlab =lab.x,ylab=lab.y)
text(res$load[,1:2],labels=colnames(Data),cex=1.5,font.lab=2)
abline(h=c(0,0),v=c(0,0))

return(res)

}

#---------------------------------------------------------------------
within_PLSDA_analysis=function(Data,groups,H){
  
res=list()
#---------------- design matrix and projection matrix
dummay.matrix=model.matrix(~-1+groups)
proj.g=dummay.matrix %*% solve(t(dummay.matrix) %*% dummay.matrix) %*% t(dummay.matrix)

#---------------- data centered globaly
Data=scale(Data,center=TRUE,scale=FALSE)
#---------------- Between and within group dataset
data.B= proj.g %*%  Data
data.W= (diag(1,nrow(Data))-proj.g )%*%  Data
#---------------- PLS data.W on data
plswd=plsr(data.W~Data,comps=1:H)
res$load.pls=plswd$loading.weights[,1:H]
res$scores.pls=plswd$scores
res$var.x=rep(0,H)
res$var.xw=rep(0,H)
for(h in 1:H){
res$var.x[h]= t(res$load.pls[,h]) %*% t(Data) %*% Data %*% res$load.pls[,h]
res$var.xw[h]= t(res$load.pls[,h]) %*% t(data.W) %*% data.W %*% res$load.pls[,h]
}

axes=c(1,2)
  lab.x <- paste("Dim ", axes[1],sep = "")
  lab.y <- paste("Dim ", axes[2],sep = "")

plot(res$scores.pls, xlab = lab.x, ylab = lab.y,type="n",main="PLS-within")  
text(res$scores.pls,labels=groups2,cex=1,font.lab=2,col=c(as.factor((groups))))
rownames(res$scores)=groups
points.center=aggregate(res$scores[,1:2],list(groups),mean)[,-1]
text(points.center,labels=levels(groups),cex=1.9,col=c(as.factor(levels(groups))))
abline(h=c(0,0),v=c(0,0))


windows()
plot(res$load.pls[,1:2], type="n",main="PLS-within", xlab =lab.x,ylab=lab.y)
text(res$load.pls[,1:2],labels=colnames(Data),cex=1.5,font.lab=2)
abline(h=c(0,0),v=c(0,0))

return(res)
}





#---------------------------------------------------------------------
within_PLSDA_analysis22=function(Data,groups,H){
  
res=list()
#---------------- design matrix and projection matrix
dummay.matrix=model.matrix(~-1+groups)
proj.g=dummay.matrix %*% solve(t(dummay.matrix) %*% dummay.matrix) %*% t(dummay.matrix)

#---------------- data centered globaly
Data=scale(Data,center=TRUE,scale=FALSE)
Data.h=Data
res$load.pls=matrix(0,nrow=ncol(Data),ncol=H)
res$scores.pls=matrix(0,nrow=nrow(Data),ncol=H)

for(h in 1:H){
#---------------- Between and within group dataset
data.B= proj.g %*%  Data.h
data.W= (diag(1,nrow(Data.h))-proj.g )%*%  Data.h
#---------------- PLS data.W on data
plswd=plsr(data.W~Data.h)
res$load.pls[,h]=plswd$loading.weights[,1]
res$scores.pls[,h]=Data.h%*% res$load.pls[,h]
Data.h=Data.h- (res$scores[,h] %*% t(res$scores[,h]) %*% Data.h)/ as.vector(t(res$scores[,h]) %*% (res$scores[,h]))
}
axes=c(1,2)
  lab.x <- paste("Dim ", axes[1],sep = "")
  lab.y <- paste("Dim ", axes[2],sep = "")

plot(res$scores.pls, xlab = lab.x, ylab = lab.y,type="n",main="PLSDA-within")  
text(res$scores.pls,labels=groups,cex=1,font.lab=2,col=c(as.factor((groups))))
windows()
plot(res$load.pls[,1:2], type="n",main="PLSDA-within: loadings", xlab =lab.x,ylab=lab.y)
text(res$load.pls[,1:2],labels=colnames(Data),cex=1,font.lab=2)

}
