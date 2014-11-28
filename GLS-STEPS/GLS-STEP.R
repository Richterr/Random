prelim.install<-function(){
	id<-require(wmtsa)
    if(!id){install.packages("wmtsa");require(wmtsa)}
	id<-require(robust)
    if(!id){install.packages("robust");require(robust)}
	id<-require(XML)
    if(!id){install.packages("XML");require(XML)}
	id<-require(splus2R)
    if(!id){install.packages("splus2R");require(splus2R)}	
}




# packages required
check.packages<-function(){
	id<-require(wmtsa)
    if(!id){install.packages("wmtsa");require(wmtsa)}
	id<-require(robust)
    if(!id){install.packages("robust");require(robust)}
	id<-require(XML)
    if(!id){install.packages("XML");require(XML)}
	id<-require(splus2R)
    if(!id){install.packages("splus2R");require(splus2R)}
	#id<-require(msProcess)
	#if(!id){install.packages("msProcess");require(msProcess)}
	library(msProcess)    #  this is installed now from local source ahead of time
    id<-require(Matrix)
    if(!id){install.packages("Matrix");require(Matrix)}
    id<-require(caTools)
    if(!id){install.packages("caTools");require(caTools)}
    id<-require(MASS)
    if(!id){install.packages("MASS");require(MASS)}
    id<-require(corpcor)
    if(!id){install.packages("corpcor");require(corpcor)}
}

#-------------------------------------------- Getting the statistics eta / zstat -------------------------------------
get.eta<-function(x,win){
    # Getting  eta for a single window
    qleft<-runquantile(x,win,c(0.25,0.5,0.75),align="right")    # quantiles of left window of size win
    qright<-runquantile(x,win,c(0.25,0.5,0.75),align="left")    # quantiles of right window of size win
    qrange<-apply(runquantile(x,2*win+1,c(0,1)),1,diff)         # range of the window of size 2*win+1 centered at the point
    nr<-apply((qleft-qright)^2,1,mean)                          # mean squared difference of the quantiles
    eta<-nr/(qrange^2)                                          # the statistic eta
    return(eta)
}

get.zstat<-function(y,times,w){
    # Getting eta for multiple windows.
    # The function returns a matrix with each column corresponding to a window
    # get eta values for all w values
    mzstat<-sapply(w,function(k){get.eta(y,k)})
    colnames(mzstat)<-w
    return(list(y=y,w=w,times=times,mzstat=mzstat))
}
#-----------------------------------------------------------------------------------------------------------------------------

split.vector<-function(y,locs){
    # splitting a trace into segments at the step locations
    locs<-ceiling(locs)
    n<-length(y)
    if(any(locs==1)){
        locs<-locs[locs!=1]
    }
    if(any(locs==n)){
        locs<-locs[locs!=n]
    }
    nn<-diff(c(1,locs,n+1))
    id<-rep(1:(length(locs)+1),nn)
    id<-id[1:length(y)]
    yy<-split(y,id)
    return(yy)
}

# function to form the design matrix for regression
get.xmat<-function(n,locs){
    # n - number of data points
    # locs - location of steps
    locs<-ceiling(locs)
    if(any(locs==1)){
        locs<-locs[locs!=1]
    }
    if(any(locs==n)){
        locs<-locs[locs!=n]
    }
    nn<-diff(c(1,locs,n+1))
    id<-rep(1:(length(locs)+1),nn)
    id<-id[1:n]
    id2<-1:n
    id2<-split(id2,id)
    x<-matrix(0,nrow=n,ncol=length(id2))
    id3<-sapply(id2,max)
    for(i in 1:length(id2)){
        if(i==1){
            x[,i]<-1
        }else{
            x[-c(1:id3[i-1]),i]<-1
        }
    }
    return(x)
}

# function to perform efficient regression 
sp.lm<-function(x,y,fx,fy){
    xtx<-crossprod(fx)                          # matrix operation x'x
    xty<-crossprod(fx,as.numeric(fy))           # matrix operation x'y
    betas<-solve(xtx,xty)                       # matrix operation (x'x)^(-1)x'y
    fit<-as.numeric(fx%*%betas)              
    res<-fy-fit
    dof<-length(fy)-dim(betas)[1]
    sigma2<-var(res)
    betas.vcov<-sigma2*solve(xtx)
    #if(!is.positive.definite(betas.vcov)){
    #    betas.vcov<-make.postive.definite(betas.vcov)
    #}
    tstat<-abs(betas/sqrt(diag(betas.vcov)))
    pval<-2*pt(as.numeric(tstat),df=dof,lower.tail=FALSE)
    fit1<-as.numeric(x%*%betas)
    res1<-y-fit1
    res<-res+mean(res1)
    llhood<-sum(log(dnorm(res,sd=sqrt(sigma2))))
    aic<-(-2*llhood)+(2*(ncol(fx)))
    bic<-(-2*llhood)+(log(nrow(fx))*ncol(fx))
    return(list(betas=betas,betas.vcov=betas.vcov,tstat=tstat,fit=fit1,res=res,sigma2=sigma2,pval=pval,aic=aic,bic=bic))
}

robust.ar<-function(x,p){
    lma<-rlm(x~1)
    w<-lma$w
    mu<-lma$coef
    x<-x-mu
    z<-embed(x,p+1)
    z<-z[,-1]
    x<-x[-c(1:p)]
    w<-w[-c(1:p)]
    dat1<-cbind(x,z)
    colnames(dat1)<-c("x",paste("x",1:ncol(z),sep=""))
    data1<-as.data.frame(dat1)
    lm1<-lm(x~.-1,data=data1,weights=w)
    ar1<-lm1$coef
    x.mean<-mu
    return(list(ar=ar1,x.mean=x.mean,w=w))
}

ChPt<-function(chpt0,cutoff=0.9,gaussian1=FALSE){
    # cutoff is the upper percentile for choosing plausible step locations i.e. only top 10% highest values are choosen by default
    # chpt0 contains the output from the command get.zstat() which contains the zstats computed for various windows
    #-----------------------------------------------------------------------------------------------------------------------------
    # extract values from chpt0
    y<-chpt0$y
    w<-chpt0$w
    times<-chpt0$times
    mzstat<-chpt0$mzstat
    #-----------------------------------------------------------------------------------------------------------------------------
    # Getting the imitial set of step locations
    MU<-apply(mzstat,1,mean)
    snr<-quantile(MU,cutoff)
    if(length(y)>=1000){
        mpeak1<-msPeak(msSet(mzstat),FUN="search",span.supsmu="cv",snr=snr)
        mpeak2<-msAlign(mpeak1,snr=snr)
        peaks1<-mpeak2$peak.class[,1]
    }else{
        peaks1<-which(MU>snr)
    }
    # peaks1 contains the inital list of plausible step locations
    #-----------------------------------------------------------------------------------------------------------------------------
    split.y<-split.vector(y,peaks1)     # split y at the initial step locations
    xmat<-get.xmat(length(y),peaks1)    # design matrix corresponding to the initial step locations
    ind.mat<-xmat                       # ind.mat is matrix that speeds up matrix assignment
    ar.fit<-ar(diff(y),method="mle")    # Getting the order of the AR noise 
    arp<-ar.fit$order
    len<-cumsum(sapply(split.y,length))+1
    len<-c(1,len[-ncol(xmat)])
    len<-len+arp
    index<-1:length(y)
    for(i in 1:length(len)){
        ind.mat[index>len[i],i]<-2
    }
    fx<-xmat                            # fx mat will be the matrix corrsponding to the Cochrane-Orcutt filtered matrix
    # Set Storage
    RESULTS<-list()
    count<-1
    ind<-0
    if(gaussian1==FALSE){
        while(ind==0){
            lm2<-sp.lm(xmat,y,xmat,y)                         # simple linear regression
            lm2[[length(lm2)+1]]<-arp
            names(lm2)[length(lm2)]<-"AR-order"
            if(arp!=0){
                #ar.fit<-ar(lm2$res,order=arp,aic=FALSE,method="mle")     # Fit AR model to noise for pre-determined order above
                ar.fit<-ar(lm2$res,aic=FALSE,method="mle")     # Fit AR model to noise for pre-determined order above
                phi<-ar.fit$ar              
                mu<-ar.fit$x.mean
                # Begin Filtering: Transform xmat to fx and y to fy for regression acoounting correlation
                y2<-y-mu
                n<-length(y2)
                phi<-c(1,-phi)
                n2<-length(phi)-1
                y2<-c(rep(0,n2),y2)
                fy<-filter(y2,phi,method="convolution",sides=1)
                fy<-fy[-c(1:n2)]
                nc<-length(phi)
                sumcoefs<-cumsum(phi)
                sumcoefs<-sumcoefs-(sumcoefs*mu)
                fx[ind.mat==2]<-sumcoefs[nc]
                fx[ind.mat==1]<-rep(sumcoefs,ncol(fx))
                # End Filtering:  at this stage the fx and fy contain the filtered matrix and vector
                lm2<-sp.lm(xmat,y,fx,fy)                      # regression after taking into account the correlation
                lm2[[length(lm2)+1]]<-arp
                names(lm2)[length(lm2)]<-"AR-order"
                lm2[[length(lm2)+1]]<-phi
                names(lm2)[length(lm2)]<-"AR-Coefs"
            }
            RESULTS[[count]]<-lm2
            pval<-lm2$pval[-1]                              # Always ignore the intercept 
            id<-rev(order(pval))[1]                         # is is the column to be removed
            id<-id+1
            if(ncol(fx)!=1){
                # here we remove the corresponding columns in all the relevenat matrices
                xmat<-xmat[,-id,drop=FALSE]
                ind.mat<-ind.mat[,-id,drop=FALSE]
                fx<-fx[,-id,drop=FALSE]
                count<-count+1
            }else{
                ind<-1
            }
            gc()            # this is a command to free up available memory
        }
    }else{
        while(ind==0){
            lm2<-sp.lm(xmat,y,xmat,y)                         # simple linear regression
            RESULTS[[count]]<-lm2
            pval<-lm2$pval[-1]                              # Always ignore the intercept 
            id<-rev(order(pval))[1]                         # is is the column to be removed
            id<-id+1
            if(ncol(fx)!=1){
                # here we remove the corresponding columns in all the relevenat matrices
                xmat<-xmat[,-id,drop=FALSE]
                ind.mat<-ind.mat[,-id,drop=FALSE]
                fx<-fx[,-id,drop=FALSE]
                count<-count+1
            }else{
                ind<-1
            }
            gc()            # this is a command to free up available memory
        }
    }
    return(RESULTS)
}


# Get the step results
get.results<-function(rlist,type=c("bic","aic")){
    # type can be "bic", "aic" (in which case the corresponding model is chosen) or
    # integer between 1 and number of models specifying a particular model
    # rlist is the list of results from teh command chpt1()
    if(length(type)>1){
        type<-type[1]
    }
    if(type=="bic"){
        type<-order(sapply(rlist,function(z){z$bic}))[1]
    }else{
        if(type=="aic"){
            type<-order(sapply(rlist,function(z){z$aic}))[1]
        }
    }
    model<-rlist[[type]]
    betas<-model$betas
    ses<-sqrt(diag(model$betas.vcov))
    tstat<-model$tstat
    p.value<-model$pval
    coefs<-cbind(betas,ses,tstat,p.value)
    colnames(coefs)<-c("Step Size","Stdandard Error","t-statistic","P-Value")
    rownames(coefs)<-c("Base",paste("Step - ",1:(length(betas)-1),sep=""))
    coefs<-round(coefs,3)
    return(coefs)
}

# PLot the steps
plot.step<-function(wlist,rlist,type=c("bic","aic"),pdfname=NULL){
    # wlist is the results from the get.zstat() command
    # rlist is the list of results from teh command chpt1()
    # type can be "bic", "aic" (in which case the corresponding model is chosen) or
    # integer between 1 and number of models specifying a particular model
    # pdfname a file name for the pdf file in whih the plot is saved
    y<-wlist$y
    times<-wlist$times
    if(length(type)>1){
            type<-type[1]
    }
    if(type=="bic"){
        type<-order(sapply(rlist,function(z){z$bic}))[1]
    }else{
        if(type=="aic"){
            type<-order(sapply(rlist,function(z){z$aic}))[1]
        }
    }
    model<-rlist[[type]]
    fit<-model$fit
    locs<-which(diff(fit)!=0)+1
    fit<-cumsum(model$betas)
    ylims<-range(c(y,fit))
    stepfit<-stepfun(times[locs],fit)
    if(!is.null(pdfname)){
        pdf(file=pdfname,width=14,height=7)
    }
    xlims<-range(times)
    plot(times,y,type="l",col="lightgray",ylim=ylims,xlim=xlims,xlab="Time",ylab="Base Pair",main="Unwinding Steps")
    plot(stepfit,add=TRUE,do.points=FALSE,col=2,ylim=ylims,xlim=xlims,lwd=1.5,xlab="Time",ylab="Base Pair",main="Unwinding Steps")
    grid()
    if(!is.null(pdfname)){
        dev.off()
    }
}

get.model<-function(rlist,type=c("bic","aic")){
    if(length(type)>1){
        type<-type[1]
    }
    if(type=="bic"){
        type<-order(sapply(rlist,function(z){z$bic}))[1]
    }else{
        if(type=="aic"){
            type<-order(sapply(rlist,function(z){z$aic}))[1]
        }
    }
    model<-rlist[[type]]
    return(model)
}

get.locs<-function(wlist,rlist,type=c("bic","aic")){
    # wlist is the results from the get.zstat() command
    # rlist is the list of results from teh command chpt1()
    # type can be "bic", "aic" (in which case the corresponding model is chosen) or
    # integer between 1 and number of models specifying a particular model
    # pdfname a file name for the pdf file in whih the plot is saved
    y<-wlist$y
    times<-wlist$times
    if(length(type)>1){
            type<-type[1]
    }
    if(type=="bic"){
        type<-order(sapply(rlist,function(z){z$bic}))[1]
    }else{
        if(type=="aic"){
            type<-order(sapply(rlist,function(z){z$aic}))[1]
        }
    }
    model<-rlist[[type]]
    fit<-model$fit
    locs<-which(diff(fit)!=0)+1
    return(locs)
}

#-----------------------------------------------------------------------------------------------------------------------------
# An example of how the code is run
# Be careful. Note for a long series like 60000 data points it is likely to take about an hour to finish the computations
# input the data i.e.  y the unwinding trace and times is the time points
# w<-c(seq(10,90,by=10),seq(100,1000,by=25))              # give a suitable vector of window sizes
# chpt0<-get.zstat(y,times,w)
# chpt1<-ChPt(chpt0)
# to see the best BIC fit plot
# plot.step(chpt0,chpt1)
# locs<-get.locs(chpt0,chpt1)
# to see estimates from best BIC fit
# get.results(chpt1)
#-----------------------------------------------------------------------------------------------------------------------------
