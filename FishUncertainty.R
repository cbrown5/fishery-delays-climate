#Code to examine type II error in a stock decline
#CJ Brown 25th Feb 2011
rm(list=ls(all=T))
popmod<-function(N,H,r,k){max(c(N+(N*r*(1-(N/k)))-H,0))}

################################################
#parameters
maxsteps=100
rinit=0.5
r=rep(rinit,maxsteps)
#r[50:maxsteps]=rinit*.8

k=20
initN=10
H1=(rinit*k)/4
F=rinit/2
osd=0.3
testlen=5 #number of years for t-test window
alpha=0.2

################################################
#Pre allocation/ set up
H=rep(0,maxsteps)
H[1:(testlen+1)]=H1
obserr=rnorm(maxsteps,mean=0,sd=osd)
N=rep(0,maxsteps)
Nobs=rep(0,maxsteps)
pvals=rep(1,maxsteps)
N[1]=initN
Nobs[1]=N[1]*exp(obserr[1])


################################################
#timesteps
for (tstep in 2:maxsteps){
	N[tstep]<-popmod(N[tstep-1],H[tstep-1],r[tstep],k)
	Nobs[tstep]<-N[tstep]*exp(obserr[tstep])
	###############################################
#t.test
	if (tstep>(2*testlen+1)){ 
		tgroup1<-rep(FALSE,maxsteps)
		tgroup2<-rep(FALSE,maxsteps) 
		tgroup1[(tstep-(2*testlen)):(tstep-testlen)]<-TRUE
		tgroup2[(tstep-testlen):(tstep)]<-TRUE
		mod1<-t.test(Nobs[tgroup1],Nobs[tgroup2])
		pvals[tstep]<-mod1$p.value
			}#end if

			###############################################
	#Management decisions
	if (pvals[tstep]<alpha) (H[tstep]<-F*Nobs[tstep]) else (H[tstep]<-H[tstep-1])
	#end if
    
} #end for loop

###############################################	
#Plot
par(mfrow=c(2,2))
plot(1:maxsteps,N,ylim=c(0,15),ylab='N')
lines(Nobs)
plot(1:maxsteps,H,ylim=c(0,5),type='l',ylab='H')
plot(1:maxsteps,pvals,ylim=c(0,1),type='l')
hist(pvals)

	

	
	