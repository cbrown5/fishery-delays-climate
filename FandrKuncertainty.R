#CJ Brown 3 mar 2011

#What if population is declining but you are not sure if it is because F is too high or if r is declining (or K).

rm(list=ls(all=T))
popmod<-function(N,H,r,k){max(c(N+(N*r*(1-(N/k)))-H,0))}

################################################
#parameters
maxsteps=50
rinit=0.2
maxi=500
k=20
threshex=0.05*k
r=rep(rinit,maxsteps)
r[10:maxsteps]=rinit*0.5
initN=k/2
H1=(r*kinit)/4
F=r/2
testlen=5 #number of years for t-test window

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
		if (N[tstep]<threshex) {break}
	###############################################
	#Management decisions
    
    
} #end timestep for loop  