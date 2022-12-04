#Code to examine type II error in a stock decline
#CJ Brown 25th Feb 2011
rm(list=ls(all=T))
popmod<-function(N,H,r,k){max(c(N+(N*r*(1-(N/k)))-H,0))}

################################################
#parameters
maxsteps=50
r=0.2
maxi=500

kinit=20
k=rep(kinit,maxsteps)
initN=10
H1=(r*kinit)/4
F=r/2
testlen=5 #number of years for t-test window

#parameters to vary
osdreps=seq(0,0.5,0.1)
alphareps=c(0.01,0.05,0.1,0.2)
kmultreps=seq(0.3,1.9,by=0.1)

#Pre allocation
crashes<-array(rep(NA,length(alphareps)*length(kmultreps)*length(osdreps)),dim=c(length(alphareps),length(kmultreps),length(osdreps)))

for (iosd in 1:length(osdreps)){
	for (ikmult in 1:length(kmultreps)){
		for (ialpha in 1:length(alphareps)){
		osd=osdreps[iosd]
		k[10:maxsteps]=kinit*kmultreps[ikmult]
		alpha=alphareps[ialpha]
		crash=0
		
		for (ireps in 1:maxi){
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
	N[tstep]<-popmod(N[tstep-1],H[tstep-1],r,k[tstep])
	Nobs[tstep]<-N[tstep]*exp(obserr[tstep])
	###############################################
	if (N[tstep]==0) break
#t.test
	if (tstep>(2*testlen+1)){ 
		tgroup1<-rep(FALSE,maxsteps)
		tgroup2<-rep(FALSE,maxsteps) 
		tgroup1[(tstep-(2*testlen)):(tstep-testlen)]<-TRUE
		tgroup2[(tstep-testlen):(tstep)]<-TRUE
		if (sd(Nobs[tgroup2])>0.001){
		mod1<-t.test(Nobs[tgroup1],Nobs[tgroup2])
		pvals[tstep]<-mod1$p.value}
			}#end if

			###############################################
	#Management decisions
	if (pvals[tstep]<alpha) (H[tstep]<-F*Nobs[tstep]) else (H[tstep]<-H[tstep-1])
	#end if
    
} #end timestep for loop  
###############################################
#########Saving results
if (N[maxsteps]==0){crash=crash+1}

} #end iteration loop
#########Saving results
crashes[ialpha,ikmult,iosd]=crash
} #end alpha loop
} #end kmult loop
} #end osd loop
###############################################	
#Plot

par(mfrow=c(2,3))
for (i in 1:6){
filled.contour(x=alphareps,y=kmultreps,crashes[,,i])}
	

	
	