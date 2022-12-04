#Code to examine type II error in a stock decline
#CJ Brown 25th Feb 2011

#library(nlme)
rm(list=ls(all=T))
popmod<-function(N,H,r,k){max(c(N+(N*r*(1-(N/k)))-H,0))}

################################################
#parameters
maxsteps=30
maxi=1
rinit=0.5
rchange=0.01
r=rep(rinit,maxsteps)
RSstart=10
r[RSstart:maxsteps]=rinit-((0:(maxsteps-RSstart))*rchange)

k=20
initN=10
H=(rinit*k)/4
F=rinit/2
osd=0
testlen=5 #number of years for t-test window
pval<-rep(NA,maxi)

################################################
#simulation
	for (randi in 1:maxi){
################################################
#Pre allocation/ set up
obserr=rnorm(maxsteps,mean=0,sd=osd)
N=rep(0,maxsteps)
Nobs=rep(0,maxsteps)
N[1]=initN
Nobs[1]=N[1]*exp(obserr[1])
timesteps=1:maxsteps
################################################
#timesteps
for (tstep in 2:maxsteps){
	N[tstep]<-popmod(N[tstep-1],H,r[tstep],k)
	Nobs[tstep]<-N[tstep]*exp(obserr[tstep])
	###############################################
} #end for loop
	###############################################
#test for a change
mod1<-lm(Nobs[1:tstep]~timesteps[1:tstep])
#mod1<-t.test(Nobs[1:RSstart],Nobs[(RSstart+1):maxsteps])
#pval[randi]<-mod1$p.value
}# end randomisation loop

###############################################	
#Plot beta vs. alpha
alpha=seq(0,1,0.01)
beta=rep(NA,length(alpha))
for (alphai in 1:length(alpha)){
	beta[alphai]=sum(pval>alpha[alphai])/maxi}
	
plot(alpha,beta,type='l')
	
	