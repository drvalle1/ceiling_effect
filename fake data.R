rm(list=ls())
set.seed(1)

nobs=100
ceil1=7
floor1=1
mu=5
tau2=2
sig2=0.2

#generate pre-scores
pr=pr.true=rnorm(nobs,mean=mu,sd=sqrt(tau2))
cond=pr>ceil1;  sum(cond); pr[cond]=ceil1
cond=pr<floor1; sum(cond); pr[cond]=floor1

#generate xmat
uniq=matrix(c(0,0,1,0,0,1),3,2,byrow=T)
ind=sample(1:nrow(uniq),size=nobs,replace=T)
xmat=cbind(1,uniq[ind,])
colnames(xmat)=c('interc','trat1','trat2')

#generate xmat associated with pr
xmat.pr=cbind(pr,pr*xmat[,'trat1'],pr*xmat[,'trat2'])
xmat.pr.true=cbind(pr.true,pr.true*xmat[,'trat1'],pr.true*xmat[,'trat2'])
colnames(xmat.pr)=colnames(xmat.pr.true)=c('pr','pr_trat1','pr_trat2')

#parameters
betas=c(0,-0.1,0)
gammas=c(1,0,0.1)

#generate data 
media=xmat.pr%*%betas+xmat.pr.true%*%gammas
po=po.true=rnorm(nobs,mean=media,sd=sqrt(sig2))
cond=po>ceil1;  sum(cond); po[cond]=ceil1
cond=po<floor1; sum(cond); po[cond]=floor1

#final dataset
fim=data.frame(po=po,pr=pr,xmat[,-1])

#visualize these results
cores=rep('black',nrow(fim))
cond=fim$trat1==1; cores[cond]='red'
cond=fim$trat2==1; cores[cond]='blue'
rango=c(floor1,ceil1)
plot(fim$pr,fim$po,col=cores,xlim=rango,ylim=rango)
lines(rango,rango,col='grey',lwd=2)

#output results
setwd('U:\\GIT_models\\ceiling_effect')
write.csv(fim,'fake data.csv',row.names=F)