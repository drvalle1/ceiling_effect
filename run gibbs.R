rm(list=ls())
library('mvtnorm')
set.seed(1)

#get data
setwd('U:\\GIT_models\\ceiling_effect')
source('gibbs_ceiling_functions.R')
source('gibbs_ceiling_master.R')
dat=read.csv('fake data.csv',as.is=T)
covs.main=c('trat1','trat2','semester')
covs.interact=c('trat1','trat2')

#basic settings
ceil1=6
floor1=0

#priors
var.betas=rep(10,6)
ngibbs=10000

mod=tobit.pre.post(dat=dat,ceil1=ceil1,floor1=floor1,
                   covs.main=covs.main,covs.interact=covs.interact,
                   var.betas=var.betas,ngibbs=ngibbs)
