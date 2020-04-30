rm(list=ls())
library('mvtnorm')
set.seed(1)

#get data
setwd('U:\\GIT_models\\ceiling_effect')
source('gibbs_ceiling_functions.R')
source('gibbs_ceiling_master.R')
dat=read.csv('fake data.csv',as.is=T)
covs.interact=covs.main=c('trat1','trat2')

#basic settings
ceil1=1
floor1=-3

#priors
var.betas=rep(10,6)
ngibbs=10000

mod=tobit.pre.post(dat=dat,ceil1=ceil1,floor1=floor1,covs.main=covs.main,
                   var.betas=var.betas,ngibbs=ngibbs)
