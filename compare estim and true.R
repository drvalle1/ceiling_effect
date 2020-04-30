store.betas=mod$betas
store.others=cbind(mod$sig2,mod$mu,mod$tau2)

round(cor(store.betas),2)

par(mfrow=c(3,3),mar=rep(1,4))
param.true=c(betas.true,gammas.true)
for (i in 1:ncol(store.betas)){
  plot(store.betas[,i],type='l')
  abline(h=param.true[i],col='red')
}

par(mfrow=c(2,2))
others.true=c(sig2.true,mu.true,tau2.true)
for (i in 1:ncol(store.others)){
  plot(store.others[,i],type='l')
  abline(h=others.true[i],col='red')
}