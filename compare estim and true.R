par(mfrow=c(2,2))
for (i in 1:ncol(store.betas)){
  plot(store.betas[,i],type='l')
  abline(h=betas.true[i],col='red')
}

par(mfrow=c(2,2))
for (i in 1:ncol(store.gammas)){
  plot(store.gammas[,i],type='l')
  abline(h=gammas.true[i],col='red')
}

cor(cbind(store.betas,store.gammas))

par(mfrow=c(2,2))
others.true=c(sig2.true,mu.true,tau2.true)
for (i in 1:ncol(store.others)){
  plot(store.others[,i],type='l')
  abline(h=others.true[i],col='red')
}