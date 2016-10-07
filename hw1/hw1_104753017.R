pam1<-read.table('pam1_new.txt')

dat <- as.numeric(pam1[1,])

for(a in 1:19){
  for(b in 1:20){
    dat[a*20+b] <- as.numeric(pam1[a+1,b])
  }
}


m = matrix(dat/100, nrow=20, ncol=20, byrow=T)


mtmp <- m

for(i in 1:249){
  mtmp <- m%*%mtmp
  mtmp <- scale(mtmp,center = FALSE, scale = TRUE)
  m250 <- mtmp 
}


tmp = 0
x = 0 
for(a in 1:20){
  for(b in 1:20){
    tmp <- tmp + m250[b,a]
  }
  x <- 100/tmp
  m250[,a] <- round(m250[,a]*x)
  tmp <- 0
}

write.table(m250,'pam250.txt')
