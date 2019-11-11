prawd=function(n){
  p=1
  for (i in seq_along(1:n)-1) {
    p=p*(365-i)/365
  }
  return(1-p)
}

data=data.frame(n=vector(),p=vector())
for (n in seq_along(2:80)){
  data[n,1]=n
  data[n,2]=prawd(n)
}

plot(data$p~data$n,xlab="n", ylab="p", type="l")