# a teraz z doowlnych WEKTOROW!!! tzn ze musimy dzialac na indeksach tego wektora
sample2=function(i,a){
  v=numeric(i);
  for (k in 1:i){
    repeat{
      v[k]=a[ceiling(runif(1,0,length(a)))]
      if (all(v[k]!=v[1:k-1])){
        break
      }
    }
  }
  return(v)
}

# a teraz z obsluga bledow

sample2b=function(i,a){
  if (i<1){
    stop("Liczba losowanych element�w musi by� wi�ksza lub r�wna 1")
  } else if (i%%1!=0){
    stop("Liczba losowanych element�w musi by� liczb� ca�kowit�")
  } else if (is.vector(a)==F){
    stop("Nie podano poprawnego wektora")
  } else if (i>length(a)){
    stop("Liczba losowanych element�w nie mo�e by� wi�ksza od liczby element�w wektora")
  }
  
  v=numeric(i);
  for (k in 1:i){
    repeat{
      v[k]=a[ceiling(runif(1,0,length(a)))]
      if (all(v[k]!=v[1:k-1])){
        break
      }
    }
  }
  return(v)
}