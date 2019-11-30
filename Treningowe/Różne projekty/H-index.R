# Najwiêkszy h-index w tablicy
# h-index to najwiêksza liczba h, ¿e co najmniej h el.tablicy na wartoœci >= h */

library(tibble)

h_index = function(vector){
  indexes = tibble(index = seq(min(vector), max(vector)), value = 0)
  
  for (i in seq_along(indexes$index)){
    for (j in seq_along(vector)){
      if(indexes$index[i] <= vector[j]){
        indexes$value[i] = indexes$value[i] + 1
      }
    }
  }
  
  for (i in seq_along(indexes$index)){
    if (indexes$index[i] <= indexes$value[i]){
      h = i
    }
  }
  
  return(h_index = indexes$index[h])
}


a= c(1, 2, 3, 5, 6)
h_index(a)
