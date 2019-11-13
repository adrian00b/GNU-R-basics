# values_vector - vector with data
# quantile_vector - np c(0.5,0.6)
# number_of_users
capping = function(values_vector, quantile_vector, number_of_users){
  
  adjusted_values = integer(length(values_vector))
  quantile_vector_values = integer(length(quantile_vector))
  adjusted_rev = integer(length(quantile_vector))
  
  for (i in seq_along(quantile_vector)){
    quantile_vector_values[i] = quantile(values_vector, quantile_vector[i])
    
    for (j in seq_along(values_vector)){
      if (values_vector[j] > quantile_vector_values[i]) {
        adjusted_values[j] = quantile_vector_values[i] 
      } else {
        adjusted_values[j] = values_vector[j]
      }
    }
    adjusted_rev[i] = sum(adjusted_values)/number_of_users 
  }
  return(adjusted_rev)
}
