
my_impute <- function(d) {
  
  na_list <- is.na(d)
  not_na_data <- d[!na_list,c(2,3)]
  
  pred <- lm(formula = not_na_data[,2] ~ Year, data = not_na_data)
  
  for (i in 1:nrow(d)) {
    if(is.na(d[i,3])) {
      d[i,3] <- pred$coefficients[2] * d[i,2] + pred$coefficients[1]
    }
  }
  return(d)
}
