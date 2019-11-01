correl <- function(df, method = 'pearson'){
  if (method == 'pearson'){
    answer <- cov(df[,1], df[,2]) / (sd(df[,1]) * sd(df[,2]))
    return(answer)
  }
  else if (method == 'spearman'){
    answer <- cov(rank(df[,1]), rank(df[,2])) / (sd(rank(df[,1])) * sd(rank(df[,2])))
    return(answer)
  }
}


## Comparing results of mdf[,2] function to basic package's results

test_data <- data.frame(mtcars$mpg, mtcars$hp)

correl(test_data)
correl(test_data, method = 'spearman')

cor(test_data[,1], test_data[,2])
cor(test_data[,1], test_data[,2], method = 'spearman')