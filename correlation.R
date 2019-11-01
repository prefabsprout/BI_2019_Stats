correl <- function(x,y, method = 'pearson'){
  if (method == 'pearson'){
    answer <- cov(x, y) / (sd(x) * sd(y))
    return(answer)
  }
  else if (method == 'spearman'){
    answer <- cov(rank(x), rank(y)) / (sd(rank(x)) * sd(rank(y)))
    return(answer)
  }
}


## Comparing results of my function to basic package's results

correl(mtcars$mpg, mtcars$hp)
correl(mtcars$mpg, mtcars$hp, method = 'spearman')

cor(mtcars$mpg, mtcars$hp)
cor(mtcars$mpg, mtcars$hp, method = 'spearman')