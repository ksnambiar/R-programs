dataset = read.csv("D:\\sidharth\\R\\test\\Advertising.csv",header=TRUE,quote="\"",stringsAsFactors = TRUE, strip.white=TRUE)

#with predefined function
sales= dataset[,5]
tv = dataset[,2]
radio = dataset[,3]
newspaper = dataset[,4]

model <- lm(sales~TV+radio+newspaper,data=dataset)

plot(x =tv+radio+newspaper, y =sales,main="LRPLOT", xlab="tv+radio+newspaper",ylab="sales",abline(model)) 
a = data.frame(TV=120,radio=37,newspaper=58)
result = predict(model,a)
print(result)

#without predefined function

mlr <- function(x, y) {
  beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
  y_hat = x %*% beta_hat
  print(beta_hat)
  print(y_hat)
  X = x - mean(x)
  Y = y - mean(y)
  
  e = y - y_hat
  rss = sum(e^2)
  tss = sum(Y^2)
  
  R_squared = 1 - rss/tss
  
  RSE = sqrt(rss / (length(x) - 2))
  
  # corr = sum (X * Y) / sqrt( sum(X^2) *  sum(Y^2) )
  cat(rss, '\n', tss, '\n', R_squared, '\n', RSE, '\n')
}

X = as.matrix(cbind(1, dataset$TV, dataset$radio, dataset$newspaper))
Y = as.matrix(dataset$sales)
mlr(X,Y)
