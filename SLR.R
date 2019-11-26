dataset <- read.csv("D:\\sidharth\\R\\Advertising.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE)
print(dataset)
#using predefined function
print(names(dataset))

scatter.smooth(x = dataset$TV,y=dataset$sales,main="TV - sales")
scatter.smooth(x = dataset$radio,y=dataset$sales,main="radio - sales")
scatter.smooth(x = dataset$newspaper,y=dataset$sales,main="newspaper - sales")

cor(dataset$TV,dataset$sales)
cor(dataset$radio,dataset$sales)
cor(dataset$newspaper,dataset$sales)
linearMod<-lm(sales ~ TV,data=dataset)
print(linearMod)
a <- data.frame(TV = 200)
result <- predict(linearMod,a)

#without using predefined function

lin_reg <- function(x, y, test) {
  x_bar = mean(x)
  y_bar = mean(y)
  X = x - x_bar
  Y = y - y_bar
  
  B1 = sum(X * Y) / sum(X**2)
  B0 = y_bar - B1 * x_bar
  
  y_hat = B0 + B1 * x
  
  rss = sum( (y - y_hat) ** 2 )
  tss = sum(Y**2)
  
  R_squared = 1 - rss/tss
  
  RSE = sqrt(rss / (length(x) - 2))
  
  corr = sum (X * Y) / sqrt( sum(X**2) *  sum(Y**2) )
  cat(R_squared, '\n', B0 + B1 * test, '\n', RSE, '\n', corr)
}

lin_reg(dataset[,2], dataset[,5], c(200,100))
