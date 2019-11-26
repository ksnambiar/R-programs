dataset <- read.csv("D:\\sidharth\\R\\test\\Advertising.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE)
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

lr <- function(x, y, test) {
  x_mean = mean(x)
  y_mean = mean(y)
  X = x - x_mean
  Y = y - y_mean
  
  B1 = sum(X * Y)/ sum(X**2)
  B0 = y_mean - B1 * x_mean
  
  y_hat = B0 + B1 * x
  
  corr = sum (X * Y) / sqrt( sum(X**2) *  sum(Y**2) )
  cat(B0 + B1 * test, '\n', corr)
}

lr(dataset[,2], dataset[,5], c(200,100))
