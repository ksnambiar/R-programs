dataset <- read.csv("D:\\sidharth\\R\\Raction\\Defaulter.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE)
print(dataset)
K = 4
Age = dataset[,1]
print(Age)
Loan = dataset[,2]
age_test = 48
loan_test = 142000
decision=dataset[,3]

knn <- function(K){
  dage=Age-age_test
  dloan = Loan-loan_test
  dage2=dage*dage
  dloan2=dloan*dloan
  dist2 = dage2+dloan2
  dist = sqrt(dist2)
  n <- length(dist)
  print(dist)
  sorted=sort(dist)
  decs<-c()
  for(i in 1:K){
   temp = which(dist==sorted[i])
   print(temp)
   decs<-c(decs,decision[temp])
  }
  print(decs)
  print(names(sort(table(decs))[2])==2)
  if(names(sort(table(decs))[2])==2){
    print("Approved")
  }else{
    print("dissapproved")
  }
}
knn(3)
