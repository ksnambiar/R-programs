roll<-function(){
  die = 1:6
  dice = sample(die,size=1,replace = TRUE)
  print(dice)
}

roll()