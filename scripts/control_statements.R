if(x>3) {
  y <- 10
} else {
  y <- 0
}

for (i in 1:4){
  print(i)
}

x <- c("A", "B", "C", "D")
for(i in seq_along(x)){
  print(x[i])
}