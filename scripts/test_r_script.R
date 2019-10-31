data <- read.table("data_extract.csv",header=TRUE,sep='|')
install.packages("devtools")
install.packages("ggplot2")
library(ggplot2)c <- c('abc', 2L)
c <- c('abc', 2L)
d <- list(1, 'a', TRUE, 1+2i)
m <- matrix(ny <- data.frame(a=1, b='a'),
dput(y, file = 'y.R'),
rm(a, x, y, new.a, new.y),
x <- "foo",
y <- data.frame(a=1, b='A'),
dump(c('x', 'y'), file = 'data.R'),
rm(x,y),
source('data.R'),
new.y <- dget('y.R'),
row = 2, ncol = 2)
m <- matrix(1:4)
dim(a) <- c(3,7)
e <- cbind(c,x)
b <- factor(c("yes", "no", "yes", "no", "yes"))
table(b)
unclass(b)
x <- data.frame(foo = 1:4, bar = c(T,T,F,F))
nrow(x)
ncol(x)

data <- read.csv("data/hw1_data.csv", header=TRUE, sep = ',')
View(data)
nrow(data)
tail(data, 2)
data[47]
data[47,]
sum(is.na(data$Ozone))
mean(data$Ozone)
missing <- is.na(data$Ozone)
ozonedata <- data[!missing]
colMeans(data, na.rm = TRUE)

