install.packages("moments")
library(moments)
moments::skewness(Purchase.Amount..USD)
skewness(shopping_trends$Purchase.Amount..USD.)
kurtosis(shopping_trends$Purchase.Amount..USD.)
box.plot(shopping_trends$Review.Rating,col=c("Green"))
mode(shopping_trends$Size)


