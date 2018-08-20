result = data.frame()
s = 1.5
sc = 1.0
for(x in 1:4) {
  for(i in seq(x,5,0.1)) {
    # j = 1.5*(i^(0.5))
    result = rbind(result,data.frame(x,i))
  }
}
result$f <- ((1-dweibull(x = result$i, shape = s, scale = sc))-
               (1-dweibull(x = result$x, shape = s, scale = sc)))/
  (1-(1-dweibull(x = result$x, shape = s, scale = sc)))
result
ggplot(result, aes(i,f,color = as.factor(x)))+geom_line()

## Compare groups
df.cows$Age <- 1*(df.cows$days <= 365) + 2*(df.cows$days > 365 & df.cows$lactation == 0) +
  3*(df.cows$lactation == 1) + 4*(df.cows$lactation == 2) + 5*(df.cows$lactation > 2 &  df.cows$days <= df.cows$DIM.last) +
  6*(df.cows$lactation > 2 & df.cows$days > df.cows$DIM.last & df.cows$MY > 0)

moving_avg <- function(x, n) {
  cx <- c(rep(NA,365),cumsum(x))
  return((cx[(365+1):length(cx)] - cx[1:(length(cx) - 365)]) / 365)
}

#### Comparing two cull strategies
# # Plot Stats
par(mfrow= c(2,2))
par(mar = c(3,4.5,3,4.5))
#Milk
rsum <- moving_avg((one[1001:fDay,1])/total.start.cows, 305)
plot(rsum, type = 'l',main = paste('305-RMA'),
     xlim = c(0,fDay-1001),ylab = paste('Milk/',total.start.cows, ' cows/305 d', sep = ""),
     lwd = 2, col = 'red', ylim = c(25,40))
rsum <- moving_avg((two[1001:fDay,1])/total.start.cows,305)
lines(rsum, type = 'l',
     xlim = c(0,fDay-1001),ylab = 'Milk/Lactating Herd Size', lwd = 2, col = 'blue')
# Add a legend
# legend(1500,30, legend=c("Production", "Age"),
#        col=c("red", "blue"), lty=1:1,lwd = 2:2, cex=0.8)



bp <- barplot(height = c(one[fDay,10],two[fDay,10]),
        names.arg = c('prod','age'), xlim = c(0,1), width = 0.3 ,main = "Average # of Lacts")
text(text(bp, 1, c(one[fDay,10],two[fDay,10]), pos = 3))
# Add a legend
# legend(2000,12500, legend=c("Production", "Age"),
#        col=c("red", "blue"), lty=1:1, lwd = 2:2, cex=0.8)


# Milk/Nitrogen
rsum <- cumsum(one[1001:fDay,1])/(cumsum(one[1001:fDay,2]/1000))
plot(rsum, type = 'l',main = 'Milk/N excreted, kg/g',
     xlim = c(0,fDay-1001),ylab = 'Milk/N, kg/g', lwd = 2, col = 'red', ylim = c(2,4))
rsum <- cumsum(two[1001:fDay,1])/(cumsum(two[1001:fDay,2]/1000))
lines(rsum, type = 'l',
      xlim = c(0,fDay-1001), lwd = 2, col = 'blue')
# Add a legend
# legend(2000,3, legend=c("Production", "Age"),
#        col=c("red", "blue"), lty=1:1, lwd = 2:2, cex=0.8)
#

# DMI
rsum <- cumsum(one[1001:fDay,1])/cumsum(one[1001:fDay,6])
plot(rsum, type = 'l',main = 'Milk/DMI, kg/kg',
     xlim = c(0,fDay-1001),ylab = 'Milk/DMI, kg/kg', lwd = 2, col = 'red', ylim = c(2,3))
rsum <- cumsum(two[1001:fDay,1])/cumsum(two[1001:fDay,6])
lines(rsum, type = 'l',
      xlim = c(0,fDay-1001), lwd = 2, col = 'blue')
# Add a legend
# legend(2000,2.3, legend=c("Production", "Age"),
#        col=c("red", "blue"), lty=1:1, lwd = 2:2, cex=0.8)
