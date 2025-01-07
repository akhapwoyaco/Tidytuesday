
mm = read.csv("E:\\FREELANCE\\2025/January/melvin-6/Book1.csv")
mm
#
# mm[is.na(mm$workers),4] <- 0
mm
#
library(heumilkr)
#
demand <- mm$workers[-1]

positions <- mm[,2:3]
positions
#
clarke_wright(
  demand,
  dist(positions),
  data.frame(n = NA_integer_, caps = 9)
)
#