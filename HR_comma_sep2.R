

install.packages(c("dplyr", "reshape"))

library(dplyr)
library(reshape)


HR = read.csv("C:\\workspace\\R\\data_sample\\HR_comma_sep2\\HR_comma_sep.csv")



head(apply(HR[,1:2], 1, mean))
head(rowMeans(HR[,1:2]))

HR[,1:2] %>%
  rowMeans() %>%
  head()


apply(HR[, 1:5], 2, mean)

colMeans(HR[, 1:5])

HR[, 1:5] %>%
  colMeans()
