library('dplyr')
library(ggplot2)
library("car")

capture.output(
  print(v1)
  , summary(table_v1) 
  , quantile(table_v1$revenue_d30, seq(0.1, 1 , 0.05))
  , quantile(table_v1$revenue_d30, seq(0.95,0.99,0.01))
  , print(v2)
  , summary(table_v2)
  , quantile(table_v2$revenue_d30, seq(0.1, 1 , 0.05))
  , quantile(table_v2$revenue_d30, seq(0.95,0.99,0.01))
  , file = "Output/summaries.txt"
)



png(file = "Output/ecdf.png")
plot(ecdf(table_v1$revenue_d30), pch=16)
plot(ecdf(table_v2$revenue_d30), add=T, col='grey', ph=16)
legend("bottomright",c("18.1701","19.1700"), col = c('black','grey'), pch = c(16,16))
dev.off()

#png(file = "Output/density.png")
plot(density(table_v1$revenue_d30), bw = 2.5)
plot(density(table_v2$revenue_d30), bw = 2.5, add = T)
#dev.off()

png(file = "Output/boxplot_outliers.png")
boxplot(revenue_d30 ~ fr_install_version, data = table_v1_v2, horizontal = T, xlab = "Revenue")
dev.off()

png(file = "Output/boxplot.png")
boxplot(revenue_d30 ~ fr_install_version, data = table_v1_v2, horizontal = T, outline = F , xlab = "Revenue")
dev.off()

# from function file
capping(table_v1$revenue_d30, seq(0.9, 0.99, 0.01), table_number_of_users_v1[[1]]) -> arpu_v1
capping(table_v2$revenue_d30, seq(0.9, 0.99, 0.01), table_number_of_users_v2[[1]]) -> arpu_v2

capture.output(
  print(v1)
  , arpu_v1
  , print(v2)
  , arpu_v2
  , cat(v1, 'vs', v2)
  , arpu_v1/arpu_v2-1 
  , file = "Output/adjusted_arpu.txt"
)
