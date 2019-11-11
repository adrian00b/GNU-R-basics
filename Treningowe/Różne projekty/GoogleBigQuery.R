library(bigrquery)
project <- "proj1brodowicz" 
sql <- "SELECT ROUND(WEIGHT_POUNDS,2), is_male , cigarette_use, alcohol_use 
FROM `bigquery-public-data.samples.natality`
WHERE YEAR>=2003 AND plurality=1 AND (gestation_weeks BETWEEN 38 AND 42) AND (cigarette_use IS NOT NULL) AND (alcohol_use IS NOT NULL)"
alldata=query_exec(sql, project = project, use_legacy_sql = F, max_pages = Inf)
library(dplyr)
summary(alldata)
allM=filter(alldata, is_male==T)
allF=filter(alldata, is_male==F)
summary(allM)
summary(allF)

boxplot(allM$f0_,allF$f0_, range=1,outline=F, ylim=c(5,10), main="P+F",
        col=c("powderblue","thistle1"), names=c("P³eæ mêska","P³eæ ¿eñska"))

FF_M=filter(alldata, is_male==T, cigarette_use==F, alcohol_use==F)
FF_F=filter(alldata, is_male==F, cigarette_use==F, alcohol_use==F)
summary(FF_M)
summary(FF_F)
TT_M=filter(alldata, is_male==T, cigarette_use==T, alcohol_use==T)
TT_F=filter(alldata, is_male==F, cigarette_use==T, alcohol_use==T)
summary(TT_M)
summary(TT_F)

boxplot(FF_M$f0_,TT_M$f0_, range=1,outline=F, main="P³eæ mêska P/F", ylim=c(5,10),
        col=c("powderblue","powderblue"), names=c("Brak alkoholu i papierosów","Alkohol lub papierosy"))

boxplot(FF_F$f0_,TT_F$f0_, range=1,outline=F, main="P³eæ ¿eñska P/F", ylim=c(5,10),
        col=c("thistle1","thistle1"), names=c("Brak alkoholu lub papierosów","Alkohol lub papierosy"))

plot(density(na.omit(FF_M$f0_), bw=1.5), main = "P³eæ mêska", xlim=c(0,17))
lines(density(na.omit(TT_M$f0_), bw=1.5), col="red")
legend("topright",legen=c("Brak alkoholu i papierosów","Alkohol lub papierosy"), lty=c(1,1), col=c("black","red"),
       cex=0.8)

plot(density(na.omit(FF_F$f0_), bw=1.5), main = "P³eæ ¿eñska", xlim=c(0,17))
lines(density(na.omit(TT_F$f0_), bw=1.5), col="deeppink")
legend("topright",legen=c("Brak alkoholu i papierosów","Alkohol lub papierosy"), lty=c(1,1), col=c("black","deeppink"),
       cex=0.8)

