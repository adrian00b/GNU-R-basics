#######################
####      MODEL 2   ###
#######################

library(MASS)
b <- read.table(paste("import/125.csv"))

c <-as.name("csv")

prognozaspot <- matrix(nrow=24, ncol=1)
prognozarb <- matrix(nrow=24, ncol=1)
errspot <- matrix(nrow=24, ncol=1)
errrb <- matrix(nrow=24, ncol=1)

m=101


## WPISZ TUTAJ P-value 	##
		p<-0.05

## WPISZ TUTAJ P-value 	##
while (m<125)

{
p<-0.05
  
dane <- read.table(paste(paste("import/", m, sep = ""), c, sep="."),sep=",",dec=".",header=T)
n<-(length(dane[,1]))-(b)
n<-as.matrix(n)


PPXAVG <- as.matrix( dane$PPXAVG,ncol=1)
PPXAVG <-  as.matrix( PPXAVG[(length(PPXAVG)-3728):(length(PPXAVG)-n)])
#-3728 liczy model dla danych od 2015 
#kiedys# -731 liczy model dla historii od 2015 roku, poni¿ej liczy od poczatku szeregu w bazie_godzinowej_bthd, dla roku 2017
#PPXAVG <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-n)]) 
PPXAVG <- as.matrix(c(PPXAVG,NA),ncol=1)


PPX8 <- as.matrix(dane$PPX8,ncol=1)
PPX8 <-  as.matrix( PPX8[(length(PPX8)-3728):(length(PPX8)-n)])
PPX8 <- as.matrix(c(PPX8,NA),ncol=1)

PPXII <- as.matrix(dane$PPXII,ncol=1)
PPXII <-  as.matrix( PPXII[(length(PPXII)-3728):(length(PPXII)-n)])
PPXII <- as.matrix(c(PPXII,NA),ncol=1)

RB <- as.matrix(dane$RB,ncol=1)
RB <- as.numeric(RB)
RB <-  as.matrix( RB[(length(RB)-3728):(length(RB)-n-1)])
RB <- as.matrix(c(RB,NA,NA),ncol=1)

HDD <- as.matrix(dane$HDD,ncol=1)
HDD <-  as.matrix( HDD[(length(HDD)-3728):(length(HDD)-n+1)])

CDD <- as.matrix(dane$CDD,ncol=1)
CDD <-  as.matrix( CDD[(length(CDD)-3728):(length(CDD)-n+1)])

Wind <- as.matrix(dane$Wind,ncol=1)
Wind <-  as.matrix( Wind[(length(Wind)-3728):(length(Wind)-n+1)])

WKzapotrzMoc <- as.matrix(dane$WKzapotrzMoc,ncol=1)
WKzapotrzMoc <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-3728):(length(WKzapotrzMoc)-n+1)])

WKSE <- as.matrix(dane$WKSE,ncol=1)
WKSE <-  as.matrix( WKSE[(length(WKSE)-3728):(length(WKSE)-n+1)])

WJWCD <- as.matrix(dane$WJWCD,ncol=1)
WJWCD <-  as.matrix( WJWCD[(length(WJWCD)-3728):(length(WJWCD)-n+1)])

PPXAVG_1 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-1)])
PPXAVG_1 <- as.matrix(c(NA,PPXAVG_1))
PPXAVG_2 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-2)])
PPXAVG_2 <- as.matrix(c(NA,NA,PPXAVG_2))
PPXAVG_3 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-3)])
PPXAVG_3 <- as.matrix(c(NA,NA,NA,PPXAVG_3))
PPXAVG_4 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-4)])
PPXAVG_4 <- as.matrix(c(NA,NA,NA,NA,PPXAVG_4))
PPXAVG_5 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-5)])
PPXAVG_5 <- as.matrix(c(NA,NA,NA,NA,NA,PPXAVG_5))
PPXAVG_6 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-6)])
PPXAVG_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,PPXAVG_6))
PPXAVG_7 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-7)])
PPXAVG_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,PPXAVG_7))
PPXAVG_8 <-  as.matrix( PPXAVG[(length(PPXAVG)-length(PPXAVG)):(length(PPXAVG)-8)])
PPXAVG_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,PPXAVG_8))

############################ ZMIENNE NIEZALEZNE #########


PPX8_1 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-1)])
PPX8_1 <- as.matrix(c(NA,PPX8_1))
PPX8_2 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-2)])
PPX8_2 <- as.matrix(c(NA,NA,PPX8_2))
PPX8_3 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-3)])
PPX8_3 <- as.matrix(c(NA,NA,NA,PPX8_3))
PPX8_4 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-4)])
PPX8_4 <- as.matrix(c(NA,NA,NA,NA,PPX8_4))
PPX8_5 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-5)])
PPX8_5 <- as.matrix(c(NA,NA,NA,NA,NA,PPX8_5))
PPX8_6 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-6)])
PPX8_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,PPX8_6))
PPX8_7 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-7)])
PPX8_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,PPX8_7))
PPX8_8 <-  as.matrix( PPX8[(length(PPX8)-length(PPX8)):(length(PPX8)-8)])
PPX8_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,PPX8_8))

#######################################################

PPXII_1 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-1)])
PPXII_1 <- as.matrix(c(NA,PPXII_1))
PPXII_2 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-2)])
PPXII_2 <- as.matrix(c(NA,NA,PPXII_2))
PPXII_3 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-3)])
PPXII_3 <- as.matrix(c(NA,NA,NA,PPXII_3))
PPXII_4 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-4)])
PPXII_4 <- as.matrix(c(NA,NA,NA,NA,PPXII_4))
PPXII_5 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-5)])
PPXII_5 <- as.matrix(c(NA,NA,NA,NA,NA,PPXII_5))
PPXII_6 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-6)])
PPXII_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,PPXII_6))
PPXII_7 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-7)])
PPXII_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,PPXII_7))
PPXII_8 <-  as.matrix( PPXII[(length(PPXII)-length(PPXII)):(length(PPXII)-8)])
PPXII_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,PPXII_8))

##################################################

RB_2 <-  as.matrix( RB[(length(RB)-length(RB)):(length(RB)-2)])
RB_2 <- as.matrix(c(NA,NA,RB_2))
RB_3 <-  as.matrix( RB[(length(RB)-length(RB)):(length(RB)-3)])
RB_3 <- as.matrix(c(NA,NA,NA,RB_3))
RB_4 <-  as.matrix( RB[(length(RB)-length(RB)):(length(RB)-4)])
RB_4 <- as.matrix(c(NA,NA,NA,NA,RB_4))
RB_5 <-  as.matrix( RB[(length(RB)-length(RB)):(length(RB)-5)])
RB_5 <- as.matrix(c(NA,NA,NA,NA,NA,RB_5))
RB_6 <-  as.matrix( RB[(length(RB)-length(RB)):(length(RB)-6)])
RB_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,RB_6))
RB_7 <-  as.matrix( RB[(length(RB)-length(RB)):(length(RB)-7)])
RB_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,RB_7))
RB_8 <-  as.matrix( RB[(length(RB)-length(RB)):(length(RB)-8)])
RB_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,RB_8))

#################################################################


HDD_1 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-1)])
HDD_1 <- as.matrix(c(NA,HDD_1))
HDD_2 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-2)])
HDD_2 <- as.matrix(c(NA,NA,HDD_2))
HDD_3 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-3)])
HDD_3 <- as.matrix(c(NA,NA,NA,HDD_3))
HDD_4 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-4)])
HDD_4 <- as.matrix(c(NA,NA,NA,NA,HDD_4))
HDD_5 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-5)])
HDD_5 <- as.matrix(c(NA,NA,NA,NA,NA,HDD_5))
HDD_6 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-6)])
HDD_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,HDD_6))
HDD_7 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-7)])
HDD_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,HDD_7))
HDD_8 <-  as.matrix( HDD[(length(HDD)-length(HDD)):(length(HDD)-8)])
HDD_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,HDD_8))

#################################################################

CDD_1 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-1)])
CDD_1 <- as.matrix(c(NA,CDD_1))
CDD_2 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-2)])
CDD_2 <- as.matrix(c(NA,NA,CDD_2))
CDD_3 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-3)])
CDD_3 <- as.matrix(c(NA,NA,NA,CDD_3))
CDD_4 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-4)])
CDD_4 <- as.matrix(c(NA,NA,NA,NA,CDD_4))
CDD_5 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-5)])
CDD_5 <- as.matrix(c(NA,NA,NA,NA,NA,CDD_5))
CDD_6 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-6)])
CDD_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,CDD_6))
CDD_7 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-7)])
CDD_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,CDD_7))
CDD_8 <-  as.matrix( CDD[(length(CDD)-length(CDD)):(length(CDD)-8)])
CDD_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,CDD_8))


#################################################################

Wind_1 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-1)])
Wind_1 <- as.matrix(c(NA,Wind_1))
Wind_2 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-2)])
Wind_2 <- as.matrix(c(NA,NA,Wind_2))
Wind_3 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-3)])
Wind_3 <- as.matrix(c(NA,NA,NA,Wind_3))
Wind_4 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-4)])
Wind_4 <- as.matrix(c(NA,NA,NA,NA,Wind_4))
Wind_5 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-5)])
Wind_5 <- as.matrix(c(NA,NA,NA,NA,NA,Wind_5))
Wind_6 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-6)])
Wind_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,Wind_6))
Wind_7 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-7)])
Wind_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,Wind_7))
Wind_8 <-  as.matrix( Wind[(length(Wind)-length(Wind)):(length(Wind)-8)])
Wind_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,Wind_8))

##################################################################

WKzapotrzMoc_1 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-1)])
WKzapotrzMoc_1 <- as.matrix(c(NA,WKzapotrzMoc_1))
WKzapotrzMoc_2 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-2)])
WKzapotrzMoc_2 <- as.matrix(c(NA,NA,WKzapotrzMoc_2))
WKzapotrzMoc_3 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-3)])
WKzapotrzMoc_3 <- as.matrix(c(NA,NA,NA,WKzapotrzMoc_3))
WKzapotrzMoc_4 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-4)])
WKzapotrzMoc_4 <- as.matrix(c(NA,NA,NA,NA,WKzapotrzMoc_4))
WKzapotrzMoc_5 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-5)])
WKzapotrzMoc_5 <- as.matrix(c(NA,NA,NA,NA,NA,WKzapotrzMoc_5))
WKzapotrzMoc_6 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-6)])
WKzapotrzMoc_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,WKzapotrzMoc_6))
WKzapotrzMoc_7 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-7)])
WKzapotrzMoc_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,WKzapotrzMoc_7))
WKzapotrzMoc_8 <-  as.matrix( WKzapotrzMoc[(length(WKzapotrzMoc)-length(WKzapotrzMoc)):(length(WKzapotrzMoc)-8)])
WKzapotrzMoc_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,WKzapotrzMoc_8))
##########################################################

WKSE_1 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-1)])
WKSE_1 <- as.matrix(c(NA,WKSE_1))
WKSE_2 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-2)])
WKSE_2 <- as.matrix(c(NA,NA,WKSE_2))
WKSE_3 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-3)])
WKSE_3 <- as.matrix(c(NA,NA,NA,WKSE_3))
WKSE_4 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-4)])
WKSE_4 <- as.matrix(c(NA,NA,NA,NA,WKSE_4))
WKSE_5 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-5)])
WKSE_5 <- as.matrix(c(NA,NA,NA,NA,NA,WKSE_5))
WKSE_6 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-6)])
WKSE_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,WKSE_6))
WKSE_7 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-7)])
WKSE_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,WKSE_7))
WKSE_8 <-  as.matrix( WKSE[(length(WKSE)-length(WKSE)):(length(WKSE)-8)])
WKSE_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,WKSE_8))
##########################################################

WJWCD_1 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-1)])
WJWCD_1 <- as.matrix(c(NA,WJWCD_1))
WJWCD_2 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-2)])
WJWCD_2 <- as.matrix(c(NA,NA,WJWCD_2))
WJWCD_3 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-3)])
WJWCD_3 <- as.matrix(c(NA,NA,NA,WJWCD_3))
WJWCD_4 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-4)])
WJWCD_4 <- as.matrix(c(NA,NA,NA,NA,WJWCD_4))
WJWCD_5 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-5)])
WJWCD_5 <- as.matrix(c(NA,NA,NA,NA,NA,WJWCD_5))
WJWCD_6 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-6)])
WJWCD_6 <- as.matrix(c(NA,NA,NA,NA,NA,NA,WJWCD_6))
WJWCD_7 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-7)])
WJWCD_7 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,WJWCD_7))
WJWCD_8 <-  as.matrix( WJWCD[(length(WJWCD)-length(WJWCD)):(length(WJWCD)-8)])
WJWCD_8 <- as.matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,WJWCD_8))

##############################################################


X<-cbind(PPXAVG_1,	PPXAVG_2,	PPXAVG_3,	PPXAVG_4,	PPXAVG_5,	PPXAVG_6,	PPXAVG_7,	PPXAVG_8, 
PPX8_1,	PPX8_2,	PPX8_3,	PPX8_4,	PPX8_5,	PPX8_6,	PPX8_7,	PPX8_8,	
PPXII_1,	PPXII_2,	PPXII_3,	PPXII_4,	PPXII_5,	PPXII_6,	PPXII_7,	PPXII_8,	
RB_2, RB_3,	RB_4,	RB_5,	RB_6,	RB_7,	RB_8,
HDD,	HDD_1,	HDD_2,	HDD_3,	HDD_4,	HDD_5,	HDD_6,	HDD_7,	HDD_8,
CDD,	CDD_1,	CDD_2,	CDD_3,	CDD_4,	CDD_5,	CDD_6,	CDD_7,	CDD_8,
Wind,	Wind_1,	Wind_2,	Wind_3,	Wind_4,	Wind_5,	Wind_6,	Wind_7,	Wind_8,
WKzapotrzMoc,	WKzapotrzMoc_1,	WKzapotrzMoc_2,	WKzapotrzMoc_3,	WKzapotrzMoc_4,	WKzapotrzMoc_5,	WKzapotrzMoc_6,	WKzapotrzMoc_7,	WKzapotrzMoc_8,	
WKSE,	WKSE_1,	WKSE_2,	WKSE_3,	WKSE_4,	WKSE_5,	WKSE_6,	WKSE_7,	WKSE_8,	
WJWCD,	WJWCD_1,	WJWCD_2,	WJWCD_3,	WJWCD_4,	WJWCD_5,	WJWCD_6,	WJWCD_7,	WJWCD_8
)

#tail(X)
# X<-cbind(PPXAVG_1,	PPXAVG_2,	PPXAVG_3,	PPXAVG_4,	PPXAVG_5,	PPXAVG_6,	PPXAVG_7,	PPXAVG_8,
#PPX8_1,	PPX8_2,	PPX8_3,	PPX8_4,	PPX8_5,	PPX8_6,	PPX8_7,	PPX8_8,	
#PPXII_1,	PPXII_2,	PPXII_3,	PPXII_4,	PPXII_5,	PPXII_6,	PPXII_7,	PPXII_8,	
#RB_3,	RB_4,	RB_5,	RB_6,	RB_7,	RB_8,
#HDD,	HDD_1,	HDD_2,	HDD_3,	HDD_4,	HDD_5,	HDD_6,	HDD_7,	HDD_8,
#CDD,	CDD_1,	CDD_2,	CDD_3,	CDD_4,	CDD_5,	CDD_6,	CDD_7,	CDD_8,
#Wind,	Wind_1,	Wind_2,	Wind_3,	Wind_4,	Wind_5,	Wind_6,	Wind_7,	Wind_8,
#WKzapotrzMoc,	WKzapotrzMoc_1,	WKzapotrzMoc_2,	WKzapotrzMoc_3,	WKzapotrzMoc_4,	WKzapotrzMoc_5,	WKzapotrzMoc_6,	WKzapotrzMoc_7,	WKzapotrzMoc_8,	
#WKSE,	WKSE_1,	WKSE_2,	WKSE_3,	WKSE_4,	WKSE_5,	WKSE_6,	WKSE_7,	WKSE_8,	
#WJWCD,	WJWCD_1,	WJWCD_2,	WJWCD_3,	WJWCD_4,	WJWCD_5,	WJWCD_6,	WJWCD_7,	WJWCD_8
#)




#X<-cbind(PPXAVG_1, PPX8_1,	PPXII_1, RB_3,	RB_7,	HDD,		CDD,	Wind,	WKzapotrzMoc, WKSE,	WJWCD)
#X<-cbind(PPXAVG_1 )

####################################
#	     	Modyfikacja - KASOWANIE ZMIENNYCH NIEISTOTNYCH
####################################
	
## 				##
for (i in c(1:ncol(X))){
	LM <- lm(PPXII~X)

	mySum <- summary(LM)
	mySum <- mySum$coefficients
	mySum[,4]
	if(max(mySum[-1,4])>p)			# je¿eli jest jakaœ zmienna nieistotna
	{
		X=X[,-(which(mySum[-1,4]==max(mySum[-1,4])))]		# usuñ j¹ z macierzy (sta³a jest pomijana)
	}
	else
	{
		break
	}
}

## 				##
#for (i in c(1:ncol(X))){
#	LM <- lm(PPXAVG~X)

#	mySum <- summary(LM)
#	mySum <- mySum$coefficients
#	mySum[,4]
#	if(max(mySum[-1,4])>p)			# je¿eli jest jakaœ zmienna nieistotna
#	{
#		X=X[,-(which(mySum[-1,4]==max(mySum[-1,4])))]		# usuñ j¹ z macierzy (sta³a jest pomijana)
#	}
#	else
#	{
#		break
#	}
#}

errspot[m-100]<-sqrt(deviance(LM)/df.residual(LM))

####################################


mySum <- summary(LM)
mySum <- mySum$coefficients
kwant <- as.numeric(quantile(mySum[,4],p=0.05))
kwant<-0.9999

Pred <- as.numeric(LM$coefficients[1])
for (ii in 2:length(LM$coefficients)){
    coef <- as.numeric(LM$coefficients[ii])
    if ((!is.na(coef))&(mySum[mySum[,1]==coef,4][1]>kwant)){
	coef <- NA
    }
    if (!is.na(coef)){
	Pred <- Pred + coef * X[nrow(X),(ii-1)]
    }
}

prognozaspot[m-100]<-Pred

#################### RB PRED
head(X)
tail(X)

X<-cbind(
PPXAVG_1,	PPXAVG_2,	PPXAVG_3,	PPXAVG_4,	PPXAVG_5,	PPXAVG_6,	PPXAVG_7,	PPXAVG_8,
PPX8_1,	PPX8_2,	PPX8_3,	PPX8_4,	PPX8_5,	PPX8_6,	PPX8_7,	PPX8_8,	
PPXII_1,	PPXII_2,	PPXII_3,	PPXII_4,	PPXII_5,	PPXII_6,	PPXII_7,	PPXII_8,	
RB_2, RB_3,	RB_4,	RB_5,	RB_6,	RB_7,	RB_8,
HDD,	HDD_1,	HDD_2,	HDD_3,	HDD_4,	HDD_5,	HDD_6,	HDD_7,	HDD_8,
CDD,	CDD_1,	CDD_2,	CDD_3,	CDD_4,	CDD_5,	CDD_6,	CDD_7,	CDD_8,
Wind,	Wind_1,	Wind_2,	Wind_3,	Wind_4,	Wind_5,	Wind_6,	Wind_7,	Wind_8,
WKzapotrzMoc,	WKzapotrzMoc_1,	WKzapotrzMoc_2,	WKzapotrzMoc_3,	WKzapotrzMoc_4,	WKzapotrzMoc_5,	WKzapotrzMoc_6,	WKzapotrzMoc_7,	WKzapotrzMoc_8,	
WKSE,	WKSE_1,	WKSE_2,	WKSE_3,	WKSE_4,	WKSE_5,	WKSE_6,	WKSE_7,	WKSE_8,	
WJWCD,	WJWCD_1,	WJWCD_2,	WJWCD_3,	WJWCD_4,	WJWCD_5,	WJWCD_6,	WJWCD_7,	WJWCD_8
)



####################################
#	     	Modyfikacja - KASOWANIE ZMIENNYCH NIEISTOTNYCH
####################################
	p=0.1
## 				##
for (i in c(1:ncol(X))){
	LM <- lm(RB~X)

	mySum <- summary(LM)
	mySum <- mySum$coefficients
	mySum[,4]
	if(max(mySum[-1,4])>p)			# je¿eli jest jakaœ zmienna nieistotna
	{
		X=X[,-(which(mySum[-1,4]==max(mySum[-1,4])))]		# usuñ j¹ z macierzy (sta³a jest pomijana)
	}
	else
	{
		break
	}
}

errrb[m-100]<-sqrt(deviance(LM)/df.residual(LM))

####################################


mySum <- summary(LM)
mySum <- mySum$coefficients
kwant <- as.numeric(quantile(mySum[,4],p=0.05))
kwant<-0.9999

Pred <- as.numeric(LM$coefficients[1])
for (ii in 2:length(LM$coefficients)){
    coef <- as.numeric(LM$coefficients[ii])
    if ((!is.na(coef))&(mySum[mySum[,1]==coef,4][1]>kwant)){
	coef <- NA
    }
    if (!is.na(coef)){
	Pred <- Pred + coef * X[nrow(X),(ii-1)]
    }
}


prognozarb [m-100]<-Pred


m=m+1
}



write.table(prognozaspot, file = paste("modelspot2_",format(Sys.Date()),".txt"), quote = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, 
            qmethod = "double")
write.table(prognozarb, file = paste("modelrb2_",format(Sys.Date()),".txt"), quote = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, 
            qmethod = "double")
      