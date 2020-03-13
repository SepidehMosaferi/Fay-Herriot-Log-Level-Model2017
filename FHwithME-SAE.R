################################################################################

  #### R code used for the paper titled:
  #### "An Adjusted Predictor for the Fay-Herriot Log-Level Model with Measurement
  #### Error in Covariates"
  #### Author: Sepideh Mosaferi
  #### Date of Publication: 1/3/2017  

################################################################################

require("sampling")
require("PracTools")
require("survey")
library("stratification")
require("MASS")
require("samplingbook")
require("plyr")
require("lattice")
require("stats")
require("coda")
require("lattice")
require("MCMCpack")
data(employment)
attach(employment)

employment <- employment[order(employment$id),] ##ordering the data by id

## y variable:
TOTEMP12 <- aggregate(employment$ftemp12~employment$id,FUN=sum)[,2]
LNGT12 <- aggregate(employment$ftemp12~employment$id,FUN=function(x){length(x)})[,2]
employment$TOTEMP12rep <- rep(TOTEMP12,LNGT12)

## x variable:
TOTEMP07 <- aggregate(employment$ftemp07~employment$id,FUN=sum)[,2]
LNGT07 <- aggregate(employment$ftemp07~employment$id,FUN=function(x){length(x)})[,2]
employment$TOTEMP07rep <- rep(TOTEMP07,LNGT07)


STATENAME <- c("Alabama(AL)", "Alaska(AK)", "Arizona(AZ)", "Arkansas(AR)", "California(CA)", "Colorado(CO)", 
               "Connecticut(CT)", "Delaware(DE)", "DistrictofColumbia(DC)","Florida(FL)", "Georgia(GA)", "Hawaii(HI)", 
               "Idaho(ID)", "Illinois(IL)", "Indiana(IN)", "Iowa(IA)", "Kansas(KS)", "Kentucky(KY)", "Louisiana(LA)", 
               "Maine(ME)", "Maryland(MD)", "Massachusetts(MA)", "Michigan(MI)", "Minnesota(MN)", "Mississippi(MS)", 
               "Missouri(MO)", "Montana(MT)", "Nebraska(NE)", "Nevada(NV)", "NewHampshire(NH)", "NewJersey(NJ)", 
               "NewMexico(NM)", "NewYork(NY)", "NorthCarolina(NC)", "NorthDakota(ND)", "Ohio(OH)", "Oklahoma(OK)", 
               "Oregon(OR)", "Pennsylvania(PA)", "RhodeIsland(RI)", "SouthCarolina(SC)", "SouthDakota(SD)", 
               "Tennessee(TN)", "Texas(TX)", "Utah(UT)", "Vermont(VT)", "Virginia(VA)", "Washington(WA)", 
               "WestVirginia(WV)", "Wisconsin(WI)", "Wyoming(WY)")

employment$STATE <- rep(STATENAME,table(state))

FACTOR <- factor(employment$STATE)

## Producing federal regions for the GVF:
employment$REGION <- revalue(FACTOR,c("Alabama(AL)"="IV","Alaska(AK)"="X","Arizona(AZ)"="IX","Arkansas(AR)"="VI","California(CA)"="IX",        
                                      "Colorado(CO)"="VIII","Connecticut(CT)"="I","Delaware(DE)"="III","DistrictofColumbia(DC)"="III",
                                      "Florida(FL)"="IV","Georgia(GA)"="IV","Hawaii(HI)"="IX","Idaho(ID)"="X","Illinois(IL)"="V","Indiana(IN)"="V",           
                                      "Iowa(IA)"="VII","Kansas(KS)"="VII","Kentucky(KY)"="IV","Louisiana(LA)"="VI","Maine(ME)"="I",             
                                      "Maryland(MD)"="III","Massachusetts(MA)"="I","Michigan(MI)"="V","Minnesota(MN)"="V","Mississippi(MS)"="IV",       
                                      "Missouri(MO)"="VII","Montana(MT)"="VIII","Nebraska(NE)"="VII","Nevada(NV)"="IX","NewHampshire(NH)"="I",      
                                      "NewJersey(NJ)"="II","NewMexico(NM)"="VI","NewYork(NY)"="II","NorthCarolina(NC)"="IV","NorthDakota(ND)"="VIII",       
                                      "Ohio(OH)"="V","Oklahoma(OK)"="VI","Oregon(OR)"="X","Pennsylvania(PA)"="III","RhodeIsland(RI)"="I",       
                                      "SouthCarolina(SC)"="IV","SouthDakota(SD)"="VIII","Tennessee(TN)"="IV","Texas(TX)"="VI","Utah(UT)"="VIII",             
                                      "Vermont(VT)"="I","Virginia(VA)"="III","Washington(WA)"="X","WestVirginia(WV)"="III","Wisconsin(WI)"="V",         
                                      "Wyoming(WY)"="VIII"))


FINALDATA <- data.frame(employment[!employment$state %in% c("09","12"),]) #removing California & Hawaii
FINALDATA <- FINALDATA[order(FINALDATA$STATE),]
FINALDATA <- FINALDATA[!duplicated(FINALDATA$id),] #un-duplicating the data
attach(FINALDATA)  #Final Dataset

###############################################################
#### correlation study and Diagnostic Analysis for X and Y
###############################################################

Ybarh <- tapply(FINALDATA$TOTEMP12rep,INDEX=FINALDATA$STATE,mean); Ybarh  #True Mean within Stratum for Y
MEANPOPy <- as.vector(Ybarh)
LnMEANPOPy <- log(MEANPOPy)

Xbarh <- tapply(FINALDATA$TOTEMP07rep,INDEX=FINALDATA$STATE,mean); Xbarh  #True Mean within Stratum for X
MEANPOPx <- as.vector(Xbarh)
LnMEANPOPx <- log(MEANPOPx)

cor(MEANPOPy,MEANPOPx)
cor(LnMEANPOPy,LnMEANPOPx)

par(mfrow=c(3,2))
plot(MEANPOPx,MEANPOPy,xlab="Average Employees (2007)",ylab="Average Employees (2012)",pch=19,
     main="Plot Y vs. X")
legend(2,750,expression(paste(R^2==0.99841)))
abline(0,1,col="black",lwd=1.5)

plot(LnMEANPOPx,LnMEANPOPy,xlab="Log Average Employees (2007)",ylab="Log Average Employees (2012)",pch=19,
     main="Plot Log(Y) vs. Log(X)")
legend(2,6.5,expression(paste(R^2==0.99856)))
abline(0,1,col="black",lwd=1.5)

FIT <- lm(MEANPOPy~MEANPOPx-1)  ##Remove Intercept: It is non-significant
FITlog <- lm(LnMEANPOPy~LnMEANPOPx-1)    ##Remove Intercept: It is non-significant

plot(FIT$residuals,xlab="Order",ylab="Residuals",main="Residual vs. Order Plot",pch=19)
abline(h=0,lty=2)
plot(FITlog$residuals,xlab="Order",ylab="Residuals of Log Model",main="Residual vs. Order Plot of Log Model",pch=19)
abline(h=0,lty=2)

qqnorm(FIT$residuals,main="Normal Q-Q Plot",xlab="Normal Scores",
       ylab="Residuals",pch=19)
legend(-2.45,13,expression(paste("P-value ", "Shapiro-Wilk=0.00257")))
qqline(FIT$residuals,lwd=1.5)

qqnorm(FITlog$residuals,main="Normal Q-Q Plot of Log Model",xlab="Normal Scores",
       ylab="Residuals",pch=19)
legend(-2.45,0.09,expression(paste("P-value ", "Shapiro-Wilk=0.824")))
qqline(FITlog$residuals,lwd=1.5)

SHAPIRO <- shapiro.test(FIT$residuals); SHAPIRO$statistic
SHAPIROLOG <- shapiro.test(FITlog$residuals); SHAPIROLOG$statistic

#############################################################################
#### Sample Selection for X (size:70,000; 10 times greater than Sample for Y)
#############################################################################

Nh <- as.vector(table(STATE))  #STATEs as STRATA
N <- sum(Nh)
nx <- 70000  #Overall sample size
ALLOC <- strAlloc(n.tot=nx,Nh=Nh,alloc="prop")  #Proportional Allocation
nhx <- floor(ALLOC$nh+0.5) #final selectd sample size

SMP.IDs <- strata(data=FINALDATA,stratanames="STATE",
                  size=nhx,method="srswor")    
SAMPLE <- getdata(FINALDATA,SMP.IDs)  # Output of selected sample

## calculate mean and sample variance
SAMPLEMEANx <- tapply(SAMPLE$TOTEMP07rep,INDEX=SAMPLE$STATE,FUN=mean)
SAMPLEMEANx <- ifelse(SAMPLEMEANx==0,1,SAMPLEMEANx)
SAMPLESDx <- tapply(SAMPLE$TOTEMP07rep,INDEX=SAMPLE$STATE,FUN=sd)
SAMPLEVARx <- (1-(nhx/Nh))*(SAMPLESDx^2/nhx)

## Final Point Estimates
Xmatrix <- as.vector(SAMPLEMEANx)
LOGX <- log(Xmatrix)
VARLOGX <- as.vector(SAMPLEVARx)/(Xmatrix^2)

#########################################
#### Sample Selection for Y (size:7000)
#########################################

## Sample size allocation
Nh <- as.vector(table(STATE))  #STATEs as STRATA
N <- sum(Nh)
n <- 7000  #Overall sample size
ALLOC <- strAlloc(n.tot=n,Nh=Nh,alloc="prop")  #Proportional Allocation
nh <- floor(ALLOC$nh+0.5) #final selectd sample size

## Population char.
Ybarh <- tapply(FINALDATA$TOTEMP12rep,INDEX=FINALDATA$STATE,mean); Ybarh  #True Mean within Stratum
MEANPOP <- as.vector(Ybarh)

Sh <- as.vector(tapply(FINALDATA$TOTEMP12rep,INDEX=FINALDATA$STATE,sd))

VARPOP <- sapply(1:49,function(i){   #Theoretical Variance within stratum
  (1/nh[i]-1/Nh[i])*Sh[i]^2})

## SQRT Var POP for 10 smallest and 10 largest small areas
SORT <- sort(nh)
MATCH <- match(c(SORT[1:10],SORT[40:49]),nh)
round(sqrt(VARPOP[MATCH]),3)
Nh[MATCH]

############################################################
#### Empirical Study for smoothing Direct Variance (GVF)
#### in a simulation study of 1000 replication
#### The relvar region v.s.: 
####                       1. sample size of region
####                       2. population size of region
############################################################

Emp.study1 <- function(){
  ## Sample selection
  
  cat("start rep",date(),"\n")
  
  SMP.IDs <- strata(data=FINALDATA,stratanames="STATE",
                    size=nh,method="srswor")    
  SAMPLE <- getdata(FINALDATA,SMP.IDs)  # Output of selected sample
  
  ## calculate mean and sample variance
  
  SAMPLEMEAN <- tapply(SAMPLE$TOTEMP12rep,INDEX=SAMPLE$STATE,FUN=mean)
  SAMPLEMEAN <- ifelse(SAMPLEMEAN==0,1,SAMPLEMEAN)
  SAMPLESD <- tapply(SAMPLE$TOTEMP12rep,INDEX=SAMPLE$STATE,FUN=sd)
  SAMPLEVAR <- (1-(nh/Nh))*(SAMPLESD^2/nh)
  
  DATASAMPLE <- subset(FINALDATA,select=c(REGION,STATE))
  DATASAMPLE$nh <- rep(nh,Nh)
  DATASAMPLE$Nh <- rep(Nh,Nh)
  DATASAMPLE$ybarh <- rep(SAMPLEMEAN,Nh)
  DATASAMPLE$varybarh <- rep(SAMPLEVAR,Nh)
  DATASAMPLE <- DATASAMPLE[order(DATASAMPLE$STATE),]
  
  Ybarh_REGION <- sapply(1:10,function(i){unique(DATASAMPLE$ybarh[DATASAMPLE$REGION==unique(REGION)[i]])})
  Varybarh_REGION <- sapply(1:10,function(i){unique(DATASAMPLE$varybarh[DATASAMPLE$REGION==unique(REGION)[i]])})
  
  Nh_REGION <- sapply(1:10,function(i){unique(DATASAMPLE$Nh[DATASAMPLE$REGION==unique(REGION)[i]])})
  nh_REGION <- sapply(1:10,function(i){unique(DATASAMPLE$nh[DATASAMPLE$REGION==unique(REGION)[i]])})
  
  NRegion <- sapply(1:10,function(i){sum(Nh_REGION[[i]])})
  nRegion <- sapply(1:10,function(i){sum(nh_REGION[[i]])})
  VARRegion <- sapply(1:10,function(i){sum(Nh_REGION[[i]]^2*Varybarh_REGION[[i]])/NRegion[i]^2})
  YBARRegion <- sapply(1:10,function(i){sum(Nh_REGION[[i]]*Ybarh_REGION[[i]])/NRegion[i]})
  RelVarRegion <- VARRegion/YBARRegion^2
  
  cat("End rep", date(),"\n")
  
  cbind(RelVarRegion,nRegion,NRegion)  #output of loop
  
}
## Replicate 1000 sample:
R <- 1000
Emp.studyR <- replicate(n=R,Emp.study1()); dim(Emp.studyR)

sqrtn <- sapply(1:R,function(i){sqrt(Emp.studyR[,2,i])})  #sqrt of n
invsqrtn <- sapply(1:R,function(i){1/sqrt(Emp.studyR[,2,i])}) #1/sqrt of n
invn <- sapply(1:R,function(i){1/Emp.studyR[,2,i]})  #1/n
dim(sqrtn); dim(invsqrtn); dim(invn)

COEF1n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~Emp.studyR[,2,i]-1)$coef)}) #Relvar vs n

COEF2n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~sqrtn[,i]-1)$coef)}) #Relvar vs sqrt n

COEF3n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invsqrtn[,i]-1)$coef)}) #Relvar vs 1/sqrt of n

COEF4n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invn[,i]-1)$coef)}) #Relvar vs 1/n

mean(COEF1n); mean(COEF2n); mean(COEF3n); mean(COEF4n) 


AdjR1n <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~Emp.studyR[,2,i]-1))$adj.r.squared)}) #Relvar vs n

AdjR2n <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~sqrtn[,i]-1))$adj.r.squared)}) #Relvar vs sqrt n

AdjR3n <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~invsqrtn[,i]-1))$adj.r.squared)}) #Relvar vs 1/sqrt of n

AdjR4n <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~invn[,i]-1))$adj.r.squared)}) #Relvar vs 1/n

mean(AdjR1n); mean(AdjR2n); mean(AdjR3n); mean(AdjR4n)


FITval1n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~Emp.studyR[,2,i]-1)$fitted.values)}) #Relvar vs n

FITval2n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~sqrtn[,i]-1)$fitted.values)}) #Relvar vs sqrt n

FITval3n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invsqrtn[,i]-1)$fitted.values)}) #Relvar vs 1/sqrt of n

FITval4n <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invn[,i]-1)$fitted.values)}) #Relvar vs 1/n


def.par <- par(no.readonly = TRUE)

layout(matrix(c(1,2,3,4,5,5), nrow=3, byrow=TRUE),heights = c(2,2))

par(mai=rep(0.7, 4))

matplot(Emp.studyR[,2,1],cbind(Emp.studyR[,1,1],FITval1n[,1]),type="pl",lty=1,pch=19,
        col="black",xlab="n",ylab="RelVar",main="Results of the 1st Replication")
matplot(Emp.studyR[,2,1],cbind(Emp.studyR[,1,1],FITval2n[,1]),type="pl",lty=2,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the 1st Replication")
matplot(Emp.studyR[,2,1],cbind(Emp.studyR[,1,1],FITval3n[,1]),type="pl",lty=3,pch=19,add=T,
        col="black",xlab="n",ylab="RlVar",main="Results of the 1st Replication")
matplot(Emp.studyR[,2,1],cbind(Emp.studyR[,1,1],FITval4n[,1]),type="pl",lty=5,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the 1st Replication")


matplot(Emp.studyR[,2,2],cbind(Emp.studyR[,1,2],FITval1n[,2]),type="pl",lty=1,pch=19,
        col="black",xlab="n",ylab="RelVar",main="Results of the 2nd Replication")
matplot(Emp.studyR[,2,2],cbind(Emp.studyR[,1,2],FITval2n[,2]),type="pl",lty=2,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the 2nd Replication")
matplot(Emp.studyR[,2,2],cbind(Emp.studyR[,1,2],FITval3n[,2]),type="pl",lty=3,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the 2nd Replication")
matplot(Emp.studyR[,2,2],cbind(Emp.studyR[,1,2],FITval4n[,2]),type="pl",lty=5,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the 2nd Replication")


matplot(Emp.studyR[,2,999],cbind(Emp.studyR[,1,999],FITval1n[,999]),type="pl",lty=1,pch=19,
        col="black",xlab="n",ylab="RelVar",main="Results of the (R-1)-th Replication")
matplot(Emp.studyR[,2,999],cbind(Emp.studyR[,1,999],FITval2n[,999]),type="pl",lty=2,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the (R-1)-th Replication")
matplot(Emp.studyR[,2,999],cbind(Emp.studyR[,1,999],FITval3n[,999]),type="pl",lty=3,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the (R-1)-th Replication")
matplot(Emp.studyR[,2,999],cbind(Emp.studyR[,1,999],FITval4n[,999]),type="pl",lty=5,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the (R-1)-th Replication")


matplot(Emp.studyR[,2,1000],cbind(Emp.studyR[,1,1000],FITval1n[,1000]),type="pl",lty=1,pch=19,
        col="black",xlab="n",ylab="RelVar",main="Results of the R-th Replication")
matplot(Emp.studyR[,2,1000],cbind(Emp.studyR[,1,1000],FITval2n[,1000]),type="pl",lty=2,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the R-th Replication")
matplot(Emp.studyR[,2,1000],cbind(Emp.studyR[,1,1000],FITval3n[,1000]),type="pl",lty=3,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the R-th Replication")
matplot(Emp.studyR[,2,1000],cbind(Emp.studyR[,1,1000],FITval4n[,1000]),type="pl",lty=5,pch=19,add=T,
        col="black",xlab="n",ylab="RelVar",main="Results of the R-th Replication")

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", ncol=4,legend=c(expression(n),expression(sqrt(n)),expression(1/sqrt(n)),
                                   expression(1/n)),
       lty=c(1,2,3,5), title="Regression Fitted Line")


sqrtN <- sapply(1:R,function(i){sqrt(Emp.studyR[,3,i])})  #sqrt of N
invsqrtN <- sapply(1:R,function(i){1/sqrt(Emp.studyR[,3,i])}) #1/sqrt of N
invN <- sapply(1:R,function(i){1/Emp.studyR[,3,i]})  #1/N
dim(sqrtN); dim(invsqrtN); dim(invN)

COEF1N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~Emp.studyR[,3,i]-1)$coef)}) #Relvar vs N

COEF2N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~sqrtN[,i]-1)$coef)}) #Relvar vs sqrt N

COEF3N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invsqrtN[,i]-1)$coef)}) #Relvar vs 1/sqrt of N

COEF4N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invN[,i]-1)$coef)}) #Relvar vs 1/N

mean(COEF1N); mean(COEF2N); mean(COEF3N); mean(COEF4N) 


AdjR1N <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~Emp.studyR[,3,i]-1))$adj.r.squared)}) #Relvar vs N

AdjR2N <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~sqrtN[,i]-1))$adj.r.squared)}) #Relvar vs sqrt N

AdjR3N <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~invsqrtN[,i]-1))$adj.r.squared)}) #Relvar vs 1/sqrt of N

AdjR4N <- sapply(1:R,function(i){
  as.numeric(summary(lm(Emp.studyR[,1,i]~invN[,i]-1))$adj.r.squared)}) #Relvar vs 1/N

mean(AdjR1N); mean(AdjR2N); mean(AdjR3N); mean(AdjR4N)


FITval1N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~Emp.studyR[,3,i]-1)$fitted.values)}) #Relvar vs N

FITval2N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~sqrtN[,i]-1)$fitted.values)}) #Relvar vs sqrt N

FITval3N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invsqrtN[,i]-1)$fitted.values)}) #Relvar vs 1/sqrt of N

FITval4N <- sapply(1:R,function(i){
  as.numeric(lm(Emp.studyR[,1,i]~invN[,i]-1)$fitted.values)}) #Relvar vs 1/N


def.par <- par(no.readonly = TRUE)

layout(matrix(c(1,2,3,4,5,5), nrow=3, byrow=TRUE),heights = c(2,2))

par(mai=rep(0.7, 4))

matplot(Emp.studyR[,3,1],cbind(Emp.studyR[,1,1],FITval1N[,1]),type="pl",lty=1,pch=19,col="black",
        xlab="N",ylab="RelVar",main="Results of 1st Simulation")
matplot(Emp.studyR[,3,1],cbind(Emp.studyR[,1,1],FITval2N[,1]),type="pl",lty=2,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of 1st Simulation")
matplot(Emp.studyR[,3,1],cbind(Emp.studyR[,1,1],FITval3N[,1]),type="pl",lty=3,pch=19,col="black",add=T,
        xlab="N",ylab="RlVar",main="Results of 1st Simulation")
matplot(Emp.studyR[,3,1],cbind(Emp.studyR[,1,1],FITval4N[,1]),type="pl",lty=5,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of 1st Simulation")


matplot(Emp.studyR[,3,2],cbind(Emp.studyR[,1,2],FITval1N[,2]),type="pl",lty=1,pch=19,col="black",
        xlab="N",ylab="RelVar",main="Results of 2nd Simulation")
matplot(Emp.studyR[,3,2],cbind(Emp.studyR[,1,2],FITval2N[,2]),type="pl",lty=2,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of 2nd Simulation")
matplot(Emp.studyR[,3,2],cbind(Emp.studyR[,1,2],FITval3N[,2]),type="pl",lty=3,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of 2nd Simulation")
matplot(Emp.studyR[,3,2],cbind(Emp.studyR[,1,2],FITval4N[,2]),type="pl",lty=5,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of 2nd Simulation")


matplot(Emp.studyR[,3,999],cbind(Emp.studyR[,1,999],FITval1N[,999]),type="pl",lty=1,pch=19,col="black",
        xlab="N",ylab="RelVar",main="Results of (R-1)-th Simulation")
matplot(Emp.studyR[,3,999],cbind(Emp.studyR[,1,999],FITval2N[,999]),type="pl",lty=2,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of (R-1)-th Simulation")
matplot(Emp.studyR[,3,999],cbind(Emp.studyR[,1,999],FITval3N[,999]),type="pl",lty=3,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of (R-1)-th Simulation")
matplot(Emp.studyR[,3,999],cbind(Emp.studyR[,1,999],FITval4N[,999]),type="pl",lty=5,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of (R-1)-th Simulation")


matplot(Emp.studyR[,3,1000],cbind(Emp.studyR[,1,1000],FITval1N[,1000]),type="pl",lty=1,pch=19,col="black",
        xlab="N",ylab="RelVar",main="Results of R-th Simulation")
matplot(Emp.studyR[,3,1000],cbind(Emp.studyR[,1,1000],FITval2N[,1000]),type="pl",lty=2,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of R-th Simulation")
matplot(Emp.studyR[,3,1000],cbind(Emp.studyR[,1,1000],FITval3N[,1000]),type="pl",lty=3,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of R-th Simulation")
matplot(Emp.studyR[,3,1000],cbind(Emp.studyR[,1,1000],FITval4N[,1000]),type="pl",lty=5,pch=19,col="black",add=T,
        xlab="N",ylab="RelVar",main="Results of R-th Simulation")

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", ncol=4,legend=c(expression(N),expression(sqrt(N)),expression(1/sqrt(N)),
                                   expression(1/N)),
       lty=c(1,2,3,5), title="Regression Fitted Line")


########################################################
#### Simulation Study for Direct, EB, and HB
#### Intro. Repetition Loop
########################################################

one.rep <- function(){
  ## Sample selection
  SMP.IDs <- strata(data=FINALDATA,stratanames="STATE",
                    size=nh,method="srswor")    
  SAMPLE <- getdata(FINALDATA,SMP.IDs)  #Output of selected sample
  
  
  ########################################################
  #### Direct Estimator
  ########################################################
  
  SAMPLEMEAN <- tapply(SAMPLE$TOTEMP12rep,INDEX=SAMPLE$STATE,FUN=mean)
  SAMPLEMEAN <- ifelse(SAMPLEMEAN==0,1,SAMPLEMEAN)
  SAMPLESD <- tapply(SAMPLE$TOTEMP12rep,INDEX=SAMPLE$STATE,FUN=sd)
  SAMPLEVAR <- (1-(nh/Nh))*(SAMPLESD^2/nh)
  
  
  coefGVF <- 22.51  #coefficient from GVF empirical study (mean(COEF4n))
  
  ## For the Log transformation
  LOGY <- log(SAMPLEMEAN) 
  VARLOGY <- (1/nh)*coefGVF   #Applying GVF technique
  
  
  #################################################
  #### Empirical Bayes with Measurement Error
  #################################################
  
  ## Scoring Algorithm for estimating
  ## unknown parameters (Beta & sigma2v)
  
  FINALFIT <- lm(LOGY~LOGX-1) #intercept is not significant
  Beta <- FINALFIT$coeff   #init value
  sigma2v <- sum(FINALFIT$res^2)/(49-1)  #init value
  r <- 1
  repeat{
    #First derivative of log likelihood with respect to the sigma2v
    TERM1 <- (-1/2)*sum((Beta*VARLOGX*Beta+sigma2v+VARLOGY)^(-1))
    TERM2 <- (1/2)*sum((LOGY-LOGX*Beta)^2/(Beta*VARLOGX*Beta+sigma2v+VARLOGY)^2)
    GradianL <- TERM1+TERM2   
    
    #Expectation of second derivative of log likelihiood with respect to the sigma2v 
    Fisher <- (1/2)*sum(1/(Beta*VARLOGX*Beta+sigma2v+VARLOGY)^2)   
    
    sigma2v1 <- sigma2v+(GradianL/Fisher)
    
    Beta1 <- sum((LOGX*LOGX-VARLOGX)/(Beta*VARLOGX*Beta+sigma2v1+VARLOGY))^(-1)*
      sum((LOGX*LOGY)/(Beta*VARLOGX*Beta+sigma2v1+VARLOGY))
    
    dif1 <- sigma2v1-sigma2v
    dif2 <- Beta1-Beta
    
    sigma2v <- sigma2v1
    sigma2v <- max(0,sigma2v)
    Beta <- Beta1
    
    if(dif1&&dif2<10^(-8)) break
    r=r+1
  }
  sigma2v.opt <- sigma2v
  Beta.opt <- Beta
  
  ## Adjusted Backtransformed EB
  gamma.star <- (sigma2v.opt+(Beta.opt*VARLOGX*Beta.opt))/(sigma2v.opt+(Beta.opt*VARLOGX*Beta.opt)+VARLOGY)
  ThetaEBwME <- as.vector(exp((gamma.star*LOGY)+((1-gamma.star)*LOGX*Beta.opt)+(VARLOGY*gamma.star/2)))
  
  
  ## MSEwME
  M1i <- as.vector(exp(VARLOGY*gamma.star)*(exp(VARLOGY*gamma.star)-1)*(exp(2*((gamma.star*LOGY)+((1-gamma.star)*(LOGX*Beta.opt))))))
  
  Beta_del <- Beta.opt_del <- rep(0,49)
  sigma2v_del <- sigma2v.opt_del <- rep(0,49)
  for(u in 1:49){
    LOGXn <- LOGX[-u]
    LOGYn <- LOGY[-u]
    VARLOGX_del <- VARLOGX[-u]
    VARLOGY_del <- VARLOGY[-u]
    Beta_del[u] <- lm(LOGYn~LOGXn-1)$coeff
    sigma2v_del[u] <- sum(lm(LOGYn~LOGXn-1)$res^2)/(48-1) 
    
    r <- 1
    repeat
    {
      TERM1 <- (-1/2)*sum(sapply(1:48,function(i){((Beta_del[u]*VARLOGX_del[i]*Beta_del[u])+sigma2v_del[u]+VARLOGY_del[i])^(-1)}))
      TERM2 <- (1/2)*sum(sapply(1:48,function(i){(LOGYn[i]-LOGXn[i]*Beta_del[u])^2/((Beta_del[u]*VARLOGX_del[i]*Beta_del[u])+sigma2v_del[u]+VARLOGY_del[i])^2}))
      
      GradianL <- TERM1+TERM2
      Fisher <- (1/2)*sum(sapply(1:48,function(i){1/((Beta_del[u]*VARLOGX_del[i]*Beta_del[u])+VARLOGY_del[i]+sigma2v_del[u])^2}))
      
      sigma2v1 <- sigma2v_del[u]+(GradianL/Fisher)
      
      Beta1 <- sum((LOGXn*LOGXn-VARLOGX_del)/(Beta_del[u]*VARLOGX_del*Beta_del[u]+sigma2v1+VARLOGY_del))^(-1)*
        sum((LOGXn*LOGYn)/(Beta_del[u]*VARLOGX_del*Beta_del[u]+sigma2v1+VARLOGY_del))
      
      dif1 <- sigma2v1-sigma2v_del[u]
      dif2 <- Beta1-Beta_del[u]
      
      sigma2v_del[u] <- sigma2v1
      sigma2v_del[u] <- max(0,sigma2v_del[u])
      Beta_del[u] <- Beta1
      
      if(dif1&&dif2<10^(-8)) break
      r=r+1
      
    }
    sigma2v.opt_del[u] <- sigma2v_del[u]
    Beta.opt_del[u] <- Beta_del[u]
  }
  
  gamma.star_del <- matrix(0,49,49) #Each row belongs to an area
  for(u in 1:49){
    gamma.star_del[u,] <- sapply(1:49,function(i){(sigma2v.opt_del[i]+Beta.opt_del[i]*VARLOGX[u]*Beta.opt_del[i])/
        (sigma2v.opt_del[i]+Beta.opt_del[i]*VARLOGX[u]*Beta.opt_del[i]+VARLOGY[u])})
  }
  
  M1i_del <- matrix(0,49,49) #Each row belongs to an area
  for(u in 1:49){
    M1i_del[u,] <- exp(VARLOGY[u]*gamma.star_del[u,])*(exp(VARLOGY[u]*gamma.star_del[u,])-1)*(exp(2*((gamma.star_del[u,]*LOGY[u])+((1-gamma.star_del[u,])*(LOGX[u]*Beta.opt_del)))))
  }
  
  Bias.T <- rep(0,49)
  for(u in 1:49){
    Bias.T[u] <- (48/49)*sum(M1i[u]-M1i_del[u,])
  }
  
  Mstar1i <- M1i-Bias.T  
  
  ThetaEBwME_del <- matrix(0,49,49) #Each row belongs to an area
  for(u in 1:49){
    ThetaEBwME_del[u,] <- exp((gamma.star_del[u,]*LOGY[u])+((1-gamma.star_del[u,])*(LOGX[u]*Beta.opt_del))+((VARLOGY[u]*gamma.star_del[u,])/2))
    
  }
  
  M2i <- rep(0,49)
  for(u in 1:49){
    M2i[u] <- (48/49)*sum((ThetaEBwME[u]-ThetaEBwME_del[u,])^2)
  }
  
  MSEwME <- Mstar1i+M2i
  
  
  #################################################
  #### Empirical Bayes without Measurement Error
  #################################################
  
  ## Scoring Algorithm for estimating
  ## unknown parameters (Beta & sigma2v)
  
  FINALFIT <- lm(LOGY~LOGX-1)
  Beta <- FINALFIT$coeff   #init value
  sigma2v <- sum(FINALFIT$res^2)/(49-1)  #init value
  r <- 1
  repeat{
    #First derivative of log likelihood
    TERM1 <- (-1/2)*sum((sigma2v+VARLOGY)^(-1))
    TERM2 <- (1/2)*sum((LOGY-LOGX*Beta)^2/(sigma2v+VARLOGY)^2)
    GradianL <- TERM1+TERM2   
    
    #Expectation of second derivative of log likelihiood  
    Fisher <- (1/2)*sum(1/(sigma2v+VARLOGY)^2)   
    
    sigma2v1 <- sigma2v+(GradianL/Fisher)
    
    Beta1 <- sum((LOGX*LOGX)/(sigma2v1+VARLOGY))^(-1)*
      sum((LOGX*LOGY)/(sigma2v1+VARLOGY))
    
    dif1 <- sigma2v1-sigma2v
    dif2 <- Beta1-Beta
    
    sigma2v <- sigma2v1
    sigma2v <- max(0,sigma2v)
    Beta <- Beta1
    
    if(dif1&&dif2<10^(-8)) break
    r=r+1
  }
  sigma2v.opt <- sigma2v
  Beta.opt <- Beta
  
  ## Adjusted Backtransformed EB
  gamma.star <- (sigma2v.opt)/(sigma2v.opt+VARLOGY)
  ThetaEBwoME <- as.vector(exp((gamma.star*LOGY)+((1-gamma.star)*LOGX*Beta.opt)+(VARLOGY*gamma.star/2)))
  
  
  ## MSEwoME
  M1i <- as.vector(exp(VARLOGY*gamma.star)*(exp(VARLOGY*gamma.star)-1)*(exp(2*((gamma.star*LOGY)+((1-gamma.star)*(LOGX*Beta.opt))))))
  
  Beta_del <- Beta.opt_del <- rep(0,49)
  sigma2v_del <- sigma2v.opt_del <- rep(0,49)
  for(u in 1:49){
    LOGXn <- LOGX[-u]
    LOGYn <- LOGY[-u]
    VARLOGX_del <- VARLOGX[-u]
    VARLOGY_del <- VARLOGY[-u]
    Beta_del[u] <- lm(LOGYn~LOGXn-1)$coeff
    sigma2v_del[u] <- sum(lm(LOGYn~LOGXn-1)$res^2)/(48-1) 
    
    r <- 1
    repeat
    {
      TERM1 <- (-1/2)*sum(sapply(1:48,function(i){(sigma2v_del[u]+VARLOGY_del[i])^(-1)}))
      TERM2 <- (1/2)*sum(sapply(1:48,function(i){(LOGYn[i]-LOGXn[i]*Beta_del[u])^2/(sigma2v_del[u]+VARLOGY_del[i])^2}))
      
      GradianL <- TERM1+TERM2
      Fisher <- (1/2)*sum(sapply(1:48,function(i){1/(VARLOGY_del[i]+sigma2v_del[u])^2}))
      
      sigma2v1 <- sigma2v_del[u]+(GradianL/Fisher)
      
      Beta1 <- sum((LOGXn*LOGXn)/(sigma2v1+VARLOGY_del))^(-1)*
        sum((LOGXn*LOGYn)/(sigma2v1+VARLOGY_del))
      
      dif1 <- sigma2v1-sigma2v_del[u]
      dif2 <- Beta1-Beta_del[u]
      
      sigma2v_del[u] <- sigma2v1
      sigma2v_del[u] <- max(0,sigma2v_del[u])
      Beta_del[u] <- Beta1
      
      if(dif1&&dif2<10^(-8)) break
      r=r+1
      
    }
    sigma2v.opt_del[u] <- sigma2v_del[u]
    Beta.opt_del[u] <- Beta_del[u]
  }
  
  gamma.star_del <- matrix(0,49,49) #Each row belongs to an area
  for(u in 1:49){
    gamma.star_del[u,] <- sapply(1:49,function(i){(sigma2v.opt_del[i])/
        (sigma2v.opt_del[i]+VARLOGY[u])})
  }
  
  M1i_del <- matrix(0,49,49) #Each row belongs to an area
  for(u in 1:49){
    M1i_del[u,] <- exp(VARLOGY[u]*gamma.star_del[u,])*(exp(VARLOGY[u]*gamma.star_del[u,])-1)*(exp(2*((gamma.star_del[u,]*LOGY[u])+((1-gamma.star_del[u,])*(LOGX[u]*Beta.opt_del)))))
  }
  
  Bias.T <- rep(0,49)
  for(u in 1:49){
    Bias.T[u] <- (48/49)*sum(M1i[u]-M1i_del[u,])
  }
  
  Mstar1i <- M1i-Bias.T  
  
  ThetaEBwoME_del <- matrix(0,49,49) #Each row belongs to an area
  for(u in 1:49){
    ThetaEBwoME_del[u,] <- exp((gamma.star_del[u,]*LOGY[u])+((1-gamma.star_del[u,])*(LOGX[u]*Beta.opt_del))+((VARLOGY[u]*gamma.star_del[u,])/2))
    
  }
  
  M2i <- rep(0,49)
  for(u in 1:49){
    M2i[u] <- (48/49)*sum((ThetaEBwoME[u]-ThetaEBwoME_del[u,])^2)
  }
  
  MSEwoME <- Mstar1i+M2i
  
  
  #################################################
  #### Hierarchical Bayes with Measurement Error
  #################################################
  
  Nsim <- 10000
  
  FINALFIT <- lm(LOGY~LOGX-1)
  Beta <- FINALFIT$coeff   #init value
  sigma2v <- sum(FINALFIT$res^2)/(49-1)  #init value
  
  #init values of Gibbs sampler chain
  THETAHB1 <- matrix(rlnorm(1,(sigma2v/(sigma2v+VARLOGY))*LOGY+(VARLOGY/(sigma2v+VARLOGY))*LOGX*Beta,
                            sqrt((sigma2v*VARLOGY)/(sigma2v+VARLOGY))),Nsim,49) #Each col. represents an area
  
  X <- matrix(rlnorm(1,LOGX+((LOGY-LOGX*Beta)/(VARLOGY+sigma2v+Beta*VARLOGX*Beta))*VARLOGX*Beta,
                     sqrt(VARLOGX-(VARLOGX*Beta*Beta*VARLOGX)/(VARLOGY+sigma2v+Beta*VARLOGX*Beta))),Nsim,49) 
  
  BETA <- rep(rnorm(1,((t(matrix(LOGX,ncol=1))%*%matrix(LOGX,ncol=1))^(-1))%*%t(matrix(LOGX,ncol=1))%*%matrix(LOGY,ncol=1),
                    sqrt(sigma2v*((t(matrix(LOGX,ncol=1))%*%matrix(LOGX,ncol=1))^(-1)))),Nsim)
  
  m <- 49
  SIGMA2V <- rep(rinvgamma(1,shape=(m-2)/2,scale=sum((LOGY-LOGX*Beta)^2)/2),Nsim)
  
  ## Gibbs Loop
  
  for(ng in 2:Nsim){
    THETAHB1[ng,] <- sapply(1:49,function(i){rlnorm(1,(SIGMA2V[ng-1]/(SIGMA2V[ng-1]+VARLOGY[i]))*LOGY[i]+(VARLOGY[i]/(SIGMA2V[ng-1]+VARLOGY[i]))*log(X[ng-1,i])*BETA[ng-1],
                                                    sqrt((SIGMA2V[ng-1]*VARLOGY[i])/(SIGMA2V[ng-1]+VARLOGY[i])))}) #Each col. represents an area
    
    X[ng,] <- sapply(1:49,function(i){rlnorm(1,LOGX[i]+((LOGY[i]-LOGX[i]*BETA[ng-1])/(VARLOGY[i]+SIGMA2V[ng-1]+BETA[ng-1]*VARLOGX[i]*BETA[ng-1]))*VARLOGX[i]*BETA[ng-1],
                                             sqrt(VARLOGX[i]-(VARLOGX[i]*BETA[ng-1]*BETA[ng-1]*VARLOGX[i])/(VARLOGY[i]+SIGMA2V[ng-1]+BETA[ng-1]*VARLOGX[i]*BETA[ng-1])))})
    
    BETA[ng] <- rnorm(1,((t(matrix(log(X[ng,]),ncol=1))%*%matrix(log(X[ng,]),ncol=1))^(-1))%*%t(matrix(log(X[ng,]),ncol=1))%*%matrix(log(THETAHB1[ng,]),ncol=1),
                      sqrt(SIGMA2V[ng-1]*((t(matrix(log(X[ng,]),ncol=1))%*%matrix(log(X[ng,]),ncol=1))^(-1)))) 
    
    SIGMA2V[ng] <- rinvgamma(1,shape=(m-2)/2,scale=sum((log(THETAHB1[ng,])-log(X[ng,])*BETA[ng])^2)/2) 
  }
  
  THETAHB1 <- THETAHB1[5001:10000,]
  ThetaHBwME <- apply(THETAHB1,2,mean) 
  PostVARwME <- apply(THETAHB1,2,var) 
  LowerwME <- apply(THETAHB1,2,function(x)quantile(x,0.25))
  UpperwME <- apply(THETAHB1,2,function(x)quantile(x,0.75))
  
  ###################################################
  #### Hierarchical Bayes without Measurement Error
  ###################################################
  
  Nsim <- 10000
  
  FINALFIT <- lm(LOGY~LOGX-1)
  Beta <- FINALFIT$coeff   #init value
  sigma2v <- sum(FINALFIT$res^2)/(49-1)  #init value
  
  #init values of Gibbs sampler chain
  THETAHB2 <- matrix(rlnorm(1,(sigma2v/(sigma2v+VARLOGY))*LOGY+(VARLOGY/(sigma2v+VARLOGY))*LOGX*Beta,
                            sqrt((sigma2v*VARLOGY)/(sigma2v+VARLOGY))),Nsim,49) #Each col. represents an area
  
  BETA <- rep(rnorm(1,((t(matrix(LOGX,ncol=1))%*%matrix(LOGX,ncol=1))^(-1))%*%t(matrix(LOGX,ncol=1))%*%matrix(LOGY,ncol=1),
                    sqrt(sigma2v*((t(matrix(LOGX,ncol=1))%*%matrix(LOGX,ncol=1))^(-1)))),Nsim)
  
  m <- 49
  SIGMA2V <- rep(rinvgamma(1,shape=(m-2)/2,scale=sum((LOGY-LOGX*Beta)^2)/2),Nsim)
  
  ## Gibbs Loop
  
  for(ng in 2:Nsim){
    THETAHB2[ng,] <- sapply(1:49,function(i){rlnorm(1,(SIGMA2V[ng-1]/(SIGMA2V[ng-1]+VARLOGY[i]))*LOGY[i]+(VARLOGY[i]/(SIGMA2V[ng-1]+VARLOGY[i]))*LOGX[i]*BETA[ng-1],
                                                    sqrt((SIGMA2V[ng-1]*VARLOGY[i])/(SIGMA2V[ng-1]+VARLOGY[i])))}) #Each col. represents an area
    
    BETA[ng] <- rnorm(1,((t(matrix(LOGX,ncol=1))%*%matrix(LOGX,ncol=1))^(-1))%*%t(matrix(LOGX,ncol=1))%*%matrix(log(THETAHB2[ng,]),ncol=1),
                      sqrt(SIGMA2V[ng-1]*((t(matrix(LOGX,ncol=1))%*%matrix(LOGX,ncol=1))^(-1)))) 
    
    SIGMA2V[ng] <- rinvgamma(1,shape=(m-2)/2,scale=sum((log(THETAHB2[ng,])-LOGX*BETA[ng])^2)/2) 
  }
  
  THETAHB2 <- THETAHB2[5001:10000,]
  ThetaHBwoME <- apply(THETAHB2,2,mean) 
  PostVARwoME <- apply(THETAHB2,2,var) 
  LowerwoME <- apply(THETAHB2,2,function(x)quantile(x,0.25))
  UpperwoME <- apply(THETAHB2,2,function(x)quantile(x,0.75))
  
  
  ################################################
  #### Final Output of Loop
  ################################################
  
  cbind(SAMPLEMEAN,SAMPLEVAR,ThetaEBwME,MSEwME,
        ThetaEBwoME,MSEwoME,
        ThetaHBwME,PostVARwME,LowerwME,UpperwME,
        ThetaHBwoME,PostVARwoME,LowerwoME,UpperwoME) 
  
}

## Replicate 1000 sample:
R <- 1000
many.reps <- replicate(n=R,one.rep()); dim(many.reps)


#####################################################
#### Comparisons
#####################################################

## Empirical Average of Relative Bias: (E(estimator-True)/True)*100

RelBias_Direct <- sapply(1:49,function(i){(mean(many.reps[i,1,]-MEANPOP[i])/MEANPOP[i])*100}) 

RelBias_EBwME <- sapply(1:49,function(i){(mean(many.reps[i,3,]-MEANPOP[i])/MEANPOP[i])*100}) 

RelBias_EBwoME <- sapply(1:49,function(i){(mean(many.reps[i,5,]-MEANPOP[i])/MEANPOP[i])*100}) 

RelBias_HBwME <- sapply(1:49,function(i){(mean(many.reps[i,7,]-MEANPOP[i])/MEANPOP[i])*100}) 

RelBias_HBwoME <- sapply(1:49,function(i){(mean(many.reps[i,11,]-MEANPOP[i])/MEANPOP[i])*100})

RelBias <- cbind(RelBias_Direct,RelBias_EBwME,RelBias_EBwoME,RelBias_HBwME,RelBias_HBwoME)
RelBias

## Strip Chart
par(mfrow=c(1,1))
colnames(RelBias) <- c("Direct","EBwME","EBwoME","HBwME","HBwoME") 
stripchart(data.frame(scale(RelBias)),method="jitter",las=1.5,vertical=TRUE,
           pch=c(15,18,17,18,17),cex=1.5)

## True Mean Squared Error Comparison: E((estimator-True)^2)

TrueMSE_Direct <- sapply(1:49,function(i){mean((many.reps[i,1,]-MEANPOP[i])^2)}) 

TrueMSE_EBwME <- sapply(1:49,function(i){mean((many.reps[i,3,]-MEANPOP[i])^2)}) 

TrueMSE_EBwoME <- sapply(1:49,function(i){mean((many.reps[i,5,]-MEANPOP[i])^2)}) 

TrueMSE_HBwME <- sapply(1:49,function(i){mean((many.reps[i,7,]-MEANPOP[i])^2)}) 

TrueMSE_HBwoME <- sapply(1:49,function(i){mean((many.reps[i,11,]-MEANPOP[i])^2)})

TrueMSE <- cbind(TrueMSE_Direct,TrueMSE_EBwME,TrueMSE_EBwoME,TrueMSE_HBwME,TrueMSE_HBwoME)
TrueMSE
SQRTMSE <- sqrt(TrueMSE)

## SQRTMSE for 10 smallest and largest sample size (SAEs)
MATCH
round(SQRTMSE[c(MATCH),],3)