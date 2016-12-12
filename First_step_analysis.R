library(lme4) # fit the model
library(lsmeans) # analysing the means of factor levels
library(car) # for using the 'Anova' function 
library(Hmisc) #for plotting means and error bars

###!!! Different file have been used with the following order!!!
#1 1k11k41k7
#2 Diam_Tube_Data
#3 vvo_conc_to_analys
#4 Category_vs_category
#5

############################################################################
############################size of the pot test############################
############################################################################


datares<-read.table(file.choose(),sep=";",dec=".",h=T)
datapots<-datares[complete.cases(datares),] # remove NA lines in the dataset

names(datapots)


datapots$ugC6H12O6<-datapots$vol_uL_Clean*datapots$X./100
datapots$ratiodiam<-datapots$DIAM/datapots$TUBE

which(datapots$DIAM==72.35)
potsclean<-datapots[-137,]# Erase the cleaning of data
potsclean<-potsclean[-172,]
potsclean<-potsclean[-174,]
potsclean<-potsclean[-173,]
potsclean<-potsclean[-148,]
potsclean<-potsclean[-22,]
potsclean<-potsclean[-150,]
potsclean<-potsclean[-151,]
potsclean<-potsclean[-154,]
potsclean<-potsclean[-131,]
potsclean<-potsclean[-116,]
potsclean<-potsclean[-120,]
potsclean<-potsclean[-76,]
potsclean<-potsclean[-74,]
potsclean<-potsclean[-63,]

summary(potsclean$FAMILY)
fam_TO_ERASE<-summary(potsclean$FAMILY)<5

rowerased<-c(which(potsclean$FAMILY=="02A"),which(potsclean$FAMILY=="04A"),which(potsclean$FAMILY=="15A"),
which(potsclean$FAMILY=="18A"),which(potsclean$FAMILY=="30A"),which(potsclean$FAMILY=="31A"),
which(potsclean$FAMILY=="32A"),which(potsclean$FAMILY=="34A"),which(potsclean$FAMILY=="36A"),which(potsclean$FAMILY=="40A")
,which(potsclean$FAMILY=="43A"))

potsclean<- potsclean[-rowerased,]

summary(potsclean$FAMILY)



max(datapots$ratiodiam)


par(mfrow=c(2,2))
plot(lm(log(potsclean$ratiodiam)~potsclean$Type+potsclean$FAMILY+potsclean$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables

par(mfrow=c(2,2))
plot(lm(log(potsclean$vol_uL_Clean)~potsclean$Type+potsclean$FAMILY+potsclean$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables

par(mfrow=c(2,2))
plot(lm(log(potsclean$ugC6H12O6)~potsclean$Type+potsclean$FAMILY+potsclean$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables


### distribution of resp var

min(potsclean$ratiodiam)
max(potsclean$ratiodiam)
par(mfrow=c(2,2))
hist(log(potsclean$ratiodiam),
	breaks=60,
	freq=FALSE,
	probability=TRUE, 
	col="dimgray",xlim=c(0,1), 
	border="white",
	main="Distribution of diameter of the flower length", 
	xlab='Flower diameter length (mm)',
	ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(RATIODIAM) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0,1, 0.01), mean=mean(log(potsclean$ratiodiam)), sd=sd(log(potsclean$ratiodiam))) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0,1, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

min(potsclean$vol_uL_Clean)
max(potsclean$vol_uL_Clean)

hist(potsclean$vol_uL_Clean,
	breaks=60,
	freq=FALSE,
	probability=TRUE, 
	col="dimgray",
	xlim=c(0,2), 
	border="white",
	main="Distribution of the volume production", 
	xlab='Nectar volume (uL)',
	ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(potsclean$vol_uL_Clean) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0,2, 0.01), mean=mean(potsclean$vol_uL_Clean), sd=sd(potsclean$vol_uL_Clean)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0,2, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

min(potsclean$X.)
max(potsclean$X.)

hist((potsclean$X.)/100,
	breaks=60,
	freq=FALSE,
	probability=TRUE, 
	col="dimgray",
	xlim=c(0,1.25), 
	border="white",main="Distribution of sugar production", 
	xlab='Sugar (ug)',
	ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density((potsclean$X.)/100) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0,1.25, 0.01), mean=mean((potsclean$X.)/100), sd=sd((potsclean$X.)/100)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0,1.25, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )


min(potsclean$ugC6H12O6)
max(potsclean$ugC6H12O6)

hist(potsclean$ugC6H12O6,
	breaks=60,
	freq=FALSE,
	probability=TRUE, 
	col="dimgray",
	xlim=c(0,1.25), 
	border="white",main="Distribution of sugar production", 
	xlab='Sugar (ug)',
	ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(potsclean$ugC6H12O6) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0,1.25, 0.01), mean=mean(potsclean$ugC6H12O6), sd=sd(potsclean$ugC6H12O6)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0,1.25, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )


is.factor(potsclean$Pos_rel)
potsclean$pos_fact<-as.factor(potsclean$Pos_rel)

ratiodiamod<-lmer(log(ratiodiam)~ Type+(1|FAMILY)+(1|ID_IND)+(1|pos_fact), data=potsclean)
volmod<-lmer(vol_uL_Clean~ Type+(1|FAMILY)+(1|ID_IND)+(1|pos_fact), data=potsclean)
CONCENTRAZmod<-lmer(X./100~ Type+(1|FAMILY)+(1|ID_IND)+(1|pos_fact), data=potsclean)
sugarmod<-lmer(ugC6H12O6~ Type+(1|FAMILY)+(1|ID_IND)+(1|pos_fact), data=potsclean)

Anova(ratiodiamod,type="2")
Anova(volmod,type="2")
Anova(CONCENTRAZmod,type="2")
Anova(sugarmod,type="2")

LsMratio<-lsmeans(ratiodiamod, ~Type,test='Chi') # explore the lsmeans
Lsvol<-lsmeans(volmod, ~Type,test='Chi') # explore the lsmeans
Lsconc<-lsmeans(CONCENTRAZmod, ~Type,test='Chi') # explore the lsmeans
LsSugar<-lsmeans(sugarmod, ~Type,test='Chi') # explore the lsmeans

ratiomod<-summary(LsMratio, type="response")
ratiomod$EJEX <- c(1,2,3)
ratiomod$Labels<-c("ak","bk","ck")
ratiomod

volmod<-summary(Lsvol, type="response")
volmod$EJEX <- c(1,2,3)
volmod$Labels<-c("ak","bk","ck")
volmod


concmod<-summary(Lsconc, type="response")
concmod$EJEX <- c(1,2,3)
concmod$Labels<-c("ak","bk","ck")
concmod

sugarmod<-summary(LsSugar, type="response")
sugarmod$EJEX <- c(1,2,3)
sugarmod$Labels<-c("ak","bk","ck")
sugarmod


par(mfrow=c(2,2))
errbar(ratiomod$EJEX, ratiomod$lsresponse, ratiomod$upper.CL, ratiomod$lower.CL,
       ylim=c(1.3,1.9),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="ln(diam./tube)",
       xlab="Pot size",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(ratiomod$EJEX,ratiomod$lsresponse, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response of diameter and tube to density of related plants")
axis(1, at=c(1,2,3), labels=c("1 Pot", "1/4 pot", "1/7 pot"), tick=FALSE) 
text(0.75,1.9, "a)",cex=1.25)


errbar(volmod$EJEX, volmod$lsmean, volmod$upper.CL, volmod$lower.CL,
       ylim=c(0.7,1.1),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Volume",
       xlab="Pot size",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(volmod$EJEX,volmod$lsmean, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response of volume production to density of related plants")
axis(1, at=c(1,2,3), labels=c("1 Pot", "1/4 pot", "1/7 pot"), tick=FALSE) 
text(0.75,1.1, "b)",cex=1.25)



errbar(concmod$EJEX, concmod$lsresponse, concmod$upper.CL, concmod$lower.CL,
       ylim=c(0.575,0.7),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar production",
	 xlab="Pot size",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(concmod$EJEX,concmod$lsresponse, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response of sugar production to density of related plants")
axis(1, at=c(1,2,3), labels=c("1 Pot", "1/4 pot", "1/7 pot"), tick=FALSE) 
text(0.75,0.75, "c)",cex=1.25)



errbar(sugarmod$EJEX, sugarmod$lsmean, sugarmod$upper.CL, sugarmod$lower.CL,
       ylim=c(0.4,0.75),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar production",
	 xlab="Pot size",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(sugarmod$EJEX,sugarmod$lsmean, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response of sugar production to density of related plants")
axis(1, at=c(1,2,3), labels=c("1 Pot", "1/4 pot", "1/7 pot"), tick=FALSE) 
text(0.75,0.75, "d)",cex=1.25)


##############################################
############################################## Family analysis
##############################################

ratioFAM<-lmer(log(ratiodiam)~ FAMILY+(1|ID_IND)+(1|pos_fact), data=potsclean)
volFAM<-lmer(vol_uL_Clean~ FAMILY+(1|ID_IND)+(1|pos_fact), data=potsclean)
concFAM<-lmer(X.~ FAMILY+(1|ID_IND)+(1|pos_fact), data=potsclean)
sugarFAM<-lmer(ugC6H12O6~ FAMILY+(1|ID_IND)+(1|pos_fact), data=potsclean)



Anova(ratioFAM)
ratioFAMtest<-glm(ratioFAM)
ratiores<-summary(ratioFAMtest)
write.csv(capture.output (ratiores), file="C:/Utenti/Nico/Desktopresultratio1.csv")


Anova(volFAM)
volFAMtest<-glm(volFAM)
volres<-summary(volFAMtest)
write.csv(capture.output (volres), file="resultvol1.csv")


Anova(concFAM)
concFAMtest<-glm(concFAM)
resconc<-summary(concFAMtest)
write.csv(capture.output (resconc), file="resultconc1.csv")


Anova(sugarFAM)
sugarFAMtest<-glm(sugarFAM)
ressugar<-summary(sugarFAMtest)
write.csv(capture.output (ressugar), file="resultsugar1.csv")


############################################################################
#######################FLORAL TRAIT TUBE AND DIAMETER#######################
############################################################################

### data entering

data2<-read.table(file.choose(),sep=";",dec=".",h=T)

### data cleaning
datatube<-data2[complete.cases(data2),] # remove NA lines in the dataset



##EXPPLORING THE DISTRIBUTIONS AND THE VAIANCE###

attach(datatube)
names(datatube)
head(datatube)


### test on the normality of data

par(mfrow=c(2,2))
plot(lm(DIAM/TUBE~Type+FAMILY+ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables



datafin<-datatube[-370,]# eliminate the line with the outliers identified by the model)
datafin<-datafin[-518,]
datafin<-datafin[-373,]
datafin<-datafin[-418,]
datafin<-datafin[-516,]
datafin<-datafin[-68,]
datafin<-datafin[-444,]
datafin<-datafin[-420,]
datafin<-datafin[-512,]
datafin<-datafin[-511,]
datafin<-datafin[-509,]
datafin<-datafin[-418,]
datafin<-datafin[-441,]
datafin<-datafin[-80,]
datafin<-datafin[-439,]
datafin<-datafin[-454,]
datafin<-datafin[-54,]
datafin<-datafin[-359,]
datafin<-datafin[-84,]
datafin<-datafin[-177,]
datafin<-datafin[-176,]
datafin<-datafin[-145,]
datafin<-datafin[-178,]
datafin<-datafin[-66,]
datafin<-datafin[-174,]
datafin<-datafin[-175,]
datafin<-datafin[-259,]
datafin<-datafin[-159,]
datafin<-datafin[-127,]
datafin<-datafin[-315,]
datafin<-datafin[-255,]
datafin<-datafin[-205,]
datafin<-datafin[-513,]


summary(datafin$FAMILY)
fam_TO_ERASE<-summary(datafin$FAMILY)<5

rowerased<-c(which(datafin$FAMILY=="18A"),which(datafin$FAMILY=="40A"))

datafin<- datafin[-rowerased,]


par(mfrow=c(2,2))
plot(lm(log(datafin$DIAM/datafin$TUBE)~datafin$Type+datafin$FAMILY+datafin$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables

### distribution of resp var


RATIODIAM<-datafin$DIAM/datafin$TUBE
min(RATIODIAM)
max(RATIODIAM)
par(mfrow=c(1,2))
hist(RATIODIAM,breaks=60,freq=FALSE,probability=TRUE, col="dimgray",xlim=c(1,2.6), border="white",main="Distribution of diamtere of the flower length", xlab='Flower diameter length (mm)',ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(RATIODIAM) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(1,2.6, 0.01), mean=mean(RATIODIAM), sd=sd(RATIODIAM)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(1,2.6, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

min(log(RATIODIAM))
max(log(RATIODIAM))

hist(log(RATIODIAM),breaks=60,freq=FALSE,probability=TRUE, col="dimgray",xlim=c(0.15,1), border="white",main="Distribution of diamtere of the flower length", xlab='Flower diameter length (mm)',ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(log(RATIODIAM)) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0.15, 1, 0.01), mean=mean(log(RATIODIAM)), sd=sd(log(RATIODIAM))) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0.15,1, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )



###############################################################################################################################
###############################################################################################################################
###############################################################################################################################

# log norm distribution of resp var

Dnorm<-log(datafin$DIAM)

par(mfrow=c(1,2))
hist(Dnorm,breaks=60,freq=FALSE,probability=TRUE, col="dimgray",xlim=c(2.5,3.75),ylim=c(0,5), border="white",main="Distribution of diamtere of the flower length")
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(Dnorm) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(2.5, 3.75, 0.01), mean=mean(Dnorm), sd=sd(Dnorm)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(2.5, 3.75, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

Tnorm<-log(datafin$TUBE)

hist(Tnorm,breaks=60,freq=FALSE,probability=TRUE, col="dimgray",xlim=c(2.2,3.2),ylim=c(0,6), border="white",main="Distribution of flower tube length")
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(Tnorm) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(2.2,3.2, 0.01), mean=mean(Tnorm), sd=sd(Tnorm)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(2.2,3.2, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

#### Normalised distribution

Tnorm2<-(datafin$TUBE-min(datafin$TUBE))/(max(datafin$TUBE)-min(datafin$TUBE))

Dnorm2<-(datafin$DIAM-min(datafin$DIAM))/(max(datafin$DIAM)-min(datafin$DIAM))


par(mfrow=c(1,2))
hist(datafin$lnRATIODIAM,
	breaks=60,
	freq=FALSE,
	probability=TRUE,
	col="dimgray", 
	border="white",
	xlim=c(0,1),
	ylim=c(0,5),
	main="Distribution of the ratio between flower diameter and tube length")
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(datafin$lnRATIODIAM) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0, 1, 0.01), mean=mean(datafin$lnRATIODIAM), sd=sd(datafin$lnRATIODIAM)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0, 1, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

Tnorm<-log(datafin$TUBE)

hist(Tnorm2,breaks=60,freq=FALSE,probability=TRUE, col="dimgray", border="white",main="Normalized distribution of flower tube length")
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(Tnorm2) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0,1, 0.01), mean=mean(Tnorm2), sd=sd(Tnorm2)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0,1, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )


Tnorm3<-(datafin$TUBE-mean(datafin$TUBE))/(sd(datafin$TUBE))


######################################################################
#######################ANALISING THE DATA#############################
######################################################################

which(DIAM==72.35)

#Estimating correlation between DIAM and TUBE

par(mfrow=c(1,5))
boxplot(DIAM,main='tube data sec')
boxplot(datafin$DIAM, main='tube data cleaned')ii
boxplot(Tnorm,main='tube log normalized data')
boxplot(Tnorm2,main='tube featuring scaling normalized data')
boxplot(Tnorm3,main='tube standard score normalized data')

plot(datafin$DIAM,datafin$TUBE) # diameter and tube are strongly correlated cor=0.3791384 p.value<<0.001
cor(datafin$DIAM,datafin$TUBE)
cor.test(datafin$DIAM,datafin$TUBE)

plot(Dnorm2,Tnorm2)
cor(Dnorm2,Tnorm2)
cor.test(Dnorm2,Tnorm2)

##########################################################################################
################################ Mixed models for Kin#####################################
##########################################################################################
datafin$lnRATIODIAM<-log(RATIODIAM)

asd<-which(datafin$Type=="4S_1")
datakin<-datafin[-asd,]
asd2<-which(datakin$Type=="7S_1")
datakin<-datakin[-asd2,] # create a new dataframe in which only the relatet plant are considered
names(datakin)

length(which(datafin=="1K_1"))+length(which(datafin=="4K_1"))+length(which(datafin=="7K_1"))
length(datakin[,1])


TubDiamModel<-lmer(lnRATIODIAM~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datakin) # -1 erase the intercept and shows the first expicated variable
	summary(TubDiamModel)

test1<-glm(lmer(lnRATIODIAM~ Type+(1|FAMILY)+(1|ID_IND), data=datakin),family="gaussian")
summary(test1)

test2=glm(lm(DIAM+TUBE-1~ Type, data=datakin))
summary(test2)

drop1(TubDiamModel, test="Chi") # to extract main effects
	#summary(main1)
	#anova(TubDiamModel, type="II", test.statistic = "Chisq") # Is better glm with our dataset

LsMeans<-lsmeans(TubDiamModel, ~Type,test='Chi') # explore the lsmeans
	cld(TubDiamModel)#,details = FALSE, sort = TRUE, by, alpha = 0.05
	summary(LsMeans, type="response")


# ploting the results
LS1<-summary(LsMeans, type="response")
LS1$EJEX <- c(1,2,3)
LS2$Labels<-c("ak","bk","ck")
LS1
 
LS1$lsresponse

errbar(LS1$EJEX, LS1$lsresponse, LS1$upper.CL, LS1$lower.CL,
       ylim=c(36,41),
       xlim=c(0.5,3.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       xlab="Plant density (kin)",
       ylab="Length (mm)", 
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS1$EJEX,LS1$lsresponse, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to density of related plants")
axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1,6.2, "a")
#text(2,2, "b")
#text(3,1.3, "c")


##########################################################################################
############################# Mixed models for Strangers##################################
##########################################################################################


asd<-which(datafin$Type=="4K_1")
datastr<-datafin[-asd,]
asd2<-which(datastr$Type=="7K_1")
datastr<-datastr[-asd2,] # create a new dataframe in which only the relatet plant are considered
names(datastr)

length(which(datafin=="1K_1"))+length(which(datafin=="4S_1"))+length(which(datafin=="7S_1"))
length(datastr[,1])


names(datastr)
summary(datastr$FAMILY)

TubDiamModelS<-lmer(lnRATIODIAM~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datastr) # -1 erase the intercept and shows the first expicated variable
	summary(TubDiamModel)
#TubModelS2<-lmer(TUBE~ Type+(1|FAMILY/ID_IND)-1, data=datastr) # -1 erase the intercept and shows the first expicated variable

test3<-glm(TubDiamModelS)
summary(test3)

#test4=glm(lm(DIAM+TUBE-1~ Type, data=datakin))
summary(test4)

drop1(TubDiamModelS, test="Chi") # to extract main effects
	#summary(main1)
test5<-Anova(TubDiamModel,, type="II", test.statistic = "Chisq") # Is better glm with our dataset
	summary(test5)

LsMeansS<-lsmeans(TubDiamModelS, ~Type,test='Chi') # explore the lsmeans
	cld(TubDiamModelS)#,details = FALSE, sort = TRUE, by, alpha = 0.05
	summary(LsMeansS, type="response")


# ploting the results
LS2<-summary(LsMeansS, type="response")
LS2$EJEX <- c(1.5,2.5,3.5)
LS2$Labels<-c("as","bs","cs")
LS2
 
LS1$lsresponse

errbar(LS2$EJEX, LS2$lsresponse, LS2$upper.CL, LS2$lower.CL,
       ylim=c(35,40),
       xlim=c(0.5,3.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       xlab="Plant density (strangers)",
       ylab="Length (mm)", 
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS2$EJEX,LS2$lsresponse, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to density of strangers plants")
axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
#text(1,6.2, "a")
#text(2,2, "b")
#text(3,1.3, "c")



##############################################
############################################## Family analysis
##############################################


TubDiamFAM<-lmer(lnRATIODIAM~ FAMILY+(1|ID_IND)+(1|Pos_rel), data=datakin) 
TubDiamFAMS<-lmer(lnRATIODIAM~ FAMILY+(1|ID_IND)+(1|Pos_rel), data=datastr) 

Anova(TubDiamFAM)
TubDiamtest<-glm(lnRATIODIAM~ FAMILY:ID_IND, data=datakin)
summary(TubDiamtest)

Anova(TubDiamFAMS)
TubDiamtestSTR<-glm(TubDiamFAMS)
summary(TubDiamtestSTR)




##########################################################################################################################
######################################################Merged kin and akin#################################################
##########################################################################################################################

par(new=T) 
disp="sites"

Anova(TubDiamModel,type="2")
Anova(TubDiamModelS,type="2")

LS1<-summary(LsMeans, type="response")
LS1$EJEX <- c(1.1,2.1,3.1)
LS1$Labels<-c("ak","bk","ck")


LS2<-summary(LsMeansS, type="response")
LS2$EJEX <- c(1.2,2.2,3.2)
LS2$Labels<-c("as","bs","cs")

errbar(LS1$EJEX, LS1$lsresponse, LS1$upper.CL, LS1$lower.CL,
       ylim=c(0.52,0.66),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="ln(DIAM/TUBE)",
       xlab="Plant density", 
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS1$EJEX,LS1$lsmean, pch=21, col="black", bg="black", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1.14,37.73, "Control",col="blue",srt=90)
text(2.05,37.62070, "?",col="black",srt=90,cex=1.5)
text(3.05,39.14408, "?",col="black",srt=-90,cex=1.5)


par(new=T) 


errbar(LS2$EJEX, LS2$lsresponse, LS2$upper.CL, LS2$lower.CL,
       ylim=c(0.52,0.66),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="ln(DIAM/TUBE)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS2$EJEX,LS2$lsmean, pch=21, col="black", bg="white", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
text(2.15,38.45601, "?",col="black",srt=-90,cex=1.5)
text(3.15,37.36915, "?",col="black",srt=90,cex=1.5)

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################




####################################################################
############################VOL ANALYSIS############################
####################################################################

data<-read.table(file.choose(), sep=";", dec=".",h=T)
length(names(data))
data<-data[,-10]
datavol<-data[complete.cases(data),]
names(datavol)


par(mfrow=c(2,2))
plot(lm(datavol$vol_uL_Clean~datavol$Type+datavol$FAMILY+datavol$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables

par(mfrow=c(2,2))
plot(lm(datavol$ug_C6H12O6~datavol$Type+datavol$FAMILY+datavol$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables

volfin<-data[complete.cases(data),]
volfin<-volfin[-173,]
volfin<-volfin[-174,]
volfin<-volfin[-179,]
volfin<-volfin[-366,]
volfin<-volfin[-173,]
volfin<-volfin[-343,]
volfin<-volfin[-397,]



summary(volfin$FAMILY)
fam_TO_ERASE<-summary(volfin$FAMILY)<5

rowerased<-c(which(volfin$FAMILY=="04A"),which(volfin$FAMILY=="15A"),
which(volfin$FAMILY=="18A"),which(volfin$FAMILY=="40A"))

volfin<- volfin[-rowerased,]

summary(volfin$FAMILY)

par(mfrow=c(2,2))
plot(lm(volfin$ug_C6H12O6~volfin$Type+volfin$FAMILY+volfin$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables


volfin$Concentraz<-volfin$X./100# create a new column with concentration data in concentration (eg 45%->0.45)


### distribution of resp var
attach(datavol)
names(datavol)


max(datavol$vol_uL_Clean)
min(datavol$vol_uL_Clean)

par(mfrow=c(2,2))
hist(datafin$lnRATIODIAM,
	breaks=60,
	freq=FALSE,
	probability=TRUE,
	col="dimgray", 
	border="white",
	xlim=c(0,1),
	ylim=c(0,5),
	main="Distribution of the ratio between flower diameter and tube length")
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(datafin$lnRATIODIAM) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0, 1, 0.01), mean=mean(datafin$lnRATIODIAM), sd=sd(datafin$lnRATIODIAM)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0, 1, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

hist(datavol$vol_uL_Clean,
	breaks=60,freq=FALSE,
	probability=TRUE, 
	col="dimgray",xlim=c(0,2.5), 
	border="white",
	main="Distribution of nectar volume produced by the flower", 
	xlab='Volume (uL)',
	ylab='Density',
	cex.lab=1.2,
      las=2,)
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(datavol$vol_uL_Clean) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0,2.5, 0.01), mean=mean(datavol$vol_uL_Clean), sd=sd(datavol$vol_uL_Clean)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0,2.5, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

max(datavol$ug_C6H12O6)
min(datavol$ug_C6H12O6)

hist(datavol$ug_C6H12O6,breaks=60,freq=FALSE,probability=TRUE, col="dimgray",xlim=c(0,1.5), border="white",main="Distribution of quantity of sugars produced by the flower", xlab='Sugars (ug)',ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(datavol$ug_C6H12O6) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0,1.5, 0.01), mean=mean(datavol$ug_C6H12O6), sd=sd(datavol$ug_C6H12O6)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0,1.5, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

datavol$Concentraz<-datavol$X./100
max(datavol$Concentraz)
min(datavol$Concentraz)

hist(datavol$Concentraz,breaks=60,freq=FALSE,probability=TRUE, col="dimgray",xlim=c(0.3,0.9), border="white",main="Distribution of quantity of sugars produced by the flower", xlab='Sugars (ug)',ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(datavol$Concentraz) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0.3,0.9, 0.01), mean=mean(datavol$Concentraz), sd=sd(datavol$Concentraz)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(0.3,0.9, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )



##########################################################################################
################################ Mixed models for Kin#####################################
##########################################################################################
#install.packages("car")
library(car)
asd<-which(datavol$Type=="4S_1")
datavolkin<-datavol[-asd,]
asd2<-which(datavolkin$Type=="7S_1")
datavolkin<-datavolkin[-asd2,] # create a new dataframe in which only the relatet plant are considered
#asd3<-which(datavolkin$Type=="1K_4")
#datavolkin<-datavolkin[-asd3,]
#asd4<-which(datavolkin$Type=="1K_7")
#datavolkin<-datavolkin[-asd4,]

summary(datavolkin)

length(which(datavol=="1K_1"))+length(which(datavol=="4K_1"))+length(which(datavol=="7K_1"))
length(datavolkin[,1])


volModel<-lmer(vol_uL_Clean~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolkin) # -1 erase the intercept and shows the first expicated variable
	summary(volModel)

asd<-Anova(volModel,type="2")


testvol<-glm(lmer(vol_uL_Clean~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolkin),family="gaussian")
summary(testvol)



drop1(volModel, test="Chi") # to extract main effects
	#summary(main1)
	#anova(TubDiamModel, type="II", test.statistic = "Chisq") # Is better glm with our dataset

VolLS<-lsmeans(volModel, ~Type,test='Chi') # explore the lsmeans
	cld(TubDiamModel)#,details = FALSE, sort = TRUE, by, alpha = 0.05
	summary(LsMeans, type="response")


# ploting the results
LSvol<-summary(VolLS, type="response")
LSvol$EJEX <- c(1,2,3)
LSvol$Labels<-c("ak","bk","ck")
LSvol
 
LSvol$lsresponse

errbar(LSvol$EJEX, LSvol$lsmean, LSvol$upper.CL, LSvol$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.5,3.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       xlab="Plant density (kin)",
       ylab="Length (mm)", 
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvol$EJEX,LSvol$lsmean, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to density of related plants")
axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1,6.2, "a")
#text(2,2, "b")
#text(3,1.3, "c")


##########################################################################################
############################# Mixed models for Strangers##################################
##########################################################################################


asd<-which(datavol$Type=="4K_1")
datavolstr<-datavol[-asd,]
asd2<-which(datavolstr$Type=="7K_1")
datavolstr<-datavolstr[-asd2,] # create a new dataframe in which only the relatet plant are considered
#asd3<-which(datavolstr$Type=="1K_4")
#datavolstr<-datavolstr[-asd3,]
#asd4<-which(datavolstr$Type=="1K_7")
#datavolstr<-datavolstr[-asd4,]
names(datavolstr)

length(which(datavol=="1K_1"))+length(which(datavol=="4S_1"))+length(which(datavol=="7S_1"))
length(datavolstr[,1])


volModelS<-lmer(vol_uL_Clean~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolstr) # -1 erase the intercept and shows the first expicated variable
	summary(TubDiamModel)

testvolS<-glm(lmer(vol_uL_Clean~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolstr),family="gaussian")
summary(testvolS)



drop1(volModelS, test="Chi") # to extract main effects

VolLSstr<-lsmeans(volModelS, ~Type,test='Chi') # explore the lsmeans
	summary(VolLSstr, type="response")


# ploting the results
LSvolStr<-summary(VolLSstr, type="response")
LSvolStr$EJEX <- c(1,2,3)
LSvolStr$Labels<-c("ak","bk","ck")
LSvolStr
 


errbar(LSvolStr$EJEX, LSvolStr$lsmean, LSvolStr$upper.CL, LSvolStr$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.5,3.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       xlab="Plant density (kin)",
       ylab="Length (mm)", 
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvolStr$EJEX,LSvolStr$lsmean, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to density of related plants")
axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1,6.2, "a")
#text(2,2, "b")
#text(3,1.3, "c")

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

plot(datavolkin$Type, datavolkin$vol_uL_Clean)
plot(datavolstr$Type, datavolstr$vol_uL_Clean)

voltrat<-glm(datavolkin$vol_uL_Clean~datavolkin$Type)
summary(voltrat)

voltratst<-glm(datavolstr$vol_uL_Clean~datavolstr$Type)
summary(voltratst)


Anova(volModel,type="2")
Anova(volModelS,type="2")

LSvol<-summary(VolLS, type="response")
LSvol$EJEX <- c(1.1,2.1,3.1)
LSvol$Labels<-c("ak","bk","ck")


LSvolStr<-summary(VolLSstr, type="response")
LSvolStr$EJEX <- c(1.2,2.2,3.2)
LSvolStr$Labels<-c("as","bs","cs")


errbar(LSvol$EJEX, LSvol$lsmean, LSvol$upper.CL, LSvol$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.5,3.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Vol (uL)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvol$EJEX,LSvol$lsmean, pch=21, col="black", bg="white", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1.14,37.73, "Control",col="blue",srt=90)
text(2.05,37.62070, "?",col="black",srt=90,cex=1.5)
text(3.05,39.14408, "?",col="black",srt=-90,cex=1.5)


par(new=T) 


errbar(LSvolStr$EJEX, LSvolStr$lsresponse, LSvolStr$upper.CL, LSvolStr$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Vol (uL)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvolStr$EJEX,LSvolStr$lsmean, pch=21, col="black", bg="black", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
text(2.15,38.45601, "?",col="black",srt=-90,cex=1.5)
text(3.15,37.36915, "?",col="black",srt=90,cex=1.5)

##########################################################################################################################
##################################################Analyisy of sugar concentration##################################################
##########################################################################################################################

names(datavolkin)
datavolkin
datavolstr

modsugar<-lmer(Concentraz~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolkin)
modsugarstr<-lmer(Concentraz~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolstr)

Anova(modsugar,type="2")
Anova(modsugarstr,type="2")

Sugar<-lsmeans(modsugar, ~Type,test='Chi')
SugarSTR<-lsmeans(modsugarstr, ~Type,test='Chi')


LSsugar<-summary(Sugar, type="response")
LSsugar$EJEX <- c(1,2,3)

LSsugarstr<-summary(SugarSTR, type="response")
LSsugarstr$EJEX <- c(1,2,3)



errbar(LSsugar$EJEX, LSsugar$lsmean, LSsugar$upper.CL, LSsugar$lower.CL,
       ylim=c(0.59,0.68),
       xlim=c(0.5,3.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar concentration %",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSsugar$EJEX,LSsugar$lsmean, pch=21, col="black", bg="white", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1.14,37.73, "Control",col="blue",srt=90)


par(new=T) 


errbar(LSsugarstr$EJEX, LSsugarstr$lsresponse, LSsugarstr$upper.CL, LSsugarstr$lower.CL,
       ylim=c(0.59,0.68),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar concentration %",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSsugarstr$EJEX,LSsugarstr$lsmean, pch=21, col="black", bg="black", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 






##########################################################################################################################
################################################Analyisy of sugar quantity################################################
##########################################################################################################################

names(datavolkin)
datavolkin
datavolstr

modugram<-lmer(ug_C6H12O6~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolkin)
modugramstr<-lmer(ug_C6H12O6~ Type+(1|FAMILY)+(1|ID_IND)+(1|Pos_rel), data=datavolstr)

Anova(modugram,type="2")
Anova(modugramstr,type="2")

gram<-lsmeans(modugram, ~Type,test='Chi')
gramSTR<-lsmeans(modugramstr, ~Type,test='Chi')


LSgram<-summary(gram, type="response")
LSgram$EJEX <- c(1,2,3)

LSgramSTR<-summary(gramSTR, type="response")
LSgramSTR$EJEX <- c(1,2,3)



errbar(LSgram$EJEX, LSgram$lsmean, LSgram$upper.CL, LSgram$lower.CL,
       ylim=c(0.45,0.75),
       xlim=c(0.5,3.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar production (ug)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSgram$EJEX,LSgram$lsmean, pch=21, col="black", bg="white", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1.14,37.73, "Control",col="blue",srt=90)


par(new=T) 


errbar(LSgramSTR$EJEX, LSgramSTR$lsresponse, LSgramSTR$upper.CL, LSgramSTR$lower.CL,
       ylim=c(0.45,0.75),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar production (ug)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSgramSTR$EJEX,LSgramSTR$lsmean, pch=21, col="black", bg="black", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 


####################################################################################################
#########################################All together###############################################
####################################################################################################

par(mfrow=c(2,2))

LS1$EJEX<-c(1,2,3)
LS2$EJEX<-c(1.2,2.2,3.2)

errbar(LS1$EJEX, LS1$lsresponse, LS1$upper.CL, LS1$lower.CL,
       ylim=c(0.52,0.66),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="ln(DIAM/TUBE)",
       xlab="Plant density", 
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS1$EJEX,LS1$lsmean, pch=21, col="black", bg="black", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
text(0.8,0.66, "a)",cex=1.25)



par(new=T) 

errbar(LS2$EJEX, LS2$lsresponse, LS2$upper.CL, LS2$lower.CL,
       ylim=c(0.52,0.66),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="ln(DIAM/TUBE)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS2$EJEX,LS2$lsmean, pch=21, col="black", bg="white", cex=1.8)
#title(main="Response to density of strangers plants")



LSvol$EJEX<-c(1,2,3)
LSvolStr$EJEX<-c(1.2,2.2,3.2)

errbar(LSvol$EJEX, LSvol$lsmean, LSvol$upper.CL, LSvol$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Vol (uL)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvol$EJEX,LSvol$lsmean, pch=21, col="black", bg="white", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
text(0.8,1.15, "b)",cex=1.25)



par(new=T) 


errbar(LSvolStr$EJEX, LSvolStr$lsresponse, LSvolStr$upper.CL, LSvolStr$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Vol (uL)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvolStr$EJEX,LSvolStr$lsmean, pch=21, col="black", bg="black", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
text(2.15,38.45601, "?",col="black",srt=-90,cex=1.5)
text(3.15,37.36915, "?",col="black",srt=90,cex=1.5)

LSsugar$EJEX<-c(1,2,3)
LSsugarstr$EJEX<-c(1.2,2.2,3.2)

errbar(LSsugar$EJEX, LSsugar$lsmean, LSsugar$upper.CL, LSsugar$lower.CL,
       ylim=c(0.59,0.68),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar concentration %",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSsugar$EJEX,LSsugar$lsmean, pch=21, col="black", bg="white", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
text(0.8,0.68, "c)",cex=1.25)


par(new=T) 


errbar(LSsugarstr$EJEX, LSsugarstr$lsresponse, LSsugarstr$upper.CL, LSsugarstr$lower.CL,
       ylim=c(0.59,0.68),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar concentration %",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSsugarstr$EJEX,LSsugarstr$lsmean, pch=21, col="black", bg="black", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 


LSgram$EJEX<-c(1,2,3)
LSgramSTR$EJEX<-c(1.2,2.2,3.2)

errbar(LSgram$EJEX, LSgram$lsmean, LSgram$upper.CL, LSgram$lower.CL,
       ylim=c(0.45,0.75),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar production (ug)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSgram$EJEX,LSgram$lsmean, pch=21, col="black", bg="white", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
text(0.8,0.75, "d)",cex=1.25)


par(new=T) 


errbar(LSgramSTR$EJEX, LSgramSTR$lsresponse, LSgramSTR$upper.CL, LSgramSTR$lower.CL,
       ylim=c(0.45,0.75),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar production (ug)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSgramSTR$EJEX,LSgramSTR$lsmean, pch=21, col="black", bg="black", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
####################################################################################################
##################################Analysis for vs 4ks 7ks###########################################
####################################################################################################

data<-read.table(file.choose(),h=T,sep=";")
head(data)
is.factor(data$DENSITY)
data$DENSITY<-as.factor(data$DENSITY)

Lk1<-which(data$TRAT=="1K1")
data1<-data[-Lk1,]

dataVol<-data1[-9]
dataVol<-dataVol[-9]
dataVol<-dataVol[complete.cases(dataVol),]
dataVol$ugC6H12O6<-dataVol$vol_uL_Clean*dataVol$X./100

dataDiam<-data1[-7]
dataDiam<-dataDiam[-7]
dataDiam<-dataDiam[complete.cases(dataDiam),]
dataDiam$RATIO<-dataDiam$DIAM/dataDiam$TUBE


Volint<-lmer(vol_uL_Clean~ (DENSITY*KIN)+(1|BLOCK)+(1|ID_IND), data=dataVol)
GRAMINT<-lmer(ugC6H12O6~ DENSITY*KIN+(1|BLOCK)+(1|ID_IND), data=dataVol)
percentint<-lmer(X./100~ DENSITY*KIN+(1|BLOCK)+(1|ID_IND), data=dataVol)
ratioint<-lmer(log(RATIO)~ DENSITY*KIN+(1|BLOCK)+(1|ID_IND), data=dataDiam)


Anova(Volint,type="2")
Anova(GRAMINT,type="2")
Anova(percentint,type="2")
Anova(ratioint,type="2")



LSVolint<-lsmeans(Volint, ~DENSITY*KIN,test='Chi')
LSGRAMINT<-lsmeans(GRAMINT, ~DENSITY*KIN,test='Chi')
LSpercentint<-lsmeans(percentint, ~DENSITY*KIN,test='Chi')
LSratioint<-lsmeans(ratioint, ~DENSITY*KIN,test='Chi')



volsum<-summary(LSVolint,type="response")
gramsum<-summary(LSGRAMINT,type="response")
percentsum<-summary(LSpercentint,type="response")
ratiosum<-summary(LSratioint,type="response")

volsum$EJEX <- c(1,3,2,4)
gramsum$EJEX <- c(1,3,2,4)
percentsum$EJEX <- c(1,3,2,4)
ratiosum$EJEX <- c(1,3,2,4)

par(mfrow=(c(2,2)))

errbar(ratiosum$EJEX, ratiosum$lsresponse, ratiosum$upper.CL, ratiosum$lower.CL,
       ylim=c(1.6,2),
       xlim=c(0.5,4.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="ln(diam/tube)",
       xlab="Plant social environment",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(ratiosum$EJEX,ratiosum$lsresponse, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1,2,3,4), labels=c("4 kin", "4 akin", "7 kin","7 akin"), tick=FALSE) 
text(0.55,2, "a)",cex=1.25)


errbar(percentsum$EJEX, percentsum$lsresponse, percentsum$upper.CL, percentsum$lower.CL,
       ylim=c(0.6,0.67),
       xlim=c(0.5,4.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="perceontage sugar",
       xlab="Plant social environment",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(percentsum$EJEX,percentsum$lsresponse, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1,2,3,4), labels=c("4 kin", "4 akin", "7 kin","7 akin"), tick=FALSE) 
text(0.55,0.67, "b)",cex=1.25)


errbar(volsum$EJEX, volsum$lsmean, volsum$upper.CL, volsum$lower.CL,
       ylim=c(0.75,1.1),
       xlim=c(0.5,4.5), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Volume production (uL)",
       xlab="Plant social environment",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(volsum$EJEX,volsum$lsmean, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1,2,3,4), labels=c("4 kin", "4 akin", "7 kin","7 akin"), tick=FALSE) 
text(0.55,1.1, "c)",cex=1.25)




errbar(gramsum$EJEX, gramsum$lsmean, gramsum$upper.CL, gramsum$lower.CL,
       ylim=c(0.45,0.70),
       xlim=c(0.5,4.5), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Sugar production (ug)",
       xlab="Plant social environment",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(gramsum$EJEX,gramsum$lsmean, pch=21, col="black", bg="gray", cex=1.8)
title(main="Response to density of strangers plants")
text(0.55,0.7, "d)",cex=1.25)

####################################################################################################
#####################################Neigbours vs interest##########################################
####################################################################################################

Dataneighbous<-read.table(file.choose(), sep=";", dec=".",h=T)

head(Dataneighbous)
length(names(Dataneighbous))


paste(Dataneighbous$FAMILY,Dataneighbous$TYPE_FOC_NEIG,sep="_")

Dataneighbous$ID_FOC_NEIG<-paste(Dataneighbous$FAMILY,Dataneighbous$TYPE_FOC_NEIG,sep="_")


dataneigvol<-Dataneighbous[-8]
dataneigvol<-dataneigvol[-9]
dataneigvol<-dataneigvol[-9]
dataneigvol<-dataneigvol[-9]



head(dataneigvol)

par(mfrow=c(2,2))
plot(lm(dataneigvol$vol_uL_Clean~dataneigvol$Type+dataneigvol$FAMILY+dataneigvol$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables

neghvol<-dataneigvol[complete.cases(dataneigvol),]
neghvol<-neghvol[-1053,]
neghvol<-neghvol[-1038,]
neghvol<-neghvol[-819,]
neghvol<-neghvol[-716,]
neghvol<-neghvol[-704,]
neghvol<-neghvol[-550,]
neghvol<-neghvol[-902,]


par(mfrow=c(2,2))
plot(lm(neghvol$vol_uL_Clean~neghvol$Type+neghvol$FAMILY+neghvol$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables

names(datavolkin)
datavolkin
datavolstr

asd<-which(neghvol$TYPE_FOC_NEIG=="4S_1")
neigvolkin<-neghvol[-asd,]
asd2<-which(neigvolkin$TYPE_FOC_NEIG=="7S_1")
neigvolkin<-neigvolkin[-asd2,]
asd3<-which(neigvolkin$TYPE_FOC_NEIG=="4S_2")
neigvolkin<-neigvolkin[-asd3,]
asd4<-which(neigvolkin$TYPE_FOC_NEIG=="7S_2")
neigvolkin<-neigvolkin[-asd4,]
asd5<-which(neigvolkin$TYPE_FOC_NEIG=="4K_1")
neigvolkin<-neigvolkin[-asd5,]
asd6<-which(neigvolkin$TYPE_FOC_NEIG=="7K_1")
neigvolkin<-neigvolkin[-asd6,]
head(neigvolkin)

asd<-which(neghvol$TYPE_FOC_NEIG=="4S_1")
neigvolSTR<-neghvol[-asd,]
asd2<-which(neigvolSTR$TYPE_FOC_NEIG=="7S_1")
neigvolSTR<-neigvolSTR[-asd2,]
asd3<-which(neigvolSTR$TYPE_FOC_NEIG=="4K_2")
neigvolSTR<-neigvolSTR[-asd3,]
asd4<-which(neigvolSTR$TYPE_FOC_NEIG=="7K_2")
neigvolSTR<-neigvolSTR[-asd4,]
asd5<-which(neigvolSTR$TYPE_FOC_NEIG=="4K_1")
neigvolSTR<-neigvolSTR[-asd5,]
asd6<-which(neigvolSTR$TYPE_FOC_NEIG=="7K_1")
neigvolSTR<-neigvolSTR[-asd6,]
head(neigvolkin)


modneigvol<-lmer(vol_uL_Clean~ TYPE_FOC_NEIG+(1|FAMILY)+(1|ID_FOC_NEIG)++(1|Pos_rel), data=neigvolkin)


Anova(modneigvol,type="2")


volfocneig<-lsmeans(modneigvol, ~TYPE_FOC_NEIG,test='Chi')





LSvol<-summary(VolLS, type="response")
LSvol$EJEX <- c(1,2,3)
LSvol$Labels<-c("ak","bk","ck")


LSvolStr<-summary(VolLSstr, type="response")
LSvolStr$EJEX <- c(1.2,2.2,3.2)
LSvolStr$Labels<-c("as","bs","cs")



LSfocneig<-summary(volfocneig, type="response")
LSfocneig$EJEX <- c(1.4,2.4,3.4)


errbar(LSvol$EJEX, LSvol$lsmean, LSvol$upper.CL, LSvol$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.75,4), 
       col="Black", 
       cap=0.020,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Vol (uL)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvol$EJEX,LSvol$lsmean, pch=21, col="black", bg="white", cex=1.8)
title(main="Response to plant density")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1.14,37.73, "Control",col="blue",srt=90)
text(2.05,37.62070, "?",col="black",srt=90,cex=1.5)
text(3.05,39.14408, "?",col="black",srt=-90,cex=1.5)


par(new=T) 


errbar(LSvolStr$EJEX, LSvolStr$lsresponse, LSvolStr$upper.CL, LSvolStr$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.75,4), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Vol (uL)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSvolStr$EJEX,LSvolStr$lsmean, pch=21, col="black", bg="black", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
text(2.15,38.45601, "?",col="black",srt=-90,cex=1.5)
text(3.15,37.36915, "?",col="black",srt=90,cex=1.5)


par(new=T) 


errbar(LSfocneig$EJEX, LSfocneig$lsresponse, LSfocneig$upper.CL, LSfocneig$lower.CL,
       ylim=c(0.75,1.15),
       xlim=c(0.75,4), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Vol (uL)",
       xlab="Plant density",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LSfocneig$EJEX,LSfocneig$lsmean, pch=21, col="black", bg="gray", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1.3,2.3,3.3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
text(2.15,38.45601, "?",col="black",srt=-90,cex=1.5)
text(3.15,37.36915, "?",col="black",srt=90,cex=1.5)





