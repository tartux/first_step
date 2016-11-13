####################################################################
############################VOL ANALYSIS############################
####################################################################

data<-read.table(file.choose(), sep=";", dec=".",h=T)


datavol<-data[complete.cases(data),]


attach(datavol)
names(datavol)

###distribution of vol###
par(mfrow=c(1,2))
qqnorm(MEAN_VOL_uL)
qqline(MEAN_VOL_uL)

density(MEAN_VOL_uL)
plot(density(MEAN_VOL_uL))


max(MEAN_VOL_uL)

NROMVOL<-(MEAN_VOL_uL-min(MEAN_VOL_uL))/(max(MEAN_VOL_uL)-min(MEAN_VOL_uL))

qqnorm(NROMVOL)
qqline(NROMVOL)
# the data seams to be already normals, apply a log transofrmation does not reduce variance instead seams to increas the anormality of the data

####Boxoplot for Vol vs Families, Trat and individuals####


#FAMILIES
summary(ID_FAM)

length(summary(ID_FAM))

par(mfrow=c(2,1))

plot(ID_FAM,MEAN_VOL_uL, col=rainbow(36))

plot(ID_FAM,NROMVOL, col=rainbow(36))

#TRAIT

summary(TRAIT)

length(summary(TRAIT))

par(mfrow=c(2,1))

plot(TRAIT,MEAN_VOL_uL, col=rainbow(5))

plot(TRAIT,NROMVOL, col=rainbow(5))

#INDIVIDUAL (NOT USEFUL)

summary(IND_ID)

length(summary(IND_ID))

par(mfrow=c(2,1))

plot(IND_ID,MEAN_VOL_uL, col=rainbow(100))

plot(IND_ID,NROMVOL, col=rainbow(100))

###TEST EFFECT ON FAMILIES 


a<-glm(lm(NROMVOL~ID_FAM),fam=gaussian,datavol)
summary(a)

### three familis produce less than other (fam30,42 and 50)

###TEST EFFECT ON TRAIT (GENERAL)


c<-glm(lm(NROMVOL~TRAIT),fam=gaussian,datavol)
summary(c)

### three familis produce less than other (fam30,42 and 50)





?glm



#######################################################
### Script for fitting and plotting simple models #####
#######################################################

install.packages("lme4")
install.packages("lsmeans")
install.packages("car")
install.packages("Hmisc")

#loading packages:
library(lme4) # fit the model
library(lsmeans) # analysing the means of factor levels
library(car) # for using the 'Anova' function 
library(Hmisc) #for plotting means and error bars


# for exploring the variation between experimental levels in the treatment:

boxplot(FLORALTRAIT ~ TRAT, 
        data = DATA_FOCAL,
        varwidth = F, 
        ylab = "Floral trait (units)",
        xlab = "Experimental groups",
        main = "")


# example of the model
mod1<-lmer(FLORALTRAIT ~ TRAT+(1|BLOCK/PLANT), data=DATA_FOCAL2) 
plot (mod1)
summary(mod1)
drop1(mod1, test="Chi") # to extract main effects
Anova (mod1, type="II", test.statistic = "Chisq") # other way to extrat main effects
L<-lsmeans(mod1, pairwise~TRAT, test="Chi") # explore the lsmeans
cld(L)
summary(L, type="response")


# ploting the results
LS1<-summary(L, type="response")$lsmeans
LS1$EJEX <- c(1,2,3)
LS1 

errbar(LS1$EJEX, LS1$response, LS1$upper.CL, LS1$lower.CL, # BE CAREFUL names of columns can change in the LSMEANS table depending of the model CHECK IT and update here the names in case if needed
       ylim=c(0,6.2),  # it should be adjusted for each plot
       xlim=c(0.5,3.5), 
       col=c(1), 
       pch=c(16),
       cex=1.6,
       cap=0.050,
       lab=c(4,4,4),
       family="Arial",
       tck=-0.03, # longitud del tick y posición hacia dentro
       xlab="Experimental groups",
       ylab="Dry mass (g)", 
       cex.lab=1.5,
       las=1,     # 
       xaxt="n")  # 
title(main="Solitary plants") # 
axis(1, at=c(1,2,3), labels=c("1K1", "1K4", "1K7"), tick=FALSE) 
text(1,6.2, "a")
text(2,2, "b")
text(3,1.3, "c")



############################################################################
########################FLORAL TRAI TUBE AND DIAMETER#######################
############################################################################

library(graphycs) # is used to modifiy the x/y lim and allow points gestion
library(lme4) # fit the model
library(lsmeans) # analysing the means of factor levels
library(car) # for using the 'Anova' function 
library(Hmisc) #for plotting means and error bars

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
plot(lm(TUBE+DIAM~Type+FAMILY+ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables



datafin<-datatube[-370,]# eliminate the line with the outliers identified by the model)
datafin<-datafin[-518,]
datafin<-datafin[-373,]
datafin<-datafin[-68,]
datafin<-datafin[-418,]
datafin<-datafin[-516,]
datafin<-datafin[-514,]
datafin<-datafin[-418,]
datafin<-datafin[-417,]
datafin<-datafin[-86,]
datafin<-datafin[-362,]
datafin<-datafin[-440,]
datafin<-datafin[-153,]
datafin<-datafin[-170,]
datafin<-datafin[-183,]
datafin<-datafin[-184,]
datafin<-datafin[-216,]
datafin<-datafin[-414,]
datafin<-datafin[-456,]
datafin<-datafin[-509,]
datafin<-datafin[-181,]
datafin<-datafin[-182,]
datafin<-datafin[-212,]
datafin<-datafin[-409,]
datafin<-datafin[-450,]
datafin<-datafin[-502 ,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]
datafin<-datafin[-,]





par(mfrow=c(2,2))
plot(lm(datafin$TUBE+datafin$DIAM~datafin$Type+datafin$FAMILY+datafin$ID_IND)) #create a test in reason to compare the normality of the repsonse variables on the explication variables


### distribution of resp var

par(mfrow=c(2,2))
hist(datafin$DIAM,breaks=60,freq=FALSE,probability=TRUE, col="dimgray",xlim=c(10,35), border="white",main="Distribution of diamtere of the flower length", xlab='Flower diameter length (mm)',ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(datafin$DIAM) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(10, 35, 0.01), mean=mean(datafin$DIAM), sd=sd(datafin$DIAM)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(10, 35, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )

hist(datafin$TUBE,breaks=60,freq=FALSE,probability=TRUE, xlim=c(8,22),col="dimgray", border="white",main="Distribution of flower tube length", xlab='Flower tube length (mm)',ylab='Density')
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(datafin$TUBE) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(8, 22, 0.01), mean=mean(datafin$TUBE), sd=sd(datafin$TUBE)) # create a set of theroetical distribution of DIAM
	par(new=T) # affort to merge to graphycs together
	plot(seq(8, 22, 0.01), curve_data, axes=FALSE, xlab='', ylab='', type='l', col='blue',lwd = 2 )


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

,xlim=c(2.2,3.2),ylim=c(0,6)

par(mfrow=c(1,2))
hist(Dnorm2,breaks=60,freq=FALSE,probability=TRUE, col="dimgray", border="white",main="Normalized distribution of diamtere of the flower length")
	mtext("histogram (grey), observed distribution (red) and theoretical distribution (blue)") # add a subtitle
	a<-density(Dnorm2) # observed distribution
	lines(a, col=c("red"), lty = 1, lwd = 2) # plot the observed distribution
	curve_data <- dnorm(seq(0, 1, 0.01), mean=mean(Dnorm2), sd=sd(Dnorm2)) # create a set of theroetical distribution of DIAM
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


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################





#mtext(substitute(paste(italic('cor ='), "0.204",italic('    p-value ='), "0.005"  )))
#points(bwt,logpeso, disp="sites", pch=21, col="black", bg="gray34", cex=1.2)
#abline(logpeso~bwt)
#cor.test(bwt,logpeso)



boxplot( DIAM+TUBE~ Type+(1/FAMILY/ID_IND) ,
        data = datatube,
        varwidth = F, 
        ylab = "Floral trait (units)",
        xlab = "Experimental groups",
        main = "")

######################################################################
#######################ANALISING THE DATA#############################
######################################################################

which(DIAM==72.35)

#Estimating correlation between DIAM and TUBE

par(mfrow=c(1,5))
boxplot(DIAM,main='tube data sec')
boxplot(datafin$DIAM, main='tube data cleaned')
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
asd<-which(datafin$Type=="4S_1")
datakin<-datafin[-asd,]
asd2<-which(datakin$Type=="7S_1")
datakin<-datakin[-asd2,] # create a new dataframe in which only the relatet plant are considered
names(datakin)

length(which(datafin=="1K_1"))+length(which(datafin=="4K_1"))+length(which(datafin=="7K_1"))
length(datakin[,1])


TubDiamModel<-lmer(DIAM+TUBE~ Type+(1|FAMILY)+(1|ID_IND)-1, data=datakin) # -1 erase the intercept and shows the first expicated variable
	summary(TubDiamModel)
TubModel<-lmer(TUBE~ Type+(1|FAMILY/ID_IND)-1, data=datakin) # -1 erase the intercept and shows the first expicated variable

test1<-glm(TubDiamModel)
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


TubDiamModelS<-lmer(DIAM+TUBE~ Type+(1|FAMILY)+(1|ID_IND)-1, data=datastr) # -1 erase the intercept and shows the first expicated variable
	summary(TubDiamModel)
TubModelS2<-lmer(TUBE~ Type+(1|FAMILY/ID_IND)-1, data=datastr) # -1 erase the intercept and shows the first expicated variable

test3<-glm(TubDiamModelS)
summary(test3)

test4=glm(lm(DIAM+TUBE-1~ Type, data=datakin))
summary(test4)

drop1(TubDiamModelS, test="Chi") # to extract main effects
	#summary(main1)
test5<-anova(TubDiamModel,, type="II", test.statistic = "Chisq") # Is better glm with our dataset
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


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

par(new=T) 
disp="sites"

LS1<-summary(LsMeans, type="response")
LS1$EJEX <- c(1.1,2.1,3.1)
LS1$Labels<-c("ak","bk","ck")


LS2<-summary(LsMeansS, type="response")
LS2$EJEX <- c(1.2,2.2,3.2)
LS2$Labels<-c("as","bs","cs")


errbar(LS1$EJEX, LS1$lsresponse, LS1$upper.CL, LS1$lower.CL,
       ylim=c(35,41),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       xlab="Plant density (kin)",
       ylab="Length (mm)", 
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS1$EJEX,LS1$lsresponse, pch=21, col="black", bg="blue", cex=1.8)
title(main="Response to density of related plants")
axis(1, at=c(1.15,2.15,3.15), labels=c("1 plant", "4 plants", "7 plants"), tick=FALSE) 
#text(1.14,37.73, "Control",col="blue",srt=90)
text(2.05,37.62070, "?",col="black",srt=90,cex=1.5)
text(3.05,39.14408, "?",col="black",srt=-90,cex=1.5)


par(new=T) 


errbar(LS2$EJEX, LS2$lsresponse, LS2$upper.CL, LS2$lower.CL,
       ylim=c(35,41),
       xlim=c(0.75,3.75), 
       col="Black", 
       cap=0.010,
       lab=c(6,6,6),
       tck=-0.0075,
       ylab="Length (mm)",
       xlab="Plant density (kin)",
       cex.lab=1.2,
       las=2,
       xaxt="n")
points(LS2$EJEX,LS2$lsresponse, pch=21, col="black", bg="darkgreen", cex=1.8)
#title(main="Response to density of strangers plants")
#axis(1, at=c(1,2,3), labels=c("1 plant", "4 plants", "7 plants"), las=1,tick=FALSE) 
text(2.15,38.45601, "?",col="black",srt=-90,cex=1.5)
text(3.15,37.36915, "?",col="black",srt=90,cex=1.5)

