

# librarys----
detach("package:plyr", unload=TRUE) #will error - no worries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel)
library(gamm4)


rm(list=ls()) #clear memory/workspace
study<-"Spanish Mackerel Model" # sets name of this workspace

# Add you work dir here-
work.dir=("G:/My Drive/Masters/Project/Spanish Mackerel Depredation/Results/R/Analysis_Carmody_depredation") #google drive

# Then set the folders within the owncloud for the output files to go into
tidy.data=paste(work.dir,"Data",sep="/") # Data folder
plots=paste(work.dir,"Plots",sep="/") # plots folder
script.dir=paste(work.dir,"Scripts",sep="/") # scripts folder
model.out=paste(work.dir,"Model Outputs",sep="/") # model outputs folder

# Load full subset gam function----
source("G:/My Drive/Masters/Project/Spanish Mackerel Depredation/Results/R/Analysis_Carmody_depredation/Scripts/function_full_subsets_gam_v1.11.R") # source file
# for the full subsets gam script made by Becky Fisher at AIMS


# Read the final dataset CSV from my owncloud documents folder

dat<-read.csv("C:/Users/Harrison Carmody/Documents/Uni Documents/Masters/Project/Spanish Mackerel Depredation/Results/Raw/CombinedlDailyQueries by SessionID to 2017HC SST Edit.csv",stringsAsFactors =T)%>%
  #   renaming predictors to make names shorter
  rename(SST=sst)%>%
  #   select predictor variables
  select(c(Date,Decimal.median.time,Hours,Month,Year,Block,Zone,Skipper,SST,Fraction.moon.illuminated,Fishing.pressure.all.years,NumberCaught,Method,NumberMackerelTaken..full.))%>%
  #   Choose response variable
  rename(response=NumberMackerelTaken..full.)%>%
  na.omit() #gets rid of NA's
head(dat,3)# view first 3 rows of each column
names(dat) # view the names of the columns
str(dat) #check format 

# No real need to subset data into trolling vs jigging

dat <- subset(dat, Fishing.method%in%c("Bottom-bashing (drifting)", "Bottom-bashing (anchored)"))

# subset to remove unreliable skippers 

dat <- subset(dat, !Skipper%in%c("Ab","Ad","Ae","Ak","Al","Ap","Aq","C","G","K"))
# Subset for zones

dat <- subset(dat, Zone%in%c("1"))


help(subset)

# Check levels of factors----
table(dat$Zone) # 8866 sessions for this zone
table(dat$Skipper) # Zone 1 dominated by D, E, I and J

# Set predictor variables for GAM---
# First continuous predictors
pred.vars=c("Max.hook..depth",
            "SST",
            "Decimal.fishing.hours",
            "Decimal.Median.time",
            "Kernel.density",
            "fivekm",
            "Lat")


# now categorical factor variables 

pred.vars.fact=c("Month.Year")


# Check for correalation of predictor variables---
round(cor(dat[,pred.vars]),2)

Max.hook..depth   SST Decimal.fishing.hours Decimal.Median.time Kernel.density

Max.hook..depth                  1.00  0.05                  0.50               -0.14          -0.15
SST                              0.05  1.00                  0.03                0.05          -0.02
Decimal.fishing.hours            0.50  0.03                  1.00               -0.20          -0.22
Decimal.Median.time             -0.14  0.05                 -0.20                1.00           0.01
Kernel.density                  -0.15 -0.02                 -0.22                0.01           1.00
fivekm                          -0.21 -0.20                 -0.16                0.03           0.44
Lat                              0.51  0.07                  0.34               -0.19           0.00
fivekm   Lat
Max.hook..depth        -0.21  0.51
SST                    -0.20  0.07
Decimal.fishing.hours  -0.16  0.34
Decimal.Median.time     0.03 -0.19
Kernel.density          0.44  0.00
fivekm                  1.00 -0.16
Lat                    -0.16  1.00

# This produces table of all combinations of predictors. 
# THe predictor vs itself is always 1 
# THen it shows the relationship between all predictors, with the higher the number
# indicating closer relationship
# If values are >0.4 for combination of variables then is strong colinearity


# Check the distribution of the predcitors----

# The code below sets the format for creating plots for each predictor variable
# It saves them as pdf files with the title and date
pdf(file=paste(file=paste(Sys.Date(),study,"predictor_plots.pdf",sep = "_")),onefile=T)
for(p in 1:length(pred.vars)){
  par(mfrow=c(2,1)) # sets it so that there is 2 plots next to each other 
  plot.dat=dat
  hist(plot.dat[,pred.vars[p]],main=pred.vars[p]) # For creating histograms of predictors
  plot(plot.dat[,pred.vars[p]]) # For creating scatter plots of predictors
}
dev.off() # Means that the plots don't show deviance

# Review of individual predictors - we have to make sure they have an even distribution---
# If the data are skewed to low numbers try sqrt>log+1 or if skewed to high numbers 
# try ^2 or ^3
# Plot each predictor individually with histogram and scatter plot side by side

par(mfrow=c(2,1))

# First plot the response - no. fish lost per hour

hist((dat$response)) 
plot((dat$response))

# Now the offset 

hist((dat$Total.fish.hooked))
plot((dat$Total.fish.hooked))

# Low skewed and huge range of values so needs log + 1 transforming


# Now depth

hist((dat$Max.hook..depth))
plot((dat$Max.hook..depth)) # low skewed


# Now SST

hist((dat$SST))
plot((dat$SST))


# Now decimal median time

hist((dat$Decimal.Median.time))
plot((dat$Decimal.Median.time))

# Now Kernel density

hist((dat$Kernel.density))
plot((dat$Kernel.density)) 

# Now decimal fishing hours

hist((dat$Decimal.fishing.hours))
plot((dat$Decimal.fishing.hours))

# Now fivekm

hist((dat$fivekm))
plot((dat$fivekm))

# Now Lat

hist((dat$Lat))
plot((dat$Lat))


# So from the results of plots and histograms only need to transform offset
dat<-dat%>%
  mutate(log.Total.fish.hooked=log(Total.fish.hooked+1))%>% # use log + 1
  mutate(Location="All")
head(dat,3)

# Now look at distribution of log + 1 offset

hist(dat$log.Total.fish.hooked)
plot(dat$log.Total.fish.hooked) # Much better with a much smaller range of values and better overall distribution

# Duplicate the data by location - so we can run models at multiple spatial scales-----
table(dat$Boat.ramp)
Bundegi<-dat%>%
  filter(Boat.ramp=="Bundegi")%>%
  mutate(Location="Bundegi")
Exmouth.marina<-dat%>%
  filter(Boat.ramp=="Exmouth marina")%>%
  mutate(Location="Exmouth.marina")



# Now put them all together in a table
dat<-dat%>%
  mutate(Location="All")%>%
  bind_rows(Bundegi,Exmouth.marina)
head(dat,3)
table(dat$Location) # Tells you the sample size for each of the individual locations
str(dat) # shows the data in string format

# now fix a formatting issue - I have no idea why we have to do this - it is a 
# dplyr() issue - have to write and then read csv
write.csv(dat,"dat.csv")
dat<-read.csv("dat.csv")


# Re-set the now transformed continuous predictors---
pred.vars=c("Max.hook..depth",
            "SST",
            "Decimal.fishing.hours",
            "Decimal.Median.time",
            "Kernel.density",
            "fivekm",
            "Lat")

# Now categorical factor predictor variables

pred.vars.fact=c("Month.Year")



# Run the full subset model selection---
# This looks at all the possible combinations of predictor variables and shows
# the AIC, BIC and Rsqaured for each model
# Set the working directory so it saves the outputs into the owncloud folder 
setwd("C:\\Users\\21644425\\ownCloud\\Shared\\Jon data\\Model outputs")



# Check response variables-- Locations in this case!
unique.vars=unique(as.character(dat$Location))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Location==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use # Shows all the different locations - i.e. each boat ramp and the
# overll east model


# Full-subset models---
head(dat,3) # shows the top three rows of the data for all variables in the model
use.dat=dat 
resp.vars=unique.vars.use # THis splits it up into different location datasets
out.all=list() # This commands it to print a list of all model outputs
var.imp=list() # THis commands it to print a list of the importance of each variable

for(i in 1:length(resp.vars)){
  png(file=paste(study,resp.vars[i],"mod_fits.png",sep="_")) # THis tells it to make
  # a png image file of the model fits output
  use.dat=dat[which(dat$Location==resp.vars[i]),]
  
  
  # To start with just put a single continuous predictor in the model (MAX HOOK DEPTH)               # and the random factor (Julian day) and then the full subsets adds 
  # in all the other predictors as it tries all the other combinations of 
  # predictor variables
  
  Model1=gam(response~s(Max.hook..depth, k=5, bs="cr") + s(Julian.day,bs="re"),
             offset=log.Total.fish.hooked, 
             family=tw(),  
             data=use.dat)
  
  out.list=full.subsets.gam(use.dat=use.dat, 
                            test.fit=Model1, # Uses the GAM above
                            pred.vars.cont=pred.vars, # Continuous predictors 
                            pred.vars.fact=pred.vars.fact, # Factor predictors
                            smooth.interactions=NA, # stops it running interactions
                            parallel=T,
                            s.re="s(Julian.day,bs='re')") # Julian day as rnd fator
  names(out.list) # Shows the names of the models
  
  # examine the list of failed models
  out.list$failed.models
  
  # look at the model selection table
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),] # Tells it to order models by AIC,
  out.i=(mod.table)
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.r2.scaled))
  #write.csv(mod.table,"test_out_modfits_mgcv.csv")
  
  # plot the best model
  all.less.2AICc=mod.table[which(mod.table$delta.AICc<2),] # plots the best AIC model
  best.model.name=as.character(all.less.2AICc$modname[which.min(all.less.2AICc$edf)])
  if(best.model.name!="null"){
    par(mfrow=c(3,1),mar=c(9,4,3,1))
    best.model=out.list$success.models[[as.character(mod.table$modname[1])]]
    # plot(best.model$gam,all.terms=T,pages=1,residuals=T,pch=16) #change if you change the gam function
    plot(best.model,all.terms=T,pages=1,residuals=T,pch=16) # Tells is to plot best model with residuals
    mtext(side=2,text=resp.vars[i],outer=F)}
  dev.off()
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits,file=paste(Sys.Date(),study,"all.mod.fits.csv",sep="_"))
# Creates the csv for all the models fitted by the full subsets gam
write.csv(all.var.imp,file=paste(Sys.Date(),study,"all.var.imp.csv",sep="_"))
# Creates the csv showing the importance of each predictor variable

# Predictor variable importance plots (heatmaps)
# pdf(file=paste(name,"var_importance_heatmap.pdf",sep="_"),onefile=T)
png(file=paste(study,"var_importance_heatmap.png",sep="_")) # Creates a png heatmap

heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,8), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)
dev.off()


