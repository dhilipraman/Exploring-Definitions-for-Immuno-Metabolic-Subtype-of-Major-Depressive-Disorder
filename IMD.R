rm(list=ls(all=TRUE))

###Set Location of Save File
setwd("~/Documents/Classes/VUmc")  #change this for your own file location

###Libraries
library(ggplot2)
library(car)
library(pastecs)
library(psych)
library(grid)
library(gridExtra)
library(DescTools)
library(Hmisc)
library(multcomp)
library(haven)
library(afex)
library(emmeans)
library(nlme)
library(lme4)
library(ez)
library(MASS)
library(ggbeeswarm)
library(reshape2)

###Reading in Demographic Data 
demo <- read_sav('N1_A100R.sav')
head(demo)
tail(demo)
dim(demo)
names(demo)
summary(demo)
psych::describe(demo)

stat.desc(demo$Age, basic=F, norm=T)
stat.desc(demo$Sexe, basic=F, norm=T)


###Distribution of Ages in Histogram
hist.demo.age <- ggplot(demo, aes(age)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) + 
  labs(x = "Age of NESDA Patients", y = "Frequency Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(demo$Age, na.rm = TRUE), sd = sd(demo$Age, na.rm = TRUE)), colour = "red", size = 1)


###Reading in IDS Data
ids <- read_sav('N1_A235D.sav')
head(ids)
tail(ids)
dim(ids)
names(ids)
summary(ids)
psych::describe(ids)

stat.desc(ids$aids, basic=F, norm=T)

###Subsetting Atypical and Melancholic Symptom Groups
#neither atypical nor melancholic
none_subset <- subset(ids, aidsatyp==0)
none_subset <- subset(none_subset, aidsmel==0)
none_subset

#only atypical
atyp_subset <- subset(ids, aidsatyp==1)
atyp_subset <- subset(atyp_subset, aidsmel==0)
atyp_subset

#only melancholic
mel_subset <- subset(ids, aidsatyp==0)
mel_subset <- subset(mel_subset, aidsmel==1)
mel_subset

#both atypical and melancholic
both_subset <- subset(ids, aidsatyp==1)
both_subset <- subset(both_subset, aidsmel==1)
both_subset


###Distributions of IDS Scores in Atypical and Melancholic Groups Using Violin Plots
violin.none.set <- ggplot(none_subset, aes(x=aidsatyp,y=aids)) + labs(x = "Patients without Atypical/Melancholic Symptoms", y = "IDS Score") + geom_violin()
violin.atyp.set <- ggplot(atyp_subset, aes(x=aidsatyp,y=aids)) + labs(x = "Patients with only Atypical Symptoms", y = "IDS Score") + geom_violin()
violin.mel.set <- ggplot(mel_subset, aes(x=aidsmel,y=aids)) + labs(x = "Patients with only Melancholic Symptoms", y = "IDS Score") + geom_violin()
violin.both.set <- ggplot(both_subset, aes(x=aidsatyp,y=aids)) + labs(x = "Patients with both Atypical and Melancholic Symptoms", y = "IDS Score") + geom_violin()



violin.aids.atyp <- arrangeGrob(violin.none.set + stat_summary(fun=median, geom="point", size=2, color="red"), 
                                violin.atyp.set + stat_summary(fun=median, geom="point", size=2, color="red"), 
                                violin.mel.set + stat_summary(fun=median, geom="point", size=2, color="red"), 
                                violin.both.set + stat_summary(fun=median, geom="point", size=2, color="red"), ncol=2,nrow=2) 
grid.draw(violin.aids.atyp) 


###Distribution of IDS Scores in Overall Population using Histograms
hist.ids.aids <- ggplot(ids, aes(aids)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) + 
  labs(x = "IDS Scores in General NESDA Population", y = "Frequency Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(ids$aids, na.rm = TRUE), sd = sd(ids$aids, na.rm = TRUE)), colour = "red", size = 1)

hist.ids.aids


###Distribution of IDS Scores in Atypical and Melancholic Subsets using Histograms
hist.atyp <- ggplot(atyp_subset, aes(aids)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) + 
  labs(x = "IDS Scores in Patients with only Atypical Symptoms", y = "Frequency Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(atyp_subset$aids, na.rm = TRUE), sd = sd(atyp_subset$aids, na.rm = TRUE)), colour = "red", size = 1)

hist.mel <- ggplot(mel_subset, aes(aids)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) + 
  labs(x = "IDS Scores in Patients with only Melancholic Symptoms", y = "Frequency Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(mel_subset$aids, na.rm = TRUE), sd = sd(mel_subset$aids, na.rm = TRUE)), colour = "red", size = 1)

hist.none <- ggplot(none_subset, aes(aids)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) + 
  labs(x = "IDS Scores in Patients without Atypical or Melancholic Symptoms", y = "Frequency Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(none_subset$aids, na.rm = TRUE), sd = sd(none_subset$aids, na.rm = TRUE)), colour = "red", size = 1)

hist.both <- ggplot(both_subset, aes(aids)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) + 
  labs(x = "IDS Scores in Patients with Atypical and Melancholic Symptoms", y = "Frequency Density")  +         
  stat_function(fun = dnorm, args = list(mean = mean(both_subset$aids, na.rm = TRUE), sd = sd(both_subset$aids, na.rm = TRUE)), colour = "red", size = 1)

hist.ids.aids <- arrangeGrob(hist.none, hist.atyp, hist.mel, hist.both, ncol=2,nrow=2)
grid.draw(hist.ids.aids) 






###Reading in IDS Item Data (Individual Questions on IDS)
items <- read_sav('N1_A235R.sav')
head(items)
tail(items)
dim(items)
names(items)
summary(items)
psych::describe(items)

###IDS Item Data Without Labels
ids_items = data.matrix(items, rownames.force = NA)
head(ids_items)



###Correlation Table of IDS Questions With Other IDS Questions
item_corrs <- cor(items[, c("aids01","aids02","aids03","aids04","aids05","aids06","aids07","aids08","aids09a","aids09b","aids10","aids11","aids12","aids13","aids14","aids15","aids16","aids17","aids18","aids19","aids20","aids21","aids22","aids23","aids24","aids25","aids26","aids27","aids28")])
item_corrs <- as.data.frame(item_corrs)
psych::describe(item_corrs)
head(item_corrs)
dim(item_corrs)



###Reading in CIDI Data
cidi <- read_sav('N1_A257D.sav')
head(cidi)
tail(cidi)
dim(cidi)
names(cidi)
summary(cidi)
psych::describe(cidi)



###Reading in BMI Data
bmi <- read_sav('N1_A357D.sav')
head(bmi)
tail(bmi)
dim(bmi)
names(bmi)
summary(bmi)
psych::describe(bmi)

###rename pident in BMI file (Very Important!)
names(bmi)[1] <- "pident"
head(bmi)


###Reading in Blood Markers Data
blood <- read_sav('N1_A401R.sav')
head(blood)
tail(blood)
dim(blood)
names(blood)
summary(blood)
psych::describe(blood)

###Adding in Trig_HDL Ratio (Very Important!)
blood$trig_hdl_ratio <- blood$Atriglyceride / blood$Ahdl_cholesterol


###Reading in INFLAMMATION Data        
infla <- read_sav('N1_A404C.sav')
head(infla)
tail(infla)
dim(infla)
names(infla)
summary(infla)
psych::describe(infla)



###Reading in Leptin Data
leptin <- read_sav('N1_A416R.sav')
head(leptin)
tail(leptin)
dim(leptin)
names(leptin)
summary(leptin)
psych::describe(leptin)

###Recoding All IDS Items to -2 to 3 Scale for Uniformity
items <- read_sav('N1_A235R.sav')
a04 <- as.integer(items$aids04)
a11 <- as.integer(items$aids11)
a12 <- as.integer(items$aids12)
a18 <- as.integer(items$aids18)
a28 <- as.integer(items$aids28)

aids04_rec <- dplyr::recode(a04, '-2' = -2, '-1' = -1, `1` = 0, `2` = 1, '3' = 2, '4' = 3)
aids11_rec <- dplyr::recode(a11, '-2' = -2, '-1' = -1, `1` = 0, `2` = 0, '3' = 0, '4' = 0, '5' = 1, '6' = 2, '7' = 3)
aids12_rec <- dplyr::recode(a12, '-2' = -2, '-1' = -1, `1` = 0, `2` = 0, '3' = 0, '4' = 0, '5' = 1, '6' = 2, '7' = 3)
aids18_rec <- dplyr::recode(a18, '-2' = -2, '-1' = -1, `1` = 0, `2` = 1, '3' = 2, '4' = 3)
aids28_rec <- dplyr::recode(a28, '-2' = -2, '-1' = -1, `1` = 0, `2` = 1, '3' = 2, '4' = 3)

items$aids04 <- aids04_rec
items$aids11 <- aids11_rec
items$aids12 <- aids12_rec
items$aids18 <- aids18_rec
items$aids28 <- aids28_rec

ids2 <- items
#Adding 'Atyp' Column for Total Atypical Score
ids2$atyp <- ids2$aids04 + ids2$aids11 + ids2$aids12 + ids2$aids18 + ids2$aids28
psych::describe(ids2)


###THE GREAT MERGE - Adding All Files into One Master File (for Wave 0 Data)
demo_new <- demo[,c(1:3,9)]
ids_all <- ids[,c(1:3,6:7)]
ids_all <- merge(demo_new, ids_all, by="pident")
items2 <- ids2[,c(1,32)]
ids_all <- merge(ids_all, items2, by="pident")
current <- cidi[,c(1,6)]
ids_all <- merge(ids_all, current, by="pident")
ids_all <- merge(ids_all, bmi, by="pident")
infla_new <- infla[,c(1,2,4,6)]
ids_all <- merge(ids_all, infla_new, by="pident")
blood_new <- blood[,c(1,8:12,28)]
ids_all <- merge(ids_all, blood_new, by="pident")
leptin_new <- leptin[,c(1,3)]
ids_all <- merge(ids_all, leptin_new, by="pident")
head(ids_all)

psych::describe(ids_all)


###Excluding NAs
ids3 <- ids_all[rowSums(is.na(ids_all)) < 1,]
head(ids3)
psych::describe(ids3)

###Excluding <0 Values (Which Are Missings)
ids4 <- subset(ids3, aids > -1)
ids4 <- subset(ids4, abmi != -1)
ids4 <- subset(ids4, atyp > -1)
#Data Without Missings (Check n)
psych::describe(ids4)
#Data With Missings (Check n)
psych::describe(ids_all)



###Subset by Gender
genids <- subset(ids4, Sexe==2)
psych::describe(genids)

###Subset by Current Depression Diagnosis
current <- subset(ids4, acidep05==1)
#current <- subset(ids_all, acidep05==1)
psych::describe(current)
#Subset by Gender and Current Diagnosis
gencurr <- subset(current, Sexe==2)
psych::describe(gencurr)



###Setting Wave 0 Cutoffs (For Quintile and Quartile Code Scroll Down to Longitudinal Data)
hsCRP_cut3 <- subset(ids4, ahsCRP < 3)
hsCRP_cut2 <- subset(ids4, ahsCRP > 3)
hsCRP_cut2 <- subset(hsCRP_cut2, ahsCRP < 6)
hsCRP_cut <- subset(ids4, ahsCRP > 6)
psych::describe(hsCRP_cut3)
psych::describe(hsCRP_cut2)
psych::describe(hsCRP_cut)

summary(hsCRP_cut$Atriglyceride)

BMI_cut3 <- subset(ids4, abmi < 24.99)
BMI_cut2 <- subset(ids4, abmi > 24.99)
BMI_cut <- subset(ids4, abmi > 29.99)
psych::describe(BMI_cut3)
psych::describe(BMI_cut2)
psych::describe(BMI_cut)

summary(BMI_cut$Atriglyceride)

trig.hdl_cut3 <- subset(ids4, trig_hdl_ratio > 1.99)
trig.hdl_cut2 <- subset(ids4, trig_hdl_ratio > 3.99)
trig.hdl_cut <- subset(ids4, trig_hdl_ratio > 5.99)
psych::describe(trig.hdl_cut3)
psych::describe(trig.hdl_cut2)
psych::describe(trig.hdl_cut)


###Overlays of Cutoff Groups using Histograms
hgA <- hist(hsCRP_cut3$abmi, breaks = 12)
hgB <- hist(hsCRP_cut$abmi, breaks = 12)
hgC <- hist(hsCRP_cut2$abmi, breaks = 12)

range(c(hgA$breaks, hgB$breaks, hgC$breaks)) # Get range for x-axis
#[1] 14 8

max(c(hgA$count, hgB$count, hgC$count)) # Get range for y-axis
#[1] 788

plot(hgA, col = "white", xlim = c(14, 60), ylim = c(0,452), main="Distribution of BMI Levels in hsCRP Benchmark Cutoffs",
     xlab="BMI Levels", ylab="Frequency")
plot(hgB, add = TRUE, col = "chartreuse4") 
plot(hgC, add = TRUE, col = "coral") 
legend("topright", inset=.01, title="Marker",
       c("hsCRP>6","hsCRP>3,<6","hsCRP<3"), fill=terrain.colors(3), horiz=TRUE, cex=0.8)












### Longitudinal Data (All Waves, All Files) ##################

###IDS Waves
Bids <- read_sav('N1_B235D.sav')
Cids <- read_sav('N1_C235D.sav')

###Inflammation Levels
Cinfla <- read_sav('N1_C404R.sav')
Einfla <- read_sav('N1_E404R.sav')
Finfla <- read_sav('N1_F404R.sav')
#Clean Inflammation File (Use Instead of Cinfla)
CinflaFL <- read_sav('N1_C404_FL.sav')
psych::describe(CinflaFL)


###Blood Marker Waves
Cblood <- read_sav('N1_C401R.sav')
Eblood <- read_sav('N1_E401R.sav')
Fblood <- read_sav('N1_F401R.sav')


###CIDI Waves
Ccidi <- read_sav('N1_C257D.sav')
names(Ccidi)

###Items Waves
Citems <- read_sav('N1_C235R.sav')
psych::describe(Citems)


###Recoding Wave 3 Items from -2 to 3 Scale
c04 <- as.integer(Citems$cids04)
c11 <- as.integer(Citems$cids11)
c12 <- as.integer(Citems$cids12)
c18 <- as.integer(Citems$cids18)
c28 <- as.integer(Citems$cids28)

cids04_rec <- dplyr::recode(c04, '-3' = -2, '-1' = -1, `1` = 0, `2` = 1, '3' = 2, '4' = 3)
cids11_rec <- dplyr::recode(c11, '-3' = -2, '-1' = -1, `1` = 0, `2` = 0, '3' = 0, '4' = 0, '5' = 1, '6' = 2, '7' = 3)
cids12_rec <- dplyr::recode(c12, '-3' = -2, '-1' = -1, `1` = 0, `2` = 0, '3' = 0, '4' = 0, '5' = 1, '6' = 2, '7' = 3)
cids18_rec <- dplyr::recode(c18, '-3' = -2, '-1' = -1, `1` = 0, `2` = 1, '3' = 2, '4' = 3)
cids28_rec <- dplyr::recode(c28, '-3' = -2, '-1' = -1, `1` = 0, `2` = 1, '3' = 2, '4' = 3)

Citems$cids04 <- cids04_rec
Citems$cids11 <- cids11_rec
Citems$cids12 <- cids12_rec
Citems$cids18 <- cids18_rec
Citems$cids28 <- cids28_rec
psych::describe(Citems)
Cids2 <- Citems
Cids2$Catyp <- Cids2$cids04 + Cids2$cids11 + Cids2$cids12 + Cids2$cids18 + Cids2$cids28

psych::describe(Cids2)







#Add Covariates Smoking and Physical Activity
Asmoke <- read_sav('N1_A200D1 (Fagerstrom).sav')
Asmoke$pident <- Asmoke$Pident
Aipaq <- read_sav('N1_A206D.sav')
Cipaq <- read_sav('N1_C206D.sav')



###IDS Longitudinal Master File (All Waves, All Files)
ids_long <- ids[,c(1:3,6:7)]
Cids_new <- Cids[,c(1:3,6:7)]
ids_long <- merge(ids_long, Cids_new, by="pident")
blood_new <- blood[,c(1,8:12,28)]
ids_long <- merge(ids_long, blood_new, by="pident")
Cblood_new <- Cblood[,c(1,12,13,15:17)]
ids_long <- merge(ids_long, Cblood_new, by="pident")
infla_new <- infla[,c(1,2,4,6)]
ids_long <- merge(ids_long, infla_new, by="pident")
CinflaFL$pident <- Cinfla$Pident
Cinfla_new <- CinflaFL[,c(2,6,19)]
ids_long <- merge(ids_long, Cinfla_new, by="pident")
cidi_new <- cidi[,c(1,6)]
ids_long <- merge(ids_long, cidi_new, by="pident")
Ccidi_new <- Ccidi[,c(1,5)]
ids_long <- merge(ids_long, Ccidi_new, by="pident")
ids4_new <- ids4[,c(1:4,9,11,21)]
ids_long <- merge(ids_long, ids4_new, by="pident")
Cids2_new <- Cids2[,c(1,32)]
ids_long <- merge(ids_long, Cids2_new, by="pident")
Asmoke_new <- Asmoke[,c(9,11)]
ids_long <- merge(ids_long, Asmoke_new, by='pident')
###IPAQ Data excluded for too many missings
#Aipaq_new <- Aipaq[,c(1,9)]
#ids_long <- merge(ids_long, Aipaq_new, by='pident')
#Cipaq_new <- Cipaq[,c(1,9)]
#ids_long <- merge(ids_long, Cipaq_new, by='pident')

psych::describe(ids_long)
head(ids_long)


###Excluding Missings for Longitudinal Data 
ids_long2 <- ids_long[rowSums(is.na(ids_long)) < 1,]
ids_long3 <- subset(ids_long2, aids > -1)
ids_long3 <- subset(ids_long3, cids > -1)
ids_long3 <- subset(ids_long3, abmi!= -1)
###Adding Trig_HDl Ratio for Wave 3
ids_long3$Ctrig_hdl_ratio <- ids_long3$CTriglyceride / ids_long3$Chdl_cholesterol


ids_long4 <- ids_long3
ids_long4$chol_change <- ids_long4$Ccholesterol - ids_long4$Acholesterol
ids_long4$hdl_change <- ids_long4$Chdl_cholesterol - ids_long4$Ahdl_cholesterol
ids_long4$ldl_change <- ids_long4$Cldl_cholesterol - ids_long4$Aldl_cholesterol
ids_long4$trig_change <- ids_long4$CTriglyceride - ids_long4$Atriglyceride
ids_long4$trig_hdl_change <- ids_long4$Ctrig_hdl_ratio - ids_long4$trig_hdl_ratio
ids_long4$gluc_change <- ids_long4$Cglucose - ids_long4$Aglucose
ids_long4$hsCRP_change <- ids_long4$cHSCRP - ids_long4$ahsCRP
ids_long4$IL6_change <- ids_long4$cIL6 - ids_long4$aIL6

### This is the Master Longitudinal File (N=1972) With All Waves and All Files
psych::describe(ids_long4)


###subset of people with Current Depression Diagnosis
current_long4 <- subset(ids_long4, acidep05==1)
current_long4 <- subset(current_long4, Catyp > -1)

### This is the Master Longitudinal File (N=675) for Current Depression
psych::describe(current_long4)
names(current_long4)



###Creating Quintile and Quartile Tables

#Quintiles
quintiles <- data.frame(Doubles=double(),
                        Ints=integer(),
                        Factors=factor(),
                        Logicals=logical(),
                        Characters=character(),
                        stringsAsFactors=FALSE)


#remove NAs in Case
ids9 <- ids4[complete.cases(ids4$aTNFa),]
ids9 <- ids9[complete.cases(ids9$Aldl_cholesterol),]
ids9 <- ids9[complete.cases(ids9$aLeptine),]
ids9 <- ids9[complete.cases(ids9$Aglucose),]

psych::describe(ids9)

aids <- quantile(current_long4$aids, probs=seq(0,1,1/5))
aidssev <- quantile(current_long4$aidssev, probs=seq(0,1,1/5))
atyp <- quantile(current_long4$atyp, probs=seq(0,1,1/5))
ahsCRP <- quantile(current_long4$ahsCRP, probs=seq(0,1,1/5))
abmi <- quantile(current_long4$abmi, probs=seq(0,1,1/5))
TNFa <- quantile(current_long4$aTNFa, probs=seq(0,1,1/5))
IL6 <- quantile(current_long4$aIL6, probs=seq(0,1,1/5))
Aglucose <- quantile(current_long4$Aglucose, probs=seq(0,1,1/5))
Acholesterol <- quantile(current_long4$Acholesterol, probs=seq(0,1,1/5))
Atriglyceride <- quantile(current_long4$Atriglyceride, probs=seq(0,1,1/5))
Ahdl_cholesterol <- quantile(current_long4$Ahdl_cholesterol, probs=seq(0,1,1/5))
Aldl_cholesterol <- quantile(current_long4$Aldl_cholesterol, probs=seq(0,1,1/5))
trig_hdl_ratio <- quantile(current_long4$trig_hdl_ratio, probs=seq(0,1,1/5))
aLeptine <- quantile(current_long4$aLeptine, probs=seq(0,1,1/5))
CIL6 <- quantile(current_long4$cIL6, probs=seq(0,1,1/5))
Cldl_cholesterol <- quantile(current_long4$Cldl_cholesterol, probs=seq(0,1,1/5))

quintiles <- data.frame(matrix(unlist(aids), nrow=6, byrow=TRUE),stringsAsFactors=FALSE)
quintiles$aids <- quintiles$matrix.unlist.aids...nrow...6..byrow...TRUE.
quintiles$atyp <- atyp
quintiles$ahsCRP <- ahsCRP
quintiles$abmi <- abmi
quintiles$TNFa <- TNFa
quintiles$IL6 <- IL6
quintiles$Acholesterol <- Acholesterol
quintiles$Aglucose <- Aglucose
quintiles$Atriglyceride <- Atriglyceride
quintiles$Ahdl_cholesterol <- Ahdl_cholesterol
quintiles$Aldl_cholesterol <- Aldl_cholesterol
quintiles$trig_hdl_ratio <- trig_hdl_ratio
quintiles$aLeptine <- aLeptine
quintiles$CIl6 <- CIL6
quintiles$Cldl_cholesterol <- Cldl_cholesterol
quintiles <- quintiles[,c(2:16)]
quintiles$percentile <- list(0, 20, 40, 60, 80, 100)


names(quintiles)
quintiles


#Quartiles
aids <- quantile(current_long4$aids, probs=seq(0,1,1/4))
aidssev <- quantile(current_long4$aidssev, probs=seq(0,1,1/4))
atyp <- quantile(current_long4$atyp, probs=seq(0,1,1/4))
ahsCRP <- quantile(current_long4$ahsCRP, probs=seq(0,1,1/4))
abmi <- quantile(current_long4$abmi, probs=seq(0,1,1/4))
TNFa <- quantile(current_long4$aTNFa, probs=seq(0,1,1/4))
IL6 <- quantile(current_long4$aIL6, probs=seq(0,1,1/4))
Aglucose <- quantile(current_long4$Aglucose, probs=seq(0,1,1/4))
Acholesterol <- quantile(current_long4$Acholesterol, probs=seq(0,1,1/4))
Atriglyceride <- quantile(current_long4$Atriglyceride, probs=seq(0,1,1/4))
Ahdl_cholesterol <- quantile(current_long4$Ahdl_cholesterol, probs=seq(0,1,1/4))
Aldl_cholesterol <- quantile(current_long4$Aldl_cholesterol, probs=seq(0,1,1/4))
trig_hdl_ratio <- quantile(current_long4$trig_hdl_ratio, probs=seq(0,1,1/4))
aLeptine <- quantile(current_long4$aLeptine, probs=seq(0,1,1/4))
CIL6 <- quantile(current_long4$cIL6, probs=seq(0,1,1/4))
Cldl_cholesterol <- quantile(current_long4$Cldl_cholesterol, probs=seq(0,1,1/4))

quartiles <- data.frame(matrix(unlist(aids), nrow=5, byrow=TRUE),stringsAsFactors=FALSE)
quartiles$aids <- quartiles$matrix.unlist.aids...nrow...5..byrow...TRUE.
quartiles$atyp <- atyp
quartiles$ahsCRP <- ahsCRP
quartiles$abmi <- abmi
quartiles$TNFa <- TNFa
quartiles$Il6 <- IL6
quartiles$Aglucose <- Aglucose
quartiles$Acholesterol <- Acholesterol
quartiles$Atriglyceride <- Atriglyceride
quartiles$Ahdl_cholesterol <- Ahdl_cholesterol
quartiles$Aldl_cholesterol <- Aldl_cholesterol
quartiles$trig_hdl_ratio <- trig_hdl_ratio
quartiles$aLeptine <- aLeptine
quartiles$CIl6 <- CIL6
quartiles$Cldl_cholesterol <- Cldl_cholesterol
quartiles <- quartiles[,c(2:16)]
quartiles$percentile <- list(0, 25, 50, 75, 100)

names(quartiles)
quartiles



###Top Quintile Groups
psych::describe(ids4)
aids_quint <- subset(ids4, aids > 42.99)
atyp_quint <- subset(ids4, atyp > 6.99)
bmi_quint <- subset(ids4, abmi > 29.75)
hsCRP_quint <- subset(ids4, ahsCRP > 4.03)
TNFa_quint <- subset(ids4, aTNFa > 1.19)
IL6_quint <- subset(ids4, aIL6 > 1.52)
chol_quint <- subset(ids4, Acholesterol > 5.89)
gluc_quint <- subset(ids4, Aglucose > 5.59)
trig_quint <- subset(ids4, Atriglyceride > 1.67)
hdl_quint <- subset(ids4, Ahdl_cholesterol > 1.98)
ldl_quint <- subset(ids4, Aldl_cholesterol > 3.89)
trig_hdl_quint <- subset(ids4, trig_hdl_ratio > 1.22)
lept_quint <- subset(ids4, aLeptine > 24.19)


###Venn Diagrams
psych::describe(atyp_quint)
psych::describe(bmi_quint)
psych::describe(hsCRP_quint)

atyp_bmi <- subset(atyp_quint, abmi > 29.75)
hsCRP_bmi <- subset(hsCRP_quint, abmi > 29.75)
atyp_hsCRP <- subset(hsCRP_quint, atyp > 6.99)
atyp_hsCRP_bmi <- subset(hsCRP_bmi, atyp > 6.99)
psych::describe(atyp_hsCRP)
rcorr(current$ahsCRP, current$atyp,type=c("spearman"))




###Venn Diagrams for Cutoffs
atyp_cutl <- subset(current_long4, atyp > 5.99)
bmi_cutl <- subset(current_long4, abmi > 29.99)
hsCRP_cutl <- subset(current_long4, ahsCRP > 5.99)

psych::describe(atyp_cutl)
psych::describe(bmi_cutl) 
psych::describe(hsCRP_cutl)

atyp_bmi <- subset(atyp_cutl, abmi > 29.99)
hsCRP_bmi <- subset(hsCRP_cutl, abmi > 29.99)
atyp_hsCRP <- subset(hsCRP_cutl, atyp > 5.99)
atyp_hsCRP_bmi <- subset(hsCRP_bmi, atyp > 5.99)
psych::describe(atyp_bmi)
rcorr(current_long$ahsCRP, current_long$abmi,type=c("spearman"))


names(current_long4)


###############################################
### Simple and Multiple Regression
###############################################

hsCRP_atyp_chx <- lm(Catyp ~ ahsCRP + atyp, data=current_long4, na.action=na.exclude)
summary(hsCRP_atyp_chx)


### Multiple Regression Models
#Atypical Score
atyp_lm <- lm(Catyp ~ ahsCRP + atyp, data=current_long4)
summary(atyp_lm)


#Add Inflammation
atyp_I_lm <- lm(Catyp ~ aIL6 + aTNFa + ahsCRP + atyp, data=current_long4)
summary(atyp_I_lm)
anova(atyp_lm, atyp_I_lm)

#Add Demographics
atyp_I_D_lm <- lm(Catyp ~ atyp + aIL6 + aTNFa + ahsCRP + Age + Sexe + aedu, data=current_long4)
summary(atyp_I_D_lm)
anova(atyp_I_lm, atyp_I_D_lm)

#Add Triglycerides
atyp_I_D_T_lm <- lm(Catyp ~ aIL6 + aTNFa + ahsCRP + atyp + Age + Sexe + aedu + Ahdl_cholesterol + Aldl_cholesterol + trig_hdl_ratio + Atriglyceride, data=current_long4)
summary(atyp_I_D_T_lm)
anova(atyp_lm, atyp_I_lm, atyp_I_D_lm, atyp_I_D_T_lm)

atyp_I_D_T_lm <- lm(Ahdl_cholesterol ~ aIL6 + aTNFa + ahsCRP + atyp + Age + Sexe + aedu + Aglucose + aLeptine + Aldl_cholesterol + trig_hdl_ratio + Atriglyceride, data=current_long4)




###hsCRP Model Current MDD
catyp_lm <- lm(Catyp ~ ahsCRP + atyp, data=current_long4)
catyp_I_lm <- lm(Catyp ~ aIL6 + aTNFa + ahsCRP + atyp, data=current_long4)
catyp_I_D_lm <- lm(Catyp ~ atyp + aIL6 + aTNFa + ahsCRP + Age + Sexe + aedu, data=current_long4)
catyp_I_D_T_lm <- lm(Catyp ~ aIL6 + aTNFa + ahsCRP + atyp + Age + Sexe + aedu + Ahdl_cholesterol + Aldl_cholesterol + trig_hdl_ratio + Atriglyceride, data=current_long4)
anova(catyp_lm, catyp_I_lm, catyp_I_D_lm, catyp_I_D_T_lm)

###hsCRP Model Current MDD within BMI Cutoff
catyp_lm <- lm(Catyp ~ ahsCRP + atyp, data=bmi_cutl)
catyp_I_lm <- lm(Catyp ~ aIL6 + aTNFa + ahsCRP + atyp, data=bmi_cutl)
catyp_I_D_lm <- lm(Catyp ~ atyp + aIL6 + aTNFa + ahsCRP + Age + Sexe + aedu, data=bmi_cutl)
catyp_I_D_T_lm <- lm(Catyp ~ aIL6 + aTNFa + ahsCRP + atyp + Age + Sexe + aedu + Ahdl_cholesterol + Aldl_cholesterol + trig_hdl_ratio + Atriglyceride, data=bmi_cutl)
anova(catyp_lm, catyp_I_lm, catyp_I_D_lm, catyp_I_D_T_lm)


# Other useful functions 
#lm.beta(CRP_I_D_lm)                    # standardized betas
#coefficients(regr1)               # model coefficients
#confint(atyp_I_D_lm, level=0.95)        # CIs for model parameters 



#MULTIPLE Regression
hsCRPchex <- lm(hsCRP_change ~ aTNFa + aIL6 + trig_hdl_ratio + abmi + Aglucose + Aldl_cholesterol + cIL6 + Cldl_cholesterol
                , data=current_long4, na.action=na.exclude)
summary(hsCRPchex)

###What Markers are Significantly Predictive of hsCRP Change?
#Winners: aTNFa + aIL6 + trig_hdl_ratio + abmi + Aglucose + Aldl_cholesterol + cIL6 + Chdl_cholesterol
#Losers: Atriglyceride, Ahdl_cholesterol, atyp, aids, Acholesterol, aLeptine, Ccholesterol, CTriglyceride, Cglucose, Cldl_cholesterol

#highest adjusted R^2: 0.02813



###Other Regressions

#Quantile Regression (Conditional Median)
install.packages("quantreg")
library(quantreg)

model1 = rq(hsCRP_change ~ Aglucose,data = current_long4,tau = 0.50)
summary(model1)


###Standardized Beta
library("QuantPsyc")
mod <- lm(Catyp ~ Age + atyp, data=current_long4)
lm.beta(mod)




#### Change on Change Analysis ####

###Setting All Change Scores
current_long4$logCatyp <- log(current_long4$Catyp)
current_long4$logatyp <- log(current_long4$atyp)
current_long4$logCglucose <- log(current_long4$Cglucose)
current_long4$logAglucose <- log(current_long4$Aglucose)
current_long4$log_gluc_change <- current_long4$logCglucose - current_long4$logAglucose
current_long4$logCIL6 <- log(current_long4$cIL6)
current_long4$logAIL6 <- log(current_long4$aIL6)
current_long4$log_Il6_change <- current_long4$logCIL6 - current_long4$logAIL6
current_long4$logAhsCRP <- log(current_long4$ahsCRP)
current_long4$logChsCRP <- log(current_long4$cHSCRP)
current_long4$log_hsCRP_change <- current_long4$logChsCRP - current_long4$logAhsCRP
current_long4$logAtrighdl <- log(current_long4$trig_hdl_ratio)
current_long4$logCtrighdl <- log(current_long4$Ctrig_hdl_ratio)
current_long4$log_trighdl_change <- current_long4$logCtrighdl - current_long4$logAtrighdl
current_long4$logAtrig <- log(current_long4$Atriglyceride)
current_long4$logCtrig <- log(current_long4$CTriglyceride)
current_long4$log_trig_change <- current_long4$logCtrig - current_long4$logAtrig
current_long4$logALDL <- log(current_long4$Aldl_cholesterol)
current_long4$logCLDL <- log(current_long4$Cldl_cholesterol)
current_long4$log_LDL_change <- current_long4$logCLDL - current_long4$logALDL
current_long4$logAHDL <- log(current_long4$Ahdl_cholesterol)
current_long4$logCHDL <- log(current_long4$Chdl_cholesterol)
current_long4$log_HDL_change <- current_long4$logCHDL - current_long4$logAHDL
current_long4$logAchol <- log(current_long4$Acholesterol)
current_long4$logCchol <- log(current_long4$Ccholesterol)
current_long4$log_chol_change <- current_long4$logCchol - current_long4$logAchol
current_long4$logTNFa <- log(current_long4$aTNFa)
current_long4$logLeptin <- log(current_long4$aLeptine)
current_long4$atyp_change <- current_long4$Catyp - current_long4$atyp
current_long4$hdl_change <- current_long4$Chdl_cholesterol - current_long4$Ahdl_cholesterol
current_long4$hdl_change <- current_long4$Chdl_cholesterol - current_long4$Ahdl_cholesterol



###Change on Change Regression With and Without Log Transformation
#Instead of repeating the code for each regression, I just replaced the variables
library(lm.beta)
model_lm <- lm(Cglucose ~ atyp_change + Aglucose, data=current_long4)
summary(lm.beta(model_lm)) #standardized model results

model_lm <- lm(logCglucose ~ atyp_change + logAglucose, data=current_long4)
summary(lm.beta(model_lm)) #standardized model results

model_lm <- lm(Catyp ~ aLeptine + atyp, data=current_long4)
summary(lm.beta(model_lm)) #standardized model results





###Chronicity of depression
#Predict if both wave0 and wave3 acidep05 are 1

###Chronicity predictors: Atypical Score, hsCRP, IL6, TNFa 

#model1: atypical + demo
#model2: add smoking and physical activity
#model 3: add bmi


###LOGISTIC REGRESSION
model_glm <- glm(ccidep05 ~ aIL6 + Age + trig_hdl_ratio + atyp, data=current_long4, family=binomial())
model_glm <- glm(ccidep05 ~ Catyp + Age + Sexe + aedu, data=current_long4, family=binomial())
model_glm <- glm(ccidep05 ~ atyp, data=current_long4, family=binomial())
summary(lm.beta(model_glm)) #model results
exp(model_glm$coefficients) #gives Odd Ratio
exp(confint(model_glm)) #gives Odd Ratio CI's











