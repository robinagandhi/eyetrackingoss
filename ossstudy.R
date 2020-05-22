setwd("./")

#Load libraries
library('corrplot')


# analysis of fixation + SUS survey data
userdata <- read.table("OSSstudy.txt", header=T)
df <- scale(userdata)
#png("subject-clusters.png")
h <- heatmap(df[,2:20], scale="column")
#graphics.off()

# just the heatmap's dendogram
#png("var-cluster.png")
plot(hclust(dist(t(df[,2:20]))), xlab="Variables")
#graphics.off()

> h$rowInd
 [1]  7  8  6  2 10  5  4  9  3  1
# based on the dendogram: 
grp1 <- c(1,3,9,4,5)
grp2 <- c(10,2,6,8,7)

# use SUS cluster 
# grp1 <- c(8,3,9) # liked the system per SUS question 
# grp2 <- c(7,5,2) # disliked the system

 
> dimnames(userdata)[[2]][2:20][h$colInd]
 [1] "melfixdurtext"     "lelfixdurhierachy" "SUS"              
 [4] "lelfixdurtext"     "lelfixcnthierachy" "melfixcnttext"    
 [7] "melfixcnthierachy" "lelfixcnttext"     "lelfixdurtask"    
[10] "melfixdurtask"     "melfixdurhierachy" "lelswitches"      
[13] "melswitches"       "melfixcnttask"     "lelfixcnttask"    
[16] "melfixcntmodel"    "lelfixcntmodel"    "lelfixdurmodel"   
[19] "melfixdurmodel"   

grpmeans <- rbind(colMeans(userdata[grp1,]), colMeans(userdata[grp2,]))
grpmeans[1,1] <- 1
grpmeans[2,1] <- 2

cat(grpmeans[1,],sep=",","\n")
cat(grpmeans[2,],sep=",","\n")
#1,21.2,393.4,442.2,170.8,36,607.8,399.2,207.2,0.4901723,0.4741953,0.4732816,0.460512,0.4123445,0.4815538,0.4806156,0.4024256,194,266.6,77.5
#2,15.2,78.2,124,96.8,31.4,102.6,179.8,160.2,0.3159235,0.2967673,0.3153115,0.3206456,0.2472615,0.2779236,0.3168535,0.3379985,83.8,121,76

#png("cluster-means.png")
par(mfrow=c(3,2))
barplot(grpmeans[,c(2,5,3,4)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="FIXATION COUNTS - MEL")
barplot(grpmeans[,c(6,9,7,8)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="FIXATION COUNTS - LEL")
barplot(grpmeans[,c(10,13,11,12)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN FIXATION DURATION (SECS) - MEL")
barplot(grpmeans[,c(14,17,15,16)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN FIXATION DURATION (SECS) - LEL")
barplot(grpmeans[,18:19], beside=T, names=c("MEL","LEL"), ylab="AOI SWITCHES")
#graphics.off()

# component analysis of SUS survey questions
#png("sus-clusters.png")
h <- heatmap(df[,21:30], scale="column") # SUS components
#graphics.off()

# corrplot view
#png("sus-corr.png")
corrplot(cor(df[,21:30]), order="hclust", addrect=3)
#graphics.off()

# show only the correlations that are significant at p=0.05
#png("sus-corr-sig-only.png")
cors <- cor.mtest(df[,21:30], conf.level=0.95)
corrplot(cor(df[,21:30]), p.mat=cors$p, order="hclust", addrec=3, sig.level=0.05, insig="blank")
# correlation groups: (1,2) and (5,6,7,8)
#graphics.off()


sgrp1 <- c(6,5,7,8,1,2,3)
sgrp2 <- c(10,4,9)
sgrpmeans <- rbind(colMeans(userdata[sgrp1,]), colMeans(userdata[sgrp2,]))
sgrpmeans[1,1] <- 1
sgrpmeans[2,1] <- 2
scolseq <- c(6,5,7,8,1,2,3,10,4,9) # SUS correlations based on heatmap clustering

#png("sus-cluster-means.png", width=720, height=480)
barplot(sgrpmeans[,20+scolseq], beside=T, names=paste("SUS",scolseq,sep=""), ylab="SUS COMPONENTS")
#graphics.off()

# examining the above cluster means, questions 6,5,7,8,2 have higher means for cluster 1
scolseq2 <- c(6,5,7,8,2) 
#png("sus65782-clusters.png")
h <- heatmap(df[,20+scolseq2], scale="column") # SUS components
#graphics.off()

# SUS: Learnability vs Usability

# learnability questions: 4,10
learncolseq <- c(4,10) 
#png("sus-learn-clusters.png")
h <- heatmap(df[,20+learncolseq], scale="column")
#graphics.off()

# usability questions 1,2,3,5,6,7,8,9
usecolseq <- c(1,2,3,5,6,7,8,9) 
#png("sus-use-clusters.png")
h <- heatmap(df[,20+usecolseq], scale="column")
#graphics.off()

##------------------------------------

# analysis of fixation + SUS survey + mouse data
userdata <- read.table("OSSstudy2.txt", header=T)
df <- scale(userdata)
#png("subject-clusters-withmouse.png")
h <- heatmap(df[,c(2:20,31:38)], scale="column")
#graphics.off()

# just the heatmap's dendogram
#png("var-cluster-withmouse.png")
plot(hclust(dist(t(df[,c(2:20,31:38)]))), xlab="Variables")
#graphics.off()

# corrplot view
#png("cluster-corr-withmouse.png")
cors <- cor.mtest(df[,c(2:20,31:38)], conf.level=0.95)
corrplot(cor(df[,c(2:20,31:38)]), p.mat=cors$p, order="hclust", addrec=6, sig.level=0.05, insig="blank")
#graphics.off()


# based on the dendogram: 
grp1 <- c(1,3,9,4,5)
grp2 <- c(10,2,6,8,7)

# use SUS cluster 
# grp1 <- c(8,3,9) # liked the system per SUS question 
# grp2 <- c(7,5,2) # disliked the system

 
grpmeans <- rbind(colMeans(userdata[grp1,]), colMeans(userdata[grp2,]))
grpmeans[1,1] <- 1
grpmeans[2,1] <- 2

cat(grpmeans[1,],sep=",","\n")
cat(grpmeans[2,],sep=",","\n")
#1,21.2,393.4,442.2,170.8,36,607.8,399.2,207.2,0.4901723,0.4741953,0.4732816,0.460512,0.4123445,0.4815538,0.4806156,0.4024256,194,266.6,77.5
#2,15.2,78.2,124,96.8,31.4,102.6,179.8,160.2,0.3159235,0.2967673,0.3153115,0.3206456,0.2472615,0.2779236,0.3168535,0.3379985,83.8,121,76



png("cluster-means-withmouse.png")
par(mfrow=c(4,2))
barplot(grpmeans[,c(2,5,3,4)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="FIXATION COUNTS - MEL")
barplot(grpmeans[,c(6,9,7,8)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="FIXATION COUNTS - LEL")
barplot(grpmeans[,c(10,13,11,12)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN FIXATION DURATION (SECS) - MEL")
barplot(grpmeans[,c(14,17,15,16)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN FIXATION DURATION (SECS) - LEL")
barplot(grpmeans[,c(31,34,32,33)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="COUNTS - MOUSE")
barplot(grpmeans[,c(35,38,36,37)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="DURATION (SECS) - MOUSE")
barplot(grpmeans[,18:19], beside=T, names=c("MEL","LEL"), ylab="AOI SWITCHES")
graphics.off()


##------------------------------------


# analysis of fixation + SUS survey + split mouse data
#userdata <- read.table("OSSstudy3.txt", header=T) # OSSstudy3.txt has total mouse duration
userdata <- read.table("OSSstudy4.txt", header=T) # OSSstudy4.txt has mean mouse duration
df <- scale(userdata)
#png("subject-clusters-splitmouse.png")
h <- heatmap(df[,c(2:20,39:54)], scale="column")
#graphics.off()


# just the heatmap's dendogram
#png("var-cluster-splitmouse.png")
plot(hclust(dist(t(df[,c(2:20,39:54)]))), xlab="Variables")
#graphics.off()

# corrplot view
#png("cluster-corr-splitmouse.png")
cors <- cor.mtest(df[,c(2:20,39:54)], conf.level=0.95)
corrplot(cor(df[,c(2:20,39:54)]), p.mat=cors$p, order="hclust", addrec=6, sig.level=0.05, insig="blank")
#graphics.off()


# based on the dendogram: 
grp1 <- c(1,3,9,4,5)
grp2 <- c(10,2,6,8,7)

# use SUS cluster 
# grp1 <- c(8,3,9) # liked the system per SUS question 
# grp2 <- c(7,5,2) # disliked the system

 
grpmeans <- rbind(colMeans(userdata[grp1,]), colMeans(userdata[grp2,]))
grpmeans[1,1] <- 1
grpmeans[2,1] <- 2

cat(grpmeans[1,],sep=",","\n")
cat(grpmeans[2,],sep=",","\n")
#1,21.2,393.4,442.2,170.8,36,607.8,399.2,207.2,0.4901723,0.4741953,0.4732816,0.460512,0.4123445,0.4815538,0.4806156,0.4024256,194,266.6,77.5
#2,15.2,78.2,124,96.8,31.4,102.6,179.8,160.2,0.3159235,0.2967673,0.3153115,0.3206456,0.2472615,0.2779236,0.3168535,0.3379985,83.8,121,76



#png("cluster-means-splitmouse.png", height=720, width=480)
par(mfrow=c(5,2))
barplot(grpmeans[,c(2,5,3,4)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="FIXATION COUNTS - MEL")
barplot(grpmeans[,c(6,9,7,8)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="FIXATION COUNTS - LEL")
barplot(grpmeans[,c(10,13,11,12)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN FIXATION DURATION (SECS) - MEL")
barplot(grpmeans[,c(14,17,15,16)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN FIXATION DURATION (SECS) - LEL")
barplot(grpmeans[,c(39,42,40,41)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MOUSE COUNTS - MEL")
barplot(grpmeans[,c(47,50,48,49)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MOUSE COUNTS - LEL")
barplot(grpmeans[,c(43,46,44,45)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MOUSE DURATION (SECS) - MEL")
barplot(grpmeans[,c(51,54,52,53)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MOUSE DURATION (SECS) - LEL")
barplot(grpmeans[,18:19], beside=T, names=c("MEL","LEL"), ylab="AOI SWITCHES")
#graphics.off()

---------------------------------------

# merge MEL and LEL data
mergedfixcount <- userdata[,2:5] + userdata[,6:9]
mergedfixdur <- (userdata[,2:5]*userdata[,10:13] + userdata[,6:9]*userdata[,14:17]) / mergedfixcount
mergedmousecount <- userdata[,39:42] + userdata[,47:50]
mergedmousedur <- (userdata[,39:42]*userdata[,43:46] + userdata[,47:50]*userdata[,51:54]) / mergedmousecount
panels <- c("hierarchy", "model", "task", "text") 
dimnames(mergedfixcount)[[2]] <- paste("fixcnt", panels, sep="")
dimnames(mergedfixdur)[[2]] <- paste("fixdur", panels, sep="")
dimnames(mergedmousecount)[[2]] <- paste("mousecnt", panels, sep="")
dimnames(mergedmousedur)[[2]] <- paste("mousedur", panels, sep="")
mergeddata <- cbind(mergedfixcount, mergedmousecount, mergedfixdur, mergedmousedur)
userdata2 <- cbind(userdata[,1], mergeddata, userdata[,18:19])

df <- scale(userdata2)
#png("subject-clusters-merged.png")
h <- heatmap(df[,c(2:19)], scale="column")  # exclude SUS data
#graphics.off()

# just the heatmap's dendogram
#png("var-cluster-merged.png")
plot(hclust(dist(t(df[,c(2:19)]))), xlab="Variables")
#graphics.off()

# corrplot view
#png("cluster-corr-merged.png")
cors <- cor.mtest(df[,c(2:19)], conf.level=0.95)
corrplot(cor(df[,c(2:19)]), p.mat=cors$p, order="hclust", addrec=6, sig.level=0.05, insig="blank")
#graphics.off()


# based on the dendogram: 
grp1 <- c(1,3,9,4,5)
grp2 <- c(10,2,6,8,7)

grpmeans <- rbind(colMeans(userdata2[grp1,]), colMeans(userdata2[grp2,]))
grpmeans[1,1] <- 1
grpmeans[2,1] <- 2

cat(grpmeans[1,],sep=",","\n")
cat(grpmeans[2,],sep=",","\n")
1,57.2,1001.2,841.4,378,183.2,979,1027.6,484.8,0.4985458,0.4793495,0.4774244,0.4329157,0.4275837,0.4707989,0.4503017,0.4223704,194,266.6,78.5,
2,46.6,180.8,303.8,257,21.8,142.2,610.6,299.4,0.2971177,0.2870401,0.3231182,0.3293223,0.4968952,0.2721106,0.3053489,0.3077527,83.8,121,75.5,

#png("cluster-means-merged.png", height=600, width=480)
par(mfrow=c(3,2))
barplot(grpmeans[,c(2,5,3,4)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="FIXATION COUNTS")
barplot(grpmeans[,c(6,9,7,8)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MOUSE COUNTS")
barplot(grpmeans[,c(10,13,11,12)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN FIXATION DURATION (SECS)")
barplot(grpmeans[,c(14,17,15,16)], beside=T, names=c("Hierarchy", "Text", "Model", "Task"), ylab="MEAN MOUSE DURATION (SECS)")
barplot(grpmeans[,18:19], beside=T, names=c("MEL","LEL"), ylab="AOI SWITCHES")
#graphics.off()
