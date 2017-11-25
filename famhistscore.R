#####################################
# Family History Score Vector Maker 
# Version 0.2
# Developed by sixtat@ymail.com
#####################################


#>>>>>>>>> Attention1: Code name for disease are "CAD", "MetS" which have to be in head of input .csv file
#>>>>>>>>> Attention2: FamilyLESS individuals Results in Errors but code works fine and result score is 99 for these individuals
#>>>>>>>>> Attention3: The code needs Refactorization in order to work in hardcore machine learning or production
#>>>>>>>>> Caution: Check and Edit address of input file @line:17 and output file @line:160


library(plyr)
#sorting Data
pedData <-
  read.csv(file = '/home/gryphosonix/Documents/Family-History/2nd.csv', header = TRUE)
sortedpedData <- arrange(pedData, PID, ID)

#building family history mock vector as fh

fh <- rep(99, nrow(sortedpedData))
fh <- cbind(fh, fh)
fh <- as.data.frame(cbind(sortedpedData$ID, fh))
# simulating Desease status as Dstatus without NA!
#Dstatus <- sample(c(0, 1), nrow(sortedpedData), replace = TRUE)
#merging deasease status with sorted pedigree data
#sortedpedData <- cbind(sortedpedData, Dstatus)

#you can switch CAD with MetS

for (i in sortedpedData$PID) {
  temp1 <- sortedpedData[sortedpedData$PID == i,]
  temp1 <- temp1[1:(nrow(temp1) - 2),]
  #temp1 <- cbind(1:nrow(temp1), temp1)
  for (j in temp1$ID) {
    fatherID <- temp1$FID[temp1$ID == j]
    motherID <- temp1$MID[temp1$ID == j]
    if (is.na(fatherID) || is.na(motherID))
    {
      fh[fh$V1 == j, 2] <- 'NA'
      next
    }
    else{
      if ((is.na(temp1$CAD[temp1$ID == motherID])) &&
          (is.na(temp1$CAD[temp1$ID == fatherID])))
      {
        fh[fh$V1 == j, 2] <- 8
        next
      }
      if ((is.na(temp1$CAD[temp1$ID == motherID])) &&
          (temp1$CAD[temp1$ID == fatherID] == 1))
      {
        fh[fh$V1 == j, 2] <- 7
        next
      }
      if ((is.na(temp1$CAD[temp1$ID == motherID])) &&
          (temp1$CAD[temp1$ID == fatherID] == 0))
      {
        fh[fh$V1 == j, 2] <- 6
        next
      }
      if ((is.na(temp1$CAD[temp1$ID == fatherID])) &&
          (temp1$CAD[temp1$ID == motherID] == 1))
      {
        fh[fh$V1 == j, 2] <- 5
        next
      }
      if ((is.na(temp1$CAD[temp1$ID == fatherID])) &&
          (temp1$CAD[temp1$ID == motherID] == 0))
      {
        fh[fh$V1 == j, 2] <- 4
        next
      }
      if ((temp1$CAD[temp1$ID == fatherID] == 1) &&
          (temp1$CAD[temp1$ID == motherID] == 1))
        fh[fh$V1 == j, 2] <- 3
      if ((temp1$CAD[temp1$ID == fatherID] == 0) &&
          (temp1$CAD[temp1$ID == motherID] == 1))
        fh[fh$V1 == j, 2] <- 2
      if ((temp1$CAD[temp1$ID == fatherID] == 1) &&
          (temp1$CAD[temp1$ID == motherID] == 0))
        fh[fh$V1 == j, 2] <- 1
      if ((temp1$CAD[temp1$ID == fatherID] == 0) &&
          (temp1$CAD[temp1$ID == motherID] == 0))
        fh[fh$V1 == j, 2] <- 0
      
      
      
      
    }
  }
  
}

#MetS
for (i in sortedpedData$PID) {
  temp1 <- sortedpedData[sortedpedData$PID == i,]
  temp1 <- temp1[1:(nrow(temp1) - 2),]
  #temp1 <- cbind(1:nrow(temp1), temp1)
  for (j in temp1$ID) {
    fatherID <- temp1$FID[temp1$ID == j]
    motherID <- temp1$MID[temp1$ID == j]
    if (is.na(fatherID) || is.na(motherID))
    {
      fh[fh$V1 == j, 3] <- 'NA'
      next
    }
    else{
      if ((is.na(temp1$MetS[temp1$ID == motherID])) &&
          (is.na(temp1$MetS[temp1$ID == fatherID])))
      {
        fh[fh$V1 == j, 3] <- 8
        next
      }
      if ((is.na(temp1$MetS[temp1$ID == motherID])) &&
          (temp1$MetS[temp1$ID == fatherID] == 1))
      {
        fh[fh$V1 == j, 3] <- 7
        next
      }
      if ((is.na(temp1$MetS[temp1$ID == motherID])) &&
          (temp1$MetS[temp1$ID == fatherID] == 0))
      {
        fh[fh$V1 == j, 3] <- 6
        next
      }
      if ((is.na(temp1$MetS[temp1$ID == fatherID])) &&
          (temp1$MetS[temp1$ID == motherID] == 1))
      {
        fh[fh$V1 == j, 3] <- 5
        next
      }
      if ((is.na(temp1$MetS[temp1$ID == fatherID])) &&
          (temp1$MetS[temp1$ID == motherID] == 0))
      {
        fh[fh$V1 == j, 3] <- 4
        next
      }
      if ((temp1$MetS[temp1$ID == fatherID] == 1) &&
          (temp1$MetS[temp1$ID == motherID] == 1))
        fh[fh$V1 == j, 3] <- 3
      if ((temp1$MetS[temp1$ID == fatherID] == 0) &&
          (temp1$MetS[temp1$ID == motherID] == 1))
        fh[fh$V1 == j, 3] <- 2
      if ((temp1$MetS[temp1$ID == fatherID] == 1) &&
          (temp1$MetS[temp1$ID == motherID] == 0))
        fh[fh$V1 == j, 3] <- 1
      if ((temp1$MetS[temp1$ID == fatherID] == 0) &&
          (temp1$MetS[temp1$ID == motherID] == 0))
        fh[fh$V1 == j, 3] <- 0
  
    }
  }
  
}
names(fh) <- c('ID', 'CAT.HIST', 'MetS.HIST')

outfileMerged <- cbind(sortedpedData,fh)
write.csv(outfileMerged, file = '/home/gryphosonix/Documents/Family-History/mergedFh.csv')
