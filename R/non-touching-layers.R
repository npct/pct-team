# Aim: find lowest number of layers of non-overlapping lines

#Doesn work yet

# the rgeos way
library(rgeos)
library(sp)
library(rgdal)
library(gdalUtils)
library(stplanr)

rf = readRDS("../pct-data/isle-of-wight/rf.Rds")
rfA = rf
overlap = gOverlaps(rfA, byid = T) # find overlapping lines
rownames(overlap) <- rfA$id
colnames(overlap) <- rfA$id
overlap = 1 * overlap # convert T/F to 1/0
overlap2 = overlap # create duplicate for working on
groups =  matrix(nrow = nrow(rf), ncol = 2) #create a maxtrix of IDs to store which group they should go in
groups[,1] = rfA$id

#Loop which pulls out the most intersecting route and adds it to a group
for(i in 1:nrow(rf)){
  rowsum = data.frame(name=rownames(overlap2),count=rowSums(overlap2))

  if(max(rowsum[,2]) == 0){ # if no overlapping lines do this
    #writeOGR(rf1,layer = paste0("ras",i), dsn="../pct-lsoa-test/data/rastest", driver = "ESRI Shapefile")
    print("no overlapping lines")
    break()
  }
  else{ #some over-lapping lines so do this
    row = as.character(subset(rowsum, count == max(rowsum$count))[1,1])
    rf1 = rfA[which(rfA$id==row),] # find the line with the most overlaps
    groups[which(groups[,1] == rf1$id),2] <- i # Add this line to a group
    print(paste0(rf1$id," added to group ",i, " in the outer loop"))
    #check for other lines that don't overlap with this line
    partners = subset(overlap2, rownames(overlap2) == row) #subset to jsut the relavant line
    sel = !partners[1,]
    partners = colnames(partners)[sel]
    partners_overlap = overlap2[partners,partners] #make a subset of matrix just for possible partners
    print(paste0("there are ",nrow(partners_overlap)," in the outer loop"))
    #print("get ready")
    overlap3 = subset(overlap2, colnames(overlap2) != row, rownames(overlap2) != row)
    #print("subset worked")
    overlap2 = overlap3
    #print("overlap2 worked")
    #overlap2 = subset(overlap2, rownames(overlap2) != row) # remove the line from the matrix
    rfA = rfA[which(rfA$id!=rf1$id),] # remove the line from the remaining set of lines
    go = TRUE
    print(paste0("start of the inner loop go is ",go))
    if(go == TRUE){ #should we look for another set of partners
      #make a rowsum for just partners
      rowsumP = data.frame(name=rownames(partners_overlap),count=rowSums(partners_overlap))
      rowP = as.character(subset(rowsumP, count == max(rowsumP$count))[1,1])
      rf2 = rfA[which(rfA$id==rowP),]
      groups[which(groups[,1] == rf2$id),2] <- i # Add this line to a group
      print(paste0(rf2$id," added to group ",i, "in the inner loop"))
      #print("get ready inner")
      overlap3 = subset(overlap2, rownames(overlap2) != rowP, colnames(overlap2) != rowP) # remove the line from the matrix
      #print("subset worked inner")
      overlap2 = overlap3
      #print("inner 1 worked")
      #overlap2 = subset(overlap2, colnames(overlap2) != rowP)
      #print("get ready inner again")
      
      print(paste0("there are ",nrow(partners_overlap)," in the inner loop"))
      #print("inner 2 worked")
      #partners_overlap = subset(partners_overlap, colnames(partners_overlap) != rowP)
      partners3 = subset(partners_overlap, rownames(partners_overlap) == rowP) #subset to jsut the relavant line
      #print("subset worked partners")
      partners = partners3
      
      
      
      
      
      
      
      partners_overlap3 = subset(partners_overlap, rownames(partners_overlap) != rowP, colnames(partners_overlap) != rowP) # remove the line from the matrix
      #print("subset worked inner again")
      partners_overlap = partners_overlap3
      #print("overwrite worked partners")
      #sel = !partners[1,]
      #partners = colnames(partners)[sel]
      if(length(partners)==0){ #if no more partners then we can't go again
        go <- FALSE
        print("zero partners")
      }
      else{
        sel = !partners[1,]
        partners = colnames(partners)[sel]
        partners_overlap = partners_overlap[names(partners_overlap),names(partners_overlap)]
        print("in the else of the inner loop")
      }
    }

  }
} 
  
