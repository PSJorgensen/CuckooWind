####################  		THIS VERSION SHOULD NOW BE SYNCED TO GITHUB

####################			SCRIPT TO SELECT THE BEST POSITION IN A TRANSMISSION CYCLE

### set working directory
wd<-"G:/Documents/Marta/MartaScripts/CuckooWind"
setwd(wd)

### read in csv file
dframe<-read.csv("best positions_ed.csv",header=TRUE,stringsAsFactors=FALSE)

### based on three variables choose the best localized position

dframe[,"best.pos.r"]<-NA

#### in each transmission.cycle do the following

#### 1. choose row(s) with highest loc.qual

#### 2. if more than one row with highest loc.qual, choose the one with lowest dist.from.noon value

dframe[,"best.pos.rownumber"]<-unlist(c(

  by(dframe,dframe[,"transmission.cycle"], 
     function(x, y = x[which(x[,"loc.qual"] == max(x[,"loc.qual"])),], z = min(y[,"dist.from.noon"])) 
    rep( intersect( which( x[,"loc.qual"] == max( x[,"loc.qual"]) ), which( x[,"dist.from.noon"] == z) ),nrow(x) )
  )
)
)


#### 3. give this row the value 1 and all other rows 0

for(i in 1:length(unique(dframe[,"transmission.cycle"]))){

dframe.i<-dframe[which(dframe[,"transmission.cycle"]==i),]

dframe.i[,"sequence"]<-c(1:nrow(dframe.i))

dframe.i[which(dframe.i[,"best.pos.rownumber"]==dframe.i[,"sequence"]),"best.pos.r"]<-1
dframe.i[which(dframe.i[,"best.pos.rownumber"]!=dframe.i[,"sequence"]),"best.pos.r"]<-0

dframe[which(dframe[,"transmission.cycle"]==i),"best.pos.r"]<-dframe.i[,"best.pos.r"]

}


##############################			CALCULATING MEAN U AND V WIND COMPONENT AFTER CROSSING 100 KM FOR "Distance..km." EACH COMBINATION OF tag and year


dframe.best<-dframe[which(dframe[,"best.pos.r"]==1),]

dframe.best[,"year"]<-strptime(dframe.best[,"timestamp"],"%Y-%m-%d %H:%M:%S")$year+1900
dframe.best[,"tag.by.year"]<-paste(dframe.best[,"tag"],"_",dframe.best[,"year"],sep="")


#########################

write.csv(dframe.best,paste("best_positions_after_rcalc_",Sys.Date(),".csv",sep=""))



