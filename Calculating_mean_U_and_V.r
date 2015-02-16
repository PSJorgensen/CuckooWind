####################			SCRIPT TO CALCULATE MEAN U AND V COMPONENTS BEFORE AND AFTER A BIRD MOVES 100 KM FROM ITS ORIGIN


### set working directory
wd<-"G:/Documents/MartaScripts"
setwd(wd)

### read in csv file
dframe<-read.csv("best_positions_after_rcalc_OrigPos.csv",header=TRUE,stringsAsFactors=FALSE)


##############################			CALCULATING MEAN U AND V WIND COMPONENT AFTER CROSSING 100 KM FOR "Distance..km." EACH COMBINATION OF tag and year

dframe.tagyear<-data.frame("tag.by.year"=unique(dframe[,"tag.by.year"]),"mean.U"=NA,"mean.V"=NA,"direction"=NA,"distance"=NA)

for(i in unique(dframe[,"tag.by.year"])){

dframe.i<-dframe[which(dframe[,"tag.by.year"]==i),]

min.after.i<-min(dframe.i[which(dframe.i[,"Distance..km."]>100),"Distance..km."])

dframe.i<-as.data.frame(rbind(dframe.i[(which(dframe.i[,"Distance..km."]==min.after.i)-1),],dframe.i[which(dframe.i[,"Distance..km."]==min.after.i)[1],]))


dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"mean.U"]<-mean(dframe.i[,"U"])
dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"mean.V"]<-mean(dframe.i[,"V"])

####	CALCULATING DISTANCE

####	note x and y values are given oppposite of what the formula requires
dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"direction"]<-ifelse(
	((atan2((dframe.i[2,"X1"]-dframe.i[1,"X1"]),(dframe.i[2,"Y1"]-dframe.i[1,"Y1"]))*180)/pi)<0,
	360+(atan2((dframe.i[2,"X1"]-dframe.i[1,"X1"]),(dframe.i[2,"X1"]-dframe.i[1,"Y1"]))*180/pi),
	atan2((dframe.i[2,"X1"]-dframe.i[1,"X1"]),(dframe.i[2,"Y1"]-dframe.i[1,"Y1"]))*180/pi
	)

direction<-dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"direction"]


####	CALCULATING DISTANCE

dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"distance"]<-
ifelse(
#COS(BH10*PI()/180)=0
cos(direction*pi/180)==0
,
#1.852*60*(BA10-BC10)*COS(AZ10*PI()/180)
1.852*60*(dframe.i[2,"Long."]-dframe.i[1,"Long."])*cos(dframe.i[2,"Lat"]*pi/180)
,
#1.852*60*(AZ10-BB10)/COS(BH10*PI()/180)
1.852*60*(dframe.i[2,"Lat"]-dframe.i[1,"Lat"])/cos(direction*pi/180)
)


rm("direction")
}





#####		WRITE THE FILE


write.csv(dframe.tagyear,"tag_by_year_mean_u_v_direction_distance.csv",row.names=FALSE)







###################################################################################################################################################################################


#X1=BD
#Y1=BE
#OrigX1=BF
#OrigY1=BG

#AZ=Lat = Lat row 2
#BA=Long. = Long. row 2
#BB=Orgin.Lat = Lat row 1
#BC=Origin.Long = Long. row 1
#BH=Direction = direction

#=IF((ATAN2(BE10-BG10,BD10-BF10)*180/PI())<0,360+(ATAN2(BE10-BG10,BD10-BF10)*180/PI()),ATAN2(BE10-BG10,BD10-BF10)*180/PI())

#=IF(COS(BH10*PI()/180)=0,1.852*60*(BA10-BC10)*COS(AZ10*PI()/180),1.852*60*(AZ10-BB10)/COS(BH10*PI()/180))


k<-2


direction<-ifelse(
	(atan2((dframe[k,"Y1"]-dframe[k,"OrigY1"]),(dframe[k,"X1"]-dframe[k,"OrigX1"]))*180/pi)<0,
	360+(atan2((dframe[k,"Y1"]-dframe[k,"OrigY1"]),(dframe[k,"X1"]-dframe[k,"OrigX1"]))*180/pi),
	atan2((dframe[k,"Y1"]-dframe[k,"OrigY1"]),(dframe[k,"X1"]-dframe[k,"OrigX1"]))*180/pi
	)



ifelse(
#COS(BH10*PI()/180)=0
cos(direction*pi/180)==0
,
#1.852*60*(BA10-BC10)*COS(AZ10*PI()/180)
1.852*60*(dframe[k,"Long."]-dframe[k,"Origin.Long"])*cos(dframe[k,"Lat"]*pi/180)
,
#1.852*60*(AZ10-BB10)/COS(BH10*PI()/180)
1.852*60*(dframe[k,"Lat"]-dframe[k,"Origin.Lat"])/cos(direction*pi/180)

)



#######################

####	CALCULATING DISTANCE

dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"direction"]<-ifelse(
	((atan2((dframe.i[2,"Y1"]-dframe.i[1,"Y1"]),(dframe.i[2,"X1"]-dframe.i[1,"X1"]))*180)/pi)<0,
	360+(atan2((dframe.i[2,"Y1"]-dframe.i[1,"Y1"]),(dframe.i[2,"X1"]-dframe.i[1,"X1"]))*180/pi),
	atan2((dframe.i[2,"Y1"]-dframe.i[1,"Y1"]),(dframe.i[2,"X1"]-dframe.i[1,"X1"]))*180/pi
	)

direction<-dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"direction"]


####	CALCULATING DISTANCE

dframe.tagyear[which(dframe.tagyear[,"tag.by.year"]==i),"distance"]<-
ifelse(
#COS(BH10*PI()/180)=0
cos(direction*pi/180)==0
,
#1.852*60*(BA10-BC10)*COS(AZ10*PI()/180)
1.852*60*(dframe.i[2,"Long."]-dframe.i[1,"Long."])*cos(dframe.i[2,"Lat"]*pi/180)
,
#1.852*60*(AZ10-BB10)/COS(BH10*PI()/180)
1.852*60*(dframe.i[2,"Lat"]-dframe.i[1,"Lat"])/cos(direction*pi/180)
)












