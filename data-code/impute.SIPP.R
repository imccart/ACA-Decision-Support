

# SIPP Wave 1 (2013) ------------------------------------------------------

# Create Household Size variable for year
pu2014w1 <- fread("data/ACS-SIPP/pu2014w1.csv")
DT <- pu2014w1
family_size<-DT[, .(family_size=length(unique(PNUM))),by="SSUID"]
pu2014w1<-merge(pu2014w1,family_size,by="SSUID")

# Create household size variable by month
family_size_month<-DT[, .(family_size=length(unique(PNUM))),by=c("SSUID", "MONTHCODE")]
pu2014w1<-merge(pu2014w1,family_size_month,by=c("SSUID", "MONTHCODE"))

# Create percentage of household male
pu2014w1$ESEX[pu2014w1$ESEX==2]<-0 # Now 0 if female 1 if male
percentmale<-aggregate(pu2014w1$ESEX, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percentmale)[colnames(percentmale)=="x"] <- "numbermale"
pu2014w1<-merge(pu2014w1,percentmale,by=c("SSUID", "MONTHCODE"))
pu2014w1$percentmale<-pu2014w1$numbermale/pu2014w1$family_size.y

# Create percentage of household Asian
pu2014w1$ASIAN <- 0
pu2014w1[pu2014w1$ERACE==3, "ASIAN"] <- 1
percentasian<-aggregate(pu2014w1$ASIAN, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percentasian)[colnames(percentasian)=="x"] <- "numberasian"
pu2014w1<-merge(pu2014w1,percentasian,by=c("SSUID", "MONTHCODE"))
pu2014w1$percentasian<-pu2014w1$numberasian/pu2014w1$family_size.y

# Create percentage of household Black
pu2014w1$BLACK <- 0
pu2014w1[pu2014w1$ERACE==2, "BLACK"] <- 1
percentblack<-aggregate(pu2014w1$BLACK, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percentblack)[colnames(percentblack)=="x"] <- "numberblack"
pu2014w1<-merge(pu2014w1,percentblack,by=c("SSUID", "MONTHCODE"))
pu2014w1$percentblack<-pu2014w1$numberblack/pu2014w1$family_size.y


# Create percentage of household Hispanic
pu2014w1$HISPANIC <- 0
pu2014w1[pu2014w1$EORIGIN==1, "HISPANIC"] <- 1
percenthispanic<-aggregate(pu2014w1$HISPANIC, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percenthispanic)[colnames(percenthispanic)=="x"] <- "numberhispanic"
pu2014w1<-merge(pu2014w1,percenthispanic,by=c("SSUID", "MONTHCODE"))
pu2014w1$percenthispanic<-pu2014w1$numberhispanic/pu2014w1$family_size.y


# Create percentage of household other non-white
pu2014w1$OTHER <- 0
pu2014w1[pu2014w1$ERACE==4 & pu2014w1$EORIGIN==2, "OTHER"] <- 1
percentother<-aggregate(pu2014w1$OTHER, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percentother)[colnames(percentother)=="x"] <- "numberother"
pu2014w1<-merge(pu2014w1,percentother,by=c("SSUID", "MONTHCODE"))
pu2014w1$percentother<-pu2014w1$numberother/pu2014w1$family_size.y



# Create percentage of household age 0-17
pu2014w1$YOUNG <- 0
pu2014w1[pu2014w1$TAGE<=17, "YOUNG"] <- 1
percentyoung<-aggregate(pu2014w1$YOUNG, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percentyoung)[colnames(percentyoung)=="x"] <- "numberyoung"
pu2014w1<-merge(pu2014w1,percentyoung,by=c("SSUID", "MONTHCODE"))
pu2014w1$percentyoung<-pu2014w1$numberyoung/pu2014w1$family_size.y


# Create percentage of household age 18-34
pu2014w1$MIDDLE <- 0
pu2014w1[pu2014w1$TAGE<=34 & pu2014w1$TAGE>=18, "MIDDLE"] <- 1
percentmiddle<-aggregate(pu2014w1$MIDDLE, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percentmiddle)[colnames(percentmiddle)=="x"] <- "numbermiddle"
pu2014w1<-merge(pu2014w1,percentmiddle,by=c("SSUID", "MONTHCODE"))
pu2014w1$percentmiddle<-pu2014w1$numbermiddle/pu2014w1$family_size.y


# Create percentage of household age 35-54
pu2014w1$OLD <- 0
pu2014w1[pu2014w1$TAGE<=54 & pu2014w1$TAGE>=35, "OLD"] <- 1
percentold<-aggregate(pu2014w1$OLD, by=list(SSUID=pu2014w1$SSUID, MONTHCODE=pu2014w1$MONTHCODE), FUN=sum)
colnames(percentold)[colnames(percentold)=="x"] <- "numberold"
pu2014w1<-merge(pu2014w1,percentold,by=c("SSUID", "MONTHCODE"))
pu2014w1$percentold<-pu2014w1$numberold/pu2014w1$family_size.y


# Create start and end month of nongroup coverage
status2013 <- fread("data/ACS-SIPP/status2013.csv")
pu2014w1<-merge(pu2014w1,status2013,by=c("SSUID", "PNUM", "SWAVE", "TAGE"))
pu2014w1$STARTNONGROUP<-0

pu2014w1[pu2014w1$dec2013==1, "STARTNONGROUP"] <- 12
pu2014w1[pu2014w1$nov2013==1, "STARTNONGROUP"] <- 11
pu2014w1[pu2014w1$oct2013==1, "STARTNONGROUP"] <- 10
pu2014w1[pu2014w1$sep2013==1, "STARTNONGROUP"] <- 9
pu2014w1[pu2014w1$aug2013==1, "STARTNONGROUP"] <- 8
pu2014w1[pu2014w1$jul2013==1, "STARTNONGROUP"] <- 7
pu2014w1[pu2014w1$jun2013==1, "STARTNONGROUP"] <- 6
pu2014w1[pu2014w1$may2013==1, "STARTNONGROUP"] <- 5
pu2014w1[pu2014w1$apr2013==1, "STARTNONGROUP"] <- 4
pu2014w1[pu2014w1$mar2013==1, "STARTNONGROUP"] <- 3
pu2014w1[pu2014w1$feb2013==1, "STARTNONGROUP"] <- 2
pu2014w1[pu2014w1$jan2013==1, "STARTNONGROUP"] <- 1

pu2014w1$ENDNONGROUP<-0
pu2014w1[pu2014w1$STARTNONGROUP==1 & pu2014w1$feb2013!=1, "ENDNONGROUP"] <- 1
pu2014w1[pu2014w1$STARTNONGROUP<=2 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$mar2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 2
pu2014w1[pu2014w1$STARTNONGROUP<=3 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$apr2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 3
pu2014w1[pu2014w1$STARTNONGROUP<=4 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$may2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 4
pu2014w1[pu2014w1$STARTNONGROUP<=5 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$jun2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 5
pu2014w1[pu2014w1$STARTNONGROUP<=6 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$jul2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 6
pu2014w1[pu2014w1$STARTNONGROUP<=7 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$aug2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 7
pu2014w1[pu2014w1$STARTNONGROUP<=8 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$sep2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 8
pu2014w1[pu2014w1$STARTNONGROUP<=9 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$oct2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 9
pu2014w1[pu2014w1$STARTNONGROUP<=10 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$nov2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 10
pu2014w1[pu2014w1$STARTNONGROUP<=11 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$dec2013!=1 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 11
pu2014w1[pu2014w1$STARTNONGROUP<=12 & pu2014w1$STARTNONGROUP!=0 & pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"] <- 12


pu2014w1[pu2014w1$STARTNONGROUP==0, "STARTNONGROUP"]<-NA
pu2014w1[pu2014w1$ENDNONGROUP==0, "ENDNONGROUP"]<-NA
pu2014w1[pu2014w1$jan2013==0, "jan2013"]<-NA
pu2014w1[pu2014w1$feb2013==0, "feb2013"]<-NA
pu2014w1[pu2014w1$mar2013==0, "mar2013"]<-NA
pu2014w1[pu2014w1$apr2013==0, "apr2013"]<-NA
pu2014w1[pu2014w1$may2013==0, "may2013"]<-NA
pu2014w1[pu2014w1$jun2013==0, "jun2013"]<-NA
pu2014w1[pu2014w1$jul2013==0, "jul2013"]<-NA
pu2014w1[pu2014w1$aug2013==0, "aug2013"]<-NA
pu2014w1[pu2014w1$sep2013==0, "sep2013"]<-NA
pu2014w1[pu2014w1$oct2013==0, "oct2013"]<-NA
pu2014w1[pu2014w1$nov2013==0, "nov2013"]<-NA
pu2014w1[pu2014w1$dec2013==0, "dec2013"]<-NA



# SIPP Wave 2 (2014) ------------------------------------------------------

# Create Household Size variable for year
pu2014w2 <- fread("data/ACS-SIPP/pu2014w2.csv")
DT <- pu2014w2
family_size <- DT[, .(family_size=length(unique(PNUM))),by=SSUID]

pu2014w2 <- merge(pu2014w2,family_size,by="SSUID")

# Create household size variable by month
family_size_month<-DT[, .(family_size=length(unique(PNUM))),by=c("SSUID", "MONTHCODE")]
pu2014w2<-merge(pu2014w2,family_size_month,by=c("SSUID", "MONTHCODE"))

# Create percentage of household male
pu2014w2$ESEX[pu2014w2$ESEX==2]<-0 # Now 0 if female 1 if male
percentmale<-aggregate(pu2014w2$ESEX, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percentmale)[colnames(percentmale)=="x"] <- "numbermale"
pu2014w2<-merge(pu2014w2,percentmale,by=c("SSUID", "MONTHCODE"))
pu2014w2$percentmale<-pu2014w2$numbermale/pu2014w2$family_size.y

# Create percentage of household Asian
pu2014w2$ASIAN <- 0
pu2014w2[pu2014w2$ERACE==3, "ASIAN"] <- 1
percentasian<-aggregate(pu2014w2$ASIAN, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percentasian)[colnames(percentasian)=="x"] <- "numberasian"
pu2014w2<-merge(pu2014w2,percentasian,by=c("SSUID", "MONTHCODE"))
pu2014w2$percentasian<-pu2014w2$numberasian/pu2014w2$family_size.y

# Create percentage of household Black
pu2014w2$BLACK <- 0
pu2014w2[pu2014w2$ERACE==2, "BLACK"] <- 1
percentblack<-aggregate(pu2014w2$BLACK, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percentblack)[colnames(percentblack)=="x"] <- "numberblack"
pu2014w2<-merge(pu2014w2,percentblack,by=c("SSUID", "MONTHCODE"))
pu2014w2$percentblack<-pu2014w2$numberblack/pu2014w2$family_size.y


# Create percentage of household Hispanic
pu2014w2$HISPANIC <- 0
pu2014w2[pu2014w2$EORIGIN==1, "HISPANIC"] <- 1
percenthispanic<-aggregate(pu2014w2$HISPANIC, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percenthispanic)[colnames(percenthispanic)=="x"] <- "numberhispanic"
pu2014w2<-merge(pu2014w2,percenthispanic,by=c("SSUID", "MONTHCODE"))
pu2014w2$percenthispanic<-pu2014w2$numberhispanic/pu2014w2$family_size.y


# Create percentage of household other non-white
pu2014w2$OTHER <- 0
pu2014w2[pu2014w2$ERACE==4 & pu2014w2$EORIGIN==2, "OTHER"] <- 1
percentother<-aggregate(pu2014w2$OTHER, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percentother)[colnames(percentother)=="x"] <- "numberother"
pu2014w2<-merge(pu2014w2,percentother,by=c("SSUID", "MONTHCODE"))
pu2014w2$percentother<-pu2014w2$numberother/pu2014w2$family_size.y



# Create percentage of household age 0-17
pu2014w2$YOUNG <- 0
pu2014w2[pu2014w2$TAGE<=17, "YOUNG"] <- 1
percentyoung<-aggregate(pu2014w2$YOUNG, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percentyoung)[colnames(percentyoung)=="x"] <- "numberyoung"
pu2014w2<-merge(pu2014w2,percentyoung,by=c("SSUID", "MONTHCODE"))
pu2014w2$percentyoung<-pu2014w2$numberyoung/pu2014w2$family_size.y


# Create percentage of household age 18-34
pu2014w2$MIDDLE <- 0
pu2014w2[pu2014w2$TAGE<=34 | pu2014w2$TAGE>=18, "MIDDLE"] <- 1
percentmiddle<-aggregate(pu2014w2$MIDDLE, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percentmiddle)[colnames(percentmiddle)=="x"] <- "numbermiddle"
pu2014w2<-merge(pu2014w2,percentmiddle,by=c("SSUID", "MONTHCODE"))
pu2014w2$percentmiddle<-pu2014w2$numbermiddle/pu2014w2$family_size.y


# Create percentage of household age 35-54
pu2014w2$OLD <- 0
pu2014w2[pu2014w2$TAGE<=54 | pu2014w2$TAGE>=35, "OLD"] <- 1
percentold<-aggregate(pu2014w2$OLD, by=list(SSUID=pu2014w2$SSUID, MONTHCODE=pu2014w2$MONTHCODE), FUN=sum)
colnames(percentold)[colnames(percentold)=="x"] <- "numberold"
pu2014w2<-merge(pu2014w2,percentold,by=c("SSUID", "MONTHCODE"))
pu2014w2$percentold<-pu2014w2$numberold/pu2014w2$family_size.y


# Create start and end month of nongroup coverage
status2014 <- fread("data/ACS-SIPP/status2014.csv")
pu2014w2<-merge(pu2014w2,status2014,by=c("SSUID", "PNUM", "SWAVE", "TAGE"))
pu2014w2$STARTNONGROUP<-0

pu2014w2[pu2014w2$dec2014==1, "STARTNONGROUP"] <- 12
pu2014w2[pu2014w2$nov2014==1, "STARTNONGROUP"] <- 11
pu2014w2[pu2014w2$oct2014==1, "STARTNONGROUP"] <- 10
pu2014w2[pu2014w2$sep2014==1, "STARTNONGROUP"] <- 9
pu2014w2[pu2014w2$aug2014==1, "STARTNONGROUP"] <- 8
pu2014w2[pu2014w2$jul2014==1, "STARTNONGROUP"] <- 7
pu2014w2[pu2014w2$jun2014==1, "STARTNONGROUP"] <- 6
pu2014w2[pu2014w2$may2014==1, "STARTNONGROUP"] <- 5
pu2014w2[pu2014w2$apr2014==1, "STARTNONGROUP"] <- 4
pu2014w2[pu2014w2$mar2014==1, "STARTNONGROUP"] <- 3
pu2014w2[pu2014w2$feb2014==1, "STARTNONGROUP"] <- 2
pu2014w2[pu2014w2$jan2014==1, "STARTNONGROUP"] <- 1

pu2014w2$ENDNONGROUP<-0
pu2014w2[pu2014w2$STARTNONGROUP==1 & pu2014w2$feb2014!=1, "ENDNONGROUP"] <- 1
pu2014w2[pu2014w2$STARTNONGROUP<=2 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$mar2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 2
pu2014w2[pu2014w2$STARTNONGROUP<=3 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$apr2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 3
pu2014w2[pu2014w2$STARTNONGROUP<=4 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$may2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 4
pu2014w2[pu2014w2$STARTNONGROUP<=5 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$jun2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 5
pu2014w2[pu2014w2$STARTNONGROUP<=6 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$jul2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 6
pu2014w2[pu2014w2$STARTNONGROUP<=7 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$aug2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 7
pu2014w2[pu2014w2$STARTNONGROUP<=8 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$sep2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 8
pu2014w2[pu2014w2$STARTNONGROUP<=9 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$oct2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 9
pu2014w2[pu2014w2$STARTNONGROUP<=10 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$nov2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 10
pu2014w2[pu2014w2$STARTNONGROUP<=11 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$dec2014!=1 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 11
pu2014w2[pu2014w2$STARTNONGROUP<=12 & pu2014w2$STARTNONGROUP!=0 & pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"] <- 12


pu2014w2[pu2014w2$STARTNONGROUP==0, "STARTNONGROUP"]<-NA
pu2014w2[pu2014w2$ENDNONGROUP==0, "ENDNONGROUP"]<-NA
pu2014w2[pu2014w2$jan2014==0, "jan2014"]<-NA
pu2014w2[pu2014w2$feb2014==0, "feb2014"]<-NA
pu2014w2[pu2014w2$mar2014==0, "mar2014"]<-NA
pu2014w2[pu2014w2$apr2014==0, "apr2014"]<-NA
pu2014w2[pu2014w2$may2014==0, "may2014"]<-NA
pu2014w2[pu2014w2$jun2014==0, "jun2014"]<-NA
pu2014w2[pu2014w2$jul2014==0, "jul2014"]<-NA
pu2014w2[pu2014w2$aug2014==0, "aug2014"]<-NA
pu2014w2[pu2014w2$sep2014==0, "sep2014"]<-NA
pu2014w2[pu2014w2$oct2014==0, "oct2014"]<-NA
pu2014w2[pu2014w2$nov2014==0, "nov2014"]<-NA
pu2014w2[pu2014w2$dec2014==0, "dec2014"]<-NA



# SIPP Wave 3 (2015) ------------------------------------------------------

# Create Household Size variable for year
pu2014w3<-fread("data/ACS-SIPP/pu2014w3.csv")
DT<-pu2014w3
family_size<-DT[, .(family_size=length(unique(PNUM))),by=SSUID]

pu2014w3<-merge(pu2014w3,family_size,by="SSUID")

# Create household size variable by month
family_size_month<-DT[, .(family_size=length(unique(PNUM))),by=c("SSUID", "MONTHCODE")]
pu2014w3<-merge(pu2014w3,family_size_month,by=c("SSUID", "MONTHCODE"))

# Create percentage of household male
pu2014w3$ESEX[pu2014w3$ESEX==2]<-0 # Now 0 if female 1 if male
percentmale<-aggregate(pu2014w3$ESEX, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percentmale)[colnames(percentmale)=="x"] <- "numbermale"
pu2014w3<-merge(pu2014w3,percentmale,by=c("SSUID", "MONTHCODE"))
pu2014w3$percentmale<-pu2014w3$numbermale/pu2014w3$family_size.y

# Create percentage of household Asian
pu2014w3$ASIAN <- 0
pu2014w3[pu2014w3$ERACE==3, "ASIAN"] <- 1
percentasian<-aggregate(pu2014w3$ASIAN, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percentasian)[colnames(percentasian)=="x"] <- "numberasian"
pu2014w3<-merge(pu2014w3,percentasian,by=c("SSUID", "MONTHCODE"))
pu2014w3$percentasian<-pu2014w3$numberasian/pu2014w3$family_size.y

# Create percentage of household Black
pu2014w3$BLACK <- 0
pu2014w3[pu2014w3$ERACE==2, "BLACK"] <- 1
percentblack<-aggregate(pu2014w3$BLACK, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percentblack)[colnames(percentblack)=="x"] <- "numberblack"
pu2014w3<-merge(pu2014w3,percentblack,by=c("SSUID", "MONTHCODE"))
pu2014w3$percentblack<-pu2014w3$numberblack/pu2014w3$family_size.y


# Create percentage of household Hispanic
pu2014w3$HISPANIC <- 0
pu2014w3[pu2014w3$EORIGIN==1, "HISPANIC"] <- 1
percenthispanic<-aggregate(pu2014w3$HISPANIC, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percenthispanic)[colnames(percenthispanic)=="x"] <- "numberhispanic"
pu2014w3<-merge(pu2014w3,percenthispanic,by=c("SSUID", "MONTHCODE"))
pu2014w3$percenthispanic<-pu2014w3$numberhispanic/pu2014w3$family_size.y


# Create percentage of household other non-white
pu2014w3$OTHER <- 0
pu2014w3[pu2014w3$ERACE==4 & pu2014w3$EORIGIN==2, "OTHER"] <- 1
percentother<-aggregate(pu2014w3$OTHER, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percentother)[colnames(percentother)=="x"] <- "numberother"
pu2014w3<-merge(pu2014w3,percentother,by=c("SSUID", "MONTHCODE"))
pu2014w3$percentother<-pu2014w3$numberother/pu2014w3$family_size.y



# Create percentage of household age 0-17
pu2014w3$YOUNG <- 0
pu2014w3[pu2014w3$TAGE<=17, "YOUNG"] <- 1
percentyoung<-aggregate(pu2014w3$YOUNG, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percentyoung)[colnames(percentyoung)=="x"] <- "numberyoung"
pu2014w3<-merge(pu2014w3,percentyoung,by=c("SSUID", "MONTHCODE"))
pu2014w3$percentyoung<-pu2014w3$numberyoung/pu2014w3$family_size.y


# Create percentage of household age 18-34
pu2014w3$MIDDLE <- 0
pu2014w3[pu2014w3$TAGE<=34 | pu2014w3$TAGE>=18, "MIDDLE"] <- 1
percentmiddle<-aggregate(pu2014w3$MIDDLE, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percentmiddle)[colnames(percentmiddle)=="x"] <- "numbermiddle"
pu2014w3<-merge(pu2014w3,percentmiddle,by=c("SSUID", "MONTHCODE"))
pu2014w3$percentmiddle<-pu2014w3$numbermiddle/pu2014w3$family_size.y


# Create percentage of household age 35-54
pu2014w3$OLD <- 0
pu2014w3[pu2014w3$TAGE<=54 | pu2014w3$TAGE>=35, "OLD"] <- 1
percentold<-aggregate(pu2014w3$OLD, by=list(SSUID=pu2014w3$SSUID, MONTHCODE=pu2014w3$MONTHCODE), FUN=sum)
colnames(percentold)[colnames(percentold)=="x"] <- "numberold"
pu2014w3<-merge(pu2014w3,percentold,by=c("SSUID", "MONTHCODE"))
pu2014w3$percentold<-pu2014w3$numberold/pu2014w3$family_size.y


# Create start and end month of nongroup coverage

status2015 <- fread("data/ACS-SIPP/status2015.csv")
pu2014w3<-merge(pu2014w3,status2015,by=c("SSUID", "PNUM", "SWAVE", "TAGE"))
pu2014w3$STARTNONGROUP<-0

pu2014w3[pu2014w3$dec2015==1, "STARTNONGROUP"] <- 12
pu2014w3[pu2014w3$nov2015==1, "STARTNONGROUP"] <- 11
pu2014w3[pu2014w3$oct2015==1, "STARTNONGROUP"] <- 10
pu2014w3[pu2014w3$sep2015==1, "STARTNONGROUP"] <- 9
pu2014w3[pu2014w3$aug2015==1, "STARTNONGROUP"] <- 8
pu2014w3[pu2014w3$jul2015==1, "STARTNONGROUP"] <- 7
pu2014w3[pu2014w3$jun2015==1, "STARTNONGROUP"] <- 6
pu2014w3[pu2014w3$may2015==1, "STARTNONGROUP"] <- 5
pu2014w3[pu2014w3$apr2015==1, "STARTNONGROUP"] <- 4
pu2014w3[pu2014w3$mar2015==1, "STARTNONGROUP"] <- 3
pu2014w3[pu2014w3$feb2015==1, "STARTNONGROUP"] <- 2
pu2014w3[pu2014w3$jan2015==1, "STARTNONGROUP"] <- 1

pu2014w3$ENDNONGROUP<-0
pu2014w3[pu2014w3$STARTNONGROUP==1 & pu2014w3$feb2015!=1, "ENDNONGROUP"] <- 1
pu2014w3[pu2014w3$STARTNONGROUP<=2 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$mar2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 2
pu2014w3[pu2014w3$STARTNONGROUP<=3 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$apr2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 3
pu2014w3[pu2014w3$STARTNONGROUP<=4 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$may2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 4
pu2014w3[pu2014w3$STARTNONGROUP<=5 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$jun2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 5
pu2014w3[pu2014w3$STARTNONGROUP<=6 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$jul2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 6
pu2014w3[pu2014w3$STARTNONGROUP<=7 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$aug2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 7
pu2014w3[pu2014w3$STARTNONGROUP<=8 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$sep2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 8
pu2014w3[pu2014w3$STARTNONGROUP<=9 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$oct2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 9
pu2014w3[pu2014w3$STARTNONGROUP<=10 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$nov2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 10
pu2014w3[pu2014w3$STARTNONGROUP<=11 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$dec2015!=1 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 11
pu2014w3[pu2014w3$STARTNONGROUP<=12 & pu2014w3$STARTNONGROUP!=0 & pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"] <- 12


pu2014w3[pu2014w3$STARTNONGROUP==0, "STARTNONGROUP"]<-NA
pu2014w3[pu2014w3$ENDNONGROUP==0, "ENDNONGROUP"]<-NA
pu2014w3[pu2014w3$jan2015==0, "jan2015"]<-NA
pu2014w3[pu2014w3$feb2015==0, "feb2015"]<-NA
pu2014w3[pu2014w3$mar2015==0, "mar2015"]<-NA
pu2014w3[pu2014w3$apr2015==0, "apr2015"]<-NA
pu2014w3[pu2014w3$may2015==0, "may2015"]<-NA
pu2014w3[pu2014w3$jun2015==0, "jun2015"]<-NA
pu2014w3[pu2014w3$jul2015==0, "jul2015"]<-NA
pu2014w3[pu2014w3$aug2015==0, "aug2015"]<-NA
pu2014w3[pu2014w3$sep2015==0, "sep2015"]<-NA
pu2014w3[pu2014w3$oct2015==0, "oct2015"]<-NA
pu2014w3[pu2014w3$nov2015==0, "nov2015"]<-NA
pu2014w3[pu2014w3$dec2015==0, "dec2015"]<-NA




# Next, I will rename rmover as tmover in pu2014w1
# Then I will stack the two main datasets together

pu2014w1$TMOVER<-pu2014w1$RMOVER
# Drop the columns of the dataframe
pu2014w1 <- select(pu2014w1,-c(RMOVER))

# Before I can rbind pu2014w1 and w2 together, they must have the same variables (Why can't it do this on its own?)
pu2014w1$jan2014<-NA
pu2014w1$feb2014<-NA
pu2014w1$mar2014<-NA
pu2014w1$apr2014<-NA
pu2014w1$may2014<-NA
pu2014w1$jun2014<-NA
pu2014w1$jul2014<-NA
pu2014w1$aug2014<-NA
pu2014w1$sep2014<-NA
pu2014w1$oct2014<-NA
pu2014w1$nov2014<-NA
pu2014w1$dec2014<-NA

pu2014w1$jan2015<-NA
pu2014w1$feb2015<-NA
pu2014w1$mar2015<-NA
pu2014w1$apr2015<-NA
pu2014w1$may2015<-NA
pu2014w1$jun2015<-NA
pu2014w1$jul2015<-NA
pu2014w1$aug2015<-NA
pu2014w1$sep2015<-NA
pu2014w1$oct2015<-NA
pu2014w1$nov2015<-NA
pu2014w1$dec2015<-NA


pu2014w2$jan2013<-NA
pu2014w2$feb2013<-NA
pu2014w2$mar2013<-NA
pu2014w2$apr2013<-NA
pu2014w2$may2013<-NA
pu2014w2$jun2013<-NA
pu2014w2$jul2013<-NA
pu2014w2$aug2013<-NA
pu2014w2$sep2013<-NA
pu2014w2$oct2013<-NA
pu2014w2$nov2013<-NA
pu2014w2$dec2013<-NA

pu2014w2$jan2015<-NA
pu2014w2$feb2015<-NA
pu2014w2$mar2015<-NA
pu2014w2$apr2015<-NA
pu2014w2$may2015<-NA
pu2014w2$jun2015<-NA
pu2014w2$jul2015<-NA
pu2014w2$aug2015<-NA
pu2014w2$sep2015<-NA
pu2014w2$oct2015<-NA
pu2014w2$nov2015<-NA
pu2014w2$dec2015<-NA



pu2014w3$jan2013<-NA
pu2014w3$feb2013<-NA
pu2014w3$mar2013<-NA
pu2014w3$apr2013<-NA
pu2014w3$may2013<-NA
pu2014w3$jun2013<-NA
pu2014w3$jul2013<-NA
pu2014w3$aug2013<-NA
pu2014w3$sep2013<-NA
pu2014w3$oct2013<-NA
pu2014w3$nov2013<-NA
pu2014w3$dec2013<-NA

pu2014w3$jan2014<-NA
pu2014w3$feb2014<-NA
pu2014w3$mar2014<-NA
pu2014w3$apr2014<-NA
pu2014w3$may2014<-NA
pu2014w3$jun2014<-NA
pu2014w3$jul2014<-NA
pu2014w3$aug2014<-NA
pu2014w3$sep2014<-NA
pu2014w3$oct2014<-NA
pu2014w3$nov2014<-NA
pu2014w3$dec2014<-NA

# Stack the datasets
sipp <- rbindlist(list(pu2014w1, pu2014w2, pu2014w3), use.names=TRUE)
sipp <- data.frame(sipp)

# Remove all useless dataframes
rm(DT, family_size, family_size_month, percentasian, percentblack,percentother, percentmale,percenthispanic,percentmiddle,percentyoung,percentold,status2013,status2014,status2015)



# Imputation --------------------------------------------------------------

#load(("data/ACS-SIPP/Final_Workspace_v3.RData"))
#sipp <- total

# Create Household Object



	# Reduce Household IDS
	old_ids <- unique(sipp$SSUID)
	new_ids <- 1:length(sipp$SSUID)
	names(new_ids) <- old_ids
	sipp$household_id <- new_ids[as.character(sipp$SSUID)]

	sipp$year <- 2013
	sipp[sipp$SWAVE == 2,"year"] <- 2014
	sipp[sipp$SWAVE == 3,"year"] <- 2015
	
	sipp$household_year <- paste(sipp$household_id,sipp$year,sep="_")
	sipp$FPL <- pmax(0,sipp$THTOTINC/sipp$RHPOV)
	
	#household_sizes <- by(sipp$WPFINWGT,sipp$household_year,length)
	#sipp_households$household_size <- NA
	#sipp_households[names(household_sizes),"household_size"] <- household_sizes
	
	
	household_names <- unique(sipp$household_year)
	
	match_fields <- c("TMOVER",
		"STARTNONGROUP","ENDNONGROUP",
		"FPL",
		"percentyoung","percentmiddle","percentold",
		"percentmale",
		"percentasian","percentblack","percenthispanic","percentother")
	
	# Employer offer
		# EEMPNOESI: Employed, employer offers coverage to employees, but not on employer's plan
		# Reason did not buy ESI
			# EYNOESI_EXP: too expensive (has offer)
			# EYNOESI_NEW: not at job long enough to qualify
			# EYNOESI_HRS: part-time/temporary employee
			# EYNOESI_ELG: not eligible for another reason
			# EYNOESI_HTH: healthy and don't need it (has offer)
			# EYNOESI_ELS: able to get care elsewhere, such as at a clinic (has offer)
			# EYNOESI_COV: covered under someone else's plan (has offer)
			# EYNOESI_UNH: dissatisfied or don't believe in insurance (has offer)
			# EYNOESI_MIS: missed enrollment window
			# EYNOESI_OTH: other reason
	
	sipp$employer_offer <- 0
	sipp[(sipp$EYNOESI_COV == 1 & !is.na(sipp$EYNOESI_COV)) | 
			(sipp$EYNOESI_EXP == 1 & !is.na(sipp$EYNOESI_EXP)) | 
			(sipp$EYNOESI_HTH == 1 & !is.na(sipp$EYNOESI_HTH)) | 
			(sipp$EYNOESI_ELS == 1 & !is.na(sipp$EYNOESI_ELS)) | 
			(sipp$EYNOESI_UNH == 1 & !is.na(sipp$EYNOESI_UNH)),"employer_offer"] <- 1
	
	# Drop households that dropped after waves
	
		dropped_2013 <- unique(setdiff(sipp[sipp$year == 2013,"household_id"],sipp[sipp$year == 2014,"household_id"]))
		dropped_2014 <- unique(setdiff(sipp[sipp$year == 2014,"household_id"],sipp[sipp$year == 2015,"household_id"]))
	
	# Drop households that were never on nongroup
	
		sipp_households <- sipp[!duplicated(sipp$household_year),c("SSUID","household_id","year","household_year",
			"jan2013","jan2014","jan2015","dec2013","dec2014","dec2015",match_fields)]
		sipp_households <- sipp_households[!sipp_households$household_id %in% unique(c(dropped_2013,dropped_2014)),]
			
		households_on_nongroup <- unique(sipp[!is.na(sipp$STARTNONGROUP),"household_id"])
		sipp_households <- sipp_households[sipp_households$household_id %in% households_on_nongroup,]
		rownames(sipp_households) <- sipp_households$household_year
	
	# Add Household weight
	household_weights <- by(sipp[sipp$household_year %in% rownames(sipp_households),"WPFINWGT"],
		sipp[sipp$household_year %in% rownames(sipp_households),"household_year"],sum)
	sipp_households$weight <- NA
	sipp_households[names(household_weights),"weight"] <- household_weights
	
	# Add Household size
	household_sizes <- by(sipp[sipp$household_year %in% rownames(sipp_households),"WPFINWGT"],
		sipp[sipp$household_year %in% rownames(sipp_households),"household_year"],length)/12
	sipp_households$household_size <- NA
	sipp_households[names(household_sizes),"household_size"] <- household_sizes
	
	# Add employer offers
	employer_offers <- by(sipp[sipp$household_year %in% rownames(sipp_households),"employer_offer"],
		sipp[sipp$household_year %in% rownames(sipp_households),"household_year"],sum)
	sipp_households$employer_offer <- NA
	sipp_households[names(employer_offers),"employer_offer"] <- pmin(employer_offers,1)
	
	# Add max age
	max_ages <- by(sipp[sipp$household_year %in% rownames(sipp_households),"TAGE"],
		sipp[sipp$household_year %in% rownames(sipp_households),"household_year"],max)
	sipp_households$max_age <- NA
	sipp_households[names(max_ages),"max_age"] <- max_ages
	
	# Create household age variables
	number_0to17 <- by(sipp[sipp$household_year %in% rownames(sipp_households) & sipp$TAGE < 18,"TAGE"],
		sipp[sipp$household_year %in% rownames(sipp_households) & sipp$TAGE < 18,"household_year"],length)/12
	number_18to34 <- by(sipp[sipp$household_year %in% rownames(sipp_households) & sipp$TAGE >= 18 & sipp$TAGE < 35,"TAGE"],
		sipp[sipp$household_year %in% rownames(sipp_households) & sipp$TAGE >= 18 & sipp$TAGE < 35,"household_year"],length)/12
	number_35to54 <- by(sipp[sipp$household_year %in% rownames(sipp_households) & sipp$TAGE >= 35 & sipp$TAGE < 55,"TAGE"],
		sipp[sipp$household_year %in% rownames(sipp_households) & sipp$TAGE >= 35 & sipp$TAGE < 55,"household_year"],length)/12
	
	sipp_households[,c("perc_0to17","perc_18to34","perc_35to54")] <- 0
	sipp_households[names(number_0to17),"perc_0to17"] <- number_0to17/sipp_households[names(number_0to17),"household_size"]
	sipp_households[names(number_18to34),"perc_18to34"] <- number_18to34/sipp_households[names(number_18to34),"household_size"]
	sipp_households[names(number_35to54),"perc_35to54"] <- number_35to54/sipp_households[names(number_35to54),"household_size"]
	
	# Determine which households exited or entered
	
		sipp_households$entered <- 0
		sipp_households$exited <- 0
		
		nongroup_participants2013 <- sipp_households[sipp_households$year == 2013 & !is.na(sipp_households$STARTNONGROUP),"household_id"]
		nongroup_participants2014 <- sipp_households[sipp_households$year == 2014 & !is.na(sipp_households$STARTNONGROUP),"household_id"]
		nongroup_participants2015 <- sipp_households[sipp_households$year == 2015 & !is.na(sipp_households$STARTNONGROUP),"household_id"]
		
		entrants <- sipp_households[sipp_households$year == 2013 & sipp_households$household_id %in% nongroup_participants2014 & is.na(sipp_households$STARTNONGROUP),"household_id"]
		sipp_households[paste(entrants,"2014",sep="_"),"entered"] <- 1
		
		entrants <- sipp_households[sipp_households$year == 2014 & sipp_households$household_id %in% nongroup_participants2015 & is.na(sipp_households$STARTNONGROUP),"household_id"]
		sipp_households[paste(entrants,"2015",sep="_"),"entered"] <- 1
		
		
		#entrants <- sipp_households[sipp_households$year == 2013 & sipp_households$household_id %in% nongroup_participants2013 & sipp_households$STARTNONGROUP > 1,"household_id"]
		#sipp_households[paste(entrants,"2013",sep="_"),"entered"] <- 1
		
		#entrants <- sipp_households[sipp_households$year == 2014 & sipp_households$household_id %in% nongroup_participants2014 & sipp_households$STARTNONGROUP > 1,"household_id"]
		#sipp_households[paste(entrants,"2014",sep="_"),"entered"] <- 1
		
		departures <- sipp_households[sipp_households$year == 2014 & sipp_households$household_id %in% nongroup_participants2013 & is.na(sipp_households$STARTNONGROUP),"household_id"]
		sipp_households[paste(departures,"2013",sep="_"),"exited"] <- 1
		
		departures <- sipp_households[sipp_households$year == 2013 & sipp_households$household_id %in% nongroup_participants2013 & sipp_households$ENDNONGROUP < 12,"household_id"]
		sipp_households[paste(departures,"2013",sep="_"),"exited"] <- 1
		
		departures <- sipp_households[sipp_households$year == 2015 & sipp_households$household_id %in% nongroup_participants2014 & is.na(sipp_households$STARTNONGROUP),"household_id"]
		sipp_households[paste(departures,"2014",sep="_"),"exited"] <- 1
		
		departures <- sipp_households[sipp_households$year == 2014 & sipp_households$household_id %in% nongroup_participants2014 & sipp_households$ENDNONGROUP < 12,"household_id"]
		sipp_households[paste(departures,"2014",sep="_"),"exited"] <- 1
		
		#departures <- sipp_households[sipp_households$year == 2014 & sipp_households$household_id %in% nongroup_participants2014 & sipp_households$ENDNONGROUP < 12,"household_id"]
		#sipp_households[paste(departures,"2014",sep="_"),"exited"] <- 1
		
	
	
	# New Insurance Plan (Go with household head's plan)
	
	sipp_households$new_plan_after_exit <- 1
	sipp_households$old_plan_before_enter <- 1
	
		exiting_households <- sipp_households[sipp_households$exited == 1 & sipp_households$year == 2013,"household_id"]
		entering_households <- sipp_households[sipp_households$entered == 1  & sipp_households$year == 2014,"household_id"]
		
		sipp_households[paste(exiting_households,"2013",sep="_"),"new_plan_after_exit"] <- 
			sipp_households[paste(exiting_households,"2014",sep="_"),"jan2014"]
		
		sipp_households[paste(entering_households,"2014",sep="_"),"old_plan_before_enter"] <- 
			sipp_households[paste(entering_households,"2013",sep="_"),"jan2013"]
	
		exiting_households <- sipp_households[sipp_households$exited == 1 & sipp_households$year == 2014,"household_id"]
		entering_households <- sipp_households[sipp_households$entered == 1  & sipp_households$year == 2015,"household_id"]
		
		sipp_households[paste(exiting_households,"2014",sep="_"),"new_plan_after_exit"] <- 
			sipp_households[paste(exiting_households,"2015",sep="_"),"jan2015"]
		
		sipp_households[paste(entering_households,"2015",sep="_"),"old_plan_before_enter"] <- 
			sipp_households[paste(entering_households,"2014",sep="_"),"jan2014"]
	
	
	# Create dependent variables
		# Exited market
		# Entered market
	
		# Moved (tmover 4 and above)
		# Got ESI Offer
		# Went on Medicaid
		# Aged out
		
		# 1 if nongroup, 2 if military, 3 if medicare, 4 if ESI/group, 5 if medicaid/CHIP, 6 if 'other', 7 if uninsured
	
	
	sipp_households$entered_market <- NA
	sipp_households[sipp_households$old_plan_before_enter %in% c(7),"entered_market"] <- 0
	sipp_households[sipp_households$old_plan_before_enter %in% c(4,5),"entered_market"] <- 1
	sipp_households[sipp_households$employer_offer == 1 & sipp_households$entered_market == 0 & !is.na(sipp_households$entered_market),"entered_market"] <- 1
	sipp_households[sipp_households$TMOVER >= 4 & !is.na(sipp_households$TMOVER) &
		sipp_households$entered_market == 0 & !is.na(sipp_households$entered_market),"entered_market"] <- 1
	
	
	sipp_households$exited_market <- NA
	sipp_households[sipp_households$new_plan_after_exit %in% c(7),"exited_market"] <- 0
	sipp_households[sipp_households$new_plan_after_exit %in% c(4,5),"exited_market"] <- 1
	sipp_households[sipp_households$employer_offer == 1 & sipp_households$exited_market == 0 & !is.na(sipp_households$exited_market),"exited_market"] <- 1
	sipp_households[sipp_households$TMOVER >= 4 & !is.na(sipp_households$TMOVER) &
		sipp_households$exited_market == 0 & !is.na(sipp_households$exited_market),"exited_market"] <- 1
		
	drop_2013 <- FALSE
	if(drop_2013) {
		sipp_households[sipp_households$year == 2014,"entered_market"] <- NA
		sipp_households[sipp_households$year == 2013,"exited_market"] <- NA
	}
	
	
	
# Estimate Logit

	sipp_households$start_date <- sipp_households$STARTNONGROUP
	sipp_households$end_date <- sipp_households$ENDNONGROUP	
	sipp_households$perc_male <- sipp_households$percentmale	
	sipp_households$perc_asian <- sipp_households$percentasian
	sipp_households$perc_black <- sipp_households$percentblack
	sipp_households$perc_hispanic <- sipp_households$percenthispanic
	sipp_households$perc_other <- sipp_households$percentother	
		
		
	sipp_households$entered_late <- 0
	sipp_households[sipp_households$start_date > 5 & !is.na(sipp_households$start_date),"entered_late"] <- 1
	
	sipp_households$exited_early <- 0
	sipp_households[sipp_households$end_date < 12 & !is.na(sipp_households$end_date),"exited_early"] <- 1
	
	sipp_households$family <- as.numeric(sipp_households$household_size > 1)
	
	sipp_households[,c("STARTNONGROUP","ENDNONGROUP","percentyoung","percentmiddle","percentold","percentmale",
		"percentasian","percentblack","percenthispanic","percentother")] <- NULL	
		
	sipp_households$FPL_bracket <- "138orless"
	sipp_households[sipp_households$FPL > 1.38 & sipp_households$FPL <= 2.50,"FPL_bracket"] <- "138to250"
	sipp_households[sipp_households$FPL > 2.50 & sipp_households$FPL <= 4,"FPL_bracket"] <- "250to400"
	sipp_households[sipp_households$FPL > 4,"FPL_bracket"] <- "400ormore"
		
	sipp_households$transitioned <- pmax(sipp_households$entered_market,sipp_households$exited_market,na.rm=TRUE)	
	sipp_households$midyear_transition <- pmax(sipp_households$exited_early,sipp_households$entered_late,na.rm=TRUE)	
		
	# Entrants
	spec <- entered_market ~ entered_late + FPL + household_size + perc_0to17 + perc_18to34 + perc_35to54 +
		perc_male + perc_asian + perc_black + perc_hispanic + perc_other
	spec <- entered_market ~ entered_late + FPL_bracket + household_size + perc_0to17 + perc_18to34 + perc_35to54 +
		perc_male + perc_asian + perc_black + perc_hispanic + perc_other
	#sipp_logit <- glm(spec, data = sipp_households[!is.na(sipp_households$entered_market),], 
	#	family = "binomial", weights = sipp_households[!is.na(sipp_households$entered_market),"weight"])
	sipp_logit <- glm(spec, data = sipp_households[!is.na(sipp_households$entered_market),], family = "binomial")
	
	summary(sipp_logit)
	save(sipp_logit,file="data/final/sipp_logit_enter")
	
	# Departures
	spec <- exited_market ~ exited_early + FPL_bracket + household_size + perc_0to17 + perc_18to34 + perc_35to54 +
		perc_male + perc_asian + perc_black + perc_hispanic + perc_other
	sipp_logit <- glm(spec, data = sipp_households[!is.na(sipp_households$exited_market),], family = "binomial")
	summary(sipp_logit)
	save(sipp_logit,file="data/final/sipp_logit_exit")
	
	# Combined
	spec <- transitioned ~ as.factor(start_date) + as.factor(end_date) + FPL_bracket + household_size + perc_0to17 + perc_18to34 + perc_35to54 +
		perc_male + perc_asian + perc_black + perc_hispanic + perc_other
	spec <- transitioned ~ FPL_bracket + household_size + perc_0to17 + perc_18to34 + perc_35to54 + 
		perc_male + perc_asian + perc_black + perc_hispanic + perc_other + midyear_transition
	spec <- transitioned ~ FPL_bracket + household_size + perc_0to17 + perc_18to34 + perc_35to54 +
		perc_male + perc_asian + perc_black + perc_hispanic + perc_other
	
	sipp_logit <- glm(spec, data = sipp_households[!is.na(sipp_households$exited_market) | !is.na(sipp_households$entered_market),], family = "binomial")
	summary(sipp_logit)
	save(sipp_logit,file="data/final/sipp_logit")
	
	#sipp_ols <- lm(spec, data = sipp_households[!is.na(sipp_households$exited_market) | !is.na(sipp_households$entered_market),])
	#summary(sipp_ols)
	