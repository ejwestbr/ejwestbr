
#Import the csv file from my hard drive before cleaning the dataset
install.packages("readxl")
install.packages("gridExtra")
install.packages("mosaic")

library(readxl)
library(sqldf)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(mosaic)
#sData <- read_excel ("IST_687/streamingData2.xlsx")
#sData <- read_excel ("C:\\Users\\Elizabeth's\\Documents\\IST_687\\streamingData2.xlsx")
# old data: sData <- read_excel("IST_687/streamingdata.xlsx")
sData <- read_excel ("C:\\Users\\Elizabeth's\\Documents\\IST_687\\streamingData3.xlsx")
View(sData)

#remove /10
sData$IMDb <- gsub ("/10", "", sData$IMDb)
#remove ' ' around original column heading; remove /100
colnames(sData) [6] <- "RottenTomatoes"
colnames(sData) [23] <- "SciFi"
sData$RottenTomatoes <- gsub ("/100", "", sData$RottenTomatoes)
str(sData)

#numberize function to read data as numbers and remove decimal
Numberize <-function (inputVector) {
 inputVector <-gsub (" ", "", inputVector)
  return (as.numeric(inputVector))
}

sData$IMDb <- Numberize(sData$IMDb)
sData$IMDb <- sData$IMDb * 10
sData$RottenTomatoes <- Numberize(sData$RottenTomatoes)

View(sData)

nData <- sData [ c("Title", "IMDb", "RottenTomatoes", "Comedy", "Thriller", "SciFi", 
                   "Romance", "Drama", "Action", "Biography", "Horror", "Genre 1", "Genre 2", "Genre 3", "Genre 4")]
View(nData)
#new dataframe with just some columns
#ratingGenre <- sData [ c("Title", "IMDb", "RottenTomatoes", "Comedy", "Thriller", "SciFi", "Romance", "Drama", "Action") ]
#colnames(ratingGenre) <- c("Title", "IMDb", "RottenTomatoes", "Comedy", "Thriller", "SciFi", 
                         #  "Romance", "Drama", "Action") 
View(ratingGenre)
str(ratingGenre)
summary(ratingGenre)


#don't need to run--running with sData
ratingGenre$IMDb <- Numberize(ratingGenre$IMDb)
ratingGenre$IMDb <- ratingGenre$IMDb * 10
ratingGenre$RottenTomatoes <- Numberize(ratingGenre$RottenTomatoes)

#NAs
colnames(ratingGenre)[colSums(is.na(ratingGenre))>0]
colSums(is.na(ratingGenre))>0
any(is.na(ratingGenre$IMDb))
View(ratingGenre)

IMDBmean <- mean(nData$IMDb, na.rm=TRUE)
RTmean <- mean(nData$RottenTomatoes, na.rm=TRUE)
IMDBmedian <- median(nData$IMDb, na.rm=TRUE)
RTmedian <-median(nData$RottenTomatoes, na.rm=TRUE)

#NEED A DATAFRAME FOR THE ABOVE INFO SO I CAN PLOT IT
mm <- as.numeric(c (IMDBmean, RTmean, IMDBmedian, RTmedian))
description <- c("IMDb mean", "Rotten Tomatoes mean", "IMDb median", "Rotten Tomatoes median")
meansMed <- data.frame (description, mm)       
View(meansMed)


mm.plot <- ggplot(meansMed, aes(x=description, y=mm)) + geom_col()
mm.plot
mm.plot4 <- ggplot(meansMed, aes(x=description, y=mm)) + geom_col(fill=c("lightblue", "lightblue", "darksalmon", "darksalmon"))
mm.plot4 <- mm.plot4 + coord_cartesian(ylim = c(45, 65))  + labs(x="variable", y= "rating")
mm.plot4


#never mind
IMDBlength <-length(nData$IMDb[!is.na(nData$IMDb)])
IMDBlength
RTlength <- length(nData$RottenTomatoes[!is.na(nData$RottenTomatoes)])
RTlength

###Fuck it
byGenre <- function () {
  a <- sqldf("select AVG(IMDb) from ratingGenre where x = 1") #r/y=IMDB/RottenTomatoes
  b <- sqldf("select AVG(RottenTomatoes) from ratingGenre where x = 1") #s/z =genre category
 # c <- data.frame(a, b)
  return (data.frame(a, b))
}
byGenre <- function () {
  a <- sqldf("select AVG(IMDb) from ratingGenre where x = 1") #r/y=IMDB/RottenTomatoes

  return 
}

byGenre (ratingGenre$Comedy)
byGenre( "Comedy")
byGenre(Comedy)
###################################



sqldf("select AVG(IMDb) from nData where  Comedy = 1 OR Thriller=1 OR SciFi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")
sqldf("select COUNT(IMDb) from nData where Comedy = 1 OR Thriller=1 OR Scifi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")
sqldf("select STDEV(IMDb) from nData where Comedy = 1 OR Thriller=1 OR Scifi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")
sqldf("select COUNT(IMDb) from nData where Comedy = 1 OR Thriller=1 OR Scifi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")
sqldf("select AVG(RottenTomatoes) from nData where Comedy = 1 OR Thriller=1 OR Scifi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")
sqldf("select COUNT(RottenTomatoes) from nData where Comedy = 1 OR Thriller=1 OR Scifi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")
sqldf("select AVG(IMDb) from nData where Comedy = 1 OR Thriller=1 OR Scifi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")
sqldf("select VARIANCE(RottenTomatoes) from nData where Comedy = 1 OR Thriller=1 OR Scifi = 1 OR Romance=1 OR Drama=1 OR Action=1 OR Biography=1 OR Horror=1")

#comedy ratings and counts
comedyRatIMDB <- sqldf("select AVG(IMDb) from nData where Comedy = 1")
comedyRatIMDB
comIMcount <- sqldf("select COUNT(IMDb) from nData where Comedy = 1")
comIMcount
comedyRatRT <- sqldf("select AVG(RottenTomatoes) from nData where Comedy = 1")
comedyRatRT
comRTcount <- sqldf("select COUNT(RottenTomatoes) from nData where Comedy = 1")
comRTcount

#thriller ratings and counts
thrillerRatIMDB <- sqldf("select AVG(IMDb) from nData where Thriller = 1")
thrillerRatIMDB
thrIMcount <- sqldf("select COUNT(IMDb) from nData where Thriller = 1")
thrIMcount
thrillerratRT <- sqldf("select AVG(RottenTomatoes) from nData where Thriller = 1")
thrillerratRT
thrRTcount <- sqldf("select COUNT(RottenTomatoes) from nData where Thriller = 1")
thrRTcount

#Sci-Fi ratings
sfRatIMDB <-sqldf("select AVG(IMDb) from nData where SciFi = 1")
sfRatIMDB
sfIMcount <- sqldf("select COUNT(IMDb) from nData where SciFi = 1")
sfIMcount
sfRatRT <- sqldf("select AVG(RottenTomatoes) from nData where SciFi = 1")
sfRatRT
sfRTcount <- sqldf("select COUNT(RottenTomatoes) from nData where SciFi = 1")
sfRTcount

#Romance ratings
romRatIMDB <- sqldf("select AVG(IMDb) from nData where Romance = 1")
romRatIMDB
romIMcount <- sqldf("select COUNT(IMDb) from nData where Romance = 1")
romIMcount
romRatRT <- sqldf("select AVG(RottenTomatoes) from nData where Romance = 1")
romRatRT
romRTcount <- sqldf("select COUNT(RottenTomatoes) from nData where Romance = 1")
romRTcount

#Drama ratings
dramaRatIMDB <- sqldf("select AVG(IMDb) from nData where Drama = 1")
dramaRatIMDB
dramaIMcount <- sqldf("select COUNT(IMDb) from nData where Drama = 1")
dramaIMcount
dramaRatRT <- sqldf("select AVG(RottenTomatoes) from nData where Drama = 1")
dramaRatRT
dramaRTcount <- sqldf("select COUNT(RottenTomatoes) from nData where Drama = 1")
dramaRTcount

#Action ratings
actionRatIMDB <- sqldf("select AVG(IMDb) from nData where Action = 1")
actionRatIMDB
actionIMcount <- sqldf("select COUNT(IMDb) from nData where Action = 1")
actionIMcount
actionRatRT <- sqldf("select AVG(RottenTomatoes) from nData where Action = 1")
actionRatRT
actionRTcount <- sqldf("select COUNT(RottenTomatoes) from nData where Action = 1")
actionRTcount

#Biography
bioRatIMDB <- sqldf("select AVG(IMDb) from nData where Biography = 1")
bioRatRT <- sqldf("select AVG(RottenTomatoes) from nData where Biography = 1")
bioRatIMDB
bioRatRT

#Horror
horRatIMDB <- sqldf("select AVG(IMDb) from nData where Horror = 1")
horRatRT <- sqldf("select AVG(RottenTomatoes) from nData where Horror = 1")
horRatIMDB
horRatRT
#############

IMDBratings <- as.numeric(c (IMDBmean, IMDBmedian, comedyRatIMDB, thrillerRatIMDB, sfRatIMDB, dramaRatIMDB, romRatIMDB, actionRatIMDB, bioRatIMDB, horRatIMDB))
IMDB <- as.numeric(c ( comedyRatIMDB, thrillerRatIMDB, sfRatIMDB, dramaRatIMDB, romRatIMDB, actionRatIMDB, bioRatIMDB, horRatIMDB))
RTratings <- as.numeric (c(RTmean, RTmedian, comedyRatRT, thrillerratRT, sfRatRT, dramaRatRT, romRatRT, actionRatRT, bioRatRT, horRatRT))
Rotten_Tomatoes <- as.numeric (c(comedyRatRT, thrillerratRT, sfRatRT, dramaRatRT, romRatRT, actionRatRT, bioRatRT, horRatRT))

genre <- c("total_mean", "total_median", "comedy", "thriller", "sciFi",
            "romance", "drama", "action", "biography", "horror")
genre2 <- c("comedy", "thriller", "sciFi",
                 "romance", "drama", "action", "biography", "horror")
#for cool (???) boxplot
countIMDB <-as.numeric( c(IMDBlength, IMDBlength, comIMcount, thrIMcount, sfIMcount, dramaIMcount, romIMcount, actionIMcount))
countRT <- as.numeric( c (RTlength, RTlength, comRTcount, thrRTcount, sfRTcount, dramaRTcount, romRTcount, actionRTcount))

ratings <- data.frame (genre, IMDBratings, RTratings)
View(ratings)
str(ratings)
###this one
ratings2 <- data.frame (genre2, IMDB, Rotten_Tomatoes)
View(ratings2)

mean(ratings2$IMDB)
sd(ratings2$IMDB)
mean(ratings2$Rotten_Tomatoes)
sd(ratings2$Rotten_Tomatoes)
#just choosing some stupid colors because brewer won't work
cpal <- c("cadetblue2", "darkorange2" ,"slateblue", "orange1", "rosybrown", "lightpink4", "palegreen", "palegreen3")
cpal2 <-c("cadetblue1", "cadetblue2" ,"darkorange2", "darkorange1", "lightpink2", "lightpink4", "palegreen", "palegreen3", "steelblue2", "steelblue" ,"thistle4", "thistle1", "plum1", "plum3", "indianred1", "indianred3")

##WOW!  I FINALLY GOT A PLOT!  But it's UGLY
IMDB.plot2 <-ggplot(ratings2, aes(x=genre2, y=IMDB)  )+ geom_col() 
IMDB.plot2 <- IMDB.plot2 + coord_cartesian(ylim=c(45,70))
IMDB.plot2 <- IMDB.plot2 + scale_fill_manual(values=cbPalette)
IMDB.plot2
#######


IM.plot <-ggplot(ratings2, aes(x=genre2, y=IMDB)  )+ geom_col(fill=cpal)+ geom_hline(yintercept =mean(IMDB), color="red", size=1.5 )
IM.plot <- IM.plot + coord_cartesian(ylim=c(45,70)) + labs(x="genre", y= "IMDb rating")
IM.plot

RT.plot <-ggplot(ratings2, aes(x=genre2, y=Rotten_Tomatoes)  )+ geom_col(fill=cpal) + geom_hline(yintercept =mean(Rotten_Tomatoes), color="red", size=1.5 )
RT.plot <- RT.plot + coord_cartesian(ylim=c(45,70)) + labs(x="genre", y= "Rotten Tomatoes rating")
RT.plot

#see if i can put in a line across each representing the mean and/or median

grid.arrange(IM.plot, RT.plot, ncol=2, nrow=1, top="Ratings Comparison by Platform with Mean")

#box
box.IM <- ggplot (ratings2, aes(genre2, IMDB)) + geom_boxplot()
ggplot (nData, aes(IMDb, Horror==1 ))+ geom_boxplot()
View(nData)
#MELT
# <- melt(ratings, id="genre")
#View(ratingsLong)
ratings2Long <-melt(ratings2, id="genre2")
View(ratings2Long)

#a pretty decent plot--scatter
gg1 <- ggplot(ratings2Long, aes(x=genre2, y=value, color=variable)) + geom_point(size=5) 
gg1<- gg1 + scale_color_manual(values = c("lightslategray", "tomato"))+ labs(x="Genre", y= "Rating", fill= "platform" )
gg1

#############################################
#OMG it worked!!!!!! side by side of ratings by genre and grouped by platform
gg <- ggplot(ratings2Long, aes(x=genre2, y=value, fill=variable)) + geom_col(position = position_dodge())
gg<- gg + coord_cartesian(ylim = c(50, 70))  + labs(x="Genre", y= "Rating", fill= "platform" )+  scale_fill_brewer(palette="Paired")
gg




genre.plot <- ggplot(ratings2Long, aes(x=genre2, y=value, color=variable)) + geom_point(size=6) 
genre.plot<- genre.plot + scale_color_manual(values = c("lightslategray", "tomato"))


genre.plot
IMDB.boxplot <- ggplot(sData, aes(y=IMDb)) + geom_boxplot()
IMDB.boxplot

box <- ggplot (ratings2, aes(x=genre, y=IMDBratings, fill=IMDBlength) + geom_boxplot())


############################################################################
# don't do this--need a new column with this info:  row.names(ratings) <- c("total_mean", "total_median", "comedy", "thriller", "sciFi",
          #   "romance", "drama", "action")
colnames(ratings) <- c("genre", "IMDB_ratings", "RT_ratings")


ratingsT <- data.frame(t(ratings))
View(ratingsT)
str(ratingsT)

ratingsT

ggplot (ratings, aes(ratings) + geom_col())


#let's try a tapply with new dataset, nData

tapply(nData$IMDb, nData$Comedy==1, favstats)
tapply(nData$RottenTomatoes, nData$Comedy==1, favstats)
tapply(nData$IMDb, nData$Horror==1, favstats)
tapply(nData$IMDb, nData$SciFi==1, favstats)
IM.tap <- tapply(nData$IMDb, nData$Comedy==1, favstats)

View(nData)
favstats(nData$IMDb)
favstats(nData$RottenTomatoes)

printVecinfo <- function(x){
  a<-length(x[!is.na(x)])
  b <-length(x[is.na(x)])
  c <- mean(x, na.rm=TRUE)
 d <-median(x, na.rm=TRUE)
  e <-min (x, na.rm=TRUE)
  f <- max(x, na.rm=TRUE)
  z <-sd(x, na.rm=TRUE)
  cat("length:",a,"\nNA_count:",b, "\nmean" , c , "\nmedian" , d , "\nmin:", e, "max:", f,"\nsd:", z,"\nquantile(.1-.90):", g
     )
}

printVecinfo(nData$)

summary.list = function(x ) list(
  N.with.NA.removed= length(x[!is.na(x)]),
  Count.of.NA= length(x[is.na(x)]),
  Mean=mean(x, na.rm=TRUE),
  Median=median(x, na.rm=TRUE),
  Max.Min=range(x, na.rm=TRUE),
  Variance=var(x, na.rm=TRUE),
  Std.Dev=sd(x, na.rm=TRUE),
  Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
  Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])),
  Quantile=quantile(x, na.rm=TRUE)
)


summary.list(Data$ Fish)


#colnames(IMDBratings) <- c("total_mean", "total_median", "Comedy", "Thriller", "SciFi", "Romance", "Drama", "Action")
#colnames(RTratings) <- c("total_mean", "total_median", "Comedy", "Thriller", "SciFi", "Romance", "Drama", "Action")

View(ratings)

View(IMDBratings)
str(IMDBratings)

ggplot(ratings, aes(x=ratings$IMDBratings, y=ratings$)+ geom_point())


df <- c (IMDBratings, RTratings)
df <- data.frame (df)
IMDBratingsPlot <- ggplot( IMDBratings, aes(x=) + geom_histogram())

ratlabel <- matrix (c("total_mean", "total_median", "comedy", "thriller", "sciFi",
                      "romance", "drama", "action"), nrow=1, ncol=8)
IMDBratings <- matrix (c (IMDBmean, IMDBmedian, comedyRatIMDB, thrillerRatIMDB, 
                          sfRatIMDB, dramaRatIMDB, romRatIMDB, actionRatIMDB), nrow=1, ncol=8)
RTratings <- matrix (c (RTmean, RTmedian, comedyRatRT, thrillerratRT, sfRatRT, 
                dramaRatRT, romRatRT, actionRatRT), nrow=1, ncol=8)

dfNew <- data.frame (c (ratlabel, IMDBratings, RTratings))
View(dfNew)
str(dfNew)
#df <- data.frame (IMDBratings, RTratings)
View(df)
str(df)
############

View(Comedy)
rGdf <- data.frame (comedyRatIMDB, comedyRatIMDB, thrillerRatIMDB, thrillerratRT, sfRatIMDB, sfRatRT,
                    romRatIMDB, romRatRT, dramaRatIMDB, dramaRatRT, actionRatIMDB, actionRatRT)
View(rGdf)

IMDBrat <-c (comedyRatIMDB, thrillerRatIMDB, sfRatIMDB, romRatIMDB, dramaRatIMDB, actionRatIMDB )
RTrat <- c(comedyRatRT,thrillerratRT, sfRatRT,romRatRT,dramaRatRT,actionRatRT)

genreRate <- data.frame (IMDBrat, RTrat)

View(genreRate)

tapply(ratingGenre$IMDb, ratingGenre$Comedy==1, length)

#select avg ratingGenre$IMDb

#Don't do this: merge all genre columns

#sData$Genre <- paste(sData$`Genre 1`, sData$`Genre 2`, sData$`Genre 3`, sData$`Genre 4`, 
 #                    sData$`Genre 5`, na.rm=TRUE)

ratingGenre$Genre <- sData$Genre <- paste(sData$`Genre 1`, sData$`Genre 2`, sData$`Genre 3`, sData$`Genre 4`, sData$`Genre 5`)
#ratingGenre$Genre <- sData$Genre <- paste(sData$`Genre 1`, na.rm=TRUE, sData$`Genre 2`, na.rm=TRUE,  sData$`Genre 3`, na.rm=TRUE, sData$`Genre 4`, na.rm=TRUE,   sData$`Genre 5`,na.rm=TRUE)
View(ratingGenre)
#str(ratingGenre)
#summary(ratingGenre)

#nope
tapply(ratingGenre$IMDb, ratingGenre$Comedy, mean)
tapply(ratingGenre$RottenTomatoes, ratingGenre$Genre, mean)

sum(ratingGenre$Comedy)
library(data.table)
ratingGenre$comedy<- ifelse (ratingGenre$Comedy,1, else 0)
ratingGenre$comedy <- fifelse(ratingGenre$`Genre 1`, 1, no, 0)
ratingGenre$comedy <- fifelse(ratingGenre$`Genre 1`, yes = "comedy", no=-"comedy")
ifelse(compSVM$test==compSVM$Pred,"correct","wrong")
ratingGenre$comedy <- ifelse(ratingGenre$`Genre 1`:ratingGenre$`Genre 5`=="Comedy", 1, 0)

ratingGenre$comedy <- ifelse(ratingGenre$`Genre 2`=="Comedy", 1, 0)
ratingGenre$comedy <- ifelse(ratingGenre$`Genre 3`=="Comedy", 1, 0)
ratingGenre$comedy <- ifelse(ratingGenre$`Genre 4`=="Comedy", 1, 0)
ratingGenre$comedy <- ifelse(ratingGenre$`Genre 5`=="Comedy", 1, 0)

View(ratingGenre)

#####
ratingsMean <- c(IMDBmean, RTmean)
ratingsMedian <- c(IMDBmedian, RTmedian)
#####################
Comedy <- matrix (c(comedyRatIMDB, comedyRatRT), nrow=2, ncol=1)
View(Comedy)
Thriller <- matrix (c(thrillerRatIMDB, thrillerratRT), nrow=2, ncol=1)
SciFi <- matrix (c(sfRatIMDB, sfRatRT), nrow=2, ncol=1)
Romance <- matrix (c(romRatIMDB, romRatRT) , nrow=2, ncol=1)
Drama <- matrix (c(dramaRatIMDB, dramaRatRT), nrow=2, ncol=1 )
Action <- matrix (c(actionRatIMDB, actionRatRT) , nrow=2, ncol=1)
#################

ratGen <- data.frame (Comedy, Thriller, SciFi, Romance, Drama, Action) 
lapply <-(ratGen, as.numeric)

unlist(ratGen)
View(ratGen)
str(ratGen)
ratGen <- data.frame (ratGen)
ratGen <- as.numeric(ratGen)
colnames(ratGen) <- c( "Comedy", "Thriller", "SciFi", "Romance", "Drama", "Action")
rownames(ratGen) <- c("IMDB", "RottenTomatoes")

ggplot (ratGen, aes(x=Comedy)) + geom_histogram(binwidth = 50)
plot.histogram (ratGen$Comedy)
hist(ratGen$Comedy)

