# Location based expected Goals 
# Script written during the Montreal Hackathon 2015
# Author: Manuel Dierkes / d i e r k e s . j . m @ g o o g l e m a i l . c o m 


setwd('DIRECTORY')
Match1Data <- read.csv("Mtl-Ott-game1.csv")
attach(Match1Data)

# libraries for plotly plotting
library("devtools")
library("plotly")

# put your plotly credentials. If you dont have an account make sure to create one!
set_credentials_file("", "")

py <- plotly()


# Let´s start by having a look at the field
plot(xCoord, yCoord)
max(yCoord)
min(yCoord)
max(xCoord)
min(xCoord)

# transform the Coordinates so everything happens in the same half of the field
xCoordAdj = xCoord
yCoordAdj = yCoord
yCoordAdj[which(xCoordAdj < 0)] = yAdjCoord[which(xCoordAdj < 0)]
xCoordAdj[which(xCoordAdj < 0)] = xAdjCoord[which(xCoordAdj < 0)]
# transform the Coordinates so that one half of the field fits the dimensions 200 x 168 (since Coordinates have a .5 increment)
# and lets also make sure we have a Coordinate system starting at x0 y0
yCoordNorm = (yCoordAdj + 42) * 2
xCoordNorm = xCoordAdj *2
# lets have a look
plot (xCoordNorm, yCoordNorm) 

# lets have a look from where the shots were taken
plot(xCoordNorm[which(name == "shot")], yCoordNorm[which(name == "shot")], col=c("blue","green")[team], xlim = c(0,200), ylim=c(0,168))
plot(xCoordNorm[which(name == "shot" & team == "Montreal Canadiens")], yCoordNorm[which(name == "shot" & team == "Montreal Canadiens")], col=c("red"), xlim = c(0,200), ylim=c(0,168))
plot(xCoordNorm[which(name == "shot" & team != "Montreal Canadiens")], yCoordNorm[which(name == "shot" & team != "Montreal Canadiens")], col=c("blue"), xlim = c(0,200), ylim=c(0,168))

plot(xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], col=c("red"), xlim = c(0,200), ylim=c(0,168))
plot(xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], col=c("blue"), xlim = c(0,200), ylim=c(0,168))


#plot on plotly
traceShotsHabsONNET <- list(
  x = xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], 
  y = yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], 
  mode = "markers", 
  name = "Habs", 
  text = playerLastName[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], 
  marker = list(
    color = "rgb(234, 153, 153)", 
    size = 12, 
    line = list(
      color = "white", 
      width = 0.5
    )
  ), 
  type = "scatter"
)
traceShotsSensONNET <- list(
           x = xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], 
           y = yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], 
           mode = "markers", 
           name = "Sens", 
           text = playerLastName[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], 
           marker = list(
             color = "rgb(255, 217, 102)", 
             size = 12, 
             line = list(
               color = "white", 
               width = 0.5
             )
           ), 
           type = "scatter"
  )
  data <- list(traceShotsHabsONNET, traceShotsSensONNET)
  layout <- list(
    title = "Shot Location Match 3", 
    xaxis = list(
      title = "X Coordinate", 
      range = c(0, 200),
      showgrid = FALSE, 
      zeroline = FALSE
    ), 
    yaxis = list(
      title = "y Coordinate", 
      range = c(0, 168),
      showline = FALSE
    )
  )
  response <- py$plotly(data, kwargs=list(layout=layout, filename="ShotsOnTargetGame3"))
  url <- response$url


# let create a rough irregular Grid and assign probabilities of shot success (based on guessing) to each field
abline(v =60, untf = FALSE)
abline(v =100, untf = FALSE)
abline(v =140, untf = FALSE)
abline(v =180, untf = FALSE)

abline(h =34, untf = FALSE)
abline(h =64, untf = FALSE)
abline(h =104, untf = FALSE)
abline(h =134, untf = FALSE)


expgGridProb = yCoordNorm
expgGridProb[which(expgGridProb != 5000)] = 0

expgGridProb[which(name == "shot" & xCoordNorm >= 0 & xCoordNorm < 60 & yCoordNorm >= 134)] = 0.03
expgGridProb[which(name == "shot" & xCoordNorm >= 0 & xCoordNorm < 60 & yCoordNorm >= 104 & yCoordNorm < 134)] = 0.04
expgGridProb[which(name == "shot" & xCoordNorm >= 0 & xCoordNorm < 60 & yCoordNorm >= 64 & yCoordNorm < 104)] = 0.04
expgGridProb[which(name == "shot" & xCoordNorm >= 0 & xCoordNorm < 60 & yCoordNorm >= 34 & yCoordNorm < 64)] = 0.04
expgGridProb[which(name == "shot" & xCoordNorm >= 0 & xCoordNorm < 60 & yCoordNorm < 34)] = 0.03
                
expgGridProb[which(name == "shot" & xCoordNorm >= 60 & xCoordNorm < 100 & yCoordNorm >= 134)] = 0.04
expgGridProb[which(name == "shot" & xCoordNorm >= 60 & xCoordNorm < 100 & yCoordNorm >= 104 & yCoordNorm < 134 )] = 0.06
expgGridProb[which(name == "shot" & xCoordNorm >= 60 & xCoordNorm < 100 & yCoordNorm >= 64 & yCoordNorm < 104 )] = 0.08
expgGridProb[which(name == "shot" & xCoordNorm >= 60 & xCoordNorm < 100 & yCoordNorm >= 34 & yCoordNorm < 64 )] = 0.06
expgGridProb[which(name == "shot" & xCoordNorm >= 60 & xCoordNorm < 100 & yCoordNorm < 34 )] = 0.04

expgGridProb[which(name == "shot" & xCoordNorm >= 100 & xCoordNorm < 140 & yCoordNorm >= 134)] = 0.05
expgGridProb[which(name == "shot" & xCoordNorm >= 100 & xCoordNorm < 140 & yCoordNorm >= 104 & yCoordNorm < 134 )] = 0.08
expgGridProb[which(name == "shot" & xCoordNorm >= 100 & xCoordNorm < 140 & yCoordNorm >= 64 & yCoordNorm < 104 )] = 0.1
expgGridProb[which(name == "shot" & xCoordNorm >= 100 & xCoordNorm < 140 & yCoordNorm >= 34 & yCoordNorm < 64 )] = 0.08
expgGridProb[which(name == "shot" & xCoordNorm >= 100 & xCoordNorm < 140 & yCoordNorm < 34 )] = 0.05

expgGridProb[which(name == "shot" & xCoordNorm >= 140 & xCoordNorm < 180 & yCoordNorm >= 134)] = 0.03
expgGridProb[which(name == "shot" & xCoordNorm >= 140 & xCoordNorm < 180 & yCoordNorm >= 104 & yCoordNorm < 134 )] = 0.1
expgGridProb[which(name == "shot" & xCoordNorm >= 140 & xCoordNorm < 180 & yCoordNorm >= 64 & yCoordNorm < 104 )] = 0.2
expgGridProb[which(name == "shot" & xCoordNorm >= 140 & xCoordNorm < 180 & yCoordNorm >= 34 & yCoordNorm < 64 )] = 0.1
expgGridProb[which(name == "shot" & xCoordNorm >= 140 & xCoordNorm < 180 & yCoordNorm < 34 )] = 0.03

expgGridProb[which(name == "shot" & xCoordNorm >= 180 & yCoordNorm >= 134)] = 0.01
expgGridProb[which(name == "shot" & xCoordNorm >= 180 & yCoordNorm >= 104 & yCoordNorm < 134 )] = 0.01
expgGridProb[which(name == "shot" & xCoordNorm >= 180 & yCoordNorm >= 64 & yCoordNorm < 104 )] = 0.01
expgGridProb[which(name == "shot" & xCoordNorm >= 180 & yCoordNorm >= 34 & yCoordNorm < 64 )] = 0.01
expgGridProb[which(name == "shot" & xCoordNorm >= 180 & yCoordNorm < 34 )] = 0.01


# lets get some simple stats
numberOfShots = length(name[which(name == "shot")])
numberOfShotsONNET = length(name[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0)])

sum_expG_Habs = sum(expgGridProb[which(name == "shot" & team == "Montreal Canadiens")])
sum_expG_Senators = sum(expgGridProb[which(name == "shot" & team != "Montreal Canadiens")])
sum_expG_HabsONNET = sum(expgGridProb[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")])
sum_expG_SenatorsONNET = sum(expgGridProb[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")])


sum_shots_HabsONNET = length(name[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")])
sum_shots_SenatorsONNET = length(name[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")])
sum_shots_Habs = length(name[which(name == "shot" & team == "Montreal Canadiens")])
sum_shots_Senators = length(name[which(name == "shot" & team != "Montreal Canadiens")])


AvGoalPerShot2014 = 0.0815
basic_expG_Habs = AvGoalPerShot2014 * sum_shots_HabsONNET
basic_expG_Senators = AvGoalPerShot2014 * sum_shots_SenatorsONNET

grid_expG_Habs = sum_expG_Habs 
grid_expG_Senators = sum_expG_Senators

# Ok this looks nice and the results seem reasonable, but lets see if we can improve this a bit.
# Searching the internet I have come across an awesome heatmap with shot probabilities (http://hockeymetrics.net/introducing-a-new-stat-location-adjusted-expected-goals-percentage/) made by
# (Wesley). In one of the comments he explains how he calculated the colors from the percentages. 
# This should be enough information the reverse engineer it!


# loading some libraries we will need
library("png")
library("grDevices")


# I modified the heatmap a bit (using GIMP) to get rid of the artifacts and background, and blurred it a bit (Gaussian Blur 10px) and sharpened it (twice 87%)to get rid 
# of the white spaces. I put the alpha threashhold to 0 and put an appropriate background color. 
#I also resized it to fit nicely in our 168 x 200 format, so that each pixel represents
# one of the possible x-y-coordinates

hm=readPNG(paste0('FILENAME'), FALSE)
# Limits of image
xbounds=c(1, ncol(hm))
ybounds=c(1, nrow(hm))

plot(1, type="n",
     xaxt="n", yaxt="n",
    xlim=xbounds, ylim=ybounds,
     xlab="", ylab="")

rasterImage(hm, xbounds[1], ybounds[1], xbounds[2], ybounds[2])
#looks great! And we now have an array with the RGB values of the image.


# Now we need to transform the RGB values into HSV-values, since the formula to create the heatmap
# used the Hue. Fortunately there is a nice R-package that will do this for us. But it needs the data 
# in form of a matrix with three rows. All professional programmers please close your eyes: here comes a 3-nested
# for-loop...

TheMAtrix = matrix(,nrow=3, ncol=33600, byrow=TRUE)
count = 1
for(i in 1:168){
  for(j in 1:200){
    for(k in 1:3){
      TheMAtrix[k,count] = hm[i,j,k] * 255  
    }
    count = count + 1
  }
}


# lets feed that Matrix to the rgb2hsv function
hm_HSV = rgb2hsv(TheMAtrix, maxColorValue = 255)


# Now lets create a vector with only the hue values.
# We will also need to multiply these values by 360 since this is the format the heatmap was created in
hm_Prob = hm_HSV[1,] * 360

# Lets run these values through the transformed formula to get our probabilities
for (i in 1:length(hm_Prob)){
  hm_Prob[i] = 26.53 - (0.111658 * hm_Prob[i])
}
# And now lets rearrange this back to our 168x200 format
Prob_Field = matrix(hm_Prob,nrow=168, ncol=200, byrow=TRUE)
Prob_Field = Prob_Field / 100

# lets have a look at what we got
# First lets look at some vectors
library("Cairo")
png("84.png")
col84 <- rgb(hm[84,,])
plot(Prob_Field[84,], col=col84, pch=20, cex=3,xlim = c(0,200), ylim=c(0,0.27))
dev.off()

png("5.png")
col1 <- rgb(hm[1,,])
plot(Prob_Field[1,], col=col1, pch=20, cex=3,xlim = c(0,200), ylim=c(0,0.27))
dev.off()

png("168.png")
col168 <- rgb(hm[168,,])
plot(Prob_Field[168,], col=col168, pch=20, cex=3,xlim = c(0,200), ylim=c(0,0.27))
dev.off()

png("42.png")
col42 <- rgb(hm[42,,])
plot(Prob_Field[42,], col=col42, pch=20, cex=3,xlim = c(0,200), ylim=c(0,0.27))
dev.off()

png("126.png")
col126 <- rgb(hm[126,,])
plot(Prob_Field[126,], col=col126, pch=20, cex=3,xlim = c(0,200), ylim=c(0,0.27))
dev.off()

hist(Prob_Field)
min(Prob_Field)
max(Prob_Field)
mean(Prob_Field)
# This looks great! However, there are some outliers with values below 0
# They seem to be at rather uniteresting places on the field so 
# lets fix this by just giving them a very low probability of success

Prob_Field[which(Prob_Field < 0)] = 0.01


# now lets map our shots to the probabilities 


yCoordNormShotONNET = yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0)]
xCoordNormShotONNET = xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0)]

yCoordNormShot = yCoordNorm[which(name=="shot")]
xCoordNormShot = xCoordNorm[which(name=="shot")]

yCoordNormShotHabsONNET = yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")]
xCoordNormShotHabsONNET = xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")]

yCoordNormShotSenatorsONNET = yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")]
xCoordNormShotSenatorsONNET = xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")]

yCoordNormShotHabs = yCoordNorm[which(name=="shot" & team == "Montreal Canadiens")]
xCoordNormShotHabs = xCoordNorm[which(name=="shot" & team == "Montreal Canadiens")]

yCoordNormShotSenators = yCoordNorm[which(name=="shot" & team != "Montreal Canadiens")]
xCoordNormShotSenators = xCoordNorm[which(name=="shot" & team != "Montreal Canadiens")]



shotProb = matrix(,nrow=numberOfShots, ncol=1, byrow=TRUE)
colShots = matrix(,nrow=numberOfShots, ncol=4, byrow=TRUE)
i = 0
hm_expG = 0
while(i < numberOfShots){
  i = i +1
  this_y = yCoordNormShot[i]
  this_x = xCoordNormShot[i]
  shotProb[i] =  Prob_Field[this_y, this_x]
  colShots[i,] =  hm[this_y, this_x,]
  hm_expG = hm_expG + Prob_Field[this_y, this_x]
}

shotProbHabs = matrix(,nrow=sum_shots_Habs, ncol=1, byrow=TRUE)
colShotsHabs = matrix(,nrow=sum_shots_Habs, ncol=4, byrow=TRUE)
i = 0
hm_expG_Habs = 0
while(i < sum_shots_Habs){
  i = i +1
  this_y = yCoordNormShotHabs[i]
  this_x = xCoordNormShotHabs[i]
  shotProbHabs[i] =  Prob_Field[this_y, this_x]
  colShotsHabs[i,] =  hm[this_y, this_x,]
  hm_expG_Habs = hm_expG_Habs + Prob_Field[this_y, this_x]
}

shotProbSenators = matrix(,nrow=sum_shots_Senators, ncol=1, byrow=TRUE)
colShotsSenators = matrix(,nrow=sum_shots_Senators, ncol=4, byrow=TRUE)
i = 0
hm_expG_Senators = 0
while(i < sum_shots_Senators){
  i = i +1
  this_y = yCoordNormShotSenators[i]
  this_x = xCoordNormShotSenators[i]
  shotProbSenators[i] =  Prob_Field[this_y, this_x]
  colShotsSenators[i,] =  hm[this_y, this_x,]
  hm_expG_Senators = hm_expG_Senators + Prob_Field[this_y, this_x]
}


#same for ONNET
shotProbONNET = matrix(,nrow=numberOfShots, ncol=1, byrow=TRUE)
colShotsONNET = matrix(,nrow=numberOfShots, ncol=4, byrow=TRUE)
i = 0
hm_expG = 0
while(i < numberOfShotsONNET){
  i = i +1
  this_y = yCoordNormShotONNET[i]
  this_x = xCoordNormShotONNET[i]
  shotProbONNET[i] =  Prob_Field[this_y, this_x]
  colShotsONNET[i,] =  hm[this_y, this_x,]
  hm_expGONNET = hm_expGONNET + Prob_Field[this_y, this_x]
}

shotProbHabsONNET = matrix(,nrow=sum_shots_HabsONNET, ncol=1, byrow=TRUE)
colShotsHabsONNET = matrix(,nrow=sum_shots_HabsONNET, ncol=4, byrow=TRUE)
i = 0
hm_expG_HabsONNET = 0
while(i < sum_shots_HabsONNET){
  i = i +1
  this_y = yCoordNormShotHabsONNET[i]
  this_x = xCoordNormShotHabsONNET[i]
  shotProbHabsONNET[i] =  Prob_Field[this_y, this_x]
  colShotsHabsONNET[i,] =  hm[this_y, this_x,]
  hm_expG_HabsONNET = hm_expG_HabsONNET + Prob_Field[this_y, this_x]
}

shotProbSenatorsONNET = matrix(,nrow=sum_shots_SenatorsONNET, ncol=1, byrow=TRUE)
colShotsSenatorsONNET = matrix(,nrow=sum_shots_SenatorsONNET, ncol=4, byrow=TRUE)
i = 0
hm_expG_SenatorsONNET = 0
while(i < sum_shots_SenatorsONNET){
  i = i +1
  this_y = yCoordNormShotSenatorsONNET[i]
  this_x = xCoordNormShotSenatorsONNET[i]
  shotProbSenatorsONNET[i] =  Prob_Field[this_y, this_x]
  colShotsSenatorsONNET[i,] =  hm[this_y, this_x,]
  hm_expG_SenatorsONNET = hm_expG_SenatorsONNET + Prob_Field[this_y, this_x]
}

# lets have a look at our new stats and compare them to the older ones:
hm_expG_Habs
hm_expG_Senators

hm_expG_HabsONNET
hm_expG_SenatorsONNET

basic_expG_Habs
basic_expG_Senators

grid_expG_Habs
grid_expG_Senators


# 

# Ok lets round this up by creating some nice looking visualisations

colShotsHabsRGB = rgb(colShotsHabs[,])
colShotsHabsRGBONNET = rgb(colShotsHabsONNET[,])
colShotsSenatorsRGB = rgb(colShotsSenators[,])
colShotsSenatorsRGBONNET = rgb(colShotsSenatorsONNET[,])

png("Shots_Habs3.png")
plot(xCoordNormShotHabs, yCoordNormShotHabs, col=colShotsHabsRGB, pch=19, cex=2, xlim = c(0,200), ylim=c(0,168))
dev.off()


png("Shots_Senators3.png")
plot(xCoordNormShotSenators, yCoordNormShotSenators, col=colShotsSenatorsRGB, pch=19, cex=3, xlim = c(0,200), ylim=c(0,168))
dev.off()

# using plotly
traceShotsHabsHM <- list(
  x = xCoordNorm[which(name == "shot" & team == "Montreal Canadiens")], 
  y = yCoordNorm[which(name == "shot" & team == "Montreal Canadiens")], 
  mode = "markers", 
  name = "Habs", 
  text = playerLastName[which(name == "shot" & team == "Montreal Canadiens")], 
  marker = list(
    color = colShotsHabsRGB, 
    size = 12, 
    line = list(
      color = "white", 
      width = 0.5
    )
  ), 
  type = "scatter"
)

data <- list(traceShotsHabsHM)
layout <- list(
  title = "Shot Location Match 2", 
  xaxis = list(
    title = "X Coordinate", 
    range = c(0, 200),
    showgrid = FALSE, 
    zeroline = FALSE
  ), 
  yaxis = list(
    title = "y Coordinate", 
    range = c(0, 168),
    showline = FALSE
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="ShotLocationHabsMatch2"))
url <- response$url

#ONNET HABS
traceShotsHabsHMONNET <- list(
  x = xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], 
  y = yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], 
  mode = "markers", 
  name = "Habs", 
  text = playerLastName[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team == "Montreal Canadiens")], 
  marker = list(
    color = colShotsHabsRGBONNET, 
    size = 12, 
    line = list(
      color = "white", 
      width = 0.5
    )
  ), 
  type = "scatter"
)

data <- list(traceShotsHabsHMONNET)
layout <- list(
  title = "Shot Location Match 1 Habs", 
  xaxis = list(
    title = "X Coordinate", 
    range = c(0, 200),
    showgrid = FALSE, 
    zeroline = FALSE
  ), 
  yaxis = list(
    title = "y Coordinate", 
    range = c(0, 168),
    showline = FALSE
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="ShotLocationHabsONNETMatch1"))
url <- response$url

#ONNET SENS
traceShotsSenatorsHMONNET <- list(
  x = xCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], 
  y = yCoordNorm[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], 
  mode = "markers", 
  name = "Habs", 
  text = playerLastName[which(regexpr(".*ONNET.*", shorthand, perl=TRUE) >0 & team != "Montreal Canadiens")], 
  marker = list(
    color = colShotsSenatorsRGBONNET, 
    size = 12, 
    line = list(
      color = "white", 
      width = 0.5
    )
  ), 
  type = "scatter"
)

data <- list(traceShotsSenatorsHMONNET)
layout <- list(
  title = "Shot Location Match 1 Senators", 
  xaxis = list(
    title = "X Coordinate", 
    range = c(0, 200),
    showgrid = FALSE, 
    zeroline = FALSE
  ), 
  yaxis = list(
    title = "y Coordinate", 
    range = c(0, 168),
    showline = FALSE
  )
)
response <- py$plotly(data, kwargs=list(layout=layout, filename="ShotLocationSenatorMatch1sONNET"))
url <- response$url

####
colShotsHabsRGBONNET = rgb(colShotsHabsONNET[,])
png("Shots_Habs1ONNET3.png")
plot(xCoordNormShotHabsONNET, yCoordNormShotHabsONNET, col=colShotsHabsRGBONNET, pch=19, cex=3, xlim = c(0,200), ylim=c(0,168))
dev.off()

colShotsSenatorsRGBONNET = rgb(colShotsSenatorsONNET[,])
png("Shots_Senators1ONNET3.png")
plot(xCoordNormShotSenatorsONNET, yCoordNormShotSenatorsONNET, col=colShotsSenatorsRGBONNET, pch=19, cex=3, xlim = c(0,200), ylim=c(0,168))
dev.off()
