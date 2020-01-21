install.packages(c("sf"
                   #, "tmap"
                   #, "tmaptools"
                   , "RSQLite"
                   , "tidyverse"
                   , "sandwich"
                   , "msm"), repos = "https://www.stats.bris.ac.uk/R/")
install.packages("sandwich")
install.packages("msm")
#sf: simple features, standard way to encode spatial vector data
#tmap: layer-based and easy approach to make thematic maps
#tmaptools: set of tools for reading and processing spatial data
#RSQLite: embeds the SQLite database engine in R

install.packages("rgdal") # FOR MAC

#install.packages("raster") # TO MANIPULATE RASTERS

install.packages("shinyjs") #FOR COOL COLORED MAPS

#install.packages("leaflet.extras")
install.packages('reshape')

install.packages("processx")

# LOAD LIBRARIES
library(processx)
library(rgdal)
#library(sf) #TO READ SHAPEFILES 
#library(sp)
library(tidyverse) #TO MANIPULATE CSV FILES 
#library(tmap) #TO PLOT MAPS
#library(tmaptools) 
library(readr)
#library(RSQLite)  #TO CONNECT CSV DATA TO GEOPACKAGE
#library(raster)
library(tibble)
library(leaflet)
library(leaflet.extras)
#library(maptools)
library(rgeos)
library(rgdal)
library(reshape)
library(dplyr)
library(plotly)
library(stats)
library(ggplot2)
library(sandwich)
library(msm)

setwd("~/desktop/QMfinalpaper")
LDDnewhousing <- read_csv2("LDD - Housing Completions unit level.csv", skip = 0
                           , col_types = cols(
                             Borough = col_character(),
                             `Planning Authority` = col_character(),
                             `Borough Reference` = col_character(),
                             `Permission type` = col_character(),
                             `Existing units` = col_double(),
                             `Proposed units` = col_double(),
                             `Net unit level` = col_double(),
                             `Number of bedrooms` = col_double(),
                             `Affordable (Yes/No)` = col_character(),
                             `Unit Tenure` = col_character(),
                             `Unit provider` = col_character(),
                             `Unit Type` = col_character(),
                             `Multiple Occupancy (Y/N)` = col_character(),
                             `Housing for Older People (Y/N)` = col_character(),
                             `Other Sheltered (Y/N)` = col_character(),
                             `Plot description` = col_character(),
                             `Maximum building height (storeys)` = col_double(),
                             `Development type` = col_character(),
                             `Permission Status` = col_character(),
                             `Permission Date` = col_date(format = "%d/%m/%Y"),
                             `Completed Date (Res)` = col_date(format = "%d/%m/%Y"),
                             `Completed Financial Year (Res)` = col_character(),
                             `Scheme Name` = col_character(),
                             `Site Name/Number` = col_character(),
                             `Primary Street Name` = col_character(),
                             `Secondary Street(s)` = col_character(),
                             `Post Code` = col_character(),
                             Ward = col_character(),
                             Easting = col_double(),
                             Northing = col_double(),
                             `Total proposed units` = col_number(),
                             `Residential Site Area` = col_double(),
                             `Total site area` = col_double(),
                             `Development Description` = col_character(),
                             X35 = col_logical(),
                             X36 = col_logical()
                           )
                           )
spec(LDDnewhousing)
LDDnewhousing
LDDnewhousing <- filter(LDDnewhousing, `Proposed units` != 0 
                        & `Development type` == "New build" 
                        & `Affordable (Yes/No)` == "Yes"
                        & `Residential Site Area`!=0)
LDDnewhousing <- LDDnewhousing[,-c(3,4,5,7,13,14,15,16,17,18,19,23,24,35,36)]
LDDnewhousing <- unique(LDDnewhousing)
colonnes <- colnames(LDDnewhousing)
unitTypes <- unique(LDDnewhousing$`Unit Type`)
boroughs <- unique(LDDnewhousing$Borough)



#UNITS DENSITY
unitdens <- data.frame(c(1:6850))
unitdens$east <- LDDnewhousing$Easting
unitdens$north <- LDDnewhousing$Northing
unitdens$total <- LDDnewhousing$`Total proposed units`
unitdens$area <- LDDnewhousing$`Total site area` #IN HECTARES
unitdens[,-1]
unitdens <- unique(unitdens)
unitdens$dens <- unitdens$total/unitdens$area

max(unitdens$dens)

boxplot(unitdens$dens,legend ="Density of units of housing per")

dwellingdens <- data.frame(c(2013:2018),c(21.3,21.5,21.7,21.9,22.1,22.3)) #dwellings per hectare
colnames(dwellingdens) <- c("Year","dwellhec")
plot(unitdens$area,unitdens$total)

#GRAPH OF DENSITY PER HECTRE

xaxis2 <- list(title = list(text = "data"
                            ,font = list(family = 'Helvetica'
                                         ,size = 18
                                         ,color = 'rgb(82, 82, 82)')
                            ,standoff = 0
)
,showline = TRUE
,showgrid = FALSE
,showticklabels = TRUE
,linecolor = 'rgb(204, 204, 204)'
,linewidth = 2
,autotick = FALSE
,ticks = 'outside'
,tickcolor = 'rgb(204, 204, 204)'
,tickwidth = 2
,ticklen = 5
,tickfont = list(family = 'Helvetica'
                 ,size = 12
                 ,color = 'rgb(82, 82, 82)')
)

yaxis2 <- list(title = list(text = "Density(units per hectare)"
                            ,font = list(family = 'Helvetica'
                                         ,size = 18
                                         ,color = 'rgb(82, 82, 82)')
                            ,standoff = 0
)

,showgrid = FALSE
,zeroline = FALSE
,showline = TRUE
,showticklabels = TRUE
,tickfont = list(family = 'Helvetica',
                 size = 12,
                 color = 'rgb(82, 82, 82)')
,range= c(0,1000))


density <- plot_ly(unitdens,y=~dens, type = "box",name = "1") %>%
  # add_trace(y=~Hbrooms2018$`Proposed units`[`Number of bedrooms`==2],name="2") %>%
  # add_trace(y=~Hbrooms2018$`Proposed units`[`Number of bedrooms`==3],name="3") %>%
  # add_trace(y=~Hbrooms2018$`Proposed units`[`Number of bedrooms`==4],name="4") %>%
  layout(title ="Density of units per hectare"
         , xaxis = xaxis2
         , yaxis = yaxis2
         , autosize = FALSE
         , showlegend = FALSE
         , margin = margin
  )
density




unique(Hbrooms2018$`Unit provider`)

testdates <- filter(LDDnewhousing,  `Completed Date (Res)` > "2013-01-01" & `Completed Date (Res)` < "2013-12-31")
testdates
# YEARS " 2013 - 2018
#HISTOGRAM OF THE NUMBER OF ROOMS PER UNIT BUILT

Hbrooms2018 <- filter(LDDnewhousing
                  , `Completed Date (Res)` > "2018-01-01" 
                   &  `Completed Date (Res)` < "2018-12-31") %>%
   group_by(`Number of bedrooms`) #%>%
  # summarise(`Number of units built`=sum(`Proposed units`))

Hbrooms2018

xaxis1 <- list(title = list(text = "Number of bedrooms"
                           ,font = list(family = 'Helvetica'
                                        ,size = 18
                                        ,color = 'rgb(82, 82, 82)')
                           ,standoff = 0
                           )
               ,showline = TRUE
               ,showgrid = FALSE
               ,showticklabels = TRUE
               ,linecolor = 'rgb(204, 204, 204)'
               ,linewidth = 2
               ,autotick = FALSE
               ,ticks = 'outside'
               ,tickcolor = 'rgb(204, 204, 204)'
               ,tickwidth = 2
               ,ticklen = 5
               ,tickfont = list(family = 'Helvetica'
                                ,size = 12
                                ,color = 'rgb(82, 82, 82)')
               )

yaxis1 <- list(title = list(text = "Units built per project"
                           ,font = list(family = 'Helvetica'
                                        ,size = 18
                                        ,color = 'rgb(82, 82, 82)')
                           ,standoff = 0
                           )
               
              ,showgrid = FALSE
              ,zeroline = FALSE
              ,showline = TRUE
              ,showticklabels = TRUE
              ,tickfont = list(family = 'Helvetica',
                               size = 12,
                               color = 'rgb(82, 82, 82)')
              ,range= c(0,30))

p <- plot_ly(Hbrooms2018,y=~Hbrooms2018$`Proposed units`[`Number of bedrooms`==1], type = "box",name = "1") %>%
  add_trace(y=~Hbrooms2018$`Proposed units`[`Number of bedrooms`==2],name="2") %>%
  add_trace(y=~Hbrooms2018$`Proposed units`[`Number of bedrooms`==3],name="3") %>%
  add_trace(y=~Hbrooms2018$`Proposed units`[`Number of bedrooms`==4],name="4") %>%
  layout(title ="Building projects by units built with respect to number of bedrooms"
         , xaxis = xaxis1
         , yaxis = yaxis1
         , autosize = FALSE
         , showlegend = FALSE
         , margin = margin
         )
p
# Nbofroomsbuilt <- data.frame(nbrooms2013$`Number of bedrooms`
#                        ,nbrooms2013$`Number of units built`
#                        ,nbrooms2014$`Number of units built`
#                        ,nbrooms2015$`Number of units built`
#                        ,nbrooms2016$`Number of units built`
#                        ,nbrooms2017$`Number of units built`
#                        ,nbrooms2018$`Number of units built`
#                        ,check.rows = TRUE
# )

# histo <- plot(nbrooms2013, type = "h",main = "2013")  
#   plot(nbrooms2014, type = "h",main = "2014") 
#   plot(nbrooms2015, type = "h",main = "2015")  
#   plot(nbrooms2016, type = "h",main = "2016")  
#   plot(nbrooms2017, type = "h",main = "2017")  
#   plot(nbrooms2018, type = "h",main = "2018")
  
# colnames(Nbofroomsbuilt) <-  c("Nb of units built 2013"
#   ,"Nb of units built 2014"
#   ,"Nb of units built 2015"
#   ,"Nb of units built 2016"
#   ,"Nb of units built 2017"
#   ,"Nb of units built 2018"
#   )

m1 <- glm(`Number of bedrooms` ~ `Number of units built`,family="poisson", data = nbrooms2018)
summary(m1, dispersion = 1)
anova (m1, dispersion = 1, test = "Chisq")

plot(m1)


mymean <- function(donnees){
  return(sum(donnees[,1]*donnees[,2])/sum(donnees[,2]))
}
mu2018 <- mymean(nbrooms2018)

meanI <- data.frame(mu2013,mu2014,mu2015,mu2016,mu2017,mu2018)


mean <- data.frame(c(2013:2018), c(0))
for (i in c(1:6)){
  mean[i,2] <- meanI[i]
}

colnames(mean) <- c("Year", "Average number of bedrooms")

mu2018 <- mymean(nbrooms2018)

totalunits <- data.frame(c(2013:2018),c(0))

totalunits[1,2] <- sum(nbrooms2018$`Number of units built`)
totalunits[2,2] <- sum(nbrooms2017$`Number of units built`)
totalunits[3,2] <- sum(nbrooms2016$`Number of units built`)
totalunits[4,2] <- sum(nbrooms2015$`Number of units built`)
totalunits[5,2] <- sum(nbrooms2014$`Number of units built`)
totalunits[6,2] <- sum(nbrooms2013$`Number of units built`)

colnames(totalunits) <- c("Year","Total units built")
ttunitsYear <- plot(totalunits, type = "b")
meanroomsYear <- plot(mean, type = "b")
vari <- var(nbrooms2013$`Number of bedrooms`)

resume <- data.frame(totalunits,mean$`Average number of bedrooms`)

margin <- list(autoexpand = FALSE,
               l = 50,
               r = 50,
               t = 50)


xaxis <- list(title = list(text = "Year"
                           ,font = list(family = 'Helvetica'
                                        ,size = 18
                                        ,color = 'rgb(82, 82, 82)')
                           ,standoff = 0
                           )
              ,showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Helvetica',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))

yaxis <- list(title = list(text = "Units built"
                           ,font = list(family = 'Helvetica'
                                        ,size = 18
                                        ,color = 'rgb(82, 82, 82)')
                           ,standoff = 0
                           )
              ,showgrid = FALSE
              ,zeroline = FALSE
              ,showline = TRUE
              ,showticklabels = TRUE
              ,tickfont = list(family = 'Helvetica',
                               size = 12,
                               color = 'rgb(82, 82, 82)'))

unitsbuilt <- plot_ly(totalunits
                      ,x = ~Year
                      ,y = ~`Total units built`
                      ,type = 'scatter'
                      ,mode = 'lines+markers'
                      ,width = 500
                      ,height = 400
                      ,line = list(color = 'rgb(0, 0, 0)', width = 2)
                      ,marker = list(size = 5
                                     ,color = 'rgb(0, 0, 0)'
                                     ,line = list(color = 'rgb(0, 0, 0)'
                                     ,width = 2)
                                     )) %>%
  layout(title = "Number of units built"
         , xaxis = xaxis
         , yaxis = yaxis
         , autosize = FALSE
         , showlegend = FALSE
         , margin = margin) 
unitsbuilt

avbedrooms <- plot_ly(mean
                      , x = ~Year
                      , y = ~`Average number of bedrooms`
                      , type = 'scatter'
                      , mode = 'lines+markers'
                      , width = 500
                      , height = 400
                      ,line = list(color = 'rgb(0, 0, 0)', width = 2)
                      ,marker = list(size = 5
                                     ,color = 'rgb(0, 0, 0)'
                                     ,line = list(color = 'rgb(0, 0, 0)'
                                     ,width = 2)
                                     ) 
                      ) %>%
  layout(title = "Average number of bedrooms per unit"
         , xaxis = xaxis
         , yaxis = yaxis
         , autosize = FALSE
         , showlegend = FALSE
         , margin = margin) 

avbedrooms
colonnes

x <-c(1:5)
y <- dpois(x, lambda = mean$`2013`)
plot(x,y)
colnames(meanI) <- c(2013:2018)
mean <- meanI

nbrooms2013$poisson=dpois(nbrooms2013$`Number of bedrooms`,mean$`2013`)
nbrooms2014$poisson=dpois(nbrooms2014$`Number of bedrooms`,mean$`2014`)
nbrooms2015$poisson=dpois(nbrooms2015$`Number of bedrooms`,mean$`2015`)
nbrooms2016$poisson=dpois(nbrooms2016$`Number of bedrooms`,mean$`2016`)
nbrooms2017$poisson=dpois(nbrooms2017$`Number of bedrooms`,mean$`2017`)
nbrooms2018$poisson=dpois(nbrooms2018$`Number of bedrooms`,mean$`2018`)

nbrooms2013$`Number of units built` <- nbrooms2013$`Number of units built`/sum(nbrooms2013$`Number of units built`) 
nbrooms2014$`Number of units built` <- nbrooms2014$`Number of units built`/sum(nbrooms2014$`Number of units built`) 
nbrooms2015$`Number of units built` <- nbrooms2015$`Number of units built`/sum(nbrooms2015$`Number of units built`) 
nbrooms2016$`Number of units built` <- nbrooms2016$`Number of units built`/sum(nbrooms2016$`Number of units built`) 
nbrooms2017$`Number of units built` <- nbrooms2017$`Number of units built`/sum(nbrooms2017$`Number of units built`) 
nbrooms2018$`Number of units built` <- nbrooms2018$`Number of units built`/sum(nbrooms2018$`Number of units built`) 

totalunits

plot(Hbrooms2018$`Number of bedrooms`,Hbrooms2018$`Proposed units`)

# KIND OFF HISTOGRAMM OF THE SIZE OF EACH CONSTRUCTION PROJECT
Hbrooms <- plot_ly(Hbrooms2018 
                   , x= ~`Number of bedrooms`
                   , y= ~`Proposed units`
                   , name = ''
                   , type = 'scatter'
                   , mode = 'markers'
                   #,connectgaps = TRUE
                   #,line = list(color = 'rgb(0, 0, 0)', width = 2)
                   ,marker = list(size = 5
                                  ,color = 'rgb(0, 0, 0)'
                                  ,line = list(color = 'rgb(0, 0, 0)'
                                               ,width = 2))) %>%
  layout(title ="Projects by units built with respect to bedrooms number"
         , xaxis = xaxis
         , yaxis = yaxis
         , autosize = FALSE
         , showlegend = FALSE
         , margin = margin
  )
Hbrooms

#OUTLINE OF THE POISSON DENSITY IN THE DATA 

NBRooms <- plot_ly(nbrooms2018 
                   , x= ~`Number of bedrooms`
                   , y= ~`Number of units built`
                   , name = ''
                   , type = 'scatter'
                   , mode = 'lines+markers'
                   #,connectgaps = TRUE
                   ,line = list(color = 'rgb(0, 0, 0)', width = 2)
                   ,marker = list(size = 5
                                  ,color = 'rgb(0, 0, 0)'
                                  ,line = list(color = 'rgb(0, 0, 0)'
                                  ,width = 2))) %>%
  layout(title ="Units built in 2018"
         , xaxis = xaxis
         , yaxis = yaxis
         , autosize = FALSE
         , showlegend = FALSE
         , margin = margin
         )
NBRooms
  # add_trace(nbrooms2017,x~=`Number of bedrooms`, y=~`Number of units built`
  #           ,type = 'scatter'
  #           , mode = 'markers'
  #           #,connectgaps = TRUE
  #           ,line = list(color = 'rgb(0, 0, 0)', width = 2)
  #           ,marker = list(size = 5
  #                          ,color = 'rgb(0, 0, 255)'
  #                          ,line = list(color = 'rgb(0, 0, 0)'
  #                          ,width = 2))) 


# add_trace(nbrooms2014
#           ,x= ~`Number of bedrooms`
#           ,y= ~`Number of units built`
#           ,name = "2014"
#           ,line = list(color = 'rgb(22, 96, 167)'
#                        , width = 4)
#           ,marker = list(size = 5
#                          ,color = 'rgb(0, 0, 0)'
#                          ,line = list(color = 'rgb(0, 0, 0)'
#                          ,width = 2))
#           ) %>%