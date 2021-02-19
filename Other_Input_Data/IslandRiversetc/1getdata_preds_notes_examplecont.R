
# long_society and lat_society are a pair of coordinates


# first get region
focal_region<-map.where(database = "world", x=long_society,y=lat_society)

# correct some of the names
# if p1 = vector of regions
p1[grepl("USA", p1)]<-"United States of America"
p1[p1%in%"Republic of Congo"]<-"Republic of the Congo"
p1[p1%in%"Tanzania"]<-"United Republic of Tanzania"
p1[grepl("Guinea-Bissau",p1)]<-"Guinea Bissau"
p1[p1%in%"Serbia"]<-"Republic of Serbia"
p1[p1%in%"Timor-Leste"]<-"East Timor"
p1[grepl("Micronesia", p1)]<-"Federated States of Micronesia"

# get continents
data(countryRegions,envir=environment(),package="rworldmap") # countryRegions

current_reg<-p1[1]
if(grepl(":", current_reg)) current_reg<-strsplit(current_reg,split=":")[[1]][1]
if(current_reg%in%countryRegions$ADMIN)
  {current_continent<-countryRegions[countryRegions$ADMIN%in%current_reg,]$continent
   current_continent_region<-countryRegions[countryRegions$ADMIN%in%current_reg,]$REGION
  }

# correct some of the names
# if p2 = vector of continent and p3 = vector of continent_region
  # p2 and p3 = in the same order as p1
p2[p1%in%"Canary Islands:Gran Canaria"] <-"Africa" # same for p3
p2[p1%in%"Palestine:2"] <-"Eurasia"# same for p3

# other unsolved regions 
p2[p1%in%"South-Central Pacific"]<-"Australia" # same for p3
p2[p1%in%"Southwestern Pacific"]<-"Australia"  # same for p3
p2[p1%in%"Western Canada"]<-"North America" # same for p3
p2[p1%in%"Northwestern Pacific"]<-"Eurasia" # same for p3
p2[p1%in%"Southern South America"]<-"South America" # same for p3
p2[p1%in%"Papuasia"]<-"Australia" # same for p3
p2[p1%in%"Subarctic America"]<-"North America" # same for p3
p2[p1%in%"Indo-China"]<-"Eurasia" # same for p3
p2[grepl("Africa",p1)]<-"Africa" # same for p3
p2[p1%in%"Eastern Asia"]<-"Eurasia" # same for p3
p2[p1%in%"Malaesia"]<-"Eurasia" # same for p3
p2[p2%in%"Malesia"]<-"Eurasia" # same for p3

