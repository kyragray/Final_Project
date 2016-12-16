library(tidyverse)
library(lubridate)
library(Quandl)
library(dplyr)
library(ggplot2)
library(stringr)
library(broom)
library(forcats)
library(ggmap)
library(leaflet)
library(readr)
library(colorspace)


attacks <- read_csv("~/Desktop/Middlebury/Data Science/Final-Project/attacks.csv")
attacks <- attacks %>% 
  tibble::rownames_to_column(var="id_internal")

##Cleaning up the main data frame 

attacks <- attacks %>%
  ##Renaming Activities 
  mutate(Activity1 = Activity) %>% 
    ##Wading
    mutate(Activity1 = ifelse(grepl("Wading", Activity), "Wading", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("wading", Activity), "Wading", Activity1)) %>% 
    ##Standing
    mutate(Activity1 = ifelse(grepl("Standing", Activity), "Standing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("standing", Activity), "Standing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Stamding", Activity), "Standing", Activity1)) %>% 
    ##Playing
    mutate(Activity1 = ifelse(grepl("Playing", Activity), "Playing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("playing", Activity), "Playing", Activity1)) %>% 
    ##Fell
    mutate(Activity1 = ifelse(grepl("Fell", Activity), "Fell", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("fell", Activity), "Fell", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Overboard", Activity), "Fell", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("overboard", Activity), "Fell", Activity1)) %>% 
    ##Bathing
    mutate(Activity1 = ifelse(grepl("Bathing", Activity), "Bathing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("bathing", Activity), "Bathing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Bathe", Activity), "Bathing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("bathe", Activity), "Bathing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Bath", Activity), "Bathing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("bath", Activity), "Bathing", Activity1)) %>% 
    ##Treading water 
    mutate(Activity1 = ifelse(grepl("Treading water", Activity), "Treading Water", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("treading water", Activity), "Treading Water", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Treading", Activity), "Treading Water", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("treading", Activity), "Treading Water", Activity1)) %>% 
    ##Washing
    mutate(Activity1 = ifelse(grepl("Washing", Activity), "Washing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("washing", Activity), "Washing", Activity1)) %>% 
    ##Murder
    mutate(Activity1 = ifelse(grepl("Murder", Activity), "Murder", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("murder", Activity), "Murder", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Murdered", Activity), "Murder", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("murdered", Activity), "Murder", Activity1)) %>% 
    ##Splashing
    mutate(Activity1 = ifelse(grepl("Splashing", Activity), "Splashing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("splashing", Activity), "Splashing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Splash", Activity), "Splashing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("splash", Activity), "Splashing", Activity1)) %>% 
    ##Walking 
    mutate(Activity1 = ifelse(grepl("Walking", Activity), "Walking", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("walking", Activity), "Walking", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Walk", Activity), "Walking", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("walk", Activity), "Walking", Activity1)) %>% 
    ##Rowing
    mutate(Activity1 = ifelse(grepl("Rowing", Activity), "Rowing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("rowing", Activity), "Rowing", Activity1)) %>% 
    ##Floating
    mutate(Activity1 = ifelse(grepl("Floating", Activity), "Floating", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("floating", Activity), "Floating", Activity1)) %>% 
    ##Paddling
    mutate(Activity1 = ifelse(grepl("Paddling", Activity), "Paddling", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Sculling", Activity), "Paddling", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("paddling", Activity), "Paddling", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Paddle", Activity), "Paddling", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("paddle", Activity), "Paddling", Activity1)) %>% 
    ##Body Surfing
    mutate(Activity1 = ifelse(grepl("Body Surfing", Activity), "Body Surfing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Body surfing", Activity), "Body Surfing", Activity1)) %>% 
    #Snorkeling
    mutate(Activity1 = ifelse(grepl("Snorkeling", Activity), "Snorkeling", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("snorkeling", Activity), "Snorkeling", Activity1)) %>% 
    ##Kite Boarding
    mutate(Activity1 = ifelse(grepl("Kite Boarding", Activity), "Kite Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Kite boarding", Activity), "Kite Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Kite-boarding", Activity), "Kite Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Kiteboarding", Activity), "Kite Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Kite-Boarding", Activity), "Kite Boarding", Activity1)) %>%
    ##Canoeing
    mutate(Activity1 = ifelse(grepl("Canoeing", Activity), "Canoeing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("canoeing", Activity), "Canoeing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("canoe", Activity), "Canoeing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Canoe", Activity), "Canoeing", Activity1)) %>% 
    ##Kayaking
    mutate(Activity1 = ifelse(grepl("Kayaking", Activity), "Kayaking", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("kayaking", Activity), "Kayaking", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Kayak", Activity), "Kayaking", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("kayak", Activity), "Kayaking", Activity1)) %>% 
    ##Boogie Boarding
    mutate(Activity1 = ifelse(grepl("Boogie Boarding", Activity), "Boogie Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Boogie boarding", Activity), "Boogie Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("boogie boarding", Activity), "Boogie Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Body Boarding", Activity), "Boogie Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Body boarding", Activity), "Boogie Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Bodyboarding", Activity), "Boogie Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Body-boarding", Activity), "Boogie Boarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("body board", Activity), "Boogie Boarding", Activity1)) %>% 
    ##Paddleboarding
    mutate(Activity1 = ifelse(grepl("Paddleboarding", Activity), "Paddleboarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Paddle boarding", Activity), "Paddleboarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Paddle-boarding", Activity), "Paddleboarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Stand-Up Paddleboarding", Activity), "Paddleboarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("SUP", Activity), "Paddleboarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Paddle Boarding", Activity), "Paddleboarding", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Paddleboarding", Activity), "Paddleboarding", Activity1)) %>% 
    ##Jumping
    mutate(Activity1 = ifelse(grepl("Jumping", Activity), "Jumping", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("jumping", Activity), "Jumping", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Jumped", Activity), "Jumping", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("jumped", Activity), "Jumping", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Jump", Activity), "Jumping", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("jump", Activity), "Jumping", Activity1)) %>% 
    ##Sailing
    mutate(Activity1 = ifelse(grepl("Sailing", Activity), "Sailing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sailing", Activity), "Sailing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sailboat", Activity), "Sailing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Sailboat", Activity), "Sailing", Activity1)) %>% 
    ##Raft
    mutate(Activity1 = ifelse(grepl("Raft", Activity), "Raft", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("raft", Activity), "Raft", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("floatation device", Activity), "Raft", Activity1)) %>% 
    #Boating
    mutate(Activity1 = ifelse(grepl("Boating", Activity), "Boating", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("boating", Activity), "Boating", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Boat", Activity), "Boating", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("boat", Activity), "Boating", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Yacht", Activity), "Boating", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("yachtsman", Activity), "Boating", Activity1)) %>% 
    ##Diving
    mutate(Activity1 = ifelse(grepl("Diving", Activity), "Diving", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("diving", Activity), "Diving", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Dived", Activity), "Diving", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("dived", Activity), "Diving", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Dive", Activity), "Diving", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("dive", Activity), "Diving", Activity1)) %>% 
    ##Swimming
    mutate(Activity1 = ifelse(grepl("Swimming", Activity), "Swimming", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("swimming", Activity), "Swimming", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Swim", Activity), "Swimming", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("swim", Activity), "Swimming", Activity1)) %>% 
    ##Air or Sea Disaster 
    mutate(Activity1 = ifelse(grepl("Air Disaster", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Sea Disaster", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Air disaster", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Sea disaster", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("air disaster", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sea disaster", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Aircraft", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("aircraft", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Sank", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sank", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Explosion", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("explosion", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("burned", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("burning", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("wrecked", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sinking", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("exploded", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Adrift", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("crashed", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("plunged", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sunk", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("torpedoed", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("torpedoes", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("capsized", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("fire", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Sinking", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("crash", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("seaplane", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("foundered", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Foundering", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("collided", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("collision", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Wreck", Activity), "Air or Sea Disaster", Activity1)) %>%
    mutate(Activity1 = ifelse(grepl("wreck", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("accident", Activity), "Air or Sea Disaster", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("plane", Activity), "Air or Sea Disaster", Activity1)) %>% 
    ##Surfing
    mutate(Activity1 = ifelse(grepl("Surfing", Activity), "Surfing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("surfing", Activity), "Surfing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Surf", Activity), "Surfing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("surf", Activity), "Surfing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("surfboard", Activity), "Surfing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Surfboard", Activity), "Surfing", Activity1)) %>% 
    ##Fishing 
    mutate(Activity1 = ifelse(grepl("Fishing", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("fishing", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Fisherman", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Fish", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("fish", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Spearfishing", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("spearfishing", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sardines", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Clamming", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Crabbing", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("shrimp", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("lobsters", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("whale", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Hunting", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("hunting", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Lobstering", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Netting", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("netting", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Net", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("net", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("prawns", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Shrimping", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("dolphins", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("crocodile", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("turtle", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("crabs", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("stingrays?", Activity), "Fishing", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Oystering", Activity), "Fishing", Activity1)) %>% 
    ##Shark
    mutate(Activity1 = ifelse(grepl("Shark", Activity), "Shark", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("shark", Activity), "Shark", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("Sharks", Activity), "Shark", Activity1)) %>% 
    mutate(Activity1 = ifelse(grepl("sharks", Activity), "Shark", Activity1)) %>% 

  ##Type of attack boating and boat are the same 
  mutate(Type = gsub("Boating", "Boat", Type)) %>% 
  
  ##Renaming injury classification 
  replace_na(list(Injury = "No injury")) %>% 
  mutate(Injury1 = Injury) %>% 
  ##Injury 
  mutate(Injury1 = "Injury") %>% 
  ##No injury
  mutate(Injury1 = ifelse(grepl("No injury", Injury), "No injury", Injury1)) %>% 
  mutate(Injury1 = ifelse(grepl("no injury", Injury), "No injury", Injury1)) %>% 
  ##Fatal
  mutate(Injury1 = ifelse(grepl("FATAL", Injury), "Fatal", Injury1)) %>% 

  ##Cleaning Fatal variable 
  mutate(Fatal= `Fatal (Y/N)`) %>% 
  mutate(Fatal1 = Fatal) %>% 
  mutate(Fatal1= ifelse(grepl("n", Fatal), "N", Fatal1)) %>% 
  mutate(Fatal1= ifelse(grepl("F", Fatal), "Y", Fatal1)) %>% 
  
  ## Making one long location string 
  unite(location_string, Location, Area, Country, sep=" ", remove = FALSE) %>% 
  mutate(new_string = gsub("miles off", "", location_string)) %>% 
  mutate(new_string = gsub("k off", "", new_string)) %>% 
  mutate(new_string = gsub("Off", "", new_string)) %>% 
  mutate(new_string= gsub("[[:digit:]]", "", new_string)) %>% 
  mutate(new_string= gsub("[[:punct:]]", "", new_string))


## making a clean attacks data frame 
attacks_clean <- attacks %>% 
  select(id_internal, 
         Year, 
         Type, 
         Country, 
         Area, 
         Location, 
         location_string,
         new_string, 
         Activity,
         Activity1,
         Sex, 
         Age, 
         Injury,
         Injury1,
         Fatal1, 
         Species)


## Prelude to making a plot for attacks over the years 
attacks_year <- attacks_clean %>%
  select(Year) %>% 
  filter(Year>1500) %>% 
  na.omit(Year) %>% 
  group_by(Year) %>% 
  tally()

##Plot of attacks over the years 
ggplot(attacks_year, aes(x=Year, y=n)) +
  geom_line() +
  labs(title="Shark Attacks per Year, 1543-Present", x="Year", y= "Number of Attacks")

ggplot(attacks_year, aes(x=Year, y=n)) +
  geom_line() +
  labs(title="Shark Attacks per Year, 1800-Present", x="Year", y= "Number of Attacks") +
  annotate("point", x=1975, y=49, col="red", size=2) +
  annotate("point", x=1959, y=93, col="blue", size=2) +
  xlim(1800, 2017)


## plot of distribution of shark attack type 
attacks_type <- attacks_clean %>%
  group_by(Type) %>% 
  tally()

ggplot(attacks_type, aes(x=Type, y=n)) +
  geom_bar(stat="identity") +
  labs(title="Distribution of Shark Attack Type", x="Attack Type", y="Number of Attacks")

## top 15 countrys of attacks plot 
attacks_country <- attacks_clean %>%
  group_by(Country) %>% 
  tally() %>% 
  na.omit(Country) %>% 
  filter(n > 40) %>% 
  arrange(desc(n))

ggplot(attacks_country, aes(x=reorder(Country, n), y=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Distribution of Shark Attack Location-Top 15", x="Attack Country", y="Number of Attacks")


##fatal attacks over time
attacks_time_fatal <- attacks_clean %>% 
  select(Year, Fatal1) %>% 
  rename(Fatal=Fatal1) %>% 
  filter(Fatal == "Y" | Fatal == "N") %>% 
  group_by(Year, Fatal) %>% 
  na.omit(Fatal) %>% 
  tally()

ggplot(attacks_time_fatal, aes(x=Year, y=n, col=Fatal)) +
  geom_point() +
  xlim(1800, 2017) +
  labs(title="Fatal vs Non-Fatal Attacks over Time", x="Year", y="Number of Attacks")


##Dealing with locations of attacks
attacks_location <- attacks %>% 
  select(Country, Area, Location) %>% 
  unique() %>% 
  na.omit(Country) %>% 
  na.omit(Area) %>% 
  na.omit(Location) %>% 
  unite(location_string, Location, Area, Country, sep=" ", remove = TRUE) 

## getting rid of all the locatiosn with "bad characters 
index_location_good <- Encoding(attacks_location$location_string) == "unknown"
# now we've deleted all rows where the character string was bad
attacks_location <- attacks_location[index_location_good, ]

attacks_location <- attacks_location %>%
  mutate(new_string = gsub("miles off", "", location_string)) %>% 
  mutate(new_string = gsub("k off", "", new_string)) %>% 
  mutate(new_string = gsub("Off", "", new_string)) %>% 
  mutate(new_string= gsub("[[:digit:]]", "", new_string)) %>% 
  mutate(new_string= gsub("[[:punct:]]", "", new_string))

## Geocode Function
## gc <- do.call(rbind, lapply(as.character(attacks_location$location_string), geocode, override_limit = TRUE))
## save(gc, file="location.Rdata")
load("location.Rdata")
attacks_location <- attacks_location %>% mutate(lon=gc$lon, lat=gc$lat)

## Omitting all the locations with no lat or long coords 
attacks_location_na <- attacks_location %>% 
  na.omit(lat)

## Joining so attacks_locations_na will get all the observations 
attacks_location_na <- attacks_location_na %>% 
  left_join(attacks_clean, attacks_location_na, by="new_string")

attacks_location_na <- attacks_location_na %>% 
  select(id_internal,
         Year,
         new_string, 
         lon,
         lat,
         Country,
         Area,
         Location,
         Type,
         Activity1,
         Injury1,
         Fatal1
         ) %>% 

##Renaming/generalizing the categorical buckets for the Activity variable 
  mutate(Activity2 = Activity1) %>% 
##Board Involved 
  ##Body Surfing
  mutate(Activity2 = ifelse(grepl("Body Surfing", Activity1), "Board involved", Activity2)) %>% 
  ##Surfing
  mutate(Activity2 = ifelse(grepl("Surfing", Activity1), "Board involved", Activity2)) %>% 
  ##Boogie Boarding
  mutate(Activity2 = ifelse(grepl("Boogie Boarding", Activity1), "Board involved", Activity2)) %>% 
  ##Kite Boarding
  mutate(Activity2 = ifelse(grepl("Kite Boarding", Activity1), "Board involved", Activity2)) %>% 
  ##Kayaking
  mutate(Activity2 = ifelse(grepl("Kayaking", Activity1), "Board involved", Activity2)) %>% 
  ##Paddleboarding
  mutate(Activity2 = ifelse(grepl("Paddleboarding", Activity1), "Board involved", Activity2)) %>% 
  ##Canoeing
  mutate(Activity2 = ifelse(grepl("Canoeing", Activity1), "Board involved", Activity2)) %>% 
  ###Paddling
  mutate(Activity2 = ifelse(grepl("Paddling", Activity1), "Board involved", Activity2)) %>% 
  
## Fishing Involved 
  mutate(Activity2 = ifelse(grepl("Spearfishing", Activity1), "Fishing involved", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Fishing", Activity1), "Fishing involved", Activity2)) %>% 
  
## Swimming/Diving 
  mutate(Activity2 = ifelse(grepl("Swimming", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Diving", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Snorkeling", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Bathing", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Treading water", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Wading", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Playing", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Splashing", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Fell", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Jumping", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Standing", Activity1), "Swimming/Diving", Activity2)) %>% 
  mutate(Activity2 = ifelse(grepl("Floating", Activity1), "Swimming/Diving", Activity2)) %>% 

## Making an "Other" categorical Activity variable   
  mutate(Activity3 = ifelse(Activity2 == "Board involved", "Board involved", 
                            ifelse(Activity2 == "Fishing involved", "Fishing involved", 
                                   ifelse(Activity2 == "Swimming/Diving", "Swimming/Diving",
                                          ifelse(Activity2 == "Air or Sea Disaster", "Air or Sea Disaster",
                                                 ifelse(Activity2 == "Shark", "Shark", "Other")))))) %>% 
  mutate(Activity3 = ifelse(is.na(Activity3), "Other", Activity3))


## Leaflets with vary emphasis on different activities 
## so they can use the same palette 
    ## 1. emphasis on fishing involved 
pal <- colorFactor(rainbow(6), attacks_location_na$Activity3)

leaflet(attacks_location_na) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Activity3 == "Fishing involved", 6, 4),
                   color = ~pal(Activity3),
                   stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("topleft",
            pal=pal, 
            values = ~Activity3, 
            title = "Activity prior to Attack")


    ## 2.emphasis on board involved 
leaflet(attacks_location_na) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Activity3 == "Board involved", 6, 4),
                   color = ~pal(Activity3),
                   stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("topleft",
            pal=pal, 
            values = ~Activity3, 
            title = "Activity prior to Attack")
    
    ## 3. emphasis on Air or Sea Disaster 
leaflet(attacks_location_na) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Activity3 == "Air or Sea Disaster", 6, 4),
                   color = ~pal(Activity3),
                   stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("topleft",
            pal=pal, 
            values = ~Activity3, 
            title = "Activity prior to Attack")

  ## 4. emphasis on Swimming/Diving
leaflet(attacks_location_na) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Activity3 == "Swimming/Diving", 6, 4),
                   color = ~pal(Activity3),
                   stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("topleft",
            pal=pal, 
            values = ~Activity3, 
            title = "Activity prior to Attack")


##Leaflet of attacks, cluster circles with numbers 
leaflet(attacks_location_na) %>% 
  addTiles() %>% 
  addMarkers(
    clusterOptions = markerClusterOptions()
  )
  

##Leaflet of Fatal attacks
pal2 <- colorFactor(c("navy", "red"), domain= c("Y", "N"))

attacks_location_na_fatal <- attacks_location_na %>% 
  na.omit(Fatal1) %>% 
  filter(Fatal1 == "Y" | Fatal1 == "N")

leaflet(attacks_location_na_fatal) %>% 
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
    radius = ~ifelse(Fatal1 == "Y", 6, 6),
    color = ~pal2(Fatal1),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("topleft",
            pal=pal2, 
            value= ~Fatal1,
            title = "Fatal Attack? Yes or No")

##Leaflet of Surfing attacks 
pal3 <- colorFactor(heat.colors(2), domain = c("Surfing"))

leaflet(attacks_location_na) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat,
    radius = ~ifelse(Activity1 == "Surfing", 6, 6),
    color = ~pal3(Activity1),
    stroke = FALSE, fillOpacity = 0.5
  )

##Leaflet of Fishing attacks 
pal4 <- colorFactor(diverge_hcl(2), domain = c("Fishing"))

leaflet(attacks_location_na) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Activity1 == "Fishing", 6, 6),
                   color = ~pal4(Activity1),
                   stroke = FALSE, fillOpacity = 0.5
  )

##Leaflet of Provoked attacks 
pal5 <- colorFactor(topo.colors(2), domain = c("Provoked"))

leaflet(attacks_location_na) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Type == "Provoked", 6, 6),
                   color = ~pal5(Type),
                   stroke = FALSE, fillOpacity = 0.5
  )


##Leaflet of No Injury attacks 
pal6 <- colorFactor(diverge_hsv(2), domain = c("No injury"))

leaflet(attacks_location_na) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Injury1 == "No injury", 6, 6),
                   color = ~pal6(Injury1),
                   stroke = FALSE, fillOpacity = 0.5
  )

## Leaflets with different injury levels  

pal8 <- colorFactor(topo.colors(3), attacks_location_na$Injury1)

leaflet(attacks_location_na) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = ~ifelse(Injury1 == "Fatal", 5, 5),
                   color = ~pal8(Injury1),
                   stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("topleft",
            pal=pal8, 
            values = ~Injury1, 
            title = "Injury level from Attack")

##Leaflet of attacks over the years 
attacks_location_na_year <- attacks_location_na %>% 
  na.omit(Year) %>% 
  filter(Year>1950) %>% 
  mutate(Year=as.Date(year))

pal7 <- colorNumeric( palette = "Reds",
                      domain = attacks_location_na_year$Year
  )
  
leaflet(attacks_location_na_year) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   radius = 5,
                   color = ~pal7(Year),
                   stroke = FALSE, fillOpacity = 0.5, 
                   popup = ~as.character(Year)
                   ) %>% 
  addLegend("topleft",
            pal=pal7,
            value= ~Year,
            title = "Year of Attack", 
            labFormat = labelFormat(big.mark = ""))


##starting to build the model 
  
attacks_model <- attacks %>% 
  filter(grepl('Y|N', Fatal1)) %>% 
  mutate(is_fatal=ifelse(Fatal1=='Y', 1, 0)) %>% 
  mutate(Activity2 = Activity1) %>% 
## Board Involved 
    ##Body Surfing
    mutate(Activity2 = ifelse(grepl("Body Surfing", Activity1), "Board involved", Activity2)) %>% 
    ##Surfing
    mutate(Activity2 = ifelse(grepl("Surfing", Activity1), "Board involved", Activity2)) %>% 
    ##Boogie Boarding
    mutate(Activity2 = ifelse(grepl("Boogie Boarding", Activity1), "Board involved", Activity2)) %>% 
    ##Kite Boarding
    mutate(Activity2 = ifelse(grepl("Kite Boarding", Activity1), "Board involved", Activity2)) %>% 
    ##Kayaking
    mutate(Activity2 = ifelse(grepl("Kayaking", Activity1), "Board involved", Activity2)) %>% 
    ##Paddleboarding
    mutate(Activity2 = ifelse(grepl("Paddleboarding", Activity1), "Board involved", Activity2)) %>% 
    ##Canoeing
    mutate(Activity2 = ifelse(grepl("Canoeing", Activity1), "Board involved", Activity2)) %>% 
    ###Paddling
    mutate(Activity2 = ifelse(grepl("Paddling", Activity1), "Board involved", Activity2)) %>% 
## Fishing Involved 
    mutate(Activity2 = ifelse(grepl("Spearfishing", Activity1), "Fishing involved", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Fishing", Activity1), "Fishing involved", Activity2)) %>% 
## Swimming/Diving 
    mutate(Activity2 = ifelse(grepl("Swimming", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Diving", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Snorkeling", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Bathing", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Treading water", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Wading", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Playing", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Splashing", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Fell", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Jumping", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Standing", Activity1), "Swimming/Diving", Activity2)) %>% 
    mutate(Activity2 = ifelse(grepl("Floating", Activity1), "Swimming/Diving", Activity2)) 
  
  
attacks_model <- attacks_model %>% 
  mutate(Activity3 = ifelse(Activity2 == "Board involved", "Board involved", 
                  ifelse(Activity2 == "Fishing involved", "Fishing involved", 
                  ifelse(Activity2 == "Swimming/Diving", "Swimming/Diving",
                  ifelse(Activity2 == "Air or Sea Disaster", "Air or Sea Disaster",
                  ifelse(Activity2 == "Shark", "Shark", "Other"))))))

attacks_model <- attacks_model %>% 
  mutate(Activity3 = ifelse(is.na(Activity3), "Other", Activity3))

attacks_model <- attacks_model %>% 
  mutate(Country = ifelse(Country == "USA", "USA", 
                          ifelse(Country == "AUSTRALIA", "AUSTRALIA", 
                                 ifelse(Country == "SOUTH AFRICA", "SOUTH AFRICA", "Other")))) %>% 
  mutate(Country = ifelse(is.na(Country), "Other", Country))

##check predictive power of each predictor variable
 ##Country 
attacks_model_country <- attacks_model %>% 
  select(Country, Fatal1) %>% 
  filter(Fatal1=="Y" | Fatal1=="N") %>% 
  rename(Fatal=Fatal1) %>% 
  group_by(Country, Fatal) %>% 
  tally() %>% 
  group_by(Country) %>% 
  mutate(prop=n/sum(n))

ggplot(attacks_model_country, aes(x=Country, y=prop, fill=Fatal)) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept=.264) +
  labs(title = "Proportion of Fatal Attacks by Country", x="Country", y="Proportion of Attacks") +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25 )

  ##Activity3 
attacks_model_activity <- attacks_model %>% 
  select(Activity3, Fatal1) %>% 
  filter(Fatal1=="Y" | Fatal1=="N") %>% 
  rename(Fatal=Fatal1) %>% 
  group_by(Activity3, Fatal) %>% 
  tally() %>% 
  group_by(Activity3) %>% 
  mutate(prop=n/sum(n))

ggplot(attacks_model_activity, aes(x=Activity3, y=prop, fill=Fatal)) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept=.264) +
  labs(title = "Proportion of Fatal Attacks by Activity", x="Activity", y="Proportion of Attacks") +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25 )

  ##Type 
attacks_model_type <- attacks_model %>% 
  select(Type, Fatal1) %>% 
  filter(Fatal1=="Y" | Fatal1=="N") %>% 
  rename(Fatal=Fatal1) %>% 
  group_by(Type, Fatal) %>% 
  tally() %>% 
  group_by(Type) %>% 
  mutate(prop=n/sum(n))

ggplot(attacks_model_type, aes(x=Type, y=prop, fill=Fatal)) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept=.264) +
  labs(title = "Proportion of Fatal Attacks by Type", x="Type", y="Proportion of Attacks") +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25 )



## actually making the model 

    ##training the model 

set.seed(9)
training <- sample_n(attacks_model, 598)
test <- attacks_model %>% 
  filter(!(id_internal%in% training$id_internal))

predict_fatal_model <- glm(is_fatal ~Activity3 + Country + Type, data = training, family = "binomial")
broom::tidy(predict_fatal_model)

prediction_training <- predict(predict_fatal_model, newdata=training, type="response")
  
training <- training %>% 
  mutate(phat=predict(predict_fatal_model, newdata=training, type="response")) %>% 
  select(Activity3, Country, Type, phat, is_fatal, Fatal1) %>% 
  mutate(predicted_correct=
           ifelse((phat>=.2642 & is_fatal==1) | (phat<.2642 & is_fatal==0), 1, 0))

training_correct <- training %>% 
  group_by(predicted_correct) %>% 
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) 
knitr::kable(training_correct)

    ##testing the model 
prediction_test <- predict(predict_fatal_model, newdata=test, type="response")

test <- test %>% 
  mutate(phat=predict(predict_fatal_model, newdata=test, type="response")) %>% 
  select(Activity3, Country, Type, phat, is_fatal, Fatal1) %>% 
  mutate(predicted_correct=
           ifelse((phat>=.2642 & is_fatal==1) | (phat<.2642 & is_fatal==0), 1, 0))

test_correct <- test %>% 
  group_by(predicted_correct) %>% 
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) 
knitr::kable(test_correct)  
 



## EXTRA code that I may not need, working with other variables 
attacks_sex <- attacks_clean %>% 
  filter(Sex == "F" | Sex == "M" ) %>% 
  group_by(Sex) %>% 
  tally()

attacks_location_na_count <- attacks_location_na %>% 
  group_by(Type) %>% 
  tally()

attacks_type <- attacks_clean %>%
  group_by(Type) %>% 
  tally() 

attacks_sex <- attacks_clean %>% 
  group_by(Sex) %>% 
  tally()

attacks_age <- attacks_clean %>% 
  group_by(Age) %>% 
  tally()

attacks_time <- attacks_clean %>% 
  group_by(Time) %>% 
  tally()

attacks_species <- attacks_clean %>% 
  mutate(Species1 = Species) %>% 
  mutate(Species1 = ifelse(grepl("White", Species), "White Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("white", Species), "White Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Tiger", Species), "Tiger Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("tiger", Species), "Tiger Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Bull", Species), "Bull Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("bull", Species), "Bull Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Blacktip", Species), "Blacktip Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("blacktip", Species), "Blacktip Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Bronze", Species), "Bronze Whaler Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("bronze", Species), "Bronze Whaler Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Nurse", Species), "Nurse Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("nurse", Species), "Nurse Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Dusky", Species), "Dusky Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("dusky", Species), "Dusky Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Grey reef", Species), "Grey Reef Shark", Species1)) %>% 
  mutate(Species1 = ifelse(grepl("Hammerhead", Species), "Hammerhead Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("hammerhead", Species), "Hammerhead Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Lemon", Species), "Lemon Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("lemon", Species), "Lemon Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Mako", Species), "Mako Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("mako", Species), "Mako Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Oceanic whitetip", Species), "Oceanic Whitetip Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Raggedtooth", Species), "Raggedtooth Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Wobbegong", Species), "Wobbegong Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Whaler", Species), "Whaler Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("whaler", Species), "Whaler Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Zambesi", Species), "Zambesi Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Unconfirmed", Species), "Shark involvement not confirmed", Species1)) %>%
  mutate(Species1 = ifelse(grepl("uncomfirmed", Species), "Shark involvement not confirmed", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Shark involvement prior to death", Species), "Shark involvement not confirmed", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Sevengill", Species), "Sevengill Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Sandbar", Species), "Sandbar Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("sandbar", Species), "Sandbar Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Sand", Species), "Sandbar Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("sand", Species), "Sandbar Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Porbeagle", Species), "Porbeagle Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("porbeagle", Species), "Porbeagle Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Caribbean reef", Species), "Caribbean Reef Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("Blue", Species), "Blue Shark", Species1)) %>%
  mutate(Species1 = ifelse(grepl("blue", Species), "Blue Shark", Species1)) %>%
  group_by(Species1) %>% 
  tally()








