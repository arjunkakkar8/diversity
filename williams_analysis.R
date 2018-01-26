library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wru)

# Get data from WSO for all class years
building_rosters <- read.csv("~/Desktop/Diversity_Project/building_rosters.csv", header=TRUE, stringsAsFactors = FALSE)
williams_data <- read.delim("~/Desktop/Diversity_Project/williams_data", header=FALSE, stringsAsFactors = FALSE)
colnames(williams_data) <- c("name","unix","room")
# Remove unwanted search results
williams_data <- williams_data[grepl("[[:digit:]]",williams_data$name),]
# Create year, bulding and roomnum columns
williams_data <- mutate(williams_data, year = as.factor(str_extract(williams_data$name, "[0-9]{2}.*[0-9]*")),
                        building = as.factor(str_extract(williams_data$room,"[[:alpha:]]{1,}")),
                        roomnum = as.factor(str_extract(williams_data$room,"[[:alpha:]]*[[:digit:]]{1,}[[:alpha:]]*")),
                        firstname = as.factor(str_extract(williams_data$name, "\\w*")),
                        lastname = as.factor(str_extract(williams_data$name, "(?:\\S+\\s+){1}\\S+(?=\\s*$)")))
williams_data <- mutate(williams_data, lastname = as.factor(str_extract(williams_data$lastname, "\\w*")))

building_rosters <- mutate(building_rosters, firstname = as.factor(str_extract(building_rosters$Name, "\\w*")),
                           surname = as.factor(str_extract(building_rosters$Name, "\\S+(?=\\s*$)")))

year <- numeric(nrow(building_rosters))
for(i in 1:nrow(building_rosters)){
  pattern = paste(".*(",building_rosters[i, 8],").*(",building_rosters[i, 9],").*", sep = "")
  match = grep(pattern, williams_data$name)
  if (length(match == 1)){
    year[i] = as.numeric(as.character(williams_data[match, 4]))
  } else {
    year[i] = NA
  }
}
building_rosters$year <- as.factor(year)

building_rosters <- merge_surnames(building_rosters, impute.missing = FALSE)

# Need to add missing data from the housing directory online.
# Just copy paste the entire directory into one file and then do a matching of dataframes

# Can do similar stuff for majors

# Merge ethinicity data
#names_census <- read.csv("~/Desktop/Diversity_Project/Names_2010Census_reduced.csv")
#matches <- match(tolower(building_rosters$lastname), tolower(names_census$name))
#building_rosters <- cbind(building_rosters, names_census[matches,6:11])
building_rosters$predrace <- as.factor(colnames(building_rosters)[11+max.col(building_rosters[,12:16], "random")])

building_rosters <- mutate(building_rosters, predrace = ifelse(building_rosters$predrace == "p_bla", "p_whi", as.character(building_rosters$predrace)))
building_rosters <- mutate(building_rosters, predrace = ifelse(building_rosters$predrace == "p_oth", "p_whi", as.character(building_rosters$predrace)))
black_students <- mutate(black_students, lastname = as.factor(str_extract(black_students$V1, "\\S+(?=\\s*$)")))
index<-match(tolower(black_students$lastname), tolower(building_rosters$surname))
building_rosters$predrace[index] = "p_bla"
# Note that we have set (S) values to 0. Check this assumption in the future.

# For now omit the NA values. Add extra data to reduce the NA values in future
building_rosters <- select(building_rosters, -surname.match, -p_whi, -p_bla, -p_his, -p_asi, -p_oth, -Check.in.Date, -Check.out.Date) %>%
  na.omit()


# Import sports dataset
sports <- read.csv("~/Desktop/Diversity_Project/sports.csv", header=TRUE, stringsAsFactors = FALSE)

building_rosters <- left_join(building_rosters, sports, by = "Name")
building_rosters$Sport[is.na(building_rosters$Sport)] = "Non-Athlete"

# Create summary of race by building to calculate the amount of a certain kind of people
# Compare this to the null hypothesis by using the actual proportion of students from different races.
race_by_building <- building_rosters %>% group_by(Building)%>%count(predrace)%>%spread(predrace, n)
plot(race_by_building[1:38,])

plot_data <- building_rosters %>% group_by(Building)%>%count(predrace) 
grouped_data <- group_by(plot_data, Building) %>% summarise(total = sum(n))
plot_data <- left_join(plot_data, summarized, by = "Building") %>%
  mutate(freq = n/total)

ggplot(data = plot_data[!is.na(plot_data$Building),], aes(x=Building, group=predrace, fill =predrace))+
  geom_bar(aes(weight=n)) +
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_data <- building_rosters %>% group_by(Building)%>%count(Sport)
grouped_data <- group_by(plot_data, Building) %>% summarise(total = sum(n))
plot_data <- left_join(plot_data, summarized, by = "Building") %>%
  mutate(freq = n/total)

ggplot(data = plot_data[!is.na(plot_data$Building),], aes(x=Building, group=Sport, fill =Sport))+
  geom_bar(aes(weight=freq)) +
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_data <- williams_data %>% group_by(building)%>%count(year, sort = TRUE)
ggplot(data = plot_data[!is.na(plot_data$building),], aes(x=building, group=year, fill =year))+
  geom_bar(aes(weight=n)) +
  theme_bw()
# Do some form of data validation (maybe by hand) on a random subset of the dataframe
test <- filter(building_rosters, Room.Type == "Double Room") %>%
  filter(year == 21) %>%
  mutate(roomnum = as.factor(str_extract(Room, "[[:alpha:]]{1,}\\s\\w+")),
         predrace = ifelse(predrace == "p_whi", "white", "minority"))

doubles <- filter(building_rosters, Room.Type == "Double Room") %>%
  filter(year == 21) %>%
  mutate(roomnum = as.factor(str_extract(Room, "[[:alpha:]]{1,}\\s\\w+")),
         predrace = ifelse(predrace == "p_whi", "white", "minority")) %>%
  group_by(roomnum) %>%
  summarize(pair = paste(predrace, collapse = ","))%>%
  filter(str_length(pair)>8) %>%
  mutate(pair = as.factor(pair))




