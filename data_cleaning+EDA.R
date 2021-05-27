source("library.R")

##### Read Data #####

df <- read_csv("data/mass_shooting_events_stanford_msa_release_06142016.csv")



#### Preliminary EDA ####

# 1) Structure of the dataset

# 0) check the dimension of the input dataset and the time of variables
# we have a lot of Continuous variables
plot_str(df)

#1) Introduce the data
# 299 complete observations if looking across all rows
# 30 columns
t(introduce(df))


# 2) Missing data


# No need to delete a column
plot_missing(df, missing_only=TRUE)



# 3) Description of the data
str(df)
describe(df)


##### Data cleaning #####


### rename columns ###

# replace space by '_' in the column names
names(df) <- gsub("\\s+", "_", names(df))
# replace '_-_' or '-' by '_' in the column names
names(df) <- gsub("_-_|-", "_", names(df))
# replace '/s' space by 's' in the column names
names(df) <- gsub("/s", "s", names(df))
# remove 'General'in the column names
names(df) <- gsub("_General", "", names(df))
#Rename some columns
df <- df %>% 
  rename(Shooter_Age = `Shooter_Age(s)`,
         Shooters_Cause_of_Death = `Shooter's_Cause_of_Death`)

# remove lines with only NA's
df[is.na(df$CaseID) == TRUE, ]  # last 40 rows are NAs only
df <- df[is.na(df$CaseID) == FALSE, ] # just keeping non NAs rows
summary(df) # there are still some other variables which have NAs


### transform some categorical variables into factors ###

## Sex ##
# levels(as.factor(df$Shooter_Sex)) # "Female"      "Male"        "Male/Female" "Unknown"
# Factorize Shooter_Sex
# 0: Male
# 1: Female
# 2: Unknown
df$Shooter_Sex <- gsub("Male/Female|Unknown", "2", df$Shooter_Sex)
df$Shooter_Sex <- gsub("Female", "1", df$Shooter_Sex)
df$Shooter_Sex <- gsub("Male", "0", df$Shooter_Sex)

## Race ##
# levels(as.factor(df$Shooter_Race))
# Factorize Shooter_Race
# 0: "White American or European American" or "White American or European American/Some other Race"
# 1: "Black American or African American" or "Black American or African American/Unknown"
# 2: "Asian American" or "Asian American/Some other race"
# 3: "Native American or Alaska Native"
# 4: "Some other race"  or "Two or more races" or "Unknown"
df$Shooter_Race <- gsub("White American or European American|White American or European American/Some other Race", "0", df$Shooter_Race)
df$Shooter_Race <- gsub("Black American or African American|Black American or African American/Unknown", "1", df$Shooter_Race)
df$Shooter_Race <- gsub("Asian American|Asian American/Some other race", "2", df$Shooter_Race)
df$Shooter_Race <- gsub("Native American or Alaska Native", "3", df$Shooter_Race)
df$Shooter_Race <- gsub("Some other race|Two or more races|Unknown", "4", df$Shooter_Race)


## Type of gun ##
# levels(as.factor(df$Type_of_Gun))  # "handgun"       "Handgun"       "Multiple guns" "Multiple Guns" "Rifle"  "Shotgun"  "Unknown" 
# Factorize Type_of_Gun
# 0: Shotgun
# 1: Rifle
# 2: Handgun
# 3: Multiple Guns
# 4: Unknown
df$Type_of_Gun <- gsub("Unknown", "4", df$Type_of_Gun)
df$Type_of_Gun <- gsub("Multiple guns|Multiple Guns", "3", df$Type_of_Gun)
df$Type_of_Gun <- gsub("handgun|Handgun", "2", df$Type_of_Gun)
df$Type_of_Gun <- gsub("Rifle", "1", df$Type_of_Gun)
df$Type_of_Gun <- gsub("Shotgun", "0", df$Type_of_Gun)


## Fate of the Shooter ##
#levels(as.factor(df$Fate_of_Shooter_at_the_scene)) # "Arrested"  "Arrested/Escaped" "Deceased" "Escaped" "Escaped\r(Arrested)" "Killed"
# Factorize Fate_of_Shooter_at_the_scene
# 0: Arrested or Arrested/Escaped or Escaped\r(Arrested) Escaped
# 1: Deceased or Killed
# 2: Escaped
df$Fate_of_Shooter_at_the_scene[df$Fate_of_Shooter_at_the_scene=="Escaped\r(Arrested)"] <- "Arrested"
df$Fate_of_Shooter_at_the_scene <- gsub("Arrested|Arrested/Escaped", "0", df$Fate_of_Shooter_at_the_scene)
df$Fate_of_Shooter_at_the_scene <- gsub("Deceased|Killed", "1", df$Fate_of_Shooter_at_the_scene)
df$Fate_of_Shooter_at_the_scene <- gsub("Escaped", "2", df$Fate_of_Shooter_at_the_scene)

## Cause of death of the shooter ##
#levels(as.factor(df$Shooters_Cause_of_Death))   "Suicide or Killed/Suicide" "Killed" "Not applicable" "Not Applicable"
# Factorize Military_Experience
# 0: Suicide or Killed/Suicide
# 1: Killed
# 2: "Not applicable"or "Not Applicable"
df$Shooters_Cause_of_Death <- gsub("Suicide|Killed/Suicide", "0", df$Shooters_Cause_of_Death)
df$Shooters_Cause_of_Death <- gsub("Killed", "1", df$Shooters_Cause_of_Death)
df$Shooters_Cause_of_Death <- gsub("Not applicable|Not Applicable", "2", df$Shooters_Cause_of_Death)

## Shcool related ? ##
# levels(as.factor(df$School_Related))  # "Killed"  "no"      "No"      "Unknown" "Yes"
# Factorize School_Related
# 0: No
# 1: Yes
# 2: Unknown
df$School_Related <- gsub("Killed|Unknown", "2", df$School_Related)
df$School_Related <- gsub("Yes", "1", df$School_Related)
df$School_Related <- gsub("no|No", "0", df$School_Related)

## History of mental illness? ##
# levels(as.factor(df$History_of_Mental_Illness))  # "No" "Unknown" "Yes" 
# Factorize History_of_Mental_Illness
# 0: No
# 1: Yes
# 2: Unknown
df$History_of_Mental_Illness <- gsub("Killed|Unknown", "2", df$History_of_Mental_Illness)
df$History_of_Mental_Illness <- gsub("Yes", "1", df$History_of_Mental_Illness)
df$History_of_Mental_Illness <- gsub("No", "0", df$History_of_Mental_Illness)

## Military experience? ##
# levels(as.factor(df$Military_Experience))  # "No" "Unknown" "Yes" "Killed"
# Factorize Military_Experience
# 0: No
# 1: Yes
# 2: Unknown
df$Military_Experience <- gsub("Killed|Unknown", "2", df$Military_Experience)
df$Military_Experience <- gsub("Yes", "1", df$Military_Experience)
df$Military_Experience <- gsub("No", "0", df$Military_Experience)


## convert the previous columns into integer ##
int_cols = c("Average_Shooter_Age",
             "Number_of_Shotguns",
             "Number_of_Rifles", 
             "Number_of_Handguns", 
             "Total_Number_of_Guns", 
             "Number_of_Automatic_Guns", 
             "Number_of_Semi_Automatic_Guns",
             "Shooters_Cause_of_Death",
             "School_Related",
             "History_of_Mental_Illness",
             "Military_Experience",
             "Shooter_Sex",
             "Shooter_Race",
             "Type_of_Gun",
             "Fate_of_Shooter_at_the_scene"
)

# convert the types
df = df %>% 
  mutate(across(all_of(int_cols), as.integer))



### Clean categrical data with strings ###

## Place Type ##
df$Place_Type <- gsub("Retail/ Wholesale/Services facility|Retail/ Wholesale/Services facility\rand Primary school|Retail/Wholesale/Services facility|Retail/Wholesale/Services facility\r/Residential home/Neighborhood", "Retail/Wholesale/Services facility", df$Place_Type)
df$Place_Type <- gsub("Residential home/Neighborhood|Residential Home/Neighborhood|Residential home/Neighborhood \rand Street/Highway|Residential home/Neighborhood,\rRetail/Wholesale/Services facility", "Residential area", 
                      df$Place_Type)
df$Place_Type <- gsub("Entertainment venue|Entertainment Venue", "Entertainment Venue", 
                      df$Place_Type)
df$Place_Type <- gsub("Park/Wilderness|Park/Wildness", "Park", 
                      df$Place_Type)
df$Place_Type <- gsub("Restaurant/Cafe?|Restaurant/cafe|Restaurant/Cafeé", "Restaurant/Cafe", 
                      df$Place_Type)
df$Place_Type[df$Place_Type=="Restaurant/Cafe?"] <- "Restaurant/Cafe"
df$Place_Type <- gsub("Secondary school|Secondary School", "Secondary School", 
                      df$Place_Type)

# Then reduce the number of categories of Place Type #
df$Place_Type <- gsub("College/University/Adult education|Primary school|Secondary School", "School", 
                      df$Place_Type)
df$Place_Type <- gsub("Street/Highway|Park|Public transportation", "Public places", 
                      df$Place_Type)

df$Place_Type <- gsub("Retail/Wholesale/Services facility|Restaurant/Cafe|Entertainment Venue", "Services facility", 
                      df$Place_Type)

df$Place_Type <- gsub("Military facility|Government facility", "Government facility", 
                      df$Place_Type)


## Relationship to Incident Location ##
df$Relationship_to_Incident_Location <- gsub("Unknown|None", "NA", 
                                             df$Relationship_to_Incident_Location)
df$Relationship_to_Incident_Location <- gsub("Place of business/employment|Place of Business/employment", "Employment", 
                                             df$Relationship_to_Incident_Location)
df$Relationship_to_Incident_Location <- gsub("Place of Residency|Residential home/Neighborhood|Place of residency", "Place of Residency", 
                                             df$Relationship_to_Incident_Location)
df$Relationship_to_Incident_Location[df$Relationship_to_Incident_Location =="Employment\rPlace of Residency" ] <- "Employment"


## Targeted Victimcs ##
df$Targeted_Victims <- gsub("None|Unknown", "NA", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("Family/Government|Family, Government|Family/General public|Family/Social|Family", "Family", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("Racial/Religious group\rand Social|Racial/Religious group", "Racial/Religious group", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("Former domestic partner and her family|Romantic partner, and her family|Romantic partner and Family|Romantic Partner|Romantic partner", "Romantic Partner", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("Students/Classmates/Teacher\rand Government|Students/Classmates/Teacher", "Students/Classmates/Teacher", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("Colleague/Workmate/Business acquaintance\rand Family|Colleague/Workmate/Business acquaintance", "Colleague/Workmate/Business acquaintance", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("Social/General public", "Social", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("General public\rand Government", "Government", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("General public", "General Public", 
                            df$Targeted_Victims)

# Then reduce the number of categories of Place Type #
df$Targeted_Victims <- gsub("College/University/Adult education|Primary school|Secondary School", "School", 
                            df$Targeted_Victims)
df$Targeted_Victims <- gsub("Street/Highway|Park|Public transportation", "Public places", 
                            df$Place_Type)

df$Targeted_Victims <- gsub("Retail/Wholesale/Services facility|Restaurant/Cafe|Entertainment Venue", "Services facility", 
                            df$Targeted_Victims)

df$Targeted_Victims <- gsub("Military facility|Government facility", "Government facility", 
                            df$Targeted_Victims)


## Possible Motive ##
df$Possible_Motive <- gsub("Drug use/Financial difficulties|Drug use", "Drug use", 
                           df$Possible_Motive)
df$Possible_Motive <- gsub("NA|Unknown|Multiple motives", "NA", 
                           df$Possible_Motive)
df$Possible_Motive <- gsub("Domestic dispute|Domestic Dispute|Legal dispute|Social dispute|Social Dispute|Terminated/Denied/Reprimanded", "Dispute", 
                           df$Possible_Motive)
df$Possible_Motive <- gsub("Financial dispute|Financial difficulties|Robbery", "Money", 
                           df$Possible_Motive)
df$Possible_Motive <- gsub("Expulsion/Suspension|Rejection|Failure|Harassment", "Rejection", 
                           df$Possible_Motive)
df$Possible_Motive <- gsub("Political/Religious ideals|Race|Gender", "Political/Discrimination", 
                           df$Possible_Motive)
df$Possible_Motive <- gsub("Mental illness|Drug use", "Mental illness/Drug", 
                           df$Possible_Motive)




### Change Date from character to type Date and add 4 digits to each year ###
library(lubridate)
x <- mdy(df$Date)
foo <- function(x, year=1965){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

df$Date <- foo(mdy(df$Date))

## add the year as a separate column
df <- df %>%
  mutate(Year =  year(df$Date))



##### EDA #####

# Take a look at the variables after transformations
str(df)

# subset interesting columns
df <- subset(df, select=c(Year, Title, City, State, Latitude, Longitude, Number_of_Victim_Fatalities, Total_Number_of_Fatalities, Number_of_Victims_Injured, Total_Number_of_Victims, Date, Date_Detailed, Average_Shooter_Age, Shooter_Sex, Shooter_Race, Type_of_Gun, Number_of_Shotguns, Number_of_Rifles, Number_of_Handguns, Total_Number_of_Guns, Number_of_Automatic_Guns, Number_of_Semi_Automatic_Guns,Fate_of_Shooter_at_the_scene, Shooters_Cause_of_Death, School_Related, Place_Type, Relationship_to_Incident_Location, Targeted_Victims, Possible_Motive, History_of_Mental_Illness, Military_Experience, Class))



# 1) Histogram and density plots to analyze/represent continuous variables 
# The average age of the shooter is between 18 and 40 years old.
# Shooter's gender is mainly Male
# Between 1 and 10 people are killed most of the time
# Shooter's gender is mainly Male
# Shooter's race is mainly White, Black and Unknown
# Type_of_Gun is mainly Handgun 
# Half of the shooters have mental illness
# When it is known, a lot of them have had military experience
# Most of the mass shoot are not school related
# A large proportion of the shooter escape
plot_density(df)
plot_histogram(df)

# 2) Categorical Variables - Barplots to analyze/represent categorical variables
plot_bar(df$State, title = "State")  # States having the larger records are California, Florida, Texas

plot_bar(df$Place_Type, title = "Place_Type")  # The type of place is mainly the place of residence, place of business, schools and place of recreation
plot_bar(df$Relationship_to_Incident_Location, title = "Relationship_to_Incident_Location")  # The shooters principally attack school related person, family and colleagues
plot_bar(df$Targeted_Victims, title = "Targeted_Victims") 
plot_bar(df$Possible_Motive, title = "Possible_Motive")
plot_bar(df$Class, title = "Class")  # Most of the incident reported are Mass Shooting




# 3) Correlations

### correlations
data_for_cor <- df %>% select(Year,
                              Number_of_Victim_Fatalities, 
                              #Total_Number_of_Fatalities, 
                              Number_of_Victims_Injured, 
                              #Total_Number_of_Victims, 
                              Average_Shooter_Age,
                              Shooter_Sex, 
                              Shooter_Race, 
                              Type_of_Gun,
                              #Number_of_Shotguns, 
                              #Number_of_Rifles, 
                              #Number_of_Handguns, 
                              Total_Number_of_Guns, 
                              #Number_of_Automatic_Guns, 
                              #Number_of_Semi_Automatic_Guns,
                              Shooters_Cause_of_Death, 
                              School_Related, 
                              History_of_Mental_Illness, 
                              Military_Experience)

# the "pairwise.complete.obs" makes sure to avoid NAs accross all variables
corrs = cor(data_for_cor, use="pairwise.complete.obs")
# Number_of_Victim_Fatalities positively correlated to Total_Number_of_Guns
# Shooter's race is correlated to the Shooter_Sex
# Average_Shooter_Age is correlated to School_Related
corrplot(corrs, type="upper", method="color", addCoef.col = "black")


# 4) Histograms

### Histogram with the number of total incidents

h1<-ggplot(data = df[!is.na(df$Year),],aes(x=Year))+
  geom_bar(aes(fill="Total incidents by years"))+
  geom_text(stat = "count",aes(label=..count..),vjust=-1,angel=65,size=2)+
  labs(title="Total Incidents by Year",xlab="Year",ylab="Total Incidents",size=3)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6,size = 6))

### Histogram with the number of fatalities
h2 <- plot_ly(data = df
              ,type = 'bar'
              ,x = ~ Year
              ,y = ~ sum(Number_of_Victim_Fatalities  + Number_of_Victims_Injured)
              ,alpha = 0.4
              , name = "Total victims by years"
              ,text = ~paste(
                'Fatalities : ', Number_of_Victim_Fatalities
                ,'\n Injured : ', Number_of_Victims_Injured
              )) %>% 
  layout(title = "Number of Total victims by years"
         , xaxis = list(title = "Year")
         , yaxis = list(title = "Number of victims"))

subplot(h1, h2, titleX = T, titleY = T, margin = 0.07)

# 5) Boxplots

### boxplot of the Number_of_Victim_Fatalities by State
b1 <- df %>% group_by(Number_of_Victim_Fatalities, State) %>%
  ungroup() %>%
  mutate(State = forcats::fct_reorder(State, Number_of_Victim_Fatalities)) %>%
  plot_ly(x = ~State, y = ~Number_of_Victim_Fatalities, type = "box", name='number of victims')%>%
  layout(title="Boxplot of the Number_of_Victim_Fatalities by State",
         yaxis=list(title="Number_of_Victim_Fatalities"))

### boxplot of the Average_Shooter_Age by State
b2 <- df %>% group_by(Average_Shooter_Age, State) %>%
  ungroup() %>%
  mutate(State = forcats::fct_reorder(State, Average_Shooter_Age)) %>%
  plot_ly(x = ~State, y = ~Average_Shooter_Age, type = "box", name='age of the shooter') %>%
  layout(title="Boxplot of the Average_Shooter_Age by State",
         yaxis=list(title="Average_Shooter_Age"))

subplot(list(b1, b2), nrows = length(list(b1, b2)), 
        shareX= F, titleX = T, titleY = T, margin = 0.2)



save(df, file = "data/df_mass_shooting.Rdata")


