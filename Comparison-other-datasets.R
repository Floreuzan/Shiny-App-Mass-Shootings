source("library.R")

df <- get(load("data/df_mass_shooting.Rdata"))

#### Create table of num_victims, num_incidents, num_fatalities for each states ####
victims_state <- df %>%
  filter((Year >= 2013) & (Year <= 2016)) %>%
  group_by(State) %>%
  summarise(num_victims = sum(Total_Number_of_Victims),
            num_incidents = n(),
            num_fatalities = sum(Total_Number_of_Fatalities))

victims_state$StateID <- state.abb[match(victims_state$State, state.name)]

#### Add new datasets ####

## 1st dataset: number of people killed by guns by states ##
# https://www.kaggle.com/jameslko/gun-violence-data
gun_violence <- read.csv('data/gun-violence-data_01-2013_03-2018.csv')

violence_state <- gun_violence %>%
  filter((date>='2013-01-01') & (date<'2017-01-01'))%>%
  group_by(state) %>%
  summarise(num_killed = sum(n_killed))

violence_state$StateID <- state.abb[match(violence_state$state, state.name)]

## 2nd dataset: with the number of gun laws by states ##
# https://www.kaggle.com/jboysen/state-firearms
provisions <- read.csv('data/provisions.csv')

provisions <- provisions %>%
  filter(year==2016) %>%
  select(c('state', 'lawtotal'))

provisions$StateID <- state.abb[match(provisions$state, state.name)]

## 3rd dataset with the estimated population by states ##
# https://www.kaggle.com/peretzcohen/2019-census-us-population-data-by-state
population <- read.csv('data/2019_Census_US_Population_Data_By_State_Lat_Long.csv')
population$StateID <- state.abb[match(population$STATE, state.name)]


## merge the 4 datasets into data_map ##
data_map <- population %>%
  left_join(victims_state, by='StateID') %>%
  left_join(violence_state, by='StateID') %>%
  left_join(provisions, by='StateID') %>%
  mutate(incidents_per_1m = num_incidents/(POPESTIMATE2019/1000000)) %>%
  mutate(fatalities_per_1m = num_fatalities/(POPESTIMATE2019/1000000))%>%
  mutate(gun_killed_per_1m = num_killed/(POPESTIMATE2019/1000000))%>%
  mutate(law_per_state = lawtotal) 
data_map <- data_map[ , -which(names(data_map) %in% c("State","state.x", "state.y"))]



#### VISUALIZATION ####

## Bar chart of the number of incidents by state ##
p1 <- data_map %>%
  select(c(StateID, num_incidents)) %>%
  arrange((num_incidents)) %>%
  top_n(10)
p1$StateID <- factor(p1$StateID, levels = unique(p1$StateID))
p1 <- p1 %>%
  plot_ly(x = ~num_incidents,
          y = ~StateID)  %>%
  add_bars(orientation = "h", name = "Number of fatalities in Mass Shooting per State",
           marker = list(color='blue'),
           text = ~paste0("State: ", StateID, "\n Number of fatalities in Mass Shooting: ", num_incidents),
           hoverinfo= "text") %>%
  layout(hovermode = "y",
         yaxis = list(title = ""),
         xaxis = list(title = "Number of fatalities in Mass Shooting"),
         title = "Number of fatalities in Mass Shooting")

## Bar chart of the number of fatalities by state ##
p2 <- data_map %>%
  select(c(StateID, num_fatalities)) %>%
  arrange((num_fatalities)) %>%
  top_n(10)
p2$StateID <- factor(p2$StateID, levels = unique(p2$StateID))
p2 <- p2 %>%
  plot_ly(x = ~num_fatalities,
          y = ~StateID,
          name = "Number of incidents of Mass Shooting per State")  %>%
  add_bars(orientation = "h", name = "Number of incidents of Mass Shooting per State",
           marker = list(color='orange'),
           text = ~paste0("State: ", StateID, "\n Number of incidents of Mass Shooting: ", num_fatalities),
           hoverinfo= "text") %>%
  layout(hovermode = "y",
         yaxis = list(title = ""),
         xaxis = list(title = "Number of incidents of Mass Shooting"),
         title = "Number of incidents of Mass Shooting")

## Bar chart of the number of people killed by state ##
p3 <- data_map %>%
  select(c(StateID, num_killed)) %>%
  arrange((num_killed)) %>%
  top_n(10)
p3$StateID <- factor(p3$StateID, levels = unique(p3$StateID))
p3 <- p3 %>%
  plot_ly(x = ~num_killed,
          y = ~StateID,
          name = "Number of death related with guns per state")  %>%
  add_bars(orientation = "h", 
           name = "Number of death related with guns per state",
           marker = list(color='green'),
           text = ~paste0("State: ", StateID, "\n Number of death related with guns: ", num_killed),
           hoverinfo= "text") %>%
  layout(hovermode = "y",
         yaxis = list(title = ""),
         xaxis = list(title = "Number of death related with guns per state"),
         title = "Number of death related with guns per state")

## Bar chart of the number of gun laws by state ##
p4 <- data_map %>%
  select(c(StateID, lawtotal)) %>%
  arrange((lawtotal)) %>%
  top_n(10)
p4$StateID <- factor(p4$StateID, levels = unique(p4$StateID))
p4 <- p4 %>%
  plot_ly(x = ~lawtotal,
          y = ~StateID, 
          name = "Number of gun laws per state")  %>%
  add_bars(orientation = "h", 
           name = "Number of gun laws per state",
           marker = list(color='red'),
           text = ~paste0("State: ", StateID, "\n Number of gun laws per state: ", lawtotal),
           hoverinfo= "text") %>%
  layout(hovermode = "y",
         yaxis = list(title = ""),
         xaxis = list(title = "Number of gun laws per state"),
         title = "Number of gun laws per state")

## Add the barplots together ##
barcharts <- list()
barcharts[[1]] <- p1
barcharts[[2]] <- p2
barcharts[[3]] <- p3
barcharts[[4]] <- p4


## Maps ##
Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoiZmxvdXpuIiwiYSI6ImNrcDZqdWptcDJpbzAydnF3bWJwMnFnajMifQ.FIixEBguTFm5a_l-XgOUww")

# common map properties
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)

map_plot1 <-
  data_map %>% plot_geo(
    z = ~fatalities_per_1m,
    text = ~paste0("State: ", STATE, "\n Number of fatalities in Mass Shooting per 1 M: ", round(fatalities_per_1m, 4)),
    hoverinfo= "text",
    locations = ~ StateID,
    locationmode = 'USA-states',
    colors = brewer.pal(9,"Blues")[5:9]
  ) %>%
  layout(geo = g,
         title="Mass shootings fatalities per one million for each state (2013-2016)",
         legend = list(title = "Number of death per 1 M"))

map_plot2 <-
  data_map %>% plot_geo(
    z = ~incidents_per_1m,
    text = ~paste0("State: ", STATE, "\n Number of incidents of Mass Shooting per 1 M: ", round(incidents_per_1m, 4)),
    hoverinfo= "text",
    locations = ~ StateID,
    locationmode = 'USA-states',
    colors = brewer.pal(9,"Oranges")[5:9]
  ) %>%
  layout(geo = g,
         title="Mass shootings fatalities incidents of Mass Shooting per state (2013-2016)",
         legend = list(title = "Number of incidents per 1 M"))

map_plot3 <-
  data_map %>% plot_geo(
    z = ~gun_killed_per_1m,
    text = ~paste0("State: ", STATE, "\n Number of death related with guns per 1 M: ", round(gun_killed_per_1m, 4)),
    hoverinfo= "text",
    locations = ~ StateID,
    locationmode = 'USA-states',
    colors = brewer.pal(9,"Greens")[5:9]
  ) %>%
  layout(geo = g,
         title="Number of death related with guns per state (2013-2016)",
         legend = list(title = "Number of death per 1 M"))

map_plot4 <-
  data_map %>% plot_geo(
    z = ~law_per_state,
    text = ~paste0("State: ", STATE, "\n Number of gun laws per state: ", round(law_per_state, 4)),
    hoverinfo= "text",
    locations = ~ StateID,
    locationmode = 'USA-states',
    colors = brewer.pal(9,"Reds")[5:9]
  ) %>%
  layout(geo = g,
         showlegend=FALSE,
         title="Number of gun laws per state",
         legend = list(title = "Number of gun laws per state"))


subplot(map_plot1, map_plot2, map_plot3, map_plot4, subplot(barcharts), nrows = 3)%>% 
  layout(title = "Number of death related to Mass Shooting compared to gun laws")

