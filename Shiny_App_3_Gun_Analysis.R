source("library.R")

df <- get(load("data/df_mass_shooting.Rdata"))

#### Type_of_Gun
# change the names of levels in a factor.
df$Type_of_Gun <- fct_recode(as.factor(df$Type_of_Gun), "Shotgun" = "0", "Rifle" = "1", "Handgun" = "2", "Multiple_Guns" = "3", "Unknown" = "4")

#### Total_Number_of_Guns
df$Total_Number_of_Guns <- as.numeric(df$Total_Number_of_Guns)

#### Year
# typeof(df$Year) ## Double
df$year <- as.numeric(df$Year)

#### Add new variable: automatic-----
df$Automatic <- NA
df$Automatic <- ifelse(is.na(df$Number_of_Automatic_Guns), "Unknown", ifelse(df$Number_of_Automatic_Guns=="0", "Not Automatic","Automatic"))
(df$Automatic <- as.factor(df$Automatic))

#### Add new variable: semi.automatic-----
df$Semi_Automatic <- NA
df$Semi_Automatic <- ifelse(is.na(df$Number_of_Semi_Automatic_Guns), "Unknown", ifelse(df$Number_of_Semi_Automatic_Guns=="0", "Not Semi-Automatic","Semi-Automatic"))
(df$Semi_Automatic <- as.factor(df$Semi_Automatic))


#### Add the summary statistics for Type_of_Gun and Semi_Automatic gun

## N: Total number of observations
summary = df %>%
  select(Total_Number_of_Fatalities, Type_of_Gun, Semi_Automatic) %>%
  group_by(Type_of_Gun, Semi_Automatic) %>%
  summarise(n = n())


## Min: Minimum of observations
Min = df %>%
  select(Total_Number_of_Fatalities, Type_of_Gun, Semi_Automatic) %>%
  group_by(Type_of_Gun, Semi_Automatic) %>%
  summarise(across(everything(), min))

summary$Min <- Min$Total_Number_of_Fatalities

## First_Qu.: First quartile of observations
First_Qu. = df %>%
  select(Total_Number_of_Fatalities, Type_of_Gun, Semi_Automatic) %>%
  group_by(Type_of_Gun, Semi_Automatic) %>%
  summarise_all(quantile, 0.25)

summary$First_Qu. <- First_Qu.$Total_Number_of_Fatalities

## Mean: Mean of observations
Mean = df %>%
  select(Total_Number_of_Fatalities, Type_of_Gun, Semi_Automatic) %>%
  group_by(Type_of_Gun, Semi_Automatic) %>%
  summarise(across(everything(), mean))

summary$Mean <- Mean$Total_Number_of_Fatalities

## Third_Qu.: Third quartile of observations
Third_Qu. = df %>%
  select(Total_Number_of_Fatalities, Type_of_Gun, Semi_Automatic) %>%
  group_by(Type_of_Gun, Semi_Automatic) %>%
  summarise_all(quantile, 0.75)

summary$Third_Qu. <- Third_Qu.$Total_Number_of_Fatalities

## Max: Max of observations
Max = df %>%
  select(Total_Number_of_Fatalities, Type_of_Gun, Semi_Automatic) %>%
  group_by(Type_of_Gun, Semi_Automatic) %>%
  summarise(across(everything(), max))

summary$Max <- Max$Total_Number_of_Fatalities



#### SHINY APP 3 ####

ui <- fluidPage(
  plotlyOutput("p"),
  tableOutput("table")
)

server <- function(input, output, session) {

  # keep track of which guns have been hovered on
  guns <- reactiveVal()
  
  # On hover, the key field of the event data contains the gun name
  # Add that name to the set of all "selected" guns
  observeEvent(event_data("plotly_hover"), {
    gun <- event_data("plotly_hover")$customdata
    guns_old_new <- c(guns(), gun)
    guns(unique(guns_old_new))
  })
  
  # clear the set of guns when a double-click occurs
  observeEvent(event_data("plotly_doubleclick"), {
    guns(NULL)
  })
  
  output$p <- renderPlotly({
    gg <- summary %>%
      filter(Type_of_Gun != "") %>%
      ggplot(aes(x = Type_of_Gun, 
                 y= n,
                 fill = Semi_Automatic,
                 customdata = Semi_Automatic ) ) +
      geom_bar(stat = "identity") +
      labs(color = " ")
    
    ann1 <- list(
      text = "Types of Gun Used in Mass Shootings",
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )
    
    pp1 <- ggplotly(gg) %>%
      layout(annotations = ann1)
    
    # second plotly
    ltys <- c(
      Shotgun = "dashdot",
      Rifle = "longdash",
      `Multiple Guns` = "dash",
      Unknown = "solid",
      Handgun = "dot"
    )
    
    ann2 <- list(
      text = "Fatalities from Mass Shootings From Gun Types",
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )
    
    pp2 = df %>%
      plot_ly( 
        x = ~year, 
        y = ~Total_Number_of_Fatalities,
        # symbol = ~Type_of_Gun,
        color = ~Semi_Automatic
      ) %>%
      add_lines(linetype = ~Type_of_Gun,
                linetypes = ltys,
                line = list(width = 2)) %>%
      rangeslider() %>%
      layout(annotations = ann1
             # yaxis = list(title = "Total Number of Fatalities")
      )
    
    subplot(pp1, pp2) %>%
      layout(showlegend = TRUE, 
             legend = list( 
               title = list(text = ""),
               font = list(size = 10)))
    
  })
  
  
  output$table <- renderTable({
    
    summary %>%
      filter(Semi_Automatic %in% guns())
    },
  
  caption = paste("Table 3. Summary Statistics for Number of Fatalities from
          Mass Shooting Events by Gun Types"), 
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)
  
  )
  
}

shinyApp(ui, server)











