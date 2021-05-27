source("library.R")


df <- get(load("data/df_mass_shooting.Rdata"))



## For the Parallel Coordinate Plot, we add some noise to add some variance and spread the point around each factors ##
subset_df <- subset(df, select = c(Average_Shooter_Age, Shooter_Sex, Shooter_Race, History_of_Mental_Illness, Military_Experience, Place_Type, Relationship_to_Incident_Location, Targeted_Victims, Possible_Motive))
subset_df[,1:5] <- subset_df[,1:5] + matrix(rnorm(nrow(subset_df)*5, 0, 0.1), ncol=5)


## convert some numeric types
fact_cols = c("School_Related",
             "History_of_Mental_Illness",
             "Military_Experience",
             "Shooter_Sex",
             "Shooter_Race")

# convert the types
df = df %>% 
  mutate(across(all_of(fact_cols), as.factor))

df$Shooter_Sex <- fct_recode(as.factor(df$Shooter_Sex), "Male" = "0", "Female" = "1", "Unknown" = "2")
df$Shooter_Race <- fct_recode(as.factor(df$Shooter_Race), "White American" = "0", "Black American" = "1", "Asian American" = "2", "Native American" = "3", "Unknown" = "4")
df$History_of_Mental_Illness <- fct_recode(as.factor(df$History_of_Mental_Illness), "No" = "0", "Yes" = "1", "Unknown" = "2")
df$Military_Experience <- fct_recode(as.factor(df$Military_Experience), "No" = "0", "Yes" = "1", "Unknown" = "2")

#### SHINY APP 2 ####

ui <- fluidPage(
      theme = bslib::bs_theme(
  bg = "navy", 
  fg = "white", 
  base_font = "Source Sans Pro"),
  sidebarLayout(
    sidebarPanel(
titlePanel('Profile of the murderer (Part 2)'),
h5("Specification Graph  and Graph 2"),
  radioButtons("scale", "Choose a scale for the Parallel Coordinate Plot",
               c("globalminmax","std", "uniminmax")),
  radioButtons("ordered", "Would you like to order the boxplot?", c("no", "yes")),
h5("Profile of the shooter"),
  selectInput("shooter","", 
              choice = c("History_of_Mental_Illness", "Military_Experience", "Shooter_Sex", "Shooter_Race"),
              selected = c("Shooter_Sex")),
h5("Characteristic of the mass shooting"),
  selectInput("category","", 
                  choice = c("Place_Type", "Relationship_to_Incident_Location", "Possible_Motive"),
                  selected = c("Place_Type")),
h5("Indicator"),
  selectInput("indicator","", 
                  choice = c("Number_of_Victim_Fatalities", "Total_Number_of_Fatalities", "Number_of_Victims_Injured", "Total_Number_of_Victims"),
                  selected = c("Number_of_Victim_Fatalities")),

h5("Legend for Parallel Coordinate Plot"),
p(paste('Shooters Race - 0: White American or European American 1: Black American or African American 2: Asian American 3: Native American 4: Other or Unknown')),
p(paste("Shooter Sex - 0: Male 1: Female 2: Unknown")),
p(paste("Military Experience - 0: No 1: Yes 2: Unknown")),
p(paste("Military Experience - 0: No \n 1: Yes 2: Unknown")),
p(paste("Shooter History of Mental Illness - 0: No 1: Yes 2: Unknown"))
      ),
      mainPanel(
        column(5, plotlyOutput(outputId = "p1", width = "800px", height = "450px")),
        column(7, plotlyOutput(outputId = "p2", width = "800px", height = "450px")),
        column(5, plotOutput(outputId = "p3", width = "800px", height = "450px")),
        column(7, plotlyOutput(outputId = "p4", width = "800px", height = "450px")),
        column(9, verbatimTextOutput("legend"))
      )
      )
    )
  
  ## SERVER 
  server <- function(input, output, session) {
    observe({
      column <- ifelse(input$category=="Place_Type", 6, ifelse(input$category=="Relationship_to_Incident_Location", 7, ifelse(input$category=="Targeted_Victims", 8, ifelse(input$category=="Possible_Motive", 9, NA))))

  l <- list(
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2)

  ###Plot #1 Parallel Coordinate Plot
  output$p1 <- renderPlotly({
    gg <-
  ggparcoord(
    subset_df,
    columns = 1:5, 
    showPoints = TRUE,
    scale = input$scale,
    groupColumn = column,
    title = paste("Parallel Coordinate Plot for the", input$category, "Data"),
    alphaLines = 0.3) +
    theme_bw() +
    theme(axis.title.x=element_blank(), legend.position = "top", axis.text.x = element_text(angle = 45))

  ggplotly(gg)
    })
  
  
  ###Plot #2 Boxplot with interactions
  output$p2 <- renderPlotly({
    if (input$ordered =="no"){
      plot_ly(df, x = as.formula(paste0("~", input$indicator)), y = ~interaction(get(input$category), get(input$shooter))) %>%
  add_boxplot(color = as.formula(paste0("~", input$shooter))) %>%
  layout(yaxis = list(title = ""),
         title = paste(input$indicator, 'by', input$category, 'and', input$shooter))
    }
    else
    {d <- df %>%
          mutate(cc = interaction(get(input$category), get(input$shooter)))
  
    # interaction levels sorted by median price
    lvls <- d %>%
      group_by(cc) %>%
      summarise(m = median(get(input$indicator))) %>%
      arrange(m) %>%
      pull(cc)
    
    plot_ly(d, x = as.formula(paste0("~", input$indicator)), y = ~factor(cc, lvls)) %>%
      add_boxplot(color = as.formula(paste0("~", input$shooter))) %>%
      layout(title = paste(input$category, 'and', input$shooter, 'sorted by', input$indicator, 'median'),
             yaxis = list(title = ""))
    }
  })
  ###Plot #3 Pie Chart
  output$p3 <- renderPlot({
    
    bar <- ggplot(df,aes(x="",y=get(input$indicator),fill=get(input$category))) + geom_bar(stat = "identity")
    pie <- bar+coord_polar("y",start = 0) + ggtitle(paste(input$indicator, "by", input$category)) + xlab("input$category") + ylab("input$indicator") +
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.title = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
  )
    pie
  })
  
  
  ###Plot #4 Scatter plot
  output$p4 <- renderPlotly({
    plot_ly(data = df
   ,type = 'scatter'
   ,x = ~ Year 
   ,y = as.formula(paste0("~", input$category))
   , color = as.formula(paste0("~", input$shooter))
   ,size = as.formula(paste0("~", input$indicator))
   ,text = ~paste(' Date: ', Date 
                  ,'\n Location : ', City 
          ,'\n Total.victims :', get(input$indicator)
          ,'\n Profile :', get(input$shooter)
                  )
   ,hoverinfo= "text") %>% 
  layout(title = paste("Mass Shootings in US by", input$shooter)
              , xaxis = list(title = "Year")
             , yaxis = list(title = ""))
    })
  })
  }

shinyApp(ui = ui, server = server)


