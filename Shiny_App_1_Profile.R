source("library.R")

df <- get(load("data/df_mass_shooting.Rdata"))

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

#### SHINY APP 1 ####
ui <- fluidPage(
      theme = bslib::bs_theme(
  bg = "navy", 
  fg = "white", 
  base_font = "Source Sans Pro"),
  sidebarLayout(
    sidebarPanel(
titlePanel('Profile of the murderer (Part 1)'),
radioButtons("bar", "Choose a barmode",
               c("group",
                 "stack")),

  selectInput("category","Choose a category", 
                  choice = c("History_of_Mental_Illness", "Military_Experience", "Shooter_Sex", "Shooter_Race", "Average_Shooter_Age"),
                  selected = c("Average_Shooter_Age")),
      ),
      mainPanel(
        column(5, plotlyOutput(outputId = "p1", width = "700px", height = "400px")),
        column(7, plotlyOutput(outputId = "p2", width = "700px", height = "400px"))
      )
      )
    )
  
  ## SERVER 
server <- function(input, output, session) {
    observe({
      category <- input$category
      barmode <- input$bar
      
  l <- list(
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2)

  ###Plot #1 Place_Type
  output$p1 <- renderPlotly({
    dat <- df %>% 
      group_by(across(all_of(c("Place_Type", category)))) %>% 
      summarise(count=n()) %>%
      plot_ly(y = ~Place_Type,
              x = ~count,
              color = as.formula(paste0("~", input$category))) %>%
      layout(title = paste("Place Type by", category),
             legend=l,
             barmode = barmode)
    })
    
  ###Plot #2 Possible_Motive
  output$p2 <- renderPlotly({
    dat <- df %>% 
      group_by(across(all_of(c("Possible_Motive", category)))) %>% 
      summarise(count=n()) %>%
      plot_ly(y = ~Possible_Motive,
              x = ~count,
              color = as.formula(paste0("~", input$category))) %>%
      layout(title = paste("Possible Motive by", category),
             legend=l,
             barmode = barmode)
    })
    })
  }

shinyApp(ui = ui, server = server)
  
