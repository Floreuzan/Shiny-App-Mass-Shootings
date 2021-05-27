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
        column(7, plotlyOutput(outputId = "p2", width = "700px", height = "400px")),
        verbatimTextOutput("legend")
      )
      )
    )
  
  ## SERVER 
server <- function(input, output, session) {
    observe({
      category <- input$category
      barmode <- input$bar
      
    output$legend <- 
    renderPrint(
      ifelse(category=="Shooter_Race", return(cat(paste(' Legend \n 0: White American or European American \n 1: Black American or African American \n 2: Asian American \n 3: Native American \n 4: Other or Unknown'))),
       ifelse(category=="Shooter_Sex", return(cat(paste(' Legend \n 0: Male \n 1: Female \n 2: Unknown'))),
           ifelse(category=="Military_Experience", return(cat(paste(' Legend \n 0: No \n 1: Yes \n 2: Unknown'))),
                  ifelse(category=="History_of_Mental_Illness", return(cat(paste(' Legend \n 0: No \n 1: Yes \n 2: Unknown'))),
                         ifelse(category=="Average_Shooter_Age", return(cat(paste(" The mean of the average shooter's age is 31. \n The range of the shooter's age is [12, 70]."))))) 
       )))
      )
    
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
  
