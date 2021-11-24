
library(shiny)
library(tidyverse)
library(shinyWidgets)





df <- (read.csv('./CCSOData.csv', stringsAsFactors = FALSE, na.strings = "") 
       %>% select(RACE, SEX, EMPLOYMENT.STATUS, MARITIAL.STATUS, PRISONER.TYPE)
       %>% mutate(across(.fns = ~str_replace_na(.x)))
)


race_options = sort(levels(df$RACE))
sex_options = sort(levels(df$SEX))

employment_status = sort(levels(df$EMPLOYMENT.STATUS))
maritial_status = sort(levels(df$MARITIAL.STATUS))



       


ui <- fluidPage(

  useShinydashboard(),
  titlePanel("Prisoner Type Distribution"),
  sidebarLayout(
    sidebarPanel(

      
        checkboxGroupInput("RACE", "Race: ", 
                           choices = c("White", "Black", "Hispanic", "Asian/Pacific Islander", "Native American", "Unknown", "White (Hispanic)", "NA"),
                           selected = c("White", "Black")
                           
                    
                           ),

        
        checkboxGroupInput("SEX", "Sex: ", c("Male", "Female", "NA"),
                           selected = c("Male", "Female"),
                           inline = TRUE),

        
        # checkboxGroupInput("EMPLOYMENT.STATUS", "Employment Status: ", 
        #                    c("Employed - Full Time","Student","Unemployed","Employed - Part Time","Self Employed","Retired","Laid Off","NA"),
        #                    selected =c("Employed - Full Time","Student","Unemployed","Employed - Part Time")),

        pickerInput("EMPLOYMENT.STATUS",
                    "Employment Status: ",
                    c("Employed - Full Time","Student","Unemployed","Employed - Part Time","Self Employed","Retired","Laid Off","NA"),
                    selected =c("Employed - Full Time","Student","Unemployed","Employed - Part Time"),
                    multiple  = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Deselect-All",
                      `select-all-text` = "Select-All",
                      `none-selected-text` = "Ignored"
                    )
        ),
        
        # checkboxGroupInput("MARITIAL.STATUS", "Maritial Status: ", 
        #                    c("Single","Married","Divorced","Seperated","Significant Other","Widowed","Unknown","NA"),
        #                    selected = c("Single","Married"))

        pickerInput("MARITIAL.STATUS",
                    "Maritial Status: ",
                    c("Single","Married","Divorced","Seperated","Significant Other","Widowed","Unknown","NA"),
                    selected = c("Single","Married"),
                    multiple  = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Deselect-All",
                      `select-all-text` = "Select-All",
                      `none-selected-text` = "Ignored"
                    )
        )

    ),
    mainPanel(
      plotOutput("bar")
    )
  )
)

server <- function(input, output, session) {

  output$bar <- renderPlot({
    
    
    
    # Race
    if(length(input$RACE) != length(levels(factor(df$RACE))) 
       && length(input$RACE) != 0) {
      df <- df %>% filter(RACE %in% input$RACE)
    }
    
    # Sex
    if(length(input$SEX) != length(levels(factor(df$SEX))) 
       && length(input$SEX) != 0) {
      df <- df %>% filter(SEX %in% input$SEX)
    }
    
    
    # EMPLOYMENT.STATUS
    if(length(input$EMPLOYMENT.STATUS) != length(levels(factor(df$EMPLOYMENT.STATUS))) 
       && length(input$EMPLOYMENT.STATUS) != 0) {
      df <- df %>% filter(EMPLOYMENT.STATUS %in% input$EMPLOYMENT.STATUS)
    }
    
    
    # MARITIAL.STATUS
    if(length(input$MARITIAL.STATUS) != length(levels(factor(df$MARITIAL.STATUS))) 
       && length(input$MARITIAL.STATUS) != 0) {
      df <- df %>% filter(MARITIAL.STATUS %in% input$MARITIAL.STATUS)
    }
    
    
    
    ggplot(data = df) +
      geom_bar(mapping = aes(x = fct_rev(fct_infreq(PRISONER.TYPE)), fill = PRISONER.TYPE))+
      labs(x = "Prison Type",y="Count") +
      geom_text(aes(x = fct_rev(fct_infreq(PRISONER.TYPE)), y = ..count.., label = ..count..), stat = "count", hjust = -0.1) +
      theme(legend.position = "none") +
      coord_flip()
    
    
  }, height = 850)
}

shinyApp(ui, server)



