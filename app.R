######################  Aplicativo  #######################

# ui

header <- dashboardHeader(title = "Aplicativo hackfest - Equipe The Bits",
                          titleWidth = 700,
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = tags$div("Alisson Silva Neimaier",
                                                         tags$br(),
                                                         "Gabriel Holmer Saul",
                                                         tags$br(),
                                                         "Gustavo Machado Utpott",
                                                         tags$br(),
                                                         "Rafael Bernardoni Chaves",
                                                         style = "display: inline-block; vertical-align: middle;"),
                                         icon = icon("id-card"),
                                         status = "success"
                                       ), 
                                       headerText = "Aplicativo desenvolvido por")
                          )

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Análise Univariada", tabName = "Univariada", icon = icon("chart-bar")),
  menuItem("Análise Multivariada", tabName = "Multivariada", icon = icon("chart-bar"),
           menuSubItem("SubItem1", tabName = "sub_1"),
           menuSubItem("SubItem2", tabName = "sub_2"))
))

body <- dashboardBody(
  tabItems(
    tabItem("Univariada",
            fluidPage(
              titlePanel("Análise Univariada das características"),
              fluidRow(
                box(plotOutput("exemplo"),
                    title = "Gráfico com a proporção das espécies das plantas",
                    status = "primary")
              )
            ))
  )
)



ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "purple")


# server

server<- function(input, output) {
  
  output$exemplo <- renderPlot({
    
    exemplo <- iris %>%
      group_by(Species) %>%
      summarise(a=sum(n())) %>%
      mutate(proporção=a/(sum(a))) %>%
      arrange(desc(Species)) %>%
      mutate(lab.ypos = cumsum(proporção) - 0.5*proporção)
    
    
    ggplot(exemplo, aes(x = 2, y = proporção, fill = Species)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      geom_text(aes(y = lab.ypos, label = round(proporção, 2), fontface = "bold"), color = "black")+
      scale_fill_brewer(palette="Set2") +
      theme_void()+
      xlim(0.3, 2.5) +
      labs(fill="Espécie")
  })
  
  
  
  
}

shinyApp(ui, server)
