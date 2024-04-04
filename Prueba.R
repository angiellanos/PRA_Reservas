library(shiny)

# Define la interfaz de usuario
ui <- fluidPage(
  
  withMathJax(),
  
  titlePanel("OTRAS NORMAS APLICABLES A LAS RESERVAS TÉCNICAS DE LAS ENTIDADES ASEGURADORAS"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "seleccion",
        label = "Elige una opción de consulta:",
        choices = c("DISPOSICIONES GENERALES", "RESERVAS TÉCNICAS", "CONTABILIZACIÓN DE LAS RESERVAS TÉCNICAS")
      ),
      uiOutput("subMenu")
    ),
    
    mainPanel(
      uiOutput("definicion"),
      uiOutput("formula")
    )
  )
)

# Define el servidor
server <- function(input, output, session) {
  
  # Generar el submenú basado en la elección del menú principal
  output$subMenu <- renderUI({
    if(input$seleccion == "RESERVAS TÉCNICAS") {
      selectInput("subSeleccion", "Tipos de reservas técnicas:", 
                  choices = c("",
                              "Reserva de Riesgos en Curso",
                              "Reserva Matemática",
                              "Reserva de Insuficiencia de Activos",
                              "Reserva de Siniestros Pendientes",
                              "Reserva de Desviación de Siniestralidad",
                              "Reserva de Riesgos Catastróficos"))
    } else {
      return(NULL)
    }
  })
  
  output$definicion <- renderUI({
    if(input$seleccion == "Autos") {
      tags$div(
        tags$p(strong("Definición:"), " La ", strong("Prima de la póliza"), 
               " es el monto que el asegurado paga por su seguro."),
        tags$br(),
        tags$p("El ", strong("Deducible"), 
               " es la cantidad que el asegurado debe pagar antes de que el seguro cubra el resto de los costos en caso de un siniestro.")
      )
    }
  })
  
  #output$formula <- renderUI({
   # if (input$seleccion == "Autos") {
      # Si la selección es "Autos", mostrar la fórmula con MathJax
    #  HTML('<p class="text-center">\\(\\alpha^2 + \\beta^2 = \\gamma^2\\)</p>')
    #}
  #})
  output$formula <- renderUI({
    if (input$seleccion == "Autos") {
      # Usamos tags para insertar elementos HTML directamente
      tags$p(style = "text-align: center;", # Centramos el párrafo
             "La fórmula del área de un círculo es: ",
             tags$span("\\( A = \\pi r^2 \\)", style = "font-weight: bold;") # Aplicamos negrita a la fórmula
      )
    }
  })
  
}

# Corre la aplicación
shinyApp(ui = ui, server = server)