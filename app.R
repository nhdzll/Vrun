#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# which fields get saved 
fieldsAll <- c("id", "email", "event", "distance", "name", "age", "country", "user_agree", "conditions")

# which fields are mandatory
fieldsMandatory <- c("email", "event", "distance", "user_agree", "conditions")

event <- c("Carrera", "Caminata")


#Funciones
# add an asterisk to an input label
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

# get current Epoch time
epochTime <- function() {
    return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
    format(Sys.time(), "%d%m%Y-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
    files <- list.files(file.path(responsesDir), full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    #data <- dplyr::rbind_all(data)
    data <- do.call(rbind, data)
    data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
    ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# usernames that are admins
adminUsers <- c("admin", "prof")

# info for sharing this app on facebook/twitter
share <- list(
    title = "Formulario de Inscripción",
    url = "http://www.gob.mx/salud/conadic/",
    image = "https://raw.githubusercontent.com/mxabierto/assets/master/img/logos/conadic.png",
    description = "1ra. Carrera Virtual del Día Nacional contra el Uso Nocivo del Alcohol 2021",
    twitter_user = "CONADICmx"
)

shinyApp(
    ui = fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        title = "Formulario de Inscripción",
        tags$head(
            tags$link(rel = "shortcut icon", type="image/x-icon", href="http://daattali.com/shiny/img/favicon.ico"),
            
            # Facebook OpenGraph tags
            tags$meta(property = "og:title", content = share$title),
            tags$meta(property = "og:type", content = "website"),
            tags$meta(property = "og:url", content = share$url),
            tags$meta(property = "og:image", content = share$image),
            tags$meta(property = "og:description", content = share$description),
            
            # Twitter summary cards
            tags$meta(name = "twitter:card", content = "summary"),
            tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
            tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
            tags$meta(name = "twitter:title", content = share$title),
            tags$meta(name = "twitter:description", content = share$description),
            tags$meta(name = "twitter:image", content = share$image)
        ),
        div(id = "header",
            h1("Formulario de Inscripción"),
            h4("1ra. Carrera Virtual del Día Nacional contra el Uso Nocivo del Alcohol 2021")
        ),
            
        fluidRow(
            column(7,
                   div(
                       id = "form",
                       
                       textInput("email", labelMandatory("Email:"), ""),
                       radioButtons("event", labelMandatory("Evento en el que participará:"), event),
                       sliderInput("distance", labelMandatory("Distancia"), 1, 5, 1, ticks = FALSE),
                       textInput("name", "Su nombre:"),
                       numericInput("age", "Su edad:", value = 18, min = 18, max = 110)),
                       selectInput("country", "Estado de residenca::",
                                   c("", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", 
                                     "Chihuahua", "Ciudad de México", "Coahuila", "Colima", "Durango", "Estado de México", 
                                     "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Michoacán", "Morelos", "Nayarit", 
                                     "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", 
                                     "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", 
                                     "Zacatecas")),
                       checkboxInput("user_agree", labelMandatory(paste0("Declaro ser mayor de edad, y contar con las condiciones médicas",
                                                                         "y de salud para poder realizar la actividad mencionada.")), FALSE),
                       checkboxInput("conditions", labelMandatory("He leido y estoy de acuerdo con el aviso de privacidad "), FALSE),
                       
                       
                       actionButton("submit", "Enviar información", class = "btn-primary"),
                       
                       shinyjs::hidden(
                           span(id = "submit_msg", "Enviando la información..."),
                           div(id = "error",
                               div(br(), tags$b("Error: "), span(id = "error_msg"))
                           )
                       )
                   ),
                   
                   shinyjs::hidden(
                       div(
                           id = "thankyou_msg",
                           h3("Gracias por registrarse"),
                           actionLink("submit_another", "Ver el número de inscripción")
                          )
                   )
            
          )
    ),
    server = function(input, output, session) {
        
        # Enable the Submit button when all mandatory fields are filled out
        observe({
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        
        # Gather all the form inputs (and add timestamp)
        formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, timestamp = epochTime())
            data <- t(data)
            data
        })    
        
        # When the Submit button is clicked, submit the response
        observeEvent(input$submit, {
            
            # User-experience stuff
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            # Save the data (show an error message in case of error)
            tryCatch({
                saveData(formData())
                shinyjs::reset("form")
                shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
        })
        
        # submit another response
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        })
        
    }
)
