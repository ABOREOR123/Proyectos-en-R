library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinyjs)
library(DT)
library(shinyWidgets)


ejercicios <- data.frame(
  dia = rep(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes"), each = 4),
  ejercicio = c(
    "Press de Banca Plano", "Press Inclinado con Mancuernas", "Aperturas en Polea", "Fondos en Paralelas",
    "Sentadillas Libres", "Prensa de Piernas", "Extensiones de Cuádriceps", "Elevación de Talones de Pie",
    "Dominadas con Peso", "Peso Muerto Rumano", "Remo con Barra", "Curl de Bíceps con Barra",
    "Peso Muerto Rumano", "Curl Femoral Acostado", "Hip Thrust con Barra", "Elevación de Talones Sentado",
    "Press Militar con Mancuernas", "Elevaciones Laterales", "Face Pull en Polea", "Encogimientos con Barra"
  ),
  musculo_principal = c(
    "Pecho", "Pecho", "Pecho", "Tríceps",
    "Cuádriceps", "Cuádriceps", "Cuádriceps", "Gemelos",
    "Espalda", "Femorales", "Espalda", "Bíceps",
    "Femorales", "Femorales", "Glúteos", "Gemelos",
    "Hombros", "Hombros", "Hombros", "Trapecios"
  )
)


grupos_musculares <- c(
  "Brazo (Contraído)" = "brazo_contraido",
  "Brazo (Relajado)" = "brazo_relajado",
  "Pecho" = "pecho",
  "Cintura" = "cintura",
  "Cadera" = "cadera",
  "Muslo" = "muslo",
  "Pantorrilla" = "pantorrilla"
)

# Función para calcular 1RM (Fórmula de Epley)
calcular_1rm <- function(peso, repeticiones) {
  if (repeticiones <= 1) return(peso)
  round(peso * (1 + repeticiones / 30), 1)
}

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .progress-box {
        padding: 10px;
        margin: 10px 0;
        background: #f5f5f5;
        border-radius: 5px;
      }
      .recomendacion {
        color: #31708f;
        background-color: #d9edf7;
        padding: 10px;
        border-radius: 5px;
        margin: 10px 0;
      }
      .medidas-input {
        background: #f9f9f9;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  titlePanel("💪 Gym Progress Tracker - Seguimiento de Medidas"),
  
  # Sistema de autenticación básico
  uiOutput("auth_panel"),
  
  conditionalPanel(
    condition = "output.auth_ok == true",
    sidebarLayout(
      sidebarPanel(
        selectInput("dia", "Día de entrenamiento:", unique(ejercicios$dia)),
        uiOutput("ejercicio_selector"),
        dateInput("fecha", "Fecha:", value = Sys.Date()),
        numericInput("peso", "Peso (kg):", value = 0, min = 0),
        numericInput("repeticiones", "Repeticiones:", value = 0, min = 0),
        sliderInput("esfuerzo", "Percepción de esfuerzo (RPE 1-10):", 1, 10, 5),
        awesomeCheckbox("al_fallo", "¿Llegaste al fallo muscular?", FALSE),
        textAreaInput("notas", "Notas adicionales:"),
        actionButton("guardar", "Guardar Entrenamiento", class = "btn-primary"),
        hr(),
        
        # Panel de registro de medidas corporales
        div(class = "medidas-input",
            h4("Registro de Medidas Corporales (cm)"),
            dateInput("fecha_medidas", "Fecha de medidas:", value = Sys.Date()),
            numericInput("peso_corporal", "Peso corporal (kg):", value = NA, min = 30),
            lapply(seq_along(grupos_musculares), function(i) {
              numericInput(names(grupos_musculares)[i], names(grupos_musculares)[i], value = NA, min = 0, step = 0.5)
            }),
            actionButton("guardar_medidas", "Guardar Medidas", class = "btn-success")
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("📈 Progresión", 
                   plotOutput("progresion_plot"),
                   uiOutput("recomendacion_progreso")),
          
          tabPanel("📊 Métricas", 
                   fluidRow(
                     column(6, plotOutput("volumen_muscular_plot")),
                     column(6, plotOutput("fuerza_plot"))
                   ),
                   h4("Máximos Personales"),
                   DTOutput("maximos_table")),
          
          tabPanel("📏 Medidas Corporales",
                   h3("Evolución de tus Medidas"),
                   selectInput("medida_seleccionada", "Selecciona medida a visualizar:",
                               choices = c("Peso corporal" = "peso_corporal", grupos_musculares)),
                   plotOutput("grafico_medidas"),
                   h4("Resumen de Cambios"),
                   DTOutput("resumen_medidas"),
                   uiOutput("analisis_composicion"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Autenticación básica
  user_data <- reactiveValues(logged_in = FALSE)
  
  output$auth_panel <- renderUI({
    if (!user_data$logged_in) {
      wellPanel(
        textInput("username", "Usuario:"),
        passwordInput("password", "Contraseña:"),
        actionButton("login", "Iniciar Sesión")
      )
    }
  })
  
  observeEvent(input$login, {
    if (input$username != "" && input$password != "") {
      user_data$logged_in <- TRUE
      user_data$username <- input$username
    }
  })
  
  output$auth_ok <- reactive({
    user_data$logged_in
  })
  outputOptions(output, "auth_ok", suspendWhenHidden = FALSE)
  
  # Sistema de almacenamiento
  registros <- reactiveVal(data.frame(
    fecha = as.Date(character()),
    usuario = character(),
    ejercicio = character(),
    musculo = character(),
    peso = numeric(),
    repeticiones = numeric(),
    esfuerzo = numeric(),
    al_fallo = logical(),
    notas = character(),
    rm_estimado = numeric(),
    volumen = numeric()
  ))
  
  medidas_corporales <- reactiveVal(data.frame(
    fecha = as.Date(character()),
    usuario = character(),
    peso_corporal = numeric(),
    brazo_contraido = numeric(),
    brazo_relajado = numeric(),
    pecho = numeric(),
    cintura = numeric(),
    cadera = numeric(),
    muslo = numeric(),
    pantorrilla = numeric()
  ))
  
  # Cargar datos guardados al iniciar
  observe({
    if (file.exists("registros.rds")) {
      registros(readRDS("registros.rds"))
    }
    if (file.exists("medidas_corporales.rds")) {
      medidas_corporales(readRDS("medidas_corporales.rds"))
    }
  })
  
  # Selector de ejercicios dinámico
  output$ejercicio_selector <- renderUI({
    ejercicios_dia <- ejercicios %>% filter(dia == input$dia)
    selectInput("ejercicio", "Ejercicio:", ejercicios_dia$ejercicio)
  })
  
  # Guardar nuevo registro de entrenamiento
  observeEvent(input$guardar, {
    req(input$ejercicio, input$peso > 0, input$repeticiones > 0)
    
    rm_estimado <- calcular_1rm(input$peso, input$repeticiones)
    volumen <- input$peso * input$repeticiones
    
    nuevo_registro <- data.frame(
      fecha = input$fecha,
      usuario = user_data$username,
      ejercicio = input$ejercicio,
      musculo = ejercicios %>% 
        filter(ejercicio == input$ejercicio) %>% 
        pull(musculo_principal) %>% 
        first(),
      peso = input$peso,
      repeticiones = input$repeticiones,
      esfuerzo = input$esfuerzo,
      al_fallo = input$al_fallo,
      notas = input$notas,
      rm_estimado = rm_estimado,
      volumen = volumen
    )
    
    registros(rbind(registros(), nuevo_registro))
    saveRDS(registros(), "registros.rds")
    
    reset("peso")
    reset("repeticiones")
    reset("notas")
    showNotification("Entrenamiento guardado!", type = "message")
  })
  
  # Guardar medidas corporales
  observeEvent(input$guardar_medidas, {
    req(input$fecha_medidas)
    
    nueva_medida <- data.frame(
      fecha = input$fecha_medidas,
      usuario = user_data$username,
      peso_corporal = ifelse(is.na(input$peso_corporal), NA, input$peso_corporal),
      brazo_contraido = ifelse(is.na(input$`Brazo (Contraído)`), NA, input$`Brazo (Contraído)`),
      brazo_relajado = ifelse(is.na(input$`Brazo (Relajado)`), NA, input$`Brazo (Relajado)`),
      pecho = ifelse(is.na(input$Pecho), NA, input$Pecho),
      cintura = ifelse(is.na(input$Cintura), NA, input$Cintura),
      cadera = ifelse(is.na(input$Cadera), NA, input$Cadera),
      muslo = ifelse(is.na(input$Muslo), NA, input$Muslo),
      pantorrilla = ifelse(is.na(input$Pantorrilla), NA, input$Pantorrilla)
    )
    
    # Eliminar filas completamente vacías
    if (!all(is.na(nueva_medida[,3:ncol(nueva_medida)]))) {
      medidas_corporales(rbind(medidas_corporales(), nueva_medida))
      saveRDS(medidas_corporales(), "medidas_corporales.rds")
      
      # Resetear inputs
      lapply(names(grupos_musculares), function(x) reset(x))
      reset("peso_corporal")
      showNotification("Medidas guardadas!", type = "message")
    } else {
      showNotification("Debes ingresar al menos una medida", type = "warning")
    }
  })
  
  # Gráfico de progresión de medidas
  output$grafico_medidas <- renderPlot({
    req(nrow(medidas_corporales()) > 0)
    datos_usuario <- medidas_corporales() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(fecha)
    
    if (nrow(datos_usuario) == 0) return(NULL)
    
    medida <- input$medida_seleccionada
    
    ggplot(datos_usuario, aes_string(x = "fecha", y = medida)) +
      geom_line(color = "#3498db", size = 1) +
      geom_point(color = "#2980b9", size = 3) +
      labs(title = paste("Evolución de", 
                         ifelse(medida == "peso_corporal", "Peso Corporal", 
                                names(grupos_musculares)[grupos_musculares == medida])),
           x = "Fecha", y = ifelse(medida == "peso_corporal", "Peso (kg)", "Circunferencia (cm)")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
  
  # Resumen de cambios en medidas
  output$resumen_medidas <- renderDT({
    req(nrow(medidas_corporales()) > 0)
    
    datos_usuario <- medidas_corporales() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(fecha)
    
    if (nrow(datos_usuario) < 2) return(NULL)
    
    primera <- datos_usuario[1, ]
    ultima <- datos_usuario[nrow(datos_usuario), ]
    
    cambios <- data.frame(
      Medida = c("Peso", names(grupos_musculares)),
      Inicial = c(primera$peso_corporal, 
                  primera$brazo_contraido, primera$brazo_relajado,
                  primera$pecho, primera$cintura, primera$cadera,
                  primera$muslo, primera$pantorrilla),
      Actual = c(ultima$peso_corporal,
                 ultima$brazo_contraido, ultima$brazo_relajado,
                 ultima$pecho, ultima$cintura, ultima$cadera,
                 ultima$muslo, ultima$pantorrilla),
      Cambio = c(
        ifelse(!is.na(primera$peso_corporal) && !is.na(ultima$peso_corporal),
               ultima$peso_corporal - primera$peso_corporal, NA),
        ifelse(!is.na(primera$brazo_contraido) && !is.na(ultima$brazo_contraido),
               ultima$brazo_contraido - primera$brazo_contraido, NA),
        ifelse(!is.na(primera$brazo_relajado) && !is.na(ultima$brazo_relajado),
               ultima$brazo_relajado - primera$brazo_relajado, NA),
        ifelse(!is.na(primera$pecho) && !is.na(ultima$pecho),
               ultima$pecho - primera$pecho, NA),
        ifelse(!is.na(primera$cintura) && !is.na(ultima$cintura),
               ultima$cintura - primera$cintura, NA),
        ifelse(!is.na(primera$cadera) && !is.na(ultima$cadera),
               ultima$cadera - primera$cadera, NA),
        ifelse(!is.na(primera$muslo) && !is.na(ultima$muslo),
               ultima$muslo - primera$muslo, NA),
        ifelse(!is.na(primera$pantorrilla) && !is.na(ultima$pantorrilla),
               ultima$pantorrilla - primera$pantorrilla, NA)
      )
    )
    
    # Formatear para mostrar
    cambios %>% 
      mutate(
        Inicial = ifelse(is.na(Inicial), "-", 
                         ifelse(Medida == "Peso", 
                                paste(round(Inicial, 1), "kg"),
                                paste(round(Inicial, 1), "cm"))),
        Actual = ifelse(is.na(Actual), "-",
                        ifelse(Medida == "Peso", 
                               paste(round(Actual, 1), "kg"),
                               paste(round(Actual, 1), "cm"))),
        Cambio = ifelse(is.na(Cambio), "-",
                        ifelse(Medida == "Peso", 
                               paste(ifelse(Cambio >= 0, "+", ""), round(Cambio, 1), "kg"),
                               paste(ifelse(Cambio >= 0, "+", ""), round(Cambio, 1), "cm")))
      ) %>% 
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Análisis de composición corporal
  output$analisis_composicion <- renderUI({
    req(nrow(medidas_corporales()) > 1)
    
    datos_usuario <- medidas_corporales() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(fecha)
    
    primera <- datos_usuario[1, ]
    ultima <- datos_usuario[nrow(datos_usuario), ]
    
    # Solo mostrar análisis si tenemos las medidas necesarias
    if (!is.na(primera$pecho) && !is.na(primera$cintura) && !is.na(primera$cadera) &&
        !is.na(ultima$pecho) && !is.na(ultima$cintura) && !is.na(ultima$cadera)) {
      
      # Cálculo simple de relación cintura-cadera (indicator de distribución de grasa)
      rcc_inicial <- round(primera$cintura / primera$cadera, 2)
      rcc_actual <- round(ultima$cintura / ultima$cadera, 2)
      
      # Análisis cualitativo
      analisis <- if (rcc_actual < rcc_inicial) {
        "Mejoría en la distribución de grasa corporal (relación cintura-cadera reducida)."
      } else if (rcc_actual > rcc_inicial) {
        "Aumento en la relación cintura-cadera. Considera revisar tu nutrición y cardio."
      } else {
        "Estable en distribución de grasa corporal."
      }
      
      div(
        h4("Análisis de Composición Corporal"),
        p(paste("Relación cintura-cadera inicial:", rcc_inicial)),
        p(paste("Relación cintura-cadera actual:", rcc_actual)),
        p(analisis)
      )
    }
  })
  
  # [Resto del código del servidor permanece igual...]
  # (Mantendrías las mismas funciones para progresion_plot, volumen_muscular_plot, etc.)
}

# Run the application
shinyApp(ui = ui, server = server)
