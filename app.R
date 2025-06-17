library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinyjs)
library(DT)
library(shinyWidgets)

# Datos de ejemplo más completos
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
  ),
  tipo = rep(c("Hipertrofia", "Fuerza", "Control excéntrico", "Resistencia"), 5)
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
    "))
  ),
  
  titlePanel("💪 Gym Progress Tracker"),
  
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
        fileInput("foto_progreso", "Subir foto de progreso:", accept = c("image/png", "image/jpeg")),
        uiOutput("foto_reciente")
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
          
          tabPanel("📅 Historial", 
                   DTOutput("historial_table"),
                   downloadButton("descargar_historial", "Descargar Historial")),
          
          tabPanel("📸 Progreso Visual",
                   uiOutput("galeria_fotos"))
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
    # Aquí podrías validar contra una base de datos real
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
  
  fotos_progreso <- reactiveVal(data.frame(
    fecha = as.Date(character()),
    usuario = character(),
    ruta = character()
  ))
  
  # Cargar datos guardados al iniciar
  observe({
    if (file.exists("registros.rds")) {
      registros(readRDS("registros.rds"))
    }
    if (file.exists("fotos_progreso.rds")) {
      fotos_progreso(readRDS("fotos_progreso.rds"))
    }
  })
  
  # Selector de ejercicios dinámico
  output$ejercicio_selector <- renderUI({
    ejercicios_dia <- ejercicios %>% filter(dia == input$dia)
    selectInput("ejercicio", "Ejercicio:", ejercicios_dia$ejercicio)
  })
  
  # Obtener músculo principal del ejercicio seleccionado
  musculo_actual <- reactive({
    ejercicios %>% 
      filter(ejercicio == input$ejercicio) %>% 
      pull(musculo_principal) %>% 
      first()
  })
  
  # Guardar nuevo registro
  observeEvent(input$guardar, {
    req(input$ejercicio, input$peso > 0, input$repeticiones > 0)
    
    rm_estimado <- calcular_1rm(input$peso, input$repeticiones)
    volumen <- input$peso * input$repeticiones
    
    nuevo_registro <- data.frame(
      fecha = input$fecha,
      usuario = user_data$username,
      ejercicio = input$ejercicio,
      musculo = musculo_actual(),
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
    
    # Resetear inputs
    reset("peso")
    reset("repeticiones")
    reset("notas")
    showNotification("Entrenamiento guardado!", type = "message")
  })
  
  # Procesar foto de progreso
  observeEvent(input$foto_progreso, {
    req(input$foto_progreso)
    
    # En una app real, aquí guardarías el archivo en un directorio seguro
    nueva_foto <- data.frame(
      fecha = input$fecha,
      usuario = user_data$username,
      ruta = input$foto_progreso$name  # En producción usaría input$foto_progreso$datapath
    )
    
    fotos_progreso(rbind(fotos_progreso(), nueva_foto))
    saveRDS(fotos_progreso(), "fotos_progreso.rds")
  })
  
  # Mostrar foto más reciente
  output$foto_reciente <- renderUI({
    fotos_usuario <- fotos_progreso() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(desc(fecha))
    
    if (nrow(fotos_usuario) > 0) {
      tagList(
        h5("Última foto de progreso:"),
        tags$img(src = fotos_usuario$ruta[1], width = "100%", 
                 style = "border-radius: 5px; border: 1px solid #ddd;")
      )
    }
  })
  
  # Gráfico de progresión
  output$progresion_plot <- renderPlot({
    req(nrow(registros()) > 0)
    datos_usuario <- registros() %>% 
      filter(usuario == user_data$username, ejercicio == input$ejercicio)
    
    if (nrow(datos_usuario) == 0) return(NULL)
    
    ggplot(datos_usuario, aes(x = fecha, y = peso)) +
      geom_line(aes(color = "Peso")) +
      geom_point(aes(color = "Peso")) +
      geom_line(aes(y = rm_estimado, color = "1RM Estimado")) +
      geom_point(aes(y = rm_estimado, color = "1RM Estimado")) +
      labs(title = paste("Progresión en", input$ejercicio),
           x = "Fecha", y = "Peso (kg)", color = "Métrica") +
      scale_color_manual(values = c("Peso" = "#3498db", "1RM Estimado" = "#e74c3c")) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  # Gráfico de volumen por grupo muscular
  output$volumen_muscular_plot <- renderPlot({
    req(nrow(registros()) > 0)
    
    registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(musculo, semana = floor_date(fecha, "week")) %>% 
      summarise(volumen_total = sum(volumen), .groups = "drop") %>% 
      ggplot(aes(x = semana, y = volumen_total, fill = musculo)) +
      geom_col(position = "dodge") +
      labs(title = "Volumen de Entrenamiento por Grupo Muscular",
           x = "Semana", y = "Volumen Total (kg x reps)") +
      theme_minimal()
  })
  
  # Gráfico de progresión de fuerza
  output$fuerza_plot <- renderPlot({
    req(nrow(registros()) > 0)
    
    registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(ejercicio) %>% 
      filter(fecha == max(fecha)) %>% 
      ggplot(aes(x = reorder(ejercicio, rm_estimado), y = rm_estimado)) +
      geom_col(fill = "#2ecc71") +
      coord_flip() +
      labs(title = "1RM Estimado por Ejercicio (Última sesión)",
           x = "", y = "1RM Estimado (kg)") +
      theme_minimal()
  })
  
  # Tabla de máximos personales
  output$maximos_table <- renderDT({
    req(nrow(registros()) > 0)
    
    registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(ejercicio) %>% 
      summarise(
        `1RM Máximo` = max(rm_estimado),
        `Peso Máximo` = max(peso),
        `Última sesión` = max(fecha),
        `Mejora %` = paste0(round((max(peso) - min(peso)) / min(peso) * 100, 1), "%"),
        .groups = "drop"
      ) %>% 
      arrange(desc(`1RM Máximo`)) %>% 
      datatable(options = list(pageLength = 5))
  })
  
  # Tabla de historial completo
  output$historial_table <- renderDT({
    req(nrow(registros()) > 0)
    
    registros() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(desc(fecha)) %>% 
      select(Fecha = fecha, Ejercicio = ejercicio, Peso = peso, Reps = repeticiones, 
             RPE = esfuerzo, `Al fallo` = al_fallo, Notas = notas) %>% 
      datatable(options = list(pageLength = 10))
  })
  
  # Galería de fotos de progreso
  output$galeria_fotos <- renderUI({
    fotos_usuario <- fotos_progreso() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(desc(fecha))
    
    if (nrow(fotos_usuario) == 0) {
      return(h4("No hay fotos de progreso aún."))
    }
    
    fluidRow(
      lapply(1:nrow(fotos_usuario), function(i) {
        column(4,
               h5(fotos_usuario$fecha[i]),
               tags$img(src = fotos_usuario$ruta[i], width = "100%",
                        style = "border-radius: 5px; border: 1px solid #ddd; margin-bottom: 15px;")
        )
      })
    )
  })
  
  # Recomendaciones basadas en progreso
  output$recomendacion_progreso <- renderUI({
    req(nrow(registros()) > 0 && input$ejercicio)
    
    datos_ejercicio <- registros() %>% 
      filter(usuario == user_data$username, ejercicio == input$ejercicio) %>% 
      arrange(fecha)
    
    if (nrow(datos_ejercicio) < 2) return(NULL)
    
    ultimo <- tail(datos_ejercicio, 1)
    anterior <- tail(datos_ejercicio, 2)[1,]
    
    diferencia_peso <- ultimo$peso - anterior$peso
    diferencia_reps <- ultimo$repeticiones - anterior$repeticiones
    
    if (diferencia_peso > 0 || diferencia_reps > 0) {
      recomendacion <- "¡Buen progreso! Considera aumentar ligeramente la carga en tu próxima sesión."
    } else if (diferencia_peso == 0 && diferencia_reps == 0) {
      recomendacion <- "Estancamiento detectado. Prueba variar el ejercicio o el esquema de series/repeticiones."
    } else {
      recomendacion <- "Disminución en el rendimiento. Revisa tu recuperación y nutrición."
    }
    
    if (ultimo$esfuerzo >= 8 && ultimo$al_fallo) {
      recomendacion <- paste(recomendacion, "¡Cuidado con el sobreentrenamiento! Considera un día de descanso.")
    }
    
    div(class = "recomendacion",
        h4("Recomendación:"),
        p(recomendacion),
        p(paste("Último 1RM estimado:", ultimo$rm_estimado, "kg"))
    )
  })
  
  # Descargar historial
  output$descargar_historial <- downloadHandler(
    filename = function() {
      paste("historial_entrenamiento_", user_data$username, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(registros() %>% filter(usuario == user_data$username), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
