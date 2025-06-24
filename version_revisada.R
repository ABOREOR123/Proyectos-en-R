library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(plotly)
library(shinydashboard)
library(bslib)
library(googlesheets4)
library(digest)
library(rsconnect)

# Configuración visual
my_theme <- bs_theme(
  version = 5,
  bg = "#f8f9fa",
  fg = "#212529",
  primary = "#2c3e50",
  secondary = "#6c757d",
  success = "#28a745",
  info = "#17a2b8",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = font_google("Roboto")
)

# Datos de entrenamiento específicos
ejercicios <- data.frame(
  dia = c(
    rep("Lunes", 8),    # Pecho y Tríceps
    rep("Martes", 6),   # Piernas (Cuádriceps)
    rep("Miércoles", 7), # Espalda y Brazos
    rep("Jueves", 6),    # Piernas (Femoral)
    rep("Viernes", 7)    # Hombros y Pecho
  ),
  ejercicio = c(
    # Lunes: Pecho y Tríceps
    "Press de Banca Plano", "Press Inclinado con Mancuernas", "Aperturas en Polea", 
    "Fondos en Paralelas", "Press Declinado", "Extensiones de Tríceps en Polea Alta",
    "Fondos en Máquina", "Press Francés",
    
    # Martes: Piernas (Cuádriceps)
    "Sentadillas Libres", "Prensa de Piernas", "Extensiones de Cuádriceps", 
    "Zancadas", "Sentadilla Búlgara", "Hack Squat",
    
    # Miércoles: Espalda y Brazos
    "Dominadas con Peso", "Remo con Barra", "Remo en Polea Baja", 
    "Curl de Bíceps con Barra", "Curl Martillo", "Curl Concentrado",
    "Jalón al Pecho",
    
    # Jueves: Piernas (Femoral)
    "Peso Muerto Convencional", "Curl Femoral Acostado", "Peso Muerto Rumano", 
    "Zancadas con Desplazamiento", "Hip Thrust", "Curl Femoral de Pie",
    
    # Viernes: Hombros y Pecho
    "Press Militar", "Elevaciones Laterales", "Elevaciones Frontales", 
    "Face Pull", "Press Arnold", "Aperturas con Mancuernas",
    "Encogimientos con Barra"
  ),
  musculo_principal = c(
    # Lunes: Pecho y Tríceps
    "Pecho", "Pecho", "Pecho", "Tríceps", "Pecho", "Tríceps", "Tríceps", "Tríceps",
    
    # Martes: Cuádriceps
    "Cuádriceps", "Cuádriceps", "Cuádriceps", "Cuádriceps", "Cuádriceps", "Cuádriceps",
    
    # Miércoles: Espalda y Brazos
    "Espalda", "Espalda", "Espalda", "Bíceps", "Bíceps", "Bíceps", "Espalda",
    
    # Jueves: Femoral
    "Femorales", "Femorales", "Femorales", "Femorales", "Glúteos", "Femorales",
    
    # Viernes: Hombros y Pecho
    "Hombros", "Hombros", "Hombros", "Hombros", "Hombros", "Pecho", "Trapecios"
  ),
  tipo = c(
    # Lunes: Pecho y Tríceps
    "Hipertrofia", "Hipertrofia", "Aislamiento", "Fuerza", "Hipertrofia", "Aislamiento", "Hipertrofia", "Aislamiento",
    
    # Martes: Cuádriceps
    "Fuerza", "Hipertrofia", "Aislamiento", "Hipertrofia", "Aislamiento", "Fuerza",
    
    # Miércoles: Espalda y Brazos
    "Fuerza", "Hipertrofia", "Hipertrofia", "Hipertrofia", "Hipertrofia", "Aislamiento", "Hipertrofia",
    
    # Jueves: Femoral
    "Fuerza", "Aislamiento", "Hipertrofia", "Hipertrofia", "Hipertrofia", "Aislamiento",
    
    # Viernes: Hombros y Pecho
    "Fuerza", "Aislamiento", "Aislamiento", "Aislamiento", "Hipertrofia", "Aislamiento", "Fuerza"
  ),
  dificultad = rep(c("Principiante", "Intermedio", "Avanzado"), length.out = 34),
  equipamiento = c(
    # Lunes: Pecho y Tríceps
    "Barra", "Mancuernas", "Máquina", "Peso Corporal", "Barra", "Polea", "Máquina", "Barra",
    
    # Martes: Cuádriceps
    "Barra", "Máquina", "Máquina", "Mancuernas", "Mancuernas", "Máquina",
    
    # Miércoles: Espalda y Brazos
    "Barra", "Barra", "Polea", "Barra", "Mancuernas", "Mancuernas", "Polea",
    
    # Jueves: Femoral
    "Barra", "Máquina", "Barra", "Mancuernas", "Barra", "Máquina",
    
    # Viernes: Hombros y Pecho
    "Barra", "Mancuernas", "Mancuernas", "Polea", "Mancuernas", "Mancuernas", "Barra"
  )
)

# CMR1, No se como lo hice funcionar 7u7
calcular_1rm <- function(peso, repeticiones) {
  if (repeticiones <= 1) return(peso)
  if (repeticiones <= 6) {
    round(peso * (1 + repeticiones / 30), 1)
  } else if (repeticiones <= 12) {
    round(peso * (1 + repeticiones / 35), 1)
  } else {
    round(peso * (1 + repeticiones / 40), 1)
  }
}

# Función para calcular volumen de entrenamiento
calcular_volumen <- function(peso, repeticiones, series) {
  if(any(c(peso, repeticiones, series) <= 0)) {
    stop("Todos los valores deben ser positivos")
  }
  peso * repeticiones * series
}

# UI mejorada con sistema de autenticación
ui <- navbarPage(
  title = "GYM Spart",
  id = "nav",
  theme = my_theme,
  header = useShinyjs(),
  
  # Página de autenticación (inicial)
  tabPanel(
    "🔒 Autenticación",
    value = "auth",
    div(
      id = "login_panel",
      class = "auth-panel",
      h2("Inicio de Sesión", class = "text-center"),
      textInput("login_user", "Usuario:", placeholder = "tu_usuario"),
      passwordInput("login_pwd", "Contraseña:"),
      actionButton("login", "Ingresar", class = "btn-primary btn-block"),
      hr(),
      actionButton("show_register", "Crear nueva cuenta", class = "btn-link btn-block"),
      textOutput("auth_message")
    ),
    
    hidden(
      div(
        id = "register_panel",
        class = "auth-panel",
        h2("Registro", class = "text-center"),
        textInput("reg_user", "Nuevo usuario:", placeholder = "min. 4 caracteres"),
        passwordInput("reg_pwd1", "Contraseña:"),
        passwordInput("reg_pwd2", "Confirmar contraseña:"),
        numericInput("reg_altura", "Altura (cm):", value = 170, min = 100, max = 250),
        selectInput("reg_objetivo", "Objetivo principal:", 
                    c("Ganar músculo", "Perder grasa", "Mejorar fuerza", "Aumentar resistencia")),
        actionButton("register", "Crear cuenta", class = "btn-success btn-block"),
        hr(),
        actionButton("show_login", "Volver a inicio de sesión", class = "btn-link btn-block"),
        textOutput("reg_message")
      )
    )
  ),
  
  # Página principal (solo visible después de autenticación)
  tabPanel(
    "🏠 Inicio", 
    value = "home",
    sidebarLayout(
      sidebarPanel(
        selectInput("dia", "Día de entrenamiento:", unique(ejercicios$dia)),
        uiOutput("ejercicio_selector"),
        dateInput("fecha", "Fecha:", value = Sys.Date()),
        numericInput("series", "Series completadas:", value = 3, min = 1, max = 10),
        numericInput("peso", "Peso (kg):", value = 0, min = 0, step = 0.5),
        numericInput("repeticiones", "Repeticiones por serie:", value = 0, min = 0),
        sliderInput("esfuerzo", "Percepción de esfuerzo (RPE 1-10):", 1, 10, 7),
        awesomeCheckboxGroup(
          inputId = "sensaciones",
          label = "Sensaciones durante el ejercicio:", 
          choices = c("Fácil", "Controlado", "Desafiante", "Difícil", "Extremo"),
          selected = "Controlado"
        ),
        awesomeCheckbox("al_fallo", "¿Llegaste al fallo muscular?", FALSE),
        textAreaInput("notas", "Notas adicionales:", placeholder = "Ej: técnica, dolor, progresión..."),
        actionButton("guardar", "Guardar Entrenamiento", class = "btn-primary"),
        hr(),
        actionButton("logout", "Cerrar sesión", class = "btn-danger")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Resumen", 
                   uiOutput("bienvenida_usuario"),
                   plotlyOutput("progreso_general_plot"),
                   uiOutput("estadisticas_rapidas")),
          tabPanel("Progresión", 
                   plotlyOutput("progresion_plot"),
                   uiOutput("recomendacion_progreso"),
                   fluidRow(
                     column(6, plotOutput("progresion_rm_plot")),
                     column(6, plotOutput("progresion_volumen_plot"))
                   )),
          tabPanel("Métricas", 
                   navset_tab(
                     nav_panel("Volumen", plotlyOutput("volumen_muscular_plot")),
                     nav_panel("Fuerza", plotlyOutput("fuerza_plot")),
                     nav_panel("Frecuencia", plotlyOutput("frecuencia_plot"))
                   ),
                   card(
                     card_header("Máximos Personales"),
                     DTOutput("maximos_table"),
                     uiOutput("pr_next")
                   )),
          tabPanel("Historial", 
                   DTOutput("historial_table"),
                   downloadButton("descargar_historial", "Descargar Historial", class = "btn-success"),
                   downloadButton("exportar_google", "Exportar a Google Sheets", class = "btn-info")),
          tabPanel("Configuración",
                   numericInput("meta_peso", "Meta de peso (kg):", value = 70, min = 40, max = 200),
                   selectInput("nivel_experiencia", "Nivel de experiencia:", 
                               c("Principiante" = "beginner", 
                                 "Intermedio" = "intermediate", 
                                 "Avanzado" = "advanced")),
                   actionButton("guardar_config", "Guardar configuración", class = "btn-primary"),
                   hr(),
                   card_header("Datos de salud"),
                   numericInput("peso_actual", "Peso actual (kg):", value = 70, min = 30, max = 200),
                   numericInput("grasa_corporal", "% Grasa corporal:", value = 15, min = 5, max = 50, step = 0.1),
                   actionButton("guardar_salud", "Guardar datos de salud", class = "btn-info"))
        )
      )
    )
  )
)

# Server 
server <- function(input, output, session) {
  
  # Autenticación mejorada con encriptación
  user_data <- reactiveValues(
    logged_in = FALSE,
    username = NULL,
    user_config = list(),
    users = data.frame(
      username = character(),
      password_hash = character(),
      altura = numeric(),
      objetivo = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Cargar usuarios existentes con encriptación
  if (file.exists("users_secure.rds")) {
    user_data$users <- readRDS("users_secure.rds")
  }
  
  # Mostrar/ocultar paneles de autenticación
  observeEvent(input$show_register, {
    hide("login_panel")
    show("register_panel")
  })
  
  observeEvent(input$show_login, {
    hide("register_panel")
    show("login_panel")
  })
  
  # Iniciar sesión
  observeEvent(input$login, {
    req(input$login_user, input$login_pwd)
    
    user_row <- user_data$users %>% filter(username == input$login_user)
    
    if (nrow(user_row) == 1 && 
        digest(input$login_pwd, algo = "sha256") == user_row$password_hash) {
      user_data$logged_in <- TRUE
      user_data$username <- input$login_user
      load_user_config()
      
      # Actualizar la interfaz
      updateNavbarPage(session, "nav", selected = "home")
      
      # Limpiar campos de autenticación
      updateTextInput(session, "login_user", value = "")
      updateTextInput(session, "login_pwd", value = "")
      
      showNotification(paste("Bienvenido,", user_data$username), type = "message")
    } else {
      output$auth_message <- renderText("Usuario o contraseña incorrectos")
    }
  })
  
  # Registro de nuevo usuario
  observeEvent(input$register, {
    req(input$reg_user, input$reg_pwd1, input$reg_pwd2)
    
    if (nchar(input$reg_user) < 4) {
      output$reg_message <- renderText("El usuario debe tener al menos 4 caracteres")
    } else if (input$reg_pwd1 != input$reg_pwd2) {
      output$reg_message <- renderText("Las contraseñas no coinciden")
    } else if (input$reg_user %in% user_data$users$username) {
      output$reg_message <- renderText("El usuario ya existe")
    } else {
      new_user <- data.frame(
        username = input$reg_user,
        password_hash = digest(input$reg_pwd1, algo = "sha256"),
        altura = input$reg_altura,
        objetivo = input$reg_objetivo,
        stringsAsFactors = FALSE
      )
      
      user_data$users <- rbind(user_data$users, new_user)
      saveRDS(user_data$users, "users_secure.rds")
      
      # Autenticar al nuevo usuario automáticamente
      user_data$logged_in <- TRUE
      user_data$username <- input$reg_user
      create_user_config()
      
      # Actualizar la interfaz
      updateNavbarPage(session, "nav", selected = "home")
      
      # Limpiar campos de registro
      updateTextInput(session, "reg_user", value = "")
      updateTextInput(session, "reg_pwd1", value = "")
      updateTextInput(session, "reg_pwd2", value = "")
      
      showNotification(paste("Cuenta creada con éxito. Bienvenido,", user_data$username), type = "message")
    }
  })
  
  # Cargar configuración de usuario
  load_user_config <- function() {
    config_file <- paste0("user_config_", user_data$username, ".rds")
    if (file.exists(config_file)) {
      user_data$user_config <- readRDS(config_file)
    } else {
      create_user_config()
    }
  }
  
  # Crear configuración inicial de usuario
  create_user_config <- function() {
    user_data$user_config <- list(
      meta_peso = 70,
      nivel_experiencia = "intermediate",
      peso_actual = 70,
      grasa_corporal = 15,
      last_login = Sys.Date()
    )
    save_user_config()
  }
  
  # Guardar configuración de usuario
  save_user_config <- function() {
    config_file <- paste0("user_config_", user_data$username, ".rds")
    saveRDS(user_data$user_config, config_file)
  }
  
  # Sistema de almacenamiento 
  registros <- reactiveVal(data.frame(
    fecha = as.Date(character()),
    usuario = character(),
    ejercicio = character(),
    musculo = character(),
    series = numeric(),
    peso = numeric(),
    repeticiones = numeric(),
    esfuerzo = numeric(),
    sensaciones = character(),
    al_fallo = logical(),
    notas = character(),
    rm_estimado = numeric(),
    volumen = numeric(),
    stringsAsFactors = FALSE
  ))
  
  datos_salud <- reactiveVal(data.frame(
    fecha = as.Date(character()),
    usuario = character(),
    peso = numeric(),
    grasa_corporal = numeric(),
    notas = character(),
    stringsAsFactors = FALSE
  ))
  
  # Cargar datos guardados al iniciar
  observe({
    if (file.exists("registros_mejorados.rds")) {
      all_records <- readRDS("registros_mejorados.rds")
      if (user_data$logged_in) {
        user_records <- all_records %>% filter(usuario == user_data$username)
        registros(user_records)
      } else {
        registros(all_records)
      }
    }
    
    if (file.exists("datos_salud.rds")) {
      all_health <- readRDS("datos_salud.rds")
      if (user_data$logged_in) {
        user_health <- all_health %>% filter(usuario == user_data$username)
        datos_salud(user_health)
      } else {
        datos_salud(all_health)
      }
    }
  })
  
  # Selector de ejercicios dinámico
  output$ejercicio_selector <- renderUI({
    req(input$dia)
    ejercicios_dia <- ejercicios %>% filter(dia == input$dia)
    
    pickerInput(
      inputId = "ejercicio",
      label = "Ejercicio:", 
      choices = ejercicios_dia$ejercicio,
      options = list(
        `live-search` = TRUE,
        size = 5
      )
    )
  })
  
  # Función para obtener músculo principal del ejercicio actual
  musculo_actual <- reactive({
    req(input$ejercicio)
    ejercicios %>% 
      filter(ejercicio == input$ejercicio) %>% 
      pull(musculo_principal) %>% 
      first()
  })
  
  # Guardar nuevo registro mejorado
  observeEvent(input$guardar, {
    req(input$ejercicio, input$peso > 0, input$repeticiones > 0, input$series > 0, user_data$logged_in)
    
    rm_estimado <- calcular_1rm(input$peso, input$repeticiones)
    volumen <- calcular_volumen(input$peso, input$repeticiones, input$series)
    sensaciones_str <- paste(input$sensaciones, collapse = ", ")
    
    nuevo_registro <- data.frame(
      fecha = input$fecha,
      usuario = user_data$username,
      ejercicio = input$ejercicio,
      musculo = musculo_actual(),
      series = input$series,
      peso = input$peso,
      repeticiones = input$repeticiones,
      esfuerzo = input$esfuerzo,
      sensaciones = sensaciones_str,
      al_fallo = input$al_fallo,
      notas = input$notas,
      rm_estimado = rm_estimado,
      volumen = volumen,
      stringsAsFactors = FALSE
    )
    
    # Guardar en el archivo completo
    if (file.exists("registros_mejorados.rds")) {
      all_records <- readRDS("registros_mejorados.rds")
      all_records <- rbind(all_records, nuevo_registro)
    } else {
      all_records <- nuevo_registro
    }
    
    saveRDS(all_records, "registros_mejorados.rds")
    
    # Actualizar los registros visibles para el usuario actual
    registros(all_records %>% filter(usuario == user_data$username))
    
    # Resetear inputs
    reset("peso")
    reset("repeticiones")
    reset("notas")
    showNotification("Entrenamiento guardado exitosamente!", type = "message")
  })
  
  # Gráficos interactivos con plotly
  output$progresion_plot <- renderPlotly({
    req(nrow(registros()) > 0, user_data$logged_in, input$ejercicio)
    
    datos_usuario <- registros() %>% 
      filter(usuario == user_data$username, ejercicio == input$ejercicio) %>% 
      arrange(fecha)  # Ordenar por fecha
    
    if (nrow(datos_usuario) == 0) return(NULL)
    
    plot_ly(datos_usuario, x = ~fecha) %>%
      add_trace(y = ~peso, name = "Peso (kg)", type = 'scatter', mode = 'lines+markers',
                line = list(color = '#3498db'), marker = list(color = '#3498db'),
                hoverinfo = 'text',
                text = ~paste('Fecha:', fecha, '<br>Peso:', peso, 'kg<br>Reps:', repeticiones)) %>%
      add_trace(y = ~rm_estimado, name = "1RM Estimado", type = 'scatter', mode = 'lines+markers',
                line = list(color = '#e74c3c'), marker = list(color = '#e74c3c'),
                hoverinfo = 'text',
                text = ~paste('1RM Estimado:', rm_estimado, 'kg')) %>%
      layout(title = paste("Progresión en", input$ejercicio),
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Peso (kg)"),
             hovermode = "closest")
  })
  
  # Gráfico de volumen por grupo muscular interactivo
  output$volumen_muscular_plot <- renderPlotly({
    req(nrow(registros()) > 0, user_data$logged_in)
    
    datos_volumen <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(musculo, semana = floor_date(fecha, "week")) %>% 
      summarise(volumen_total = sum(volumen), .groups = "drop") %>% 
      arrange(semana)  # Ordenar por fecha
    
    plot_ly(datos_volumen, x = ~semana, y = ~volumen_total, color = ~musculo, type = 'bar') %>%
      layout(title = "Volumen de Entrenamiento por Grupo Muscular",
             xaxis = list(title = "Semana"),
             yaxis = list(title = "Volumen Total (kg x reps)"),
             barmode = 'stack')
  })
  
  # Gráfico de progresión de fuerza mejorado
  output$fuerza_plot <- renderPlotly({
    req(nrow(registros()) > 0, user_data$logged_in)
    
    datos_fuerza <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(ejercicio) %>% 
      filter(fecha == max(fecha)) %>% 
      arrange(desc(rm_estimado)) %>% 
      slice_head(n = 10)
    
    plot_ly(datos_fuerza, x = ~reorder(ejercicio, rm_estimado), y = ~rm_estimado, type = 'bar',
            color = ~musculo, hoverinfo = 'text',
            text = ~paste('Ejercicio:', ejercicio, '<br>1RM:', rm_estimado, 'kg<br>Fecha:', fecha)) %>%
      layout(title = "Top 10 - 1RM Estimado por Ejercicio",
             xaxis = list(title = ""),
             yaxis = list(title = "1RM Estimado (kg)"),
             showlegend = FALSE)
  })
  
  # Gráfico de frecuencia de entrenamiento
  output$frecuencia_plot <- renderPlotly({
    req(nrow(registros()) > 0, user_data$logged_in)
    
    datos_frecuencia <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(musculo, semana = floor_date(fecha, "week")) %>% 
      summarise(sesiones = n_distinct(fecha), .groups = "drop") %>% 
      arrange(semana)  # Ordenar por fecha
    
    plot_ly(datos_frecuencia, x = ~semana, y = ~sesiones, color = ~musculo, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Frecuencia de Entrenamiento por Grupo Muscular",
             xaxis = list(title = "Semana"),
             yaxis = list(title = "Sesiones por semana"))
  })
  
  # Tabla de máximos personales mejorada
  output$maximos_table <- renderDT({
    req(nrow(registros()) > 0, user_data$logged_in)
    
    datos_maximos <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(ejercicio, musculo) %>% 
      summarise(
        `1RM Máximo` = max(rm_estimado),
        `Peso Máximo` = max(peso),
        `Reps Máximas` = max(repeticiones),
        `Última sesión` = max(fecha),
        `Mejora %` = paste0(round((max(rm_estimado) - min(rm_estimado)) / min(rm_estimado) * 100, 1), "%"),
        .groups = "drop"
      ) %>% 
      arrange(desc(`1RM Máximo`))
    
    datatable(
      datos_maximos,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>% 
      formatStyle(
        'Mejora %',
        color = styleInterval(c(0, 10), c('red', 'orange', 'green')),
        fontWeight = 'bold'
      )
  })
  
  # Tabla de historial completo mejorada (ordenada por fecha)
  output$historial_table <- renderDT({
    req(nrow(registros()) > 0, user_data$logged_in)
    
    datos_historial <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(desc(fecha)) %>%  # Ordenar por fecha descendente
      select(
        Fecha = fecha, 
        Ejercicio = ejercicio, 
        Músculo = musculo,
        Series = series,
        Peso = peso, 
        Reps = repeticiones,
        RPE = esfuerzo,
        `Al fallo` = al_fallo,
        `1RM` = rm_estimado,
        Volumen = volumen,
        Notas = notas
      )
    
    datatable(
      datos_historial,
      filter = 'top',
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE,
        scrollY = "500px",
        scroller = TRUE,
        order = list(list(0, 'desc'))  # Ordenar por primera columna (Fecha) descendente
      ),
      rownames = FALSE
    )
  })
  
  # Panel de bienvenida personalizado
  output$bienvenida_usuario <- renderUI({
    req(user_data$logged_in)
    
    ultimo_entreno <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      arrange(desc(fecha)) %>% 
      slice(1)
    
    tagList(
      h3(paste("¡Bienvenido,", user_data$username, "!")),
      p(strong("Último entrenamiento: "), 
        ifelse(nrow(ultimo_entreno) > 0, 
               paste(format(ultimo_entreno$fecha, "%d/%m/%Y"), "-", ultimo_entreno$ejercicio),
               "Aún no hay registros")),
      hr()
    )
  })
  
  # Gráfico de progreso general (ordenado por fecha)
  output$progreso_general_plot <- renderPlotly({
    req(nrow(registros()) > 0, user_data$logged_in)
    
    datos_progreso <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      group_by(mes = floor_date(fecha, "month")) %>% 
      summarise(
        volumen_total = sum(volumen),
        rm_promedio = mean(rm_estimado),
        sesiones = n_distinct(fecha),
        .groups = "drop"
      ) %>% 
      arrange(mes)  # Ordenar por fecha
    
    plot_ly(datos_progreso) %>%
      add_trace(x = ~mes, y = ~volumen_total, name = "Volumen", type = 'bar',
                marker = list(color = '#3498db'),
                hoverinfo = 'text',
                text = ~paste('Mes:', format(mes, "%b %Y"), '<br>Volumen:', volumen_total)) %>%
      add_trace(x = ~mes, y = ~rm_promedio, name = "1RM Promedio", type = 'scatter', mode = 'lines+markers',
                yaxis = 'y2',
                line = list(color = '#e74c3c'),
                marker = list(color = '#e74c3c'),
                hoverinfo = 'text',
                text = ~paste('1RM Promedio:', round(rm_promedio, 1), 'kg')) %>%
      layout(title = "Progreso General Mensual",
             xaxis = list(title = "Mes"),
             yaxis = list(title = "Volumen Total", side = 'left'),
             yaxis2 = list(title = "1RM Promedio (kg)", overlaying = "y", side = "right"),
             hovermode = "x unified")
  })
  
  # Estadísticas rápidas
  output$estadisticas_rapidas <- renderUI({
    req(nrow(registros()) > 0, user_data$logged_in)
    
    stats <- registros() %>% 
      filter(usuario == user_data$username) %>% 
      summarise(
        total_sesiones = n_distinct(fecha),
        total_ejercicios = n_distinct(ejercicio),
        volumen_total = sum(volumen),
        mejor_1rm = max(rm_estimado),
        mejor_ejercicio = ejercicio[which.max(rm_estimado)]
      )
    
    tagList(
      fluidRow(
        valueBox(
          value = stats$total_sesiones,
          subtitle = "Sesiones registradas",
          icon = icon("calendar-check"),
          color = "blue"
        ),
        valueBox(
          value = stats$total_ejercicios,
          subtitle = "Ejercicios diferentes",
          icon = icon("dumbbell"),
          color = "green"
        ),
        valueBox(
          value = format(stats$volumen_total, big.mark = ","),
          subtitle = "Volumen total (kg x reps)",
          icon = icon("chart-line"),
          color = "orange"
        )
      ),
      fluidRow(
        valueBox(
          value = stats$mejor_1rm,
          subtitle = paste("Mejor 1RM en", stats$mejor_ejercicio),
          icon = icon("trophy"),
          color = "red"
        )
      )
    )
  })
  
  # Cerrar sesión
  observeEvent(input$logout, {
    user_data$logged_in <- FALSE
    user_data$username <- NULL
    updateNavbarPage(session, "nav", selected = "auth")
    showNotification("Sesión cerrada correctamente", type = "message")
  })
  
  # Exportar a Google Sheets
  output$exportar_google <- downloadHandler(
    filename = function() {
      paste("historial_entrenamiento_", user_data$username, "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      datos_exportar <- registros() %>% 
        filter(usuario == user_data$username) %>% 
        select(-usuario) %>% 
        arrange(desc(fecha))  # Ordenar por fecha descendente
      
      writexl::write_xlsx(datos_exportar, file)
    }
  )
  
  # Guardar configuración de usuario
  observeEvent(input$guardar_config, {
    user_data$user_config$meta_peso <- input$meta_peso
    user_data$user_config$nivel_experiencia <- input$nivel_experiencia
    save_user_config()
    showNotification("Configuración guardada!", type = "message")
  })
  
  # Guardar datos de salud
  observeEvent(input$guardar_salud, {
    nuevo_registro <- data.frame(
      fecha = Sys.Date(),
      usuario = user_data$username,
      peso = input$peso_actual,
      grasa_corporal = input$grasa_corporal,
      notas = "",
      stringsAsFactors = FALSE
    )
    
    # Guardar en el archivo completo
    if (file.exists("datos_salud.rds")) {
      all_health <- readRDS("datos_salud.rds")
      all_health <- rbind(all_health, nuevo_registro)
    } else {
      all_health <- nuevo_registro
    }
    
    saveRDS(all_health, "datos_salud.rds")
    datos_salud(all_health %>% filter(usuario == user_data$username))
    
    showNotification("Datos de salud guardados!", type = "message")
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
