Documentación del Gym Progress Tracker (Shiny App)

Descripción General
Aplicación Shiny para registrar y monitorear el progreso en el gimnasio, con las siguientes funcionalidades:
- Registro de entrenamientos con peso, repeticiones y percepción de esfuerzo
- Cálculo automático de 1RM (Repetición Máxima)
- Visualización de progreso por ejercicio
- Seguimiento de volumen de entrenamiento por grupo muscular
- Historial completo de entrenamientos
- Sistema de fotos de progreso
- Recomendaciones basadas en el rendimiento

Estructura del Código

1. Librerías Requeridas

library(shiny)       # Framework para la aplicación
library(ggplot2)     # Visualizaciones
library(dplyr)       # Manipulación de datos
library(lubridate)   # Manejo de fechas
library(shinyjs)     # Funcionalidades JavaScript
library(DT)          # Tablas interactivas
library(shinyWidgets) # Mejores inputs UI


2. Datos de Ejemplo

ejercicios <- data.frame(
  dia = rep(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes"), each = 4),
  ejercicio = c("Press de Banca Plano", "Press Inclinado con Mancuernas", ...),
  musculo_principal = c("Pecho", "Pecho", "Pecho", "Tríceps", ...),
  tipo = rep(c("Hipertrofia", "Fuerza", "Control excéntrico", "Resistencia"), 5)
)


3. Funciones Clave

calcular_1rm(peso, repeticiones)
Calcula la repetición máxima (1RM) usando la fórmula de Epley.

Parámetros:
- `peso`: Peso levantado en kg (numeric)
- `repeticiones`: Número de repeticiones realizadas (integer)

Retorna:
- Valor numérico del 1RM estimado

4. Interfaz de Usuario (UI)

Componentes Principales:
- Sistema de autenticación (usuario/contraseña básico)
- Panel de registro:
  - Selector de día y ejercicio
  - Inputs para peso, repeticiones, RPE (escala 1-10)
  - Checkbox para indicar fallo muscular
  - Área para notas
- Pestañas de visualización:
  - Progresión (gráfico de evolución)
  - Métricas (volumen y fuerza)
  - Historial (tabla de registros)
  - Progreso visual (galería de fotos)

5. Lógica del Servidor (Server)

Módulos Principales:
1. Autenticación:
   - Valida credenciales (simulado)
   - Mantiene estado de sesión

2. Almacenamiento de Datos:
   - Guarda registros en archivo RDS
   - Maneja fotos de progreso

3. Cálculos:
   - Estimación de 1RM
   - Cálculo de volumen (peso x repeticiones)
   - Progreso semanal por grupo muscular

4. Visualizaciones:
   - Gráfico de progresión por ejercicio
   - Volumen muscular semanal
   - Fuerza máxima por ejercicio
   - Tabla de máximos personales

5. Recomendaciones:
   - Analiza progreso entre sesiones
   - Sugiere ajustes basados en rendimiento

6. Ejecución de la Aplicación
shinyApp(ui = ui, server = server)


Manual de Uso

1. Iniciar Sesión:
   - Ingresar cualquier usuario/contraseña (el sistema es simulado)

2. Registrar Entrenamiento:
   - Seleccionar día y ejercicio
   - Ingresar peso, repeticiones y esfuerzo percibido
   - Opcional: marcar si llegó al fallo muscular y agregar notas

3. Ver Progreso:
   - Pestaña "Progresión": Gráfico de evolución del peso y 1RM
   - Pestaña "Métricas": Resumen de volumen y fuerza
   - Pestaña "Historial": Tabla completa de registros

4. Subir Fotos:
   - Usar el botón en el panel lateral para subir fotos de progreso

5. Exportar Datos:
   - Botón "Descargar Historial" para exportar a CSV

#Consideraciones de Implementación

1. Almacenamiento:
   - Los datos se guardan localmente en archivos RDS
   - En producción, recomiendo usar una base de datos real

2. Seguridad:
   - El sistema de autenticación es básico (solo para demostración)
   - En producción, implementar un sistema seguro con hash de contraseñas

3. Fotos:
   - Actualmente solo guarda nombres de archivos
   - En producción, almacenar físicamente las imágenes en un directorio seguro

Diagrama de Flujo


Usuario → Autenticación → Registro Entrenamiento → Almacenamiento
    ↓
Visualización Datos ← Análisis Progreso ← Recomendaciones


Notas de Desarrollo

1. La aplicación usa reactividad de Shiny para actualizar todas las visualizaciones automáticamente
2. El diseño responsivo funciona bien en pantallas medianas y grandes
3. Se incluyen tooltips y mensajes de ayuda contextual

Posibles Mejoras

1. Integración con wearables (Apple Health, Google Fit)
2. Sistema de objetivos y logros
3. Planificación automática de rutinas
4. Análisis de tendencias a largo plazo

Si puedes mejorarlo estaria bien. 
