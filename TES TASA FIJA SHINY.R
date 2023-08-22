# *** MODELO TES TASA FIJA ***
#Inmportamos librerías
library(readxl)
library(dplyr)
library(shiny)
library(lubridate)

# Importamos la base de Datos
BASE_TES <- read_excel(path="C:\\Users\\Usuario\\Documents\\Sebastian\\Portafolio\\Shiny Apps\\TES TASA FIJA\\Base TES.xlsx",
                       sheet = 1,col_types = c("text","numeric","numeric","numeric","numeric") )
colnames(BASE_TES) <- c("titulo","dia","mes","año","cupón","Vencimiento")

TES <- c(BASE_TES$titulo)

# CREAMOS LA SHINY

interfaz <- fluidPage(
  
  titlePanel("CALCULADORA TES TASA FIJA"),
  
  # INPUTS DE LA INTERFAZ
  
  sidebarLayout(
    # Parte de los inputs
    sidebarPanel(
      selectInput("tes_elegido", "Seleccione el título:", choices = TES),
      numericInput("VN", "Ingrese el nominal en millones", value = 1000),
      numericInput("TIR", "Ingresa tasa de descuento sin % EJ:10.25", value = 10),
      dateInput("negociacion", "Ingrese Fecha de Negociación")
    ),
    
    # Parte de los outputs
    mainPanel(
      verbatimTextOutput("Precio_Limpio")
    )))



# Inputs

proceso <- function(input, output) {
  output$Precio_Limpio <- renderText({
    
    negociacion <- input$negociacion
    tes_seleccionado <- input$tes_elegido
    VN <- (input$VN)*1000000
    TIR <- (input$TIR)/100
    
    # Traemos los datos específicos del Título Elegido (Buscar V)
    tes_seleccionado <- data.frame(tes_seleccionado)
    colnames(tes_seleccionado) <- c("titulo")
    
    new_dataframe <- left_join(tes_seleccionado, BASE_TES, "titulo")
    colnames(new_dataframe) <- c("TITULO", "DIA", "MES", "AÑO", "CUPÓN")
    
    # Extraemos los datos puntuales
    tasa_cupon <- new_dataframe$CUPÓN
    
    vencimiento <- ymd(paste(new_dataframe$AÑO, new_dataframe$MES, new_dataframe$DIA))
    
    fechacupon1 <- 0
    if (ymd(paste(year(negociacion), new_dataframe$MES, new_dataframe$DIA)) > negociacion) {
      fechacupon1 <- ymd(paste(year(negociacion), new_dataframe$MES, new_dataframe$DIA))
    } else {
      fechacupon1 <- ymd(paste((year(negociacion)) + 1, new_dataframe$MES, new_dataframe$DIA))
    }
    
    ## Función para verificar si un año es bisiesto
    es_bisiesto <- function(anio) {
      return((anio %% 4 == 0) & ((anio %% 100 != 0) | (anio %% 400 == 0)))
    }
    
    ## Función para calcular la diferencia de días ajustada excluyendo años bisiestos
    dif_dias_base365 <- function(desde, hasta) {
      dif_dias <- as.integer(difftime(hasta, desde, units = "days"))
      
      for (i in (year(desde) + 1):(year(hasta))) {
        if (es_bisiesto(i)) {
          dif_dias <- dif_dias - 1
        }
      }
      
      return(dif_dias)
    }
    
    ## Calcular la diferencia de días ajustada
    dias_al_vencimiento <- dif_dias_base365(negociacion, vencimiento)
    
    numero_cupones <- ceiling(dias_al_vencimiento/365)
    
    valor_cupon <- new_dataframe$CUPÓN * VN
    
    # PROCEDIMIENTO
    
    descuento_flujos <- as.data.frame(matrix(data = 0, nrow = (numero_cupones + 1), ncol = 2))
    colnames(descuento_flujos) <- c("VP Cupones", "Días")
    
    for (i in 1:numero_cupones) {
      # En matriz calculamos los días de Neg a cada cupón
      
      descuento_flujos[1, 2] <- dif_dias_base365(negociacion, fechacupon1)
      descuento_flujos[(i + 1), 2] <- descuento_flujos[i, 2] + 365
      
      # Ahora traemos flujos a VP
      descuento_flujos[i, 1] <- valor_cupon / ((1 + TIR)^((descuento_flujos[i, 2]) / 365))
      descuento_flujos[(numero_cupones + 1), 1] <- VN / ((1 + TIR)^(dias_al_vencimiento/365))
    }
    # Calculamos VG, PS, CC, PL
    VG <- sum(descuento_flujos[, 1])
    
    PS <- (VG / VN) * 100
    
    CC <- new_dataframe$CUPÓN * ((365 - descuento_flujos[1, 2]) / 365) * 100
    
    PL <- PS - CC
    
    # Actualizar resultado aquí
    Precio_Limpio <- paste("El precio limpio es:", PL)
    
    
    
    # Retornar el resultado
    Precio_Limpio
  })
}

# Crear la aplicación Shiny
shinyApp(ui = interfaz, server = proceso)


