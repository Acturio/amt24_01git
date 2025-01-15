# Directorio de trabajo
setwd("~/Escritorio/archivos-demografia")

# Carga de tablas: Censo 2010 y 2020
EDOMEX2010 <- read.csv("EDOMEX2010.csv")
EDOMEX2020 <- read.csv("EDOMEX2020.csv")

# Guardar tablas con Censos para futuros programas
save(EDOMEX2010, file = "EDOMEX2010.Rda")
save(EDOMEX2020, file = "EDOMEX2020.Rda")

# Piraminde poblacional
# Cargar pyramid
library(pyramid)
#help(pyramid)

# Reordenar la informacion en nuevos dataframes
piramide2010 <- data.frame(Hombres = EDOMEX2010$Hombres,
                           Mujeres = EDOMEX2010$Mujeres,
                           Edad = EDOMEX2010$Edad)

piramide2020 <- data.frame(Hombres = EDOMEX2020$Hombres,
                           Mujeres = EDOMEX2020$Mujeres,
                           Edad = EDOMEX2020$Edad)

# Se aplica la funcion pyramid a las tablas
pyramid(piramide2010)
pyramid(piramide2020)

#formato para la piramide
pyramid(piramide2010,
        Llab = "Hombres",
        Rlab = "Mujeres",
        Clab = "Edad",
        Rcol = "mediumorchid3",
        Lcol = "turquoise3",
        Cstep = 10,
        main = "Estado de México 2010",
        AxisFM = "d",
        Laxis = seq(0, 200000, 50000))

pyramid(piramide2020,
        Llab = "Hombres",
        Rlab = "Mujeres",
        Clab = "Edad",
        Rcol = "mediumvioletred",
        Lcol = "mediumspringgreen",
        Cstep = 10,
        main = "Estado de México 2020",
        AxisFM = "d",
        Laxis = seq(0, 200000, 50000))


#Calculo de indice de masculinidad 
IndiceMasculinidad2010<-(EDOMEX2010$Hombres/EDOMEX2010$Mujeres)*100
IndiceMasculinidad2020<-(EDOMEX2020$Hombres/EDOMEX2020$Mujeres)*100
#par(mar = c(1,1,1,1))
plot(IndiceMasculinidad2010, 
     type = "l",
     col="seagreen4",
     main="Indice de Masculinidad",
     ylab = "Indice de Masculinidad",
     xlab = "Edad")

lines(IndiceMasculinidad2020, type="l", col= "royalblue4")


#Tasa de crecimiento de la Entidad

crecimiento <- function(Censo1, Censo2, fecha1, fecha2){
        # Formula tasa de crecimiento poblacional:
        # 
        # pf = pi * e ^ (r * h)
        # 
        # Donde:
        # 
        # pi = poblacion inicial
        # pf = poblacion final
        # r = tasa de crecimiento (exponencial)
        # h = tiempo de crecimiento
        # 
        # ======================================================================
        # 
        # Asignacion de vriables: 
        # 
        PIH <- sum(Censo1$Hombres)
        # Pbblacion Inicial Hombres (del primer censo)
        PFH <- sum(Censo2$Hombres)
        # Poblacion Final Hombres (del segundo censo)
        PIM <- sum(Censo1$Mujeres)
        # Poblacion Inicial Mujeres (del primer censo)
        PFM <- sum(Censo2$Mujeres)
        # Poblacion Final Mujeres (del segundo censo)
        h <- as.numeric(difftime(fecha2, fecha1)) / 365
        #
        # Como queremos calcular la tasa de crecimiento (r), despejando r de la
        # igualdad anterior:
        # 
        # r = (log(pf / pi)) / h
        # 
        rH <- (log(PFH / PIH)) / h
        # tasa de crecimiento Hombres
        print(paste("La tasa de crecimiento exponencial de hombres es:", 
                    round(rH * 100, digits = 4), "%"))
        # 
        rM <- (log(PFM / PIM)) / h
        # tasa de crecimiento Mujeres
        print(paste("La tasa de crecimiento exponencial de mujeres es:", 
                    round(rM * 100, digits = 4), "%"))
        # 
        tasas <- c(rH, rM)
        # union de las tasas en un verctor
        names(tasas) <- c("Hombres", "Mujeres")
        # le colocamos nombres al vector
        return(tasas)
        # se devuelve el vector tasas
}

mitad <- function(Censo1, Censo2, fecha1, fecha2, fecha1m, fecha2m){
        #
        # Asignacion de variables:
        # 
        Edad <- Censo1$Edad
        # 
        r <- crecimiento(Censo1, Censo2, fecha1, fecha2)
        # Llamamos a la funcion Crecimiento para calcular la tasa de crecimiento
        # tanto de hombres como de mujeres
        rH <- r["Hombres"]
        # obtenemos la tasa de crecimiento Hombres
        rM <- r["Mujeres"]
        # obtenemos la tasa de crecimiento Mujeres
        hI <- (as.numeric(difftime(fecha1m, fecha1))) / 365
        # dias transcurridoa entre el primer Censo y la mitad del año
        hF <- (as.numeric(difftime(fecha2m, fecha2))) / 365
        # dias transcurridoa entre el segundo Censo y la mitad del año
        #
        Hombres = vector(mode = "numeric", length = 101)
        # Vector vacio para Hombres Mitad
        Mujeres = vector(mode = "numeric", length = 101)
        # Vector vacio para Mujeres Mitad
        EDOMEX2010M <- data.frame(Edad, Hombres, Mujeres)
        # Tabla Mitad para primer Censo
        # Data Frame con el primer Censo adicionado con los vetcores vacios
        EDOMEX2020M <- data.frame(Edad, Hombres, Mujeres)
        # Tabla Mitad para segundo Censo
        # Data Frame con el segndo Censo adicionado con los vetcores vacios
        # 
        # ======================================================================
        # 
        # Modificacion de tablas
        # 
        # ----------------------------------------------------------------------
        # 
        # Primer tabla
        # 
        EDOMEX2010M$Hombres <- round(Censo1$Hombres * exp(rH * hI), digits = 0)
        
        EDOMEX2010M$Mujeres <- round(Censo1$Mujeres * exp(rM * hI), digits = 0)
        
        EDOMEX2020M$Hombres <- round(Censo2$Hombres * exp(rH * hF), digits = 0)
        
        EDOMEX2020M$Mujeres <- round(Censo2$Mujeres * exp(rM * hF), digits = 0)
        
        save(EDOMEX2010M, file = "EDOMEX2010M.Rda")
        
        save(EDOMEX2020M, file = "EDOMEX2020M.Rda")
        
}

FCenso2010 <- as.Date("2010-06-12")

FCenso2020 <- as.Date("2020-03-15")

FMCenso2010 <- as.Date("2010-06-30")

FMCenso2020 <- as.Date("2020-06-30")


mitad(EDOMEX2010, EDOMEX2020, FCenso2010, FCenso2020, FMCenso2010, FMCenso2020)