install.packages("pacman")

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

injuv2022 <- read_dta("input/data_orig/BBDD Respuesta - Encuesta Jóvenes.dta")
View(injuv2022)

find_var(data = injuv2022,"educacion")

find_var(data = injuv2022,"mujer")

#FILTRO >=26 años
injuv2022<- injuv2022 %>% filter( EDAD >= "26")

#Hombres como datos perdidos
injuv2022$SEXO <- recode(injuv2022$SEXO, "c(1)=NA")


proc_data <- injuv2022 %>% select(SEXO, # sexo
                                  EDAD, # edad
                                  P14, # Nivel educacional alcanzado
                                  P12_1, # Maternalismo cuidados 
                                  P12_4) # Maternalismo hijos

# Comprobar
names(proc_data)

sjlabelled::get_label(proc_data)

frq(proc_data$SEXO)

frq(proc_data$EDAD)

frq(proc_data$P14)

frq(proc_data$P12_1)

frq(proc_data$P12_4)

#Datos perdidos
proc_data$P14 <- recode(proc_data$P14, "c(98, 99)=NA")
proc_data$P12_1 <- recode(proc_data$P12_1, "c(98, 99)=NA")
proc_data$P12_4 <- recode(proc_data$P12_4, "c(98, 99)=NA")
sum(is.na(proc_data))

proc_data <-na.omit(proc_data)
dim(proc_data)

proc_data_original <-proc_data
dim(proc_data)

sum(is.na(proc_data))

proc_data <-na.omit(proc_data)
dim(proc_data)


#Renombrar variables

proc_data <- proc_data %>% rename("nivel_educ"=P14, # Nivel educacional alcanzado
                                  "mater_cuidados"=P12_1, # Maternalismo cuidados
                                  "mater_hijos"=P12_4) # Maternalismo hijos

proc_data$nivel_educ <- set_label(x = proc_data$nivel_educ,label = "Nivel educacional alcanzado")
get_label(proc_data$nivel_educ)                                  

proc_data$mater_cuidados <- set_label(x = proc_data$mater_cuidados,label = "Maternalismo cuidados")
get_label(proc_data$mater_cuidados)

proc_data$mater_hijos <- set_label(x = proc_data$mater_hijos,label = "Maternalismo hijos")
get_label(proc_data$mater_hijos)

proc_data$ideas_mater <- (proc_data$mater_cuidados+proc_data$mater_hijos)
summary(proc_data$ideas_mater)

proc_data$ideas_mater  <- set_label(x = proc_data$ideas_mater, label = "Ideas Maternalistas")
get_label(proc_data$ideas_mater)

frq(proc_data$nivel_educ)

frq(proc_data$mater_cuidados)

frq(proc_data$mater_hijos)

#Recodificacion sexo
frq(proc_data$SEXO)

proc_data$sexo <- car::recode(proc_data$SEXO, "2=1")


proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Mujer"),
                         levels=c(1))


get_label(proc_data$sexo)


frq(proc_data$sexo)


# Recodificacion nivel educacional
proc_data$Re_nivel_educ <- car::recode(proc_data$nivel_educ, "c(1)=1; c(2,3)= 2; c(4,5,6)=3; c(7,8,9,10)=4; c(11,12,13, 14)=5; c(15,16)=6")
frq(proc_data$Re_nivel_educ)

proc_data$Re_nivel_educ <- factor(proc_data$Re_nivel_educ,
                                  labels = c("No educacion", "Educacion pre-escolar", "Educacion basica", "Educacion media", "Educacion superior", "Postgrado"),
                                  levels = c(1, 2, 3, 4, 5, 6))

proc_data <- rename(proc_data,"educ_alcanzada"=Re_nivel_educ)

get_label(proc_data$educ_alcanzada)

proc_data$educ_alcanzada <- set_label(x = proc_data$educ_alcanzada,label = "Educacion Alcanzada")

frq(proc_data$educ_alcanzada)

#Recodificacion edad
frq(proc_data$EDAD)

proc_data$edad_rec <- car::recode(proc_data$EDAD, "c(26,27,28,29)=1")

proc_data$edad_rec <- factor(proc_data$edad_rec,
                             labels=c("Mujeres de 26 a 29 años"),
                             levels=c(1))


get_label(proc_data$edad_rec)

proc_data$edad_rec <- set_label(x = proc_data$edad_rec,label = "Edad")

frq(proc_data$edad_rec)

#GUARDAR BBDD Filtrada

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

save(proc_data,file = "C:\\Users\\pauli\\Documents\\GitHub\\Trabajo_4\\input\\data_proc/INJUV_Maternalismo2022.RData")


#REGRESION LINEAL

pacman::p_load(dplyr, car, sjmisc, sjPlot, sjlabelled, stargazer, kableExtra, corrplot, texreg, ggplot2, ggpubr)

#RESIDUOS
#Grafico x1 = ACT
graph1 <- ggplot(proc_data, aes(x = nivel_educ, y = ideas_mater)) +
  geom_point(size = 1) +  # Puntos
  geom_smooth(method = "lm", se = FALSE) +  # Recta de regresión
  labs(x = "Nivel Educacional", y = "Ideas maternalistas")  # Etiquetas de ejes

# Gráfico 2
graph <- ggplot(proc_data, aes(x = nivel_educ, y = mater_hijos)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Educación alcanzada", y = "Ideas maternalistas en torno a hijos")
ggarrange(graph1, graph, nrow = 1) # Unir graficos

#REGRESION LINEAL SIMPLE

frq(proc_data$educ_alcanzada)

##recodificación educación
proc_data$educacion <- car::recode(proc_data$educacion, "c(1,2)=0; c(3)=1; c(4)=2; c(5)=3; c(6)=4")

proc_data$educacion <- set_labels(proc_data$educacion,
                                  labels=c( "Educacion básica"=1,
                                            "Educación media"=2,
                                            "Educación superior"=3,
                                            "Postgrado"=4))

proc_data$educacion <- as_factor(proc_data$educ_alcanzada)

proc_data <- na.omit(proc_data)

reg1 <- lm(ideas_mater ~ 1, data=proc_data)

stargazer(reg1, type="text")

#regresion
reg2 <- lm(ideas_mater ~ educ_alcanzada, data=proc_data)

knitreg(list(reg2), 
        custom.model.names = c("Modelo 1"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "Educacion media <br> <i>(Ref. Ed. básica)</i>", 
                              "Educacion superior","Postgrado"),
        caption = "Ideas maternalistas",
        caption.above = TRUE)

#graficar
plot_model(reg2, 
           title = "", #quitar titulo
           show.values = TRUE, #mostrar valor de efectos
           dot.size = 3, #tamaño circulos
           line.size = 1, #tamaño CI
           value.size = 4, #tamaño valor efectoss
           spacing = 1, #espacio entre efectos
           vline.color = "red", # linea roja en punto neutro (0)
           axis.labels = rev(c("Educación media", 
                               "Educación superior",
                               "Postgrado" )), #con rev porque automatico los tira en otro orden
           show.legend = FALSE) + # variables dependientes
  theme_bw()

