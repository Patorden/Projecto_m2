# asignamos la tabla en una variable

Data <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /DF_Final.csv"))
head(Data)
attach(Data)

# Ahora añadimos una variable más que nos indica que localidades se encuentran dentro de alguna región indígena 

reg_ind <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/regiones índígenas.csv", header = TRUE))

# Ahora juntamos las dos tablas:

Data.reg <- right_join(reg_ind,Data,by="ID_.EST.MUN.LOC.") # annexamos las regiones indígenas
attach(Data.reg)

# categorizamos las variables de REG_IND y Grado.de.rezago.social: 

Data.reg <- mutate(Data.reg, REG_IND = factor(REG_IND), Grado.de.rezago.social = factor(Grado.de.rezago.social))

summary(Data.reg$REG_IND)

# como hicimos un right join con REG_IND, tenemos que nombrar todos los nulos como localidades no indígenas

Data.reg$REG_IND[is.na(Data.reg$REG_IND)] <- "Localidades no indigenas"

# Ahora hacemos algunos box plots de diferentes variables divididas por regiones indígenas: 

# rezago social por región indígena 

Hist_sin_Rezago <- ggplot(Data.reg, aes(x = reorder(REG_IND, Indice.de.rezago.social, fun = mean), 
                                        y = Indice.de.rezago.social)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Rezago social por región indígena") +
  xlab("Región Indígena") +
  ylab("Dispersión de rezago social") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

# Acceso al agua por región indígena 

Hist_sin_Agua <- ggplot(Data.reg, aes(x = reorder(REG_IND, casasSinAgua_x100, fun = mean), 
                                      y = casasSinAgua_x100)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Acceso al agua por región indígena") +
  xlab("Región Indígena") +
  ylab("Porcentaje de viviendas por localidad \nsin acceso al agua entubada") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

# Acceso a luz eléctrica por región indígena 

Hist_sin_Luz <- ggplot(Data.reg, aes(x = reorder(REG_IND, casasSinLuz_x100, fun = mean), 
                                     y = casasSinLuz_x100)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Acceso a luz eléctrica por región indígena") +
  xlab("Región Indígena") +
  ylab("Porcentaje de viviendas por localidad \nsin acceso a luz eléctrica") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

# viviendas con piso de tierra por región indígena: 

Hist_piso_tierra <- ggplot(Data.reg, aes(x = reorder(REG_IND, casasPisoTierra_x100, fun = mean), 
                                         y = casasPisoTierra_x100)) + 
  coord_flip() +
  geom_boxplot(col="black", fill = "light blue", outlier.colour = "dark red" ) +
  ggtitle("Viviendas con piso de tierra por región indígena") +
  xlab("Región Indígena") +
  ylab("Porcentaje de viviendas por \n localidad con piso de tierra") +
  theme_linedraw() + 
  theme(text=element_text(size=10, family="Courier", face = "bold"))

par(mfrow=c(2,2))
Hist_sin_Agua
Hist_sin_Luz
Hist_sin_Rezago
Hist_piso_tierra

Data.reg.select <- filter(Data.reg, REG_IND == c("Tarahumara","Localidades no indigenas"))

summary(Data.reg.select$REG_IND)

attach(Data.reg.select)

ggplot(Data.reg.select, aes(Hablantes_leng_indig)) + 
  geom_(colour = 'green', 
        fill = 'orange',
        alpha = 0.7, # Intensidad del color fill
        binwidth = 0.5) + 
  geom_density(aes(y = 0.5*..count..))+ # te crea un smooth line con el mismo bandwith
  geom_vline(xintercept = mean(Hablantes_leng_indig), linetype="dashed", color = "black") + 
  ggtitle('Histograma para la muestra t de Student') + 
  labs(x = 'Valores obtenidos', y = 'Frecuencia')+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 



