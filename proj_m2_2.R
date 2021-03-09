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

Hist_sin_Agua
Hist_sin_Luz
Hist_sin_Rezago
Hist_piso_tierra

# ahora comparemos todas las localidades sin región indígena y los Tarahumaras:
# primero vamos a arreglar el grado de rezago social de muy bajo a muy alto: 

Data.reg$Grado.de.rezago.social <- factor(Data.reg$Grado.de.rezago.social,
                                          levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

# creamos un df sólo con las localidades tarahumara y sin región indígena: 
Data.reg.select <- filter(Data.reg, REG_IND == c("Tarahumara","Localidades no indigenas"))

# Checamos: 
summary(Data.reg.select)

attach(Data.reg.select)

layout(1:2)

hablanes.TaraYnoInd <- Data.reg.select %>%
  ggplot() + 
  aes(x = HablantesInd_x100, y = Grado.de.rezago.social) +
  facet_grid(~REG_IND) +
  geom_count(alpha=0.3, colour = "dark blue") + 
  ggtitle("Distribución de hablantes indígenas") + 
  theme_linedraw() +
  ylab("Grado de rezago social") +
  xlab("Porcentaje de hablantes\n de lenguas índígenas") +
  scale_x_continuous(labels = scales::number) +
  theme(text=element_text(size=10, family="Courier", face = "bold"))

ocup_cuarto.TaraYnoInd <- Data.reg.select %>%
  ggplot() + 
  aes(x = Prom_ocup_por_cuarto, y = Grado.de.rezago.social) +
  facet_grid(~REG_IND) +
  geom_count(alpha=0.3, colour = "dark blue") + 
  ggtitle("Distribución de ocupantes por cuarto") + 
  theme_linedraw() +
  ylab("Grado de rezago social") + 
  xlab("Promedio de ocupantes por cuarto") + 
  scale_x_continuous(labels = scales::number) +
  theme(text=element_text(size=10, family="Courier", face = "bold"))

alt.TaraYnoInd <- Data.reg.select %>%
  ggplot() + 
  aes(x = altitud, y = Grado.de.rezago.social) +
  facet_grid(~REG_IND) +
  geom_count(alpha=0.3, colour = "dark blue") + 
  ggtitle("Distribución de localidades por altura") + 
  theme_linedraw() +
  ylab("Grado de rezago social") + 
  xlab("Altitud media") + 
  scale_x_continuous(labels = scales::number) +
  theme(text=element_text(size=10, family="Courier", face = "bold"))

# ahora vemos las variables: 

hablanes.TaraYnoInd
ocup_cuarto.TaraYnoInd
alt.TaraYnoInd
