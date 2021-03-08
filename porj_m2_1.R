install.packages("ggplot2")
library("ggplot2")


# importamos los df: 

censo_general <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Censo general.csv"))
geo_datos <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Datos geográficos.csv"))
viviendas <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Descripción de viviendas.csv"))
lenguas_ind <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Hablantes de lenguas indígenas.csv"))
rezago <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Rezago social simplific 2010.csv"))
servicios <- na.omit(read.csv("/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /CSV finales/Servicios basicos de vivendas.csv"))

attach(servicios)
# función para sacar el porcentaje de casas sin ciertos servicios:

porcentaje <- function(DF,col_sin,col_con,round,nom_col){
  DF <- mutate(DF, col1 = round(
    (col_sin/(col_sin+col_con))*100, as.numeric(round)
  ))
  names(DF)[(length(DF))] <- nom_col
  return(DF)
}

# crear el porcentae de casas sin agua:

servicios <- porcentaje(servicios, vph_aguafv, vph_aguadv, 2, "casasSinAgua_x100")

# crear el porcentae de casas sin electricidad: 

servicios <- porcentaje(servicios, vph_s_elec, vph_c_elec, 2, "casasSinLuz_x100")

# crear el porcentae de casas sin drenaje: 

servicios <- porcentaje(servicios, vph_nodren, vph_drenaj, 2, "casasSinDrenaje_x100")

attach(viviendas)

# crear el porcentaje de casas con piso de tierra:

viviendas <- porcentaje(viviendas,viv_ocup_habit_piso_tierra,viv_ocup_habit_piso_no_tierra,2,"casasPisoTierra_x100")

# Ahora vamos a crear el porcentaje de hablantes de legua indígena por localidad:
# primero juntamos las tablas de censo y lenguas_ind:

lenguas_ind_censo_DF <- inner_join(lenguas_ind,censo_general,by="ID_.EST.MUN.LOC.")

# creamos una función par que nos de el porcentaje de hablantes de leguans ind
attach(lenguas_ind_censo_DF)

porcentaje2 <- function(DF,habl,pop_tot,round,nom_col){
  DF <- mutate(DF, col1 = round(
    (habl/poptot)*100, as.numeric(round)
  ))
  names(DF)[(length(DF))] <- nom_col
  return(DF)
}

# porcentaje de hablantes de lenguas indígenas
lenguas_ind_censo_DF <- porcentaje2(lenguas_ind_censo_DF,Hablantes_leng_indig,pobtot,2,"HablantesInd_x100")

# porcentaje de hablantes de lenguas indígenas y no español
lenguas_ind_censo_DF <- porcentaje2(lenguas_ind_censo_DF,Hablantes_leng_indig_no_esp,pobtot,2,"HablantesNoEsp_x100")

# porcentaje de hablantes de lenguas indígenas y español 
lenguas_ind_censo_DF <- porcentaje2(lenguas_ind_censo_DF,Hablantes_leng_indig_y_esp, pobtot, 2, "HablantesBiLing_x100")

# Borramos las columnas que no vamos a usar de todas los DF:

censo_general_selec <- censo_general[,-(3:11),drop=FALSE] # para eliminar una serie de columnas y guardar en otro DF 
viviendas_selec <- viviendas[,-(c(3:5,7:9,11,12)),drop=FALSE]
lenguas_ind_censo_select <- lenguas_ind_censo_DF[,c(1,15,16,17)]
servicios_selec <- servicios[,c(1,11:13)]

# Ahora juntamos los datos seleccionados en un solo df usando el ID 

geo_censo <- merge.data.frame(geo_datos,censo_general_selec,by="ID_.EST.MUN.LOC.") 
geo_cen_viv <- merge.data.frame(geo_censo, viviendas_selec,by="ID_.EST.MUN.LOC.")
geo_cen_viv_ind <- merge.data.frame(geo_cen_viv,lenguas_ind_censo_select,by="ID_.EST.MUN.LOC.")
geo_cen_viv_ind_rez <- merge.data.frame(geo_cen_viv_ind,rezago,by="ID_.EST.MUN.LOC.")
DF_Final <- merge.data.frame(geo_cen_viv_ind_rez,servicios_selec,by="ID_.EST.MUN.LOC.")

# Guardamos los resultados:

write.csv(DF_Final, "/Users/vottov/Desktop/Data Analysis & python /Projecto bueno /tablas listas /DF_Final.csv", row.names = TRUE)

