library(dplyr)
library(readr)

tr_endutih_hogar_anual_2021 <- read_csv("ENDUTIH2021/conjuntos_de_datos/tr_endutih_hogar_anual_2021.csv")
tr_endutih_residente_anual_2021 <- read_csv("ENDUTIH2021/conjuntos_de_datos/tr_endutih_residente_anual_2021.csv")
tr_endutih_usuario_anual_2021 <- read_csv("ENDUTIH2021/conjuntos_de_datos/tr_endutih_usuario_anual_2021.csv")
tr_endutih_usuario2_anual_2021 <- read_csv("ENDUTIH2021/conjuntos_de_datos/tr_endutih_usuario2_anual_2021.csv")
tr_endutih_hogares_anual_2022 <- read_csv("ENDUTIH2022/conjunto_de_datos/tr_endutih_hogares_anual_2022.csv")
tr_endutih_residentes_anual_2022 <- read_csv("ENDUTIH2022/conjunto_de_datos/tr_endutih_residentes_anual_2022.csv")
tr_endutih_usuarios_anual_2022 <- read_csv("ENDUTIH2022/conjunto_de_datos/tr_endutih_usuarios_anual_2022.csv")
tr_endutih_usuarios2_anual_2022 <- read_csv("ENDUTIH2022/conjunto_de_datos/tr_endutih_usuarios2_anual_2022.csv")

pond_hogar_2021 <- sum(tr_endutih_hogar_anual_2021$FAC_HOG, na.rm=T)
pond_hogar_2022 <- sum(tr_endutih_hogares_anual_2022$FAC_HOG, na.rm=T)
pond_usuario_2021 <- sum(tr_endutih_usuario_anual_2021$FAC_PER, na.rm=T)
pond_usuario_2022 <- sum(tr_endutih_usuarios_anual_2022$FAC_PER, na.rm=T)
pond_usuario2_2021 <- sum(tr_endutih_usuario2_anual_2021$FAC_PER, na.rm=T)
pond_usuario2_2022 <- sum(tr_endutih_usuarios2_anual_2022$FAC_PER, na.rm=T)
pond_residente_2021 <- sum(tr_endutih_residente_anual_2021$FAC_HOGAR, na.rm=T)
pond_residente_2022 <- sum(tr_endutih_residentes_anual_2022$FAC_HOGAR, na.rm=T)

##Fusiones de Tablas
#Hogares-Residentes-Usuarios2
tr_endutih_hogres_anual_2021 <- merge(tr_endutih_hogar_anual_2021,tr_endutih_residente_anual_2021, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC"), all.y=T)
tr_endutih_hogresusu2_anual_2021 <- merge(tr_endutih_hogres_anual_2021,tr_endutih_usuario2_anual_2021, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC", "NUM_REN"), all.y=T)
tr_endutih_hogresall_anual_2021 <- merge(tr_endutih_hogar_anual_2021,tr_endutih_residente_anual_2021, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC"), all=T)

tr_endutih_hogres_anual_2022 <- merge(tr_endutih_hogares_anual_2022,tr_endutih_residentes_anual_2022, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC"), all.y=T)
tr_endutih_hogresusu2_anual_2022 <- merge(tr_endutih_hogres_anual_2022,tr_endutih_usuarios2_anual_2022, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC", "NUM_REN"), all.y=T)
tr_endutih_hogresall_anual_2022 <- merge(tr_endutih_hogares_anual_2022,tr_endutih_residentes_anual_2022, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC"), all=T)

#Usuarios-Usuarios2
tr_endutih_usuusu2_anual_2021 <- merge(tr_endutih_usuario_anual_2021,tr_endutih_usuario2_anual_2021, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC","NUM_REN", "FAC_PER"), all.y=T)
tr_endutih_usuusu2_anual_2022 <- merge(tr_endutih_usuarios_anual_2022,tr_endutih_usuarios2_anual_2022, by=c("UPM","VIV_SEL","HOGAR","ENT","TLOC","NUM_REN", "FAC_PER", "EDAD", "SEXO",  "NIVEL", "GRADO" ,"PAREN"), all.y=T)


####Datos de falta de Internet por conectividad agrupados por entidad (Cuadro 9)
#Hogares
endutih_hogar_anual_2021_conectividad_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conectividad = sum(FAC_HOG[P4_8 == 6], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100) 
print(endutih_hogar_anual_2021_conectividad_entidad, n=32)
c(0.42,1.64,3.18,1.55,1.31,0.65,3.58,3.05,0.31,3.14,0.98,4.86,2.69,1.09,0.88,2.81,0.50,2.74,1.11,3.38,1.94,1.49,0.57,2.65,0.29,0.44,1.08,1.13,3.95,2.25,0.59,1.07)-endutih_hogar_anual_2021_conectividad_entidad$porcentaje_no_conectividad

endutih_hogar_anual_2022_conectividad_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conectividad = sum(FAC_HOG[P4_8 == 6], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100)
print(endutih_hogar_anual_2022_conectividad_entidad, n=32)
c(0.26,0.89,1.26,1.70,1.60,0.42,3.56,1.71,0.14,2.88,0.23,3.46,1.38,0.68,0.43,2.28,2.55,1.15,0.78,1.67,1.64,0.91,1.00,2.29,0.77,0.18,0.88,2.51,3.16,1.32,0.74,0.97)-endutih_hogar_anual_2022_conectividad_entidad$porcentaje_no_conectividad

#Personas
endutih_persona_anual_2021_conectividad_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conectividad = sum(FAC_PER[P7_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100)
print(endutih_persona_anual_2021_conectividad_entidad, n=32)
c(3.66,1.36,4.42,3.68,4.43,1.88,6.51,3.60,1.06,5.93,4.45,4.13,3.75,2.26,2.35,4.20,2.41,3.10,2.29,4.44,4.50,2.21,3.34,2.44,2.67,1.44,5.22,1.79,2.81,3.70,2.79,4.33)-endutih_persona_anual_2021_conectividad_entidad$porcentaje_no_conectividad

endutih_persona_anual_2022_conectividad_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conectividad = sum(FAC_PER[P7_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100)
print(endutih_persona_anual_2022_conectividad_entidad, n=32)
c(1.74,1.23,2.20,1.58,3.64,1.36,4.37,1.82,0.91,3.15,2.44,3.01,2.93,0.96,2.73,3.32,0.94,1.55,2.17,3.61,3.00,1.51,1.70,1.75,1.69,2.42,3.28,1.73,3.00,1.91,1.62,3.05)-endutih_persona_anual_2022_conectividad_entidad$porcentaje_no_conectividad



####Datos de falta de Internet por conectividad agrupados por localidad (Cuadro 10)
#Hogares
endutih_hogar_anual_2021_conectividad_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conectividad = sum(FAC_HOG[P4_8 == 6], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100) 
endutih_hogar_anual_2021_conectividad_localidad
c(0.26,0.67,1.46,5.24)-endutih_hogar_anual_2021_conectividad_localidad$porcentaje_no_conectividad

endutih_hogar_anual_2022_conectividad_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conectividad = sum(FAC_HOG[P4_8 == 6], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100)
endutih_hogar_anual_2022_conectividad_localidad
c(0.25,0.45,1.02,3.90)-endutih_hogar_anual_2022_conectividad_localidad$porcentaje_no_conectividad

#Personas
endutih_persona_anual_2021_conectividad_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conectividad = sum(FAC_PER[P7_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100)
endutih_persona_anual_2021_conectividad_localidad
c(1.84,2.81,3.61,5.73)-endutih_persona_anual_2021_conectividad_localidad$porcentaje_no_conectividad

endutih_persona_anual_2022_conectividad_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conectividad = sum(FAC_PER[P7_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_conectividad = no_conectividad/contador_pond*100)
endutih_persona_anual_2022_conectividad_localidad
c(1.21,2.16,2.57,4.35)-endutih_persona_anual_2022_conectividad_localidad$porcentaje_no_conectividad

##Nacional
#Hogares
nacional_hogar_conectividad_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_8 == 6)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_conectividad_2021
nacional_hogar_conectividad_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_8 == 6)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_conectividad_2022

#Personas
nacional_persona_conectividad_2021 <- sum(subset(tr_endutih_usuario_anual_2021, P7_2 == 1)$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_conectividad_2021
nacional_persona_conectividad_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P7_2 == 1)$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_conectividad_2022



####Datos de falta de Internet por falta de capacidad económica agrupados por entidad (Cuadro 19)
#Hogares
endutih_hogar_anual_2021_int_capacidad_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
print(endutih_hogar_anual_2021_int_capacidad_entidad, n=32)
c(17.33,10.83,13.29,25.99,16.03,13.56,49.65,13.74,8.36,25.37,25.33,23.06,26.88,15.02,12.57,24.19,19.53,18.45,12.64,36.48,27.69,16.51,18.62,21.35,13.09,7.00,32.31,18.02,27.78,31.81,20.18,22.57)-endutih_hogar_anual_2021_int_capacidad_entidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_int_capacidad_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
print(endutih_hogar_anual_2022_int_capacidad_entidad, n=32)
c(12.58,7.86,12.46,20.15,16.31,12.07,36.70,12.99,8.42,24.11,23.04,28.08,24.44,13.41,11.71,22.64,17.06,19.27,8.42,31.87,26.75,12.95,13.47,21.49,9.95,11.50,26.93,16.03,25.61,29.85,19.63,17.93)-endutih_hogar_anual_2022_int_capacidad_entidad$porcentaje_no_capacidad

#Personas
endutih_persona_anual_2021_int_capacidad_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P7_2 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_persona_anual_2021_int_capacidad_entidad, n=32)
c(1.37,0.77,1.38,4.44,1.98,2.38,14.58,1.28,1.35,4.14,2.70,5.55,3.14,1.82,2.14,4.74,2.66,2.18,1.94,7.20,4.70,2.76,3.78,4.40,1.52,1.16,5.84,2.19,2.59,4.79,0.92,4.52)-endutih_persona_anual_2021_int_capacidad_entidad$porcentaje_no_capacidad

endutih_persona_anual_2022_int_capacidad_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P7_2 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_persona_anual_2022_int_capacidad_entidad, n=32)
c(1.07,0.91,1.13,1.53,1.59,1.07,5.03,1.96,0.64,2.59,3.65,5.08,2.03,2.48,1.09,2.94,4.25,2.79,0.84,4.58,5.66,1.89,1.45,4.46,1.13,1.54,2.80,1.57,1.52,3.00,2.71,2.34)-endutih_persona_anual_2022_int_capacidad_entidad$porcentaje_no_capacidad



####Datos de falta de Internet por falta de capacidad económica agrupados por localidad (Cuadro 20)
#Hogares
endutih_hogar_anual_2021_int_capacidad_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100) 
endutih_hogar_anual_2021_int_capacidad_localidad
c(10.82,18.30,28.09,35.72)-endutih_hogar_anual_2021_int_capacidad_localidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_int_capacidad_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100) 
endutih_hogar_anual_2022_int_capacidad_localidad
c(9.15,15.49,24.87,34.36)-endutih_hogar_anual_2022_int_capacidad_localidad$porcentaje_no_capacidad

#Personas
endutih_persona_anual_2021_int_capacidad_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P7_2 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
endutih_persona_anual_2021_int_capacidad_localidad
c(1.55,1.77,4.69,7.24)-endutih_persona_anual_2021_int_capacidad_localidad$porcentaje_no_capacidad

endutih_persona_anual_2022_int_capacidad_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P7_2 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100) 
endutih_persona_anual_2022_int_capacidad_localidad
c(1.04,1.96,2.87,5.15)-endutih_persona_anual_2022_int_capacidad_localidad$porcentaje_no_capacidad

##Nacional
#Hogares
nacional_hogar_int_capacidad_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_8 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_int_capacidad_2021
nacional_hogar_int_capacidad_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_8 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_int_capacidad_2022

#Personas
nacional_persona_int_capacidad_2021 <- sum(subset(tr_endutih_usuario_anual_2021, P7_2 == 4)$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_int_capacidad_2021
nacional_persona_int_capacidad_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P7_2 == 4)$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_int_capacidad_2022



#### Falta de dispositivos o celular por falta de capacidad económica por entidad (Cuadro 21)
#Hogares (Dispositivos)
endutih_hogar_anual_2021_disp_capacidad_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2021_disp_capacidad_entidad, n=32)
c(25.71,20.94,22.46,34.67,27.52,22.26,54.08,18.68,16.85,28.81,36.55,43.64,38.06,24.43,31.45,32.62,29.03,25.46,21.90,41.83,39.78,27.01,30.08,33.17,29.89,22.30,40.63,28.51,40.38,37.70,27.35,34.13)-endutih_hogar_anual_2021_disp_capacidad_entidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_disp_capacidad_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2022_disp_capacidad_entidad, n=32)
c(21.68,15.04,19.97,30.90,23.55,22.78,41.93,18.01,17.48,28.19,34.36,41.90,33.37,20.12,28.66,29.08,28.50,26.11,15.53,38.43,36.73,25.51,30.92,32.14,25.18,22.92,38.63,22.50,33.72,36.23,26.33,29.51)-endutih_hogar_anual_2022_disp_capacidad_entidad$porcentaje_no_capacidad

#Hogares (Celular)
endutih_hogar_anual_2021_cel_capacidad_entidad = tr_endutih_hogresusu2_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P8_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2021_cel_capacidad_entidad, n=32)
c(4.71,4.14,3.63,8.14,5.61,4.36,22.91,3.18,2.50,7.28,8.31,13.06,6.25,4.13,5.97,7.53,4.73,5.97,5.10,12.05,10.51,5.21,5.33,7.63,4.50,3.32,11.82,4.75,9.07,10.67,3.45,10.50)-endutih_hogar_anual_2021_cel_capacidad_entidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_cel_capacidad_entidad = tr_endutih_hogresusu2_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P8_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2022_cel_capacidad_entidad, n=32)
c(4.19,3.00,2.91,6.66,4.34,3.38,13.99,3.14,1.88,6.52,7.73,11.29,7.97,4.33,4.91,7.42,4.34,6.45,2.78,11.65,11.44,5.16,5.24,8.59,4.55,4.67,8.84,2.93,8.78,8.79,4.62,7.72)-endutih_hogar_anual_2022_cel_capacidad_entidad$porcentaje_no_capacidad

#Personas (Dispositivos)
endutih_persona_anual_2021_disp_capacidad_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P6_3 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
print(endutih_persona_anual_2021_disp_capacidad_entidad, n=32)
c(4.88,3.44,3.25,11.20,6.05,5.45,18.43,4.21,5.00,7.30,5.84,11.51,6.66,5.28,5.68,9.28,4.98,4.28,5.98,13.16,9.38,8.08,9.41,9.28,6.29,5.91,12.49,7.46,8.36,12.03,5.57,9.69)-endutih_persona_anual_2021_disp_capacidad_entidad$porcentaje_no_capacidad

endutih_persona_anual_2022_disp_capacidad_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P6_3 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
print(endutih_persona_anual_2022_disp_capacidad_entidad, n=32)
c(4.31,3.40,3.63,8.01,4.83,3.62,9.41,4.59,3.11,5.43,8.02,14.84,6.07,5.16,3.86,5.85,7.73,6.75,3.18,11.65,11.38,7.70,7.33,9.61,5.30,3.61,8.55,4.89,5.68,6.32,8.82,5.09)-endutih_persona_anual_2022_disp_capacidad_entidad$porcentaje_no_capacidad

#Personas (Celular)
endutih_persona_anual_2021_cel_capacidad_entidad = tr_endutih_usuario2_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P8_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
print(endutih_persona_anual_2021_cel_capacidad_entidad, n=32)
c(5.22,4.62,3.65,7.93,6.10,4.91,25.20,3.26,2.68,9.34,8.52,13.98,6.34,4.43,7.04,8.30,5.32,6.83,5.79,13.28,11.79,5.06,6.16,7.91,4.73,4.02,12.76,4.87,9.67,12.07,3.53,11.46)-endutih_persona_anual_2021_cel_capacidad_entidad$porcentaje_no_capacidad

endutih_persona_anual_2022_cel_capacidad_entidad = tr_endutih_usuarios2_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P8_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
print(endutih_persona_anual_2022_cel_capacidad_entidad, n=32)
c(4.38,2.80,3.55,7.74,5.21,3.71,16.42,3.21,1.84,7.61,7.90,13.26,8.65,4.95,4.74,8.61,4.06,6.97,2.76,13.20,12.77,5.48,6.01,8.13,5.06,4.80,8.52,3.19,9.65,9.20,5.07,8.03)-endutih_persona_anual_2022_cel_capacidad_entidad$porcentaje_no_capacidad



#### Falta de dispositivos o celular por falta de capacidad económica por localidad (Cuadro 22)
#Hogares (Dispositivos)
endutih_hogar_anual_2021_disp_capacidad_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
endutih_hogar_anual_2021_disp_capacidad_localidad
c(21.43,28.81,37.97,47.52)-endutih_hogar_anual_2021_disp_capacidad_localidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_disp_capacidad_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
endutih_hogar_anual_2022_disp_capacidad_localidad
c(18.28,25.78,34.42,45.09)-endutih_hogar_anual_2022_disp_capacidad_localidad$porcentaje_no_capacidad

#Hogares (Celular)
endutih_hogar_anual_2021_cel_capacidad_localidad = tr_endutih_hogresusu2_anual_2021 %>% group_by(TLOC) %>%
 summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P8_2 == 1], na.rm=T)) %>%
 mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
endutih_hogar_anual_2021_cel_capacidad_localidad
c(3.72,5.21,8.72,14.53)-endutih_hogar_anual_2021_cel_capacidad_localidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_cel_capacidad_localidad = tr_endutih_hogresusu2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P8_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
endutih_hogar_anual_2022_cel_capacidad_localidad
c(2.64,5.63,7.31,13.26)-endutih_hogar_anual_2022_cel_capacidad_localidad$porcentaje_no_capacidad

#Personas (Dispositivos)
endutih_persona_anual_2021_disp_capacidad_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P6_3 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
endutih_persona_anual_2021_disp_capacidad_localidad
c(4.48,5.72,9.61,14.08)-endutih_persona_anual_2021_disp_capacidad_localidad$porcentaje_no_capacidad

endutih_persona_anual_2022_disp_capacidad_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P6_3 == 4], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
endutih_persona_anual_2022_disp_capacidad_localidad
c(3.62,5.83,7.11,11.04)-endutih_persona_anual_2022_disp_capacidad_localidad$porcentaje_no_capacidad

#Personas (Celular)
endutih_persona_anual_2021_cel_capacidad_localidad = tr_endutih_usuario2_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P8_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
endutih_persona_anual_2021_cel_capacidad_localidad
c(4.17,5.46,9.72,15.95)-endutih_persona_anual_2021_cel_capacidad_localidad$porcentaje_no_capacidad

endutih_persona_anual_2022_cel_capacidad_localidad = tr_endutih_usuarios2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_capacidad = sum(FAC_PER[P8_2 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)  
endutih_persona_anual_2022_cel_capacidad_localidad
c(2.79,6.18,7.79,14.23)-endutih_persona_anual_2022_cel_capacidad_localidad$porcentaje_no_capacidad

## Nacional
#Hogares (Dispositivos)
nacional_hogar_disp_capacidad_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_3 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_disp_capacidad_2021
nacional_hogar_disp_capacidad_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_3 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_disp_capacidad_2022

#Hogares(Celular)
nacional_hogar_cel_capacidad_2021 <- sum(subset(tr_endutih_hogresusu2_anual_2021, P8_2 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_cel_capacidad_2021
nacional_hogar_cel_capacidad_2022 <- sum(subset(tr_endutih_hogresusu2_anual_2022, P8_2 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_cel_capacidad_2022

#Personas (Dispositivos)
nacional_persona_disp_capacidad_2021 <- sum(subset(tr_endutih_usuario_anual_2021, P6_3 == 4)$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_disp_capacidad_2021
nacional_persona_disp_capacidad_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_3 == 4)$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_disp_capacidad_2022

#Personas (Celular)
nacional_persona_cel_capacidad_2021 <- sum(subset(tr_endutih_usuario2_anual_2021, P8_2 == 1)$FAC_PER, na.rm=T)/pond_usuario2_2021*100
nacional_persona_cel_capacidad_2021
nacional_persona_cel_capacidad_2022 <- sum(subset(tr_endutih_usuarios2_anual_2022, P8_2 == 1)$FAC_PER, na.rm=T)/pond_usuario2_2022*100
nacional_persona_cel_capacidad_2022



#### Falta de dispositivos o Internet o celular o Internet por falta de capacidad económica por entidad (Cuadro 23)
#Hogares (Dispositivos)
endutih_hogar_anual_2021_dispint_capacidad_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2021_dispint_capacidad_entidad, n=32)
c(30.46,24.18,26.24,41.69,31.06,25.67,62.92,24.57,18.88,36.74,42.14,48.49,43.89,28.38,33.55,38.49,34.38,31.05,24.92,48.83,45.37,31.18,34.22,38.42,33.56,24.47,48.15,33.30,45.39,46.04,33.67,39.88)-endutih_hogar_anual_2021_dispint_capacidad_entidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_dispint_capacidad_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2022_dispint_capacidad_entidad, n=32)
c(25.53,17.52,23.68,37.45,28.41,26.38,51.27,22.23,19.91,35.13,40.40,48.41,39.56,24.08,31.90,35.91,32.71,31.43,18.02,47.15,42.53,29.54,33.82,38.13,28.64,25.69,44.86,28.25,39.45,44.15,31.90,35.20)-endutih_hogar_anual_2022_dispint_capacidad_entidad$porcentaje_no_capacidad

# #Hogares (Celular)
endutih_hogar_anual_2021_celint_capacidad_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_8 == 1 | P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2021_celint_capacidad_entidad, n=32)
c(20.32,12.36,15.16,28.65,19.16,16.31,56.94,16.92,10.44,28.76,29.71,30.78,33.61,18.23,15.98,27.96,22.90,22.34,15.20,46.02,31.08,19.29,20.37,26.09,15.10,9.18,36.50,20.73,31.44,37.48,23.69,27.55)-endutih_hogar_anual_2021_celint_capacidad_entidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_celint_capacidad_entidad = tr_endutih_hogresusu2_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_8 == 1 | P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2022_celint_capacidad_entidad, n=32)
c(16.29,9.73,13.78,24.50,18.92,14.70,45.01,16.10,10.71,27.64,27.32,32.57,29.26,17.02,15.14,27.41,20.05,23.37,11.05,39.77,31.70,15.96,16.19,25.68,12.69,13.26,31.66,17.92,29.73,35.13,22.67,22.10)-endutih_hogar_anual_2022_celint_capacidad_entidad$porcentaje_no_capacidad

#Población (Dispositivos) 
endutih_poblacion_anual_2021_dispint_capacidad_entidad = tr_endutih_hogresall_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2021_dispint_capacidad_entidad, n=32)
c(33.91,24.26,27.43,42.79,32.59,26.37,66.69,25.69,20.31,40.33,45.11,52.80,46.27,30.57,34.62,41.09,36.61,33.61,24.40,53.60,49.83,33.43,37.09,41.64,35.36,25.52,50.89,32.39,49.62,48.53,37.32,41.58)-endutih_poblacion_anual_2021_dispint_capacidad_entidad$porcentaje_no_capacidad

endutih_poblacion_anual_2022_dispint_capacidad_entidad = tr_endutih_hogresall_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2022_dispint_capacidad_entidad, n=32)
c(26.82,17.27,24.89,40.88,30.33,27.52,54.92,23.01,20.08,38.04,44.19,53.06,41.61,27.18,34.00,39.74,35.27,33.63,18.37,50.54,47.75,32.07,35.48,38.75,32.02,27.55,47.57,29.60,43.20,46.98,35.68,36.42)-endutih_poblacion_anual_2022_dispint_capacidad_entidad$porcentaje_no_capacidad

#Población (Celular) 
endutih_poblacion_anual_2021_celint_capacidad_entidad = tr_endutih_hogresall_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_8 == 1 | P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2021_celint_capacidad_entidad, n=32)
c(19.64,10.71,13.42,27.27,17.08,13.91,58.03,14.88,8.68,28.75,26.84,27.59,30.80,16.53,13.24,26.63,20.57,20.23,12.17,45.51,31.66,17.89,19.64,24.28,12.94,7.08,35.25,17.51,30.99,35.12,23.15,23.94)-endutih_poblacion_anual_2021_celint_capacidad_entidad$porcentaje_no_capacidad

endutih_poblacion_anual_2022_celint_capacidad_entidad = tr_endutih_hogresall_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_8 == 1 | P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2022_celint_capacidad_entidad, n=32)
c(13.73,7.08,12.98,23.23,17.90,11.77,46.75,13.99,8.21,26.93,25.01,32.66,27.14,15.92,12.74,26.44,18.49,22.18,9.30,37.88,31.86,14.39,14.31,22.17,10.98,11.54,29.96,15.81,28.60,33.39,22.38,19.37)-endutih_poblacion_anual_2022_celint_capacidad_entidad$porcentaje_no_capacidad



#### Falta de dispositivos o Internet o celular o Internet por falta de capacidad económica por localidad (Cuadro 24)
#Hogares (Dispositivos)
endutih_hogar_anual_2021_dispint_capacidad_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2021_dispint_capacidad_localidad, n=32)
c(24.27,33.26,44.38,55.25)-endutih_hogar_anual_2021_dispint_capacidad_localidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_dispint_capacidad_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2022_dispint_capacidad_localidad, n=32)
c(21.16,30.24,41.37,53.37)-endutih_hogar_anual_2022_dispint_capacidad_localidad$porcentaje_no_capacidad

#Hogares (Celular)
endutih_hogar_anual_2021_celint_capacidad_localidad = tr_endutih_hogresusu2_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_1_6 == 2 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2021_celint_capacidad_localidad, n=32)
c(12.97,21.30,32.11,43.60)-endutih_hogar_anual_2021_celint_capacidad_localidad$porcentaje_no_capacidad

endutih_hogar_anual_2022_celint_capacidad_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_capacidad = sum(FAC_HOG[P4_1_6 == 2 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_hogar_anual_2022_celint_capacidad_localidad, n=32)
c(11.40,18.46,29.19,41.76)-endutih_hogar_anual_2022_celint_capacidad_localidad$porcentaje_no_capacidad

#Población (Dispositivos) 
endutih_poblacion_anual_2021_dispint_capacidad_localidad = tr_endutih_hogresall_anual_2021 %>% group_by(TLOC) %>%
 summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
 mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2021_dispint_capacidad_localidad, n=32)
c(25.02,34.61,47.61,59.25)-endutih_poblacion_anual_2021_dispint_capacidad_localidad$porcentaje_no_capacidad

endutih_poblacion_anual_2022_dispint_capacidad_localidad = tr_endutih_hogresall_anual_2022 %>% group_by(TLOC) %>%
 summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_3 == 1 | P4_8 == 1], na.rm=T)) %>%
 mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2022_dispint_capacidad_localidad, n=32)
c(22.14,32.53,44.34,57.24)-endutih_poblacion_anual_2022_dispint_capacidad_localidad$porcentaje_no_capacidad

#Población (Celular) 
endutih_poblacion_anual_2021_celint_capacidad_localidad = tr_endutih_hogresall_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_1_6 == 2 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2021_celint_capacidad_localidad, n=32)
c(10.90,18.99,30.81,41.56)-endutih_poblacion_anual_2021_celint_capacidad_localidad$porcentaje_no_capacidad

endutih_poblacion_anual_2022_celint_capacidad_localidad = tr_endutih_hogresall_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_capacidad = sum(FAC_HOGAR[P4_1_6 == 2 | P4_8 == 1], na.rm=T)) %>%
  mutate(porcentaje_no_capacidad = no_capacidad/contador_pond*100)
print(endutih_poblacion_anual_2022_celint_capacidad_localidad, n=32)
c(9.41,16.81,27.82,39.97)-endutih_poblacion_anual_2022_celint_capacidad_localidad$porcentaje_no_capacidad

##Nacional
#Hogares (Dispositivos)
nacional_hogar_dispint_capacidad_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_3 == 1 | P4_8 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_dispint_capacidad_2021
nacional_hogar_dispint_capacidad_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_3 == 1 | P4_8 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_dispint_capacidad_2022

#Hogares(Celular) 
nacional_hogar_celint_capacidad_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_1_6 == 2 | P4_8 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_celint_capacidad_2021
nacional_hogar_celint_capacidad_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_1_6 == 2 | P4_8 == 1)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_celint_capacidad_2022

#Población (Dispositivos) 
nacional_poblacion_dispint_capacidad_2021 <- sum(subset(tr_endutih_hogresall_anual_2021, P4_3 == 1 | P4_8 == 1)$FAC_HOGAR, na.rm=T)/pond_residente_2021*100
nacional_poblacion_dispint_capacidad_2021
nacional_poblacion_dispint_capacidad_2022 <- sum(subset(tr_endutih_hogresall_anual_2022, P4_3 == 1 | P4_8 == 1)$FAC_HOGAR, na.rm=T)/pond_residente_2022*100
nacional_poblacion_dispint_capacidad_2022

#Población (Celular) 
nacional_poblacion_celint_capacidad_2021 <- sum(subset(tr_endutih_hogresall_anual_2021, P4_1_6 == 2 | P4_8 == 1)$FAC_HOGAR, na.rm=T)/pond_residente_2021*100
nacional_poblacion_celint_capacidad_2021
nacional_poblacion_celint_capacidad_2022 <- sum(subset(tr_endutih_hogresall_anual_2022, P4_1_6 == 2 | P4_8 == 1)$FAC_HOGAR, na.rm=T)/pond_residente_2022*100
nacional_poblacion_celint_capacidad_2022



#### Falta de equipamiento TIC por entidad (Cuadro 27)
#Hogares
endutih_hogar_anual_2021_tic_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_hogar_anual_2021_tic_entidad, n=32)
c(4.43,2.87,2.64,6.46,4.35,4.50,18.08,4.04,2.77,6.47,7.69,12.81,9.59,4.63,4.59,6.88,4.91,5.60,3.68,15.00,6.36,5.07,3.88,7.42,3.56,3.28,7.68,4.05,6.02,9.38,5.55,9.22)-endutih_hogar_anual_2021_tic_entidad$porcentaje_no_uso

endutih_hogar_anual_2022_tic_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_hogar_anual_2022_tic_entidad, n=32)
c(4.51,2.98,1.97,6.15,3.79,4.29,13.84,4.24,3.12,5.96,7.43,7.04,6.89,5.13,4.80,7.18,5.01,6.37,3.29,12.84,8.30,4.62,5.01,7.92,3.92,3.98,7.78,3.01,5.77,8.93,5.06,7.05)-endutih_hogar_anual_2022_tic_entidad$porcentaje_no_uso

#Población 
endutih_poblacion_anual_2021_tic_entidad = tr_endutih_hogres_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_uso = sum(FAC_HOGAR[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_poblacion_anual_2021_tic_entidad, n=32)
c(2.34,1.70,1.32,4.14,2.25,2.44,16.55,2.15,1.36,4.94,4.21,8.47,5.55,2.32,2.36,4.30,2.67,2.98,2.12,10.83,3.97,2.75,2.34,4.73,1.73,1.65,4.62,1.97,3.43,6.42,3.02,5.58)-endutih_poblacion_anual_2021_tic_entidad$porcentaje_no_uso

endutih_poblacion_anual_2022_tic_entidad = tr_endutih_hogres_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_uso = sum(FAC_HOGAR[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_poblacion_anual_2022_tic_entidad, n=32)
c(2.47,1.40,1.26,3.75,2.08,2.11,13.11,2.39,1.41,3.69,4.11,5.04,3.94,2.87,2.67,4.37,2.83,4.74,1.66,8.23,5.33,2.51,4.76,4.42,1.97,2.51,4.59,1.65,3.14,4.99,2.88,3.55)-endutih_poblacion_anual_2022_tic_entidad$porcentaje_no_uso



#### Falta de equipamiento TIC (ni celular ni dispositivos) por localidad (Cuadro 28)
#Hogares 
endutih_hogar_anual_2021_tic_localidad = tr_endutih_hogresusu2_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
endutih_hogar_anual_2021_tic_localidad
c(3.14,4.46,6.91,13.88)-endutih_hogar_anual_2021_tic_localidad$porcentaje_no_uso

endutih_hogar_anual_2022_tic_localidad = tr_endutih_hogresusu2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
endutih_hogar_anual_2022_tic_localidad
c(3.09,4.31,6.77,12.64)-endutih_hogar_anual_2022_tic_localidad$porcentaje_no_uso

#Población 
endutih_poblacion_anual_2021_tic_localidad = tr_endutih_hogres_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_uso = sum(FAC_HOGAR[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
endutih_poblacion_anual_2021_tic_localidad
c(1.68,2.34,4.23,9.63)-endutih_poblacion_anual_2021_tic_localidad$porcentaje_no_uso

endutih_poblacion_anual_2022_tic_localidad = tr_endutih_hogres_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOGAR), no_uso = sum(FAC_HOGAR[!is.na(P4_3) & P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
endutih_poblacion_anual_2022_tic_localidad
c(1.61,2.33,4.08,8.46)-endutih_poblacion_anual_2022_tic_localidad$porcentaje_no_uso

##Nacional
#Hogares
nacional_hogar_tic_capacidad_2021 <- sum(subset(tr_endutih_hogar_anual_2021, !is.na(P4_3) & P4_1_6 == 2)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_tic_capacidad_2021
nacional_hogar_tic_capacidad_2022 <- sum(subset(tr_endutih_hogares_anual_2022,!is.na(P4_3) & P4_1_6 == 2)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_tic_capacidad_2022

#Población 
nacional_poblacion_tic_capacidad_2021 <- sum(subset(tr_endutih_hogresall_anual_2021, !is.na(P4_3) & P4_1_6 == 2)$FAC_HOGAR, na.rm=T)/pond_residente_2021*100
nacional_poblacion_tic_capacidad_2021
nacional_poblacion_tic_capacidad_2022 <- sum(subset(tr_endutih_hogresall_anual_2022, !is.na(P4_3) & P4_1_6 == 2)$FAC_HOGAR, na.rm=T)/pond_residente_2022*100
nacional_poblacion_tic_capacidad_2022



#### Falta de Internet por falta de conocimiento por entidad (Cuadro 29)
#Hogares
endutih_hogar_anual_2021_int_conocimiento_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_8 %in% c(2,3,4)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_hogar_anual_2021_int_conocimiento_entidad, n=32)
c(9.17,7.15,8.24,9.37,10.50,10.44,14.74,13.69,5.70,13.86,10.53,14.01,14.76,11.71,9.00,12.12,9.52,14.13,7.60,19.30,13.01,7.20,7.21,11.71,9.09,5.89,12.49,10.29,11.59,13.87,10.76,11.36)-endutih_hogar_anual_2021_int_conocimiento_entidad$porcentaje_no_conocimiento

endutih_hogar_anual_2022_int_conocimiento_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_8 %in% c(2,3,4)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_hogar_anual_2022_int_conocimiento_entidad, n=32)
c(10.32,7.11,9.55,11.05,10.35,10.07,19.30,12.76,5.08,11.62,10.87,12.47,14.20,9.37,11.27,16.80,9.66,15.00,8.36,17.18,12.86,7.29,6.77,9.78,8.14,6.77,12.89,11.70,13.94,14.36,12.07,11.07)-endutih_hogar_anual_2022_int_conocimiento_entidad$porcentaje_no_conocimiento

#Personas
endutih_persona_anual_2021_int_conocimiento_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P7_2 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2021_int_conocimiento_entidad, n=32)
c(9.17,7.15,8.24,9.37,10.50,10.44,14.74,13.69,5.70,13.86,10.53,14.01,14.76,11.71,9.00,12.12,9.52,14.13,7.60,19.30,13.01,7.20,7.21,11.71,9.09,5.89,12.49,10.29,11.59,13.87,10.76,11.36)-endutih_hogar_anual_2021_int_conocimiento_entidad$porcentaje_no_conocimiento

endutih_persona_anual_2022_int_conocimiento_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P7_2 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2022_int_conocimiento_entidad, n=32)
c(9.63,7.58,10.96,16.01,12.06,11.65,32.50,11.78,7.96,13.52,17.05,22.39,18.40,12.60,13.25,25.20,15.09,16.09,9.89,27.36,20.33,11.20,12.31,16.79,14.22,9.42,15.39,10.61,16.57,21.49,12.44,15.60)-endutih_persona_anual_2022_int_conocimiento_entidad$porcentaje_no_conocimiento



#### Falta de Internet por falta de conocimiento por localidad (Cuadro 30)
#Hogar
endutih_hogar_anual_2021_int_conocimiento_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_8 %in% c(2,3,4)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_hogar_anual_2021_int_conocimiento_localidad
c(7.08,10.08,14.03,17.00)-endutih_hogar_anual_2021_int_conocimiento_localidad$porcentaje_no_conocimiento

endutih_hogar_anual_2022_int_conocimiento_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_8 %in% c(2,3,4)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_hogar_anual_2022_int_conocimiento_localidad
c(7.11,11.31,15.00,17.36)-endutih_hogar_anual_2022_int_conocimiento_localidad$porcentaje_no_conocimiento

#Personas
endutih_persona_anual_2021_int_conocimiento_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P7_2 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_persona_anual_2021_int_conocimiento_localidad
c(9.93,14.53,21.74,29.16)-endutih_persona_anual_2021_int_conocimiento_localidad$porcentaje_no_conocimiento

endutih_persona_anual_2022_int_conocimiento_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P7_2 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_persona_anual_2022_int_conocimiento_localidad
c(9.35,14.14,18.98,26.63)-endutih_persona_anual_2022_int_conocimiento_localidad$porcentaje_no_conocimiento

##Nacional
#Hogares
nacional_hogar_int_conocimiento_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_8 %in% c(2,3,4))$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_int_conocimiento_2021
nacional_hogar_int_conocimiento_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_8 %in% c(2,3,4))$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_int_conocimiento_2022

#Personas
nacional_persona_int_conocimiento_2021 <- sum(subset(tr_endutih_usuario_anual_2021, P7_2 %in% c(2,3))$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_int_conocimiento_2021
nacional_persona_int_conocimiento_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P7_2 %in% c(2,3))$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_int_conocimiento_2022



#### Falta de dispositivos o celular por falta de conocimiento por entidad (Cuadro 31)
#Hogares
endutih_hogar_anual_2021_disp_conocimiento_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_hogar_anual_2021_disp_conocimiento_entidad, n=32)
c(18.67,18.73,19.10,17.56,20.83,23.41,21.84,24.87,13.26,24.51,20.14,22.38,21.51,22.74,17.24,24.96,18.94,28.81,18.59,26.32,19.71,17.51,17.20,20.16,21.50,20.36,21.30,20.08,20.39,25.96,21.15,22.70)-endutih_hogar_anual_2021_disp_conocimiento_entidad$porcentaje_no_conocimiento

endutih_hogar_anual_2022_disp_conocimiento_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_hogar_anual_2022_disp_conocimiento_entidad, n=32)
c(21.60,19.68,22.67,22.95,25.60,23.05,31.60,26.92,15.18,24.26,24.04,26.71,22.77,24.28,20.54,33.09,24.50,27.82,22.74,29.01,21.67,18.90,18.12,21.35,25.44,22.13,23.66,25.12,22.61,28.48,25.20,23.05)-endutih_hogar_anual_2022_disp_conocimiento_entidad$porcentaje_no_conocimiento

#Personas(Dispositivos)
endutih_persona_anual_2021_disp_conocimiento_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P6_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2021_disp_conocimiento_entidad, n=32)
c(29.30,26.06,31.19,33.76,28.64,29.86,46.32,32.46,24.70,34.34,38.85,43.14,40.13,32.31,28.42,42.93,35.00,37.00,26.63,45.89,40.45,31.11,28.23,36.48,33.33,26.27,35.21,31.77,39.01,40.42,34.02,36.06)-endutih_persona_anual_2021_disp_conocimiento_entidad$porcentaje_no_conocimiento

endutih_persona_anual_2022_disp_conocimiento_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P6_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2022_disp_conocimiento_entidad, n=32)
c(32.78,23.30,33.04,40.21,30.34,33.63,54.47,32.15,23.50,34.72,41.39,45.12,42.83,34.40,36.22,46.89,37.39,38.29,28.94,46.15,39.65,29.84,30.74,34.80,38.73,30.94,41.34,33.99,37.57,49.64,36.64,39.32)-endutih_persona_anual_2022_disp_conocimiento_entidad$porcentaje_no_conocimiento

#Personas(Celular)
endutih_persona_anual_2021_cel_conocimiento_entidad = tr_endutih_usuario2_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P8_2 %in% c(2,5)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2021_cel_conocimiento_entidad, n=32)
c(6.58,4.06,3.46,7.97,4.34,4.87,10.98,5.68,5.27,7.13,8.22,12.48,9.67,5.72,7.25,9.08,6.89,7.04,5.10,14.80,7.60,6.83,4.86,7.72,4.55,4.86,6.22,5.28,8.79,9.92,7.93,8.38)-endutih_persona_anual_2021_cel_conocimiento_entidad$porcentaje_no_conocimiento

endutih_persona_anual_2022_cel_conocimiento_entidad = tr_endutih_usuarios2_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P8_2 %in% c(2,5)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2022_cel_conocimiento_entidad, n=32)
c(4.34,2.88,4.06,8.90,4.36,4.67,14.81,4.97,5.03,5.80,7.72,11.12,7.06,6.98,6.09,9.32,5.88,6.61,5.01,13.08,7.82,5.35,6.12,7.97,5.27,3.09,7.15,3.75,7.37,11.06,8.06,7.70)-endutih_persona_anual_2022_cel_conocimiento_entidad$porcentaje_no_conocimiento



#### Falta de dispositivos o celular por falta de conocimiento por localidad (Cuadro 32)
#Hogares
endutih_hogar_anual_2021_disp_conocimiento_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_hogar_anual_2021_disp_conocimiento_localidad
c(16.71,20.30,23.72,26.51)-endutih_hogar_anual_2021_disp_conocimiento_localidad$porcentaje_no_conocimiento

endutih_hogar_anual_2022_disp_conocimiento_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_conocimiento = sum(FAC_HOG[P4_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_hogar_anual_2022_disp_conocimiento_localidad
c(19.57,23.55,26.93,29.99)-endutih_hogar_anual_2022_disp_conocimiento_localidad$porcentaje_no_conocimiento

#Personas(Dispositivos)
endutih_persona_anual_2021_disp_conocimiento_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P6_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_persona_anual_2021_disp_conocimiento_localidad
c(26.53,33.10,39.11,46.14)-endutih_persona_anual_2021_disp_conocimiento_localidad$porcentaje_no_conocimiento

endutih_persona_anual_2022_disp_conocimiento_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P6_3 %in% c(2,3)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_persona_anual_2022_disp_conocimiento_localidad
c(28.54,36.64,42.14,51.19)-endutih_persona_anual_2022_disp_conocimiento_localidad$porcentaje_no_conocimiento

#Personas(Celular)
endutih_persona_anual_2021_cel_conocimiento_localidad = tr_endutih_usuario2_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P8_2 %in% c(2,5)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2021_cel_conocimiento_localidad, n=32)
c(5.08,5.78,9.19,11.71)-endutih_persona_anual_2021_cel_conocimiento_localidad$porcentaje_no_conocimiento

endutih_persona_anual_2022_cel_conocimiento_localidad = tr_endutih_usuarios2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_conocimiento = sum(FAC_PER[P8_2 %in% c(2,5)], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_persona_anual_2022_cel_conocimiento_localidad, n=32)
c(4.74,6.27,7.97,11.92)-endutih_persona_anual_2022_cel_conocimiento_localidad$porcentaje_no_conocimiento

##Nacional
#Hogares
nacional_hogar_disp_conocimiento_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_3 %in% c(2,3))$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_disp_conocimiento_2021
nacional_hogar_disp_conocimiento_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_3 %in% c(2,3))$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_disp_conocimiento_2022

#Personas (Dispositivos)
nacional_persona_disp_conocimiento_2021 <- sum(subset(tr_endutih_usuario_anual_2021, P6_3 %in% c(2,3))$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_disp_conocimiento_2021
nacional_persona_disp_conocimiento_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_3 %in% c(2,3))$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_disp_conocimiento_2022

#Personas (Celular)
nacional_persona_cel_conocimiento_2021 <- sum(subset(tr_endutih_usuario2_anual_2021, P8_2 %in% c(2,5))$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_cel_conocimiento_2021
nacional_persona_cel_conocimiento_2022 <- sum(subset(tr_endutih_usuarios2_anual_2022, P8_2 %in% c(2,5))$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_cel_conocimiento_2022



#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por entidad (Cuadro 33)
#Sin competencias
endutih_persona_anual_2021_competencias_sin_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_sin = sum(FAC_PER[P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_persona_anual_2021_competencias_sin_entidad, n=32)
c(30.56,27.16,33.32,34.98,30.02,31.25,47.11,33.63,26.09,35.53,39.93,45.01,41.92,32.83,29.11,44.24,36.42,37.66,27.80,46.79,41.21,32.43,29.69,37.99,34.13,28.67,36.88,32.96,39.37,41.75,36.19,37.40)-endutih_persona_anual_2021_competencias_sin_entidad$porcentaje_comp_sin

endutih_persona_anual_2022_competencias_sin_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_sin = sum(FAC_PER[P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_persona_anual_2022_competencias_sin_entidad, n=32)
c(34.46,24.80,34.31,41.51,31.13,34.34,55.65,33.71,24.61,35.40,42.35,46.31,43.63,34.71,36.83,48.53,38.09,39.75,29.85,49.07,40.63,31.08,32.57,35.90,39.94,33.03,42.92,35.09,38.64,51.10,37.54,40.06)-endutih_persona_anual_2022_competencias_sin_entidad$porcentaje_comp_sin

#Competencias básicas
endutih_persona_anual_2021_competencias_basicas_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1], FAC_PER[P6_8_2 == 1], FAC_PER[P6_8_3 == 1], FAC_PER[P6_8_4 == 1]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_persona_anual_2021_competencias_basicas_entidad, n=32)
c(32.05,37.63,35.02,31.76,35.02,36.32,15.72,38.67,47.70,27.38,24.99,20.50,27.96,33.61,34.42,25.51,32.46,25.92,37.73,19.71,24.49,36.20,35.01,29.53,33.25,36.94,26.41,31.80,29.22,24.98,33.20,24.00)-endutih_persona_anual_2021_competencias_basicas_entidad$porcentaje_comp_basicas

endutih_persona_anual_2022_competencias_basicas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1], FAC_PER[P6_8_2 == 1], FAC_PER[P6_8_3 == 1], FAC_PER[P6_8_4 == 1]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_persona_anual_2022_competencias_basicas_entidad, n=32)
c(31.15,38.12,35.14,29.40,33.46,35.93,17.92,34.69,50.55,30.90,24.03,18.83,28.24,31.28,35.53,21.70,28.94,27.77,39.25,20.54,27.07,38.02,34.07,27.40,29.12,37.12,25.32,32.52,31.67,24.00,33.19,24.47)-endutih_persona_anual_2022_competencias_basicas_entidad$porcentaje_comp_basicas

#Competencias estándar
endutih_persona_anual_2021_competencias_estandar_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1], FAC_PER[P6_8_6 == 1], FAC_PER[P6_8_7 == 1]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_persona_anual_2021_competencias_estandar_entidad, n=32)
c(25.35,28.82,26.90,24.88,27.23,29.32,11.50,30.43,37.60,21.93,19.98,15.30,21.46,27.32,26.65,20.08,25.41,20.13,29.28,14.54,18.38,30.04,25.32,23.51,24.49,29.20,20.10,24.27,22.91,19.89,25.55,19.19)-endutih_persona_anual_2021_competencias_estandar_entidad$porcentaje_comp_estandar

endutih_persona_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1], FAC_PER[P6_8_6 == 1], FAC_PER[P6_8_7 == 1]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_persona_anual_2022_competencias_estandar_entidad, n=32)
c(24.79,29.46,28.03,22.89,27.18,29.76,13.91,27.32,39.25,24.02,18.97,14.24,22.00,23.70,27.69,17.20,21.79,22.12,32.09,15.71,21.49,29.72,26.44,21.40,22.36,30.29,19.00,25.65,25.44,18.99,27.21,18.04)-endutih_persona_anual_2022_competencias_estandar_entidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_persona_anual_2021_competencias_avanzadas_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_persona_anual_2021_competencias_avanzadas_entidad, n=32)
c(5.32,8.29,8.97,6.48,8.65,7.51,2.50,10.52,9.74,7.67,5.11,3.65,4.39,6.90,5.80,5.96,6.50,5.30,7.38,3.38,4.41,8.32,5.07,5.33,4.84,6.21,5.73,7.58,7.19,4.06,7.48,5.18)-endutih_persona_anual_2021_competencias_avanzadas_entidad$porcentaje_comp_avanzadas

endutih_persona_anual_2022_competencias_avanzadas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_persona_anual_2022_competencias_avanzadas_entidad, n=32)
c(8.47,7.18,9.88,5.56,8.33,7.13,3.79,7.62,8.40,8.04,5.10,3.14,4.71,6.47,7.92,4.69,5.69,5.62,10.30,3.69,6.29,6.33,4.96,4.53,5.56,7.33,3.31,6.94,9.57,4.21,6.71,5.26)-endutih_persona_anual_2022_competencias_avanzadas_entidad$porcentaje_comp_avanzadas



#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por localidad (Cuadro 34)
#Sin competencias
endutih_persona_anual_2021_competencias_sin_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_sin = sum(FAC_PER[P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
endutih_persona_anual_2021_competencias_sin_localidad
c(27.63,33.92,40.39,47.49)-endutih_persona_anual_2021_competencias_sin_localidad$porcentaje_comp_sin

endutih_persona_anual_2022_competencias_sin_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_sin = sum(FAC_PER[P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
endutih_persona_anual_2022_competencias_sin_localidad
c(29.44,37.64,43.26,52.79)-endutih_persona_anual_2022_competencias_sin_localidad$porcentaje_comp_sin

#Competencias básicas
endutih_persona_anual_2021_competencias_basicas_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1], FAC_PER[P6_8_2 == 1], FAC_PER[P6_8_3 == 1], FAC_PER[P6_8_4 == 1]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
endutih_persona_anual_2021_competencias_basicas_localidad
c(41.98,32.89,23.58,14.14)-endutih_persona_anual_2021_competencias_basicas_localidad$porcentaje_comp_basicas

endutih_persona_anual_2022_competencias_basicas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1], FAC_PER[P6_8_2 == 1], FAC_PER[P6_8_3 == 1], FAC_PER[P6_8_4 == 1]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
c(42.96,31.81,24.18,12.43)-endutih_persona_anual_2022_competencias_basicas_localidad$porcentaje_comp_basicas

#Competencias estándar
endutih_persona_anual_2021_competencias_estandar_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1], FAC_PER[P6_8_6 == 1], FAC_PER[P6_8_7 == 1]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
c(33.28,25.56,17.58,10.41)-endutih_persona_anual_2021_competencias_estandar_localidad$porcentaje_comp_estandar

endutih_persona_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1], FAC_PER[P6_8_6 == 1], FAC_PER[P6_8_7 == 1]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
c(34.15,24.69,18.77,9.04)-endutih_persona_anual_2022_competencias_estandar_localidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_persona_anual_2021_competencias_avanzadas_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
endutih_persona_anual_2021_competencias_avanzadas_localidad
c(8.77,6.18,3.94,2.41)-endutih_persona_anual_2021_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

endutih_persona_anual_2022_competencias_avanzadas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
endutih_persona_anual_2021_competencias_avanzadas_localidad
c(9.07,6.83,4.55,2.08)-endutih_persona_anual_2022_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

##Nacional
nacional_persona_competencias_sin_2021 <- sum(subset(tr_endutih_usuario_anual_2021, P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_competencias_sin_2021
nacional_persona_competencias_sin_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_competencias_sin_2022
#Competencias básicas
nacional_persona_competencias_basicas_2021 <- sum(c(subset(tr_endutih_usuario_anual_2021, P6_8_1 == 1)$FAC_PER, subset(tr_endutih_usuario_anual_2021, P6_8_2 == 1)$FAC_PER, subset(tr_endutih_usuario_anual_2021, P6_8_3 == 1)$FAC_PER, subset(tr_endutih_usuario_anual_2021, P6_8_4 == 1)$FAC_PER), na.rm=T)/(4*pond_usuario_2021)*100
nacional_persona_competencias_basicas_2021
nacional_persona_competencias_basicas_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_1 == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_2 == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_3 == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_4 == 1)$FAC_PER), na.rm=T)/(4*pond_usuario_2022)*100
nacional_persona_competencias_basicas_2022
#Competencias estándar
nacional_persona_competencias_estandar_2021 <- sum(c(subset(tr_endutih_usuario_anual_2021, P6_8_5 == 1)$FAC_PER, subset(tr_endutih_usuario_anual_2021, P6_8_6 == 1)$FAC_PER, subset(tr_endutih_usuario_anual_2021, P6_8_7 == 1)$FAC_PER), na.rm=T)/(3*pond_usuario_2021)*100
nacional_persona_competencias_estandar_2021
nacional_persona_competencias_estandar_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_5 == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_6 == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_7 == 1)$FAC_PER), na.rm=T)/(3*pond_usuario_2022)*100
nacional_persona_competencias_estandar_2022
#Competencias avanzadas
nacional_persona_competencias_avanzadas_2021 <- sum(subset(tr_endutih_usuario_anual_2021, P6_8_9 == 1)$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_competencias_avanzadas_2021
nacional_persona_competencias_avanzadas_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_8_9 == 1)$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_competencias_avanzadas_2022



#### Usa de servicios digitales gubernamentales por entidad (Cuadro 35)
endutih_persona_serviciosgob_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), uso = sum(FAC_PER[!is.na(P7_36)], na.rm=T)) %>%
  mutate(porcentaje_uso = uso/contador_pond*100)
print(endutih_persona_serviciosgob_entidad, n=32)
c(27.73,32.45,30.81,32.91,29.08,33.13,16.44,32.63,46.36,24.38,24.71,23.64,31.98,27.08,37.77,17.68,26.89,24.75,31.10,20.27,28.76,33.98,30.34,31.70,24.05,34.51,32.30,30.34,36.37,31.48,42.52,20.56)-endutih_persona_serviciosgob_entidad$porcentaje_uso



#### Usa de servicios digitales gubernamentales por localidad (Cuadro 36) 
endutih_persona_serviciosgob_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), uso = sum(FAC_PER[!is.na(P7_36)], na.rm=T)) %>%
  mutate(porcentaje_uso = uso/contador_pond*100)
endutih_persona_serviciosgob_localidad
c(39.66,31.39,25.28,15.99)-endutih_persona_serviciosgob_localidad$porcentaje_uso
## Nacional
nacional_persona_serviciosgob_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, !is.na(P7_36))$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_serviciosgob_2022



#### Falta de Internet o TIC por discapacidad por entidad (Cuadro 41)
endutih_persona_anual_2021_inttic_discapacidad_entidad = tr_endutih_usuusu2_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), discapacidad = sum(FAC_PER[P6_3 == 7 | P7_2 == 5 | P8_2 == 7 | P9_2 == 4  | P9_9 == 4], na.rm=T)) %>%
  mutate(porcentaje_discapacidad = discapacidad/contador_pond*100)
print(endutih_persona_anual_2021_inttic_discapacidad_entidad, n=32)
c(0.68,0.91,0.50,1.01,1.14,0.67,0.67,1.16,0.46,1.10,0.87,1.56,0.95,0.52,0.67,0.83,1.19,0.96,0.57,1.31,0.85,1.66,0.91,0.95,1.47,1.53,1.13,1.02,0.64,1.71,0.61,0.64)-endutih_persona_anual_2021_inttic_discapacidad_entidad$porcentaje_discapacidad

endutih_persona_anual_2022_inttic_discapacidad_entidad = tr_endutih_usuusu2_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), discapacidad = sum(FAC_PER[P6_3 == 7 | P7_2 == 5 | P8_2 == 7 | P9_2 == 4  | P9_8 == 4], na.rm=T)) %>%
  mutate(porcentaje_discapacidad = discapacidad/contador_pond*100)
print(endutih_persona_anual_2022_inttic_discapacidad_entidad, n=32)
c(0.65,0.39,0.95,0.56,1.06,0.76,1.30,0.94,0.58,0.91,0.97,1.52,1.35,0.54,0.46,0.92,0.90,0.84,1.34,1.07,0.92,0.92,0.72,1.87,1.38,0.85,1.84,1.56,0.95,0.64,0.43,1.41)-endutih_persona_anual_2022_inttic_discapacidad_entidad$porcentaje_discapacidad



#### Falta de Internet o TIC por discapacidad por localidad (Cuadro 42)
endutih_persona_anual_2021_inttic_discapacidad_localidad = tr_endutih_usuusu2_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), discapacidad = sum(FAC_PER[P6_3 == 7 | P7_2 == 5 | P8_2 == 7 | P9_2 == 4  | P9_9 == 4], na.rm=T)) %>%
  mutate(porcentaje_discapacidad = discapacidad/contador_pond*100)
endutih_persona_anual_2021_inttic_discapacidad_localidad
c(0.74,0.81,0.94,1.33)-endutih_persona_anual_2021_inttic_discapacidad_localidad$porcentaje_discapacidad

endutih_persona_anual_2022_inttic_discapacidad_localidad = tr_endutih_usuusu2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), discapacidad = sum(FAC_PER[P6_3 == 7 | P7_2 == 5 | P8_2 == 7 | P9_2 == 4  | P9_8 == 4], na.rm=T)) %>%
  mutate(porcentaje_discapacidad = discapacidad/contador_pond*100)
endutih_persona_anual_2022_inttic_discapacidad_localidad
c(0.80,0.81,0.85,1.18)-endutih_persona_anual_2022_inttic_discapacidad_localidad$porcentaje_discapacidad

##Nacional
nacional_persona_inttic_discapacidad_2021 <- sum(subset(tr_endutih_usuusu2_anual_2021, P6_3 == 7 | P7_2 == 5 | P8_2 == 7 | P9_2 == 4  | P9_8 == 4)$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_inttic_discapacidad_2021
nacional_persona_inttic_discapacidad_2022 <- sum(subset(tr_endutih_usuusu2_anual_2022, P6_3 == 7 | P7_2 == 5 | P8_2 == 7 | P9_2 == 4  | P9_8 == 4)$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_inttic_discapacidad_2022



#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por grupos de edad por entidad (Cuadro 43)
##Jóvenes
#Sin competencias
endutih_joven_anual_2022_competencias_sin_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_sin = sum(FAC_PER[EDAD < 18 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_joven_anual_2022_competencias_sin_entidad, n=32)
c(19.63,14.97,21.02,31.74,19.45,16.38,48.08,26.57,15.07,20.80,29.30,35.02,32.27,20.42,26.80,35.76,21.05,24.75,14.11,35.99,21.29,22.03,28.06,18.60,20.39,23.09,29.80,22.16,26.87,37.84,26.34,21.10)-endutih_joven_anual_2022_competencias_sin_entidad$porcentaje_comp_sin

#Competencias básicas 
endutih_joven_anual_2022_competencias_basicas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & EDAD < 18], FAC_PER[P6_8_2 == 1 & EDAD < 18], FAC_PER[P6_8_3 == 1 & EDAD < 18], FAC_PER[P6_8_4 == 1 & EDAD < 18]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_joven_anual_2022_competencias_basicas_entidad, n=32)
c(27.68,33.74,32.14,25.21,27.27,33.14,13.81,28.88,47.48,25.24,22.81,16.21,23.81,29.30,31.04,17.85,29.05,26.29,34.57,19.82,26.24,33.85,25.53,24.25,25.48,28.02,22.58,27.70,32.16,23.30,26.71,25.51)-endutih_joven_anual_2022_competencias_basicas_entidad$porcentaje_comp_basicas

#Competencias estándar 
endutih_joven_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & EDAD < 18], FAC_PER[P6_8_6 == 1 & EDAD < 18], FAC_PER[P6_8_7 == 1 & EDAD < 18]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_joven_anual_2022_competencias_estandar_entidad, n=32)
c(18.04,21.00,20.90,15.03,19.54,23.50,8.64,17.76,27.64,15.58,15.78,9.11,15.07,18.30,20.79,11.96,20.18,18.06,23.26,13.96,18.60,21.32,14.86,15.07,14.33,18.18,12.69,14.83,23.22,15.89,17.61,15.36)-endutih_joven_anual_2022_competencias_estandar_entidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_joven_anual_2022_competencias_avanzadas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & EDAD < 18], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_joven_anual_2022_competencias_avanzadas_entidad, n=32)
c(5.09,3.95,5.13,3.16,3.49,4.42,2.26,5.09,5.34,4.00,4.04,2.04,4.35,6.09,2.85,3.05,6.52,4.65,4.87,2.09,4.83,3.54,1.85,1.19,2.40,5.13,1.95,3.27,9.14,1.34,3.30,5.03)-endutih_joven_anual_2022_competencias_avanzadas_entidad$porcentaje_comp_avanzadas

##Adultos mayores
#Sin competencias
endutih_admay_anual_2022_competencias_sin_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_sin = sum(FAC_PER[EDAD > 59 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_admay_anual_2022_competencias_sin_entidad, n=32)
c(70.24,59.71,67.80,85.48,69.85,74.01,85.20,71.42,53.63,71.01,76.64,79.01,82.49,77.65,75.03,77.39,69.41,76.65,66.95,80.23,79.20,68.04,71.00,72.68,77.83,76.37,78.67,75.44,82.44,81.87,73.97,71.52)-endutih_admay_anual_2022_competencias_sin_entidad$porcentaje_comp_sin

#Competencias básicas 
endutih_admay_anual_2022_competencias_basicas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & EDAD > 59], FAC_PER[P6_8_2 == 1 & EDAD > 59], FAC_PER[P6_8_3 == 1 & EDAD > 59], FAC_PER[P6_8_4 == 1 & EDAD > 59]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_admay_anual_2022_competencias_basicas_entidad, n=32)
c(9.73,15.41,9.15,4.67,11.60,11.06,5.75,11.11,22.42,8.69,7.87,2.87,7.61,10.35,11.91,7.25,9.73,6.22,10.20,4.37,5.97,11.61,12.68,5.65,5.12,11.60,4.16,7.11,5.88,4.76,10.58,6.80)-endutih_admay_anual_2022_competencias_basicas_entidad$porcentaje_comp_basicas

#Competencias estándar 
endutih_admay_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & EDAD > 59], FAC_PER[P6_8_6 == 1 & EDAD > 59], FAC_PER[P6_8_7 == 1 & EDAD > 59]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_admay_anual_2022_competencias_estandar_entidad, n=32)
c(6.62,10.39,6.83,3.03,8.17,8.26,3.83,7.42,15.64,6.09,5.61,1.74,6.06,5.75,8.74,5.01,6.19,3.67,7.35,3.30,2.78,6.65,9.74,3.45,2.87,7.17,3.27,4.93,2.83,2.91,5.49,4.57)-endutih_admay_anual_2022_competencias_estandar_entidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_admay_anual_2022_competencias_avanzadas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & EDAD > 59], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_admay_anual_2022_competencias_avanzadas_entidad, n=32)
c(1.41,1.17,0.81,0.26,1.90,1.05,1.18,1.08,4.14,2.45,0.50,0.00,0.45,0.34,2.41,1.04,1.88,0.53,1.30,1.13,0.25,1.04,1.97,0.60,0.78,0.32,0.25,1.37,0.27,0.24,0.48,1.11)-endutih_admay_anual_2022_competencias_avanzadas_entidad$porcentaje_comp_avanzadas



#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por grupos de edad por localidad (Cuadro 44)
##Jóvenes
#Sin competencias
endutih_joven_anual_2022_competencias_sin_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_sin = sum(FAC_PER[EDAD < 18 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
endutih_joven_anual_2022_competencias_sin_localidad
c(17.48,24.49,27.92,38.70)-endutih_joven_anual_2022_competencias_sin_localidad$porcentaje_comp_sin

#Competencias básicas 
endutih_joven_anual_2022_competencias_basicas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & EDAD < 18], FAC_PER[P6_8_2 == 1 & EDAD < 18], FAC_PER[P6_8_3 == 1 & EDAD < 18], FAC_PER[P6_8_4 == 1 & EDAD < 18]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
endutih_joven_anual_2022_competencias_basicas_localidad
c(36.89,29.60,25.62,13.19)-endutih_joven_anual_2022_competencias_basicas_localidad$porcentaje_comp_basicas

#Competencias estándar 
endutih_joven_anual_2022_competencias_estandar_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & EDAD < 18], FAC_PER[P6_8_6 == 1 & EDAD < 18], FAC_PER[P6_8_7 == 1 & EDAD < 18]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
endutih_joven_anual_2022_competencias_estandar_localidad
c(23.79,19.19,17.11,8.08)-endutih_joven_anual_2022_competencias_estandar_localidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_joven_anual_2022_competencias_avanzadas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD < 18]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & EDAD < 18], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
endutih_joven_anual_2022_competencias_avanzadas_localidad
c(4.89,4.25,3.49,1.90)-endutih_joven_anual_2022_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

##Adultos mayores
#Sin competencias
endutih_admay_anual_2022_competencias_sin_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_sin = sum(FAC_PER[EDAD > 59 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
endutih_admay_anual_2022_competencias_sin_localidad
c(65.45,77.60,82.01,84.25)-endutih_admay_anual_2022_competencias_sin_localidad$porcentaje_comp_sin

#Competencias básicas 
endutih_admay_anual_2022_competencias_basicas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & EDAD > 59], FAC_PER[P6_8_2 == 1 & EDAD > 59], FAC_PER[P6_8_3 == 1 & EDAD > 59], FAC_PER[P6_8_4 == 1 & EDAD > 59]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
endutih_admay_anual_2022_competencias_basicas_localidad
c(15.99,6.62,4.20,1.45)-endutih_admay_anual_2022_competencias_basicas_localidad$porcentaje_comp_basicas

#Competencias estándar 
endutih_admay_anual_2022_competencias_estandar_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & EDAD > 59], FAC_PER[P6_8_6 == 1 & EDAD > 59], FAC_PER[P6_8_7 == 1 & EDAD > 59]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
endutih_admay_anual_2022_competencias_estandar_localidad
c(10.72,4.46,2.72,1.08)-endutih_admay_anual_2022_competencias_estandar_localidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_admay_anual_2022_competencias_avanzadas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & EDAD > 59], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
endutih_admay_anual_2022_competencias_avanzadas_localidad
c(2.25,0.69,0.50,0.22)-endutih_admay_anual_2022_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

##Nacional
#Jóvenes
#Sin competencias
nacional_joven_competencias_sin_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, EDAD < 18 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)))$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, EDAD < 18)$FAC_PER, na.rm=T)*100
nacional_joven_competencias_sin_2022
#Competencias básicas
nacional_joven_competencias_basicas_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_1 == 1 & EDAD < 18)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_2 == 1 & EDAD < 18)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_3 == 1 & EDAD < 18)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_4 == 1 & EDAD < 18)$FAC_PER), na.rm=T)/(4*sum(subset(tr_endutih_usuarios_anual_2022, EDAD < 18)$FAC_PER, na.rm=T))*100
nacional_joven_competencias_basicas_2022
#Competencias estándar
nacional_joven_competencias_estandar_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_5 == 1 & EDAD < 18)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_6 == 1 & EDAD < 18)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_7 == 1 & EDAD < 18)$FAC_PER), na.rm=T)/(3*sum(subset(tr_endutih_usuarios_anual_2022, EDAD < 18)$FAC_PER, na.rm=T))*100
nacional_joven_competencias_estandar_2022
#Competencias avanzadas
nacional_joven_competencias_avanzadas_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_8_9 == 1 & EDAD < 18)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, EDAD < 18)$FAC_PER, na.rm=T)*100
nacional_joven_competencias_avanzadas_2022

#Adultos mayores
#Sin competencias
nacional_admay_competencias_sin_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, EDAD > 59 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)))$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, EDAD > 59)$FAC_PER, na.rm=T)*100
nacional_admay_competencias_sin_2022
#Competencias básicas
nacional_admay_competencias_basicas_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_1 == 1 & EDAD > 59)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_2 == 1 & EDAD > 59)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_3 == 1 & EDAD > 59)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_4 == 1 & EDAD > 59)$FAC_PER), na.rm=T)/(4*sum(subset(tr_endutih_usuarios_anual_2022, EDAD > 59)$FAC_PER, na.rm=T))*100
nacional_admay_competencias_basicas_2022
#Competencias estándar
nacional_admay_competencias_estandar_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_5 == 1 & EDAD > 59)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_6 == 1 & EDAD > 59)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_7 == 1 & EDAD > 59)$FAC_PER), na.rm=T)/(3*sum(subset(tr_endutih_usuarios_anual_2022, EDAD > 59)$FAC_PER, na.rm=T))*100
nacional_admay_competencias_estandar_2022
#Competencias avanzadas
nacional_admay_competencias_avanzadas_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_8_9 == 1 & EDAD > 59)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, EDAD > 59)$FAC_PER, na.rm=T)*100
nacional_admay_competencias_avanzadas_2022



#### Adultos mayores con falta de internet y celular por falta de conocimiento por entidad (Cuadro 45)
#Sin Internet
endutih_admay_anual_2022_int_conocimiento_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), no_conocimiento = sum(FAC_PER[P7_2 %in% c(2,3) & EDAD > 59], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_admay_anual_2022_int_conocimiento_entidad, n=32)
c(40.92,39.90,47.80,62.65,49.02,48.76,71.62,47.75,30.92,54.94,61.12,65.39,65.94,53.52,51.10,63.58,47.77,51.08,41.74,71.72,62.65,42.63,43.83,57.62,49.22,40.21,62.33,45.20,65.79,61.77,47.16,56.00)-endutih_admay_anual_2022_int_conocimiento_entidad$porcentaje_no_conocimiento

#Sin celular
endutih_admay_anual_2022_cel_conocimiento_entidad = tr_endutih_usuarios2_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), no_conocimiento = sum(FAC_PER[P8_2 %in% c(2,5) & EDAD > 59], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
print(endutih_admay_anual_2022_cel_conocimiento_entidad, n=32)
c(17.88,12.35,19.67,34.97,17.06,20.40,39.99,19.53,21.56,21.49,32.63,36.71,29.54,33.16,27.30,27.84,23.04,22.46,23.11,36.65,32.47,23.20,20.34,33.17,19.38,13.69,29.85,15.30,32.09,37.89,28.37,30.85)-endutih_admay_anual_2022_cel_conocimiento_entidad$porcentaje_no_conocimiento



#### Adultos mayores con falta de internet y celular por falta de conocimiento por localidad (Cuadro 46)
#Sin Internet
endutih_admay_anual_2022_int_conocimiento_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), no_conocimiento = sum(FAC_PER[P7_2 %in% c(2,3) & EDAD > 59], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_admay_anual_2022_int_conocimiento_localidad
c(39.56,55.16,63.65,73.37)-endutih_admay_anual_2022_int_conocimiento_localidad$porcentaje_no_conocimiento

#Sin celular
endutih_admay_anual_2022_cel_conocimiento_localidad = tr_endutih_usuarios2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[EDAD > 59]), no_conocimiento = sum(FAC_PER[P8_2 %in% c(2,5) & EDAD > 59], na.rm=T)) %>%
  mutate(porcentaje_no_conocimiento = no_conocimiento/contador_pond*100)
endutih_admay_anual_2022_cel_conocimiento_localidad
c(20.90,26.50,32.62,39.66)-endutih_admay_anual_2022_cel_conocimiento_localidad$porcentaje_no_conocimiento

##Nacional
#Sin Internet
nacional_admay_cel_conocimiento_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P7_2 %in% c(2,3) & EDAD > 59)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, EDAD > 59)$FAC_PER, na.rm=T)*100
nacional_admay_cel_conocimiento_2022

#Sin celular
nacional_admay_cel_conocimiento_2022 <- sum(subset(tr_endutih_usuarios2_anual_2022, P8_2 %in% c(2,5) & EDAD > 59)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, EDAD > 59)$FAC_PER, na.rm=T)*100
nacional_admay_cel_conocimiento_2022



#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por nivel educativo por entidad (Cuadro 47)
##Nivel básico
#Sin competencias
endutih_nvlbas_anual_2022_competencias_sin_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17]), comp_sin = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_sin_entidad, n=32)
c(58.68,42.51,59.38,62.98,51.95,58.81,75.48,53.45,53.22,56.11,63.39,68.33,68.71,58.52,62.96,69.04,60.26,64.62,53.74,68.72,66.66,53.28,56.08,59.57,69.78,59.57,67.27,59.35,61.87,77.90,62.89,62.81)-endutih_nvlbas_anual_2022_competencias_sin_entidad$porcentaje_comp_sin

#Competencias básicas
endutih_nvlbas_anual_2022_competencias_basicas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_2 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_3 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_4 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_basicas_entidad, n=32)
c(7.13,14.48,10.25,7.68,8.56,8.17,3.10,8.19,9.87,7.97,5.13,2.82,4.84,7.50,9.49,4.02,7.16,4.76,11.38,3.51,6.00,10.63,7.56,4.66,4.97,9.07,3.51,7.90,4.89,2.65,7.33,5.50)-endutih_nvlbas_anual_2022_competencias_basicas_entidad$porcentaje_comp_basicas

#Competencias estándar
endutih_nvlbas_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_6 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_7 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_estandar_entidad, n=32)
c(4.56,9.00,6.61,4.52,6.06,5.22,1.79,4.65,5.53,4.55,2.91,1.76,2.65,3.65,5.58,2.28,3.84,2.77,7.50,2.04,3.37,6.22,4.81,2.86,2.94,4.77,1.44,5.42,3.25,0.93,4.81,3.37)-endutih_nvlbas_anual_2022_competencias_estandar_entidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_nvlbas_anual_2022_competencias_avanzadas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_avanzadas_entidad, n=32)
c(1.53,1.83,3.56,1.07,1.23,1.75,0.62,1.79,1.20,1.67,1.17,0.66,0.47,0.49,1.52,0.59,0.75,0.09,2.41,0.91,0.96,1.77,1.22,0.44,0.35,1.42,0.09,1.15,1.30,0.15,1.62,0.86)-endutih_nvlbas_anual_2022_competencias_avanzadas_entidad$porcentaje_comp_avanzadas

##Mayor a nivel básico
#Sin competencias
endutih_maynvlbas_anual_2022_competencias_sin_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_sin = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_maynvlbas_anual_2022_competencias_basicas_entidad, n=32)
c(17.19,13.80,17.17,20.51,19.09,19.70,22.63,16.32,13.15,19.32,17.21,23.46,19.25,17.55,17.59,22.59,22.69,21.53,17.57,21.93,19.36,13.31,11.26,17.69,25.88,14.18,23.30,19.09,20.75,23.87,16.97,17.62)-endutih_maynvlbas_anual_2022_competencias_sin_entidad$porcentaje_comp_sin

#Competencias básicas
endutih_maynvlbas_anual_2022_competencias_basicas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_2 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_3 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_4 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_maynvlbas_anual_2022_competencias_basicas_entidad, n=32)
c(61.00,62.82,60.93,60.93,61.93,64.62,54.07,67.60,71.50,65.37,57.40,46.29,62.20,58.43,61.24,58.90,54.65,54.94,64.48,56.15,56.99,69.55,66.19,58.06,50.38,66.36,53.21,58.40,62.09,53.49,65.04,59.85)-endutih_maynvlbas_anual_2022_competencias_basicas_entidad$porcentaje_comp_basicas

#Competencias estándar
endutih_maynvlbas_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_6 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_7 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_maynvlbas_anual_2022_competencias_estandar_entidad, n=32)
c(52.62,53.51,52.67,52.75,53.00,57.09,45.69,58.32,59.15,56.07,49.08,38.77,52.51,48.90,50.92,50.56,44.06,46.86,56.58,45.47,48.91,59.78,55.38,48.91,42.05,58.72,44.37,50.60,52.62,45.68,57.23,49.32)-endutih_maynvlbas_anual_2022_competencias_estandar_entidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_maynvlbas_anual_2022_competencias_avanzadas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & EDAD > 17]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & NIVEL > 3 & EDAD > 17], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_maynvlbas_anual_2022_competencias_avanzadas_entidad, n=32)
c(18.75,14.04,18.58,13.22,18.60,13.81,12.28,15.63,12.81,19.50,12.71,8.09,10.54,13.24,16.02,14.07,11.02,12.54,19.40,10.99,14.83,12.90,10.64,11.88,11.36,13.62,8.11,14.38,19.41,11.70,14.28,13.98)-endutih_maynvlbas_anual_2022_competencias_avanzadas_entidad$porcentaje_comp_avanzadas



#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por nivel educativo por localidad (Cuadro 48)
##Nivel básico
#Sin competencias
endutih_nvlbas_anual_2022_competencias_sin_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17]), comp_sin = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_sin_localidad, n=32)
c(56.16,64.00,65.89,68.72)-endutih_nvlbas_anual_2022_competencias_sin_localidad$porcentaje_comp_sin

#Competencias básicas
endutih_nvlbas_anual_2022_competencias_basicas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_2 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_3 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_4 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_basicas_localidad, n=32)
c(11.54,6.45,4.59,2.56)-endutih_nvlbas_anual_2022_competencias_basicas_localidad$porcentaje_comp_basicas

#Competencias estándar
endutih_nvlbas_anual_2022_competencias_estandar_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL %in% c(0,1,2,3,99) & EDAD > 17]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_6 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17], FAC_PER[P6_8_7 == 1 & NIVEL %in% c(0,2,3,99) & EDAD > 17]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_estandar_localidad, n=32)
c(6.97,3.60,2.34,1.42)-endutih_nvlbas_anual_2022_competencias_estandar_localidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_nvlbas_anual_2022_competencias_avanzadas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_sin = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_nvlbas_anual_2022_competencias_avanzadas_localidad, n=32)
c(1.85,0.86,0.61,0.44)-endutih_nvlbas_anual_2022_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

#Mayor a nivel básico
#Sin competencias
endutih_maynvlbas_anual_2022_competencias_sin_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_sin = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_maynvlbas_anual_2022_competencias_basicas_localidad, n=32)
c(16.20,18.68,20.62,25.75)-endutih_maynvlbas_anual_2022_competencias_sin_localidad$porcentaje_comp_sin

#Competencias básicas
endutih_maynvlbas_anual_2022_competencias_basicas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_2 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_3 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_4 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_maynvlbas_anual_2022_competencias_basicas_localidad, n=32)
c(66.44,59.44,53.36,41.52)-endutih_maynvlbas_anual_2022_competencias_basicas_localidad$porcentaje_comp_basicas

#Competencias estándar
endutih_maynvlbas_anual_2022_competencias_estandar_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_6 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], FAC_PER[P6_8_7 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_maynvlbas_anual_2022_competencias_estandar_localidad, n=32)
c(56.47,49.99,45.57,33.90)-endutih_maynvlbas_anual_2022_competencias_estandar_localidad$porcentaje_comp_estandar

#Competencias avanzadas
endutih_maynvlbas_anual_2022_competencias_avanzadas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[NIVEL > 3 & NIVEL < 99 & EDAD > 17]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
endutih_maynvlbas_anual_2022_competencias_avanzadas_localidad
c(15.56,14.65,11.52,7.36)-endutih_maynvlbas_anual_2022_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

##Nacional
#Nivel básico
#Sin competencias
nacional_nvlbas_competencias_sin_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, NIVEL %in% c(0,1,2,3,99) & EDAD > 17 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)))$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, na.rm=T)*100
nacional_nvlbas_competencias_sin_2022
#Competencias básicas
nacional_nvlbas_competencias_basicas_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_1 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_2 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_3 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_4 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER), na.rm=T)/(4*sum(subset(tr_endutih_usuarios_anual_2022, NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, na.rm=T))*100
nacional_nvlbas_competencias_basicas_2022
#Competencias estándar
nacional_nvlbas_competencias_estandar_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_5 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_6 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_7 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER), na.rm=T)/(3*sum(subset(tr_endutih_usuarios_anual_2022, NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, na.rm=T))*100
nacional_nvlbas_competencias_estandar_2022
#Competencias avanzadas
nacional_nvlbas_competencias_avanzadas_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_8_9 == 1 & NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, NIVEL %in% c(0,1,2,3,99) & EDAD > 17)$FAC_PER, na.rm=T)*100
nacional_nvlbas_competencias_avanzadas_2022

#Mayor a nivel básico
#Sin competencias
nacional_maynvlbas_competencias_sin_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, NIVEL > 3 & NIVEL < 99 & EDAD > 17 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)))$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, na.rm=T)*100
nacional_maynvlbas_competencias_sin_2022
#Competencias básicas
nacional_maynvlbas_competencias_basicas_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_1 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_2 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_3 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_4 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER), na.rm=T)/(4*sum(subset(tr_endutih_usuarios_anual_2022, NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, na.rm=T))*100
nacional_maynvlbas_competencias_basicas_2022
#Competencias estándar
nacional_maynvlbas_competencias_estandar_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_5 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_6 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_7 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER), na.rm=T)/(3*sum(subset(tr_endutih_usuarios_anual_2022, NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, na.rm=T))*100
nacional_maynvlbas_competencias_estandar_2022
#Competencias avanzadas
nacional_maynvlbas_competencias_avanzadas_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_8_9 == 1 & NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, NIVEL > 3 & NIVEL < 99 & EDAD > 17)$FAC_PER, na.rm=T)*100
nacional_maynvlbas_competencias_avanzadas_2022


#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por sexo por entidad (Cuadro 49)
##Mujeres
#Sin competencias
endutih_mujeres_anual_2022_competencias_sin_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_sin = sum(FAC_PER[SEXO == 2 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_sin_entidad, n=32)
c(38.24,26.09,34.44,43.04,30.77,35.11,57.91,35.14,26.04,35.04,45.53,45.97,44.00,36.16,38.86,47.42,38.65,37.95,32.62,50.10,43.11,34.63,35.34,36.53,39.51,32.71,46.41,37.85,38.21,51.41,39.79,40.74)-endutih_mujeres_anual_2022_competencias_sin_entidad$porcentaje_comp_sin

#Competencias Básicas
endutih_mujeres_anual_2022_competencias_basicas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & SEXO == 2], FAC_PER[P6_8_2 == 1 & SEXO == 2], FAC_PER[P6_8_3 == 1 & SEXO == 2], FAC_PER[P6_8_4 == 1 & SEXO == 2]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_basicas_entidad, n=32)
c(27.59,36.62,35.50,27.67,33.52,34.93,15.36,31.21,48.38,28.54,21.30,17.52,28.49,28.32,35.09,22.02,27.00,29.04,36.10,18.84,24.29,35.12,32.61,27.14,29.54,36.27,22.70,31.49,31.59,22.77,30.39,24.70)-endutih_mujeres_anual_2022_competencias_basicas_entidad$porcentaje_comp_basicas

#Competencias Estándar
endutih_mujeres_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & SEXO == 2], FAC_PER[P6_8_6 == 1 & SEXO == 2], FAC_PER[P6_8_7 == 1 & SEXO == 2]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_estandar_entidad, n=32)
c(21.40,28.42,28.12,20.81,26.57,28.43,11.71,24.29,37.43,21.52,16.29,12.96,22.00,20.68,26.62,16.55,19.64,23.20,28.36,13.85,19.11,26.13,24.19,20.67,22.62,30.24,16.89,24.09,25.00,17.72,24.24,17.67)-endutih_mujeres_anual_2022_competencias_estandar_entidad$porcentaje_comp_estandar

#Competencias Avanzadas
endutih_mujeres_anual_2022_competencias_avanzadas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & SEXO == 2], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_avanzadas_entidad, n=32)
c(7.16,6.26,9.50,4.34,8.13,5.46,2.57,5.76,5.80,6.54,3.94,2.79,3.77,6.02,6.61,3.68,3.86,6.30,8.92,2.40,4.37,4.72,3.78,3.64,4.84,6.88,3.02,5.02,9.09,3.82,4.89,4.77)-endutih_mujeres_anual_2022_competencias_avanzadas_entidad$porcentaje_comp_avanzadas

##Hombres
#Sin competencias
endutih_mujeres_anual_2022_competencias_sin_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_sin = sum(FAC_PER[SEXO == 1 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_sin_entidad, n=32)
c(30.34,23.49,34.18,39.86,31.50,33.51,53.11,32.26,22.99,35.77,38.72,46.73,43.19,33.15,34.61,49.73,37.46,41.69,26.92,47.88,37.71,27.16,29.77,35.19,40.40,33.36,39.14,32.18,39.13,50.75,35.17,39.32)-endutih_mujeres_anual_2022_competencias_sin_entidad$porcentaje_comp_sin

#Competencias Básicas
endutih_hombres_anual_2022_competencias_basicas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & SEXO == 1], FAC_PER[P6_8_2 == 1 & SEXO == 1], FAC_PER[P6_8_3 == 1 & SEXO == 1], FAC_PER[P6_8_4 == 1 & SEXO == 1]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_hombres_anual_2022_competencias_basicas_entidad, n=32)
c(35.03,39.65,34.78,31.25,33.39,37.02,20.80,38.23,53.00,33.32,27.14,20.36,27.94,34.48,36.00,21.36,31.10,26.41,42.58,22.49,30.32,41.22,35.55,27.70,28.67,38.01,28.16,33.61,31.76,25.43,36.14,24.23)-endutih_hombres_anual_2022_competencias_basicas_entidad$porcentaje_comp_basicas

#Competencias Estándar
endutih_hombres_anual_2022_competencias_estandar_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & SEXO == 1], FAC_PER[P6_8_6 == 1 & SEXO == 1], FAC_PER[P6_8_7 == 1 & SEXO == 1]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_hombres_anual_2022_competencias_estandar_entidad, n=32)
c(28.49,30.53,27.95,25.14,27.80,31.20,16.38,30.40,41.31,26.58,22.02,15.76,22.00,26.98,28.85,17.91,24.18,20.97,36.04,17.85,24.29,33.69,28.71,22.24,22.08,30.34,21.28,27.30,25.95,20.48,30.34,18.44)-endutih_hombres_anual_2022_competencias_estandar_entidad$porcentaje_comp_estandar

#Competencias Avanzadas
endutih_hombres_anual_2022_competencias_avanzadas_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & SEXO == 1], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_hombres_anual_2022_competencias_avanzadas_entidad, n=32)
c(9.90,8.11,10.26,6.88,8.54,8.95,5.16,9.50,11.32,9.58,6.42,3.55,5.83,6.96,9.35,5.79,7.74,4.88,11.77,5.16,8.54,8.11,6.16,5.53,6.33,7.79,3.62,8.97,10.13,4.66,8.64,5.79)-endutih_hombres_anual_2022_competencias_avanzadas_entidad$porcentaje_comp_avanzadas


#### Porcentaje de personas sin competencias, con competencias básicas, estándar y avanzadas por sexo por localidad (Cuadro 50)
##Mujeres
#Sin competencias
endutih_mujeres_anual_2022_competencias_sin_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_sin = sum(FAC_PER[SEXO == 2 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
endutih_mujeres_anual_2022_competencias_sin_localidad
c(31.56,38.97,45.34,52.80)-endutih_mujeres_anual_2022_competencias_sin_localidad$porcentaje_comp_sin

#Competencias Básicas
endutih_mujeres_anual_2022_competencias_basicas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & SEXO == 2], FAC_PER[P6_8_2 == 1 & SEXO == 2], FAC_PER[P6_8_3 == 1 & SEXO == 2], FAC_PER[P6_8_4 == 1 & SEXO == 2]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_basicas_localidad, n=32)
c(40.46,29.44,23.33,12.53)-endutih_mujeres_anual_2022_competencias_basicas_localidad$porcentaje_comp_basicas

#Competencias Estándar
endutih_mujeres_anual_2022_competencias_estandar_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & SEXO == 2], FAC_PER[P6_8_6 == 1 & SEXO == 2], FAC_PER[P6_8_7 == 1 & SEXO == 2]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_estandar_localidad, n=32)
c(31.52,22.00,17.87,9.06)-endutih_mujeres_anual_2022_competencias_estandar_localidad$porcentaje_comp_estandar

#Competencias Avanzadas
endutih_mujeres_anual_2022_competencias_avanzadas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 2]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & SEXO == 2], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_mujeres_anual_2022_competencias_avanzadas_localidad, n=32)
c(7.24,5.46,3.65,2.07)-endutih_mujeres_anual_2022_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

##Hombres
#Sin competencias
endutih_hombres_anual_2022_competencias_sin_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_sin = sum(FAC_PER[SEXO == 1 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2))], na.rm=T) ) %>%
  mutate(porcentaje_comp_sin = comp_sin/contador_pond*100)
endutih_hombres_anual_2022_competencias_sin_localidad
c(27.12,36.12,41.10,52.78)-endutih_hombres_anual_2022_competencias_sin_localidad$porcentaje_comp_sin

#Competencias Básicas
endutih_hombres_anual_2022_competencias_basicas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_basicas = sum(c(FAC_PER[P6_8_1 == 1 & SEXO == 1], FAC_PER[P6_8_2 == 1 & SEXO == 1], FAC_PER[P6_8_3 == 1 & SEXO == 1], FAC_PER[P6_8_4 == 1 & SEXO == 1]), na.rm=T)/4) %>%
  mutate(porcentaje_comp_basicas = comp_basicas/contador_pond*100)
print(endutih_hombres_anual_2022_competencias_basicas_localidad, n=32)
c(45.71,34.50,25.06,12.33)-endutih_hombres_anual_2022_competencias_basicas_localidad$porcentaje_comp_basicas

#Competencias Estándar
endutih_hombres_anual_2022_competencias_estandar_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_estandar = sum(c(FAC_PER[P6_8_5 == 1 & SEXO == 1], FAC_PER[P6_8_6 == 1 & SEXO == 1], FAC_PER[P6_8_7 == 1 & SEXO == 1]), na.rm=T)/3) %>%
  mutate(porcentaje_comp_estandar = comp_estandar/contador_pond*100)
print(endutih_hombres_anual_2022_competencias_estandar_localidad, n=32)
c(37.04,27.75,19.71,9.03)-endutih_hombres_anual_2022_competencias_estandar_localidad$porcentaje_comp_estandar

#Competencias Avanzadas
endutih_hombres_anual_2022_competencias_avanzadas_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER[SEXO == 1]), comp_avanzadas = sum(FAC_PER[P6_8_9 == 1 & SEXO == 1], na.rm=T)) %>%
  mutate(porcentaje_comp_avanzadas = comp_avanzadas/contador_pond*100)
print(endutih_hombres_anual_2022_competencias_avanzadas_localidad, n=32)
c(11.09,8.39,5.49,2.09)-endutih_hombres_anual_2022_competencias_avanzadas_localidad$porcentaje_comp_avanzadas

##Nacional
#Mujeres
#Sin competencias
nacional_mujeres_competencias_sin_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 2 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)))$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 2)$FAC_PER, na.rm=T)*100
nacional_mujeres_competencias_sin_2022
#Competencias básicas
nacional_mujeres_competencias_basicas_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_1 == 1 & SEXO == 2)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_2 == 1 & SEXO == 2)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_3 == 1 & SEXO == 2)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_4 == 1 & SEXO == 2)$FAC_PER), na.rm=T)/(4*sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 2)$FAC_PER, na.rm=T))*100
nacional_mujeres_competencias_basicas_2022
#Competencias estándar
nacional_mujeres_competencias_estandar_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_5 == 1 & SEXO == 2)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_6 == 1 & SEXO == 2)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_7 == 1 & SEXO == 2)$FAC_PER), na.rm=T)/(3*sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 2)$FAC_PER, na.rm=T))*100
nacional_mujeres_competencias_estandar_2022
#Competencias avanzadas
nacional_mujeres_competencias_avanzadas_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_8_9 == 1 & SEXO == 2)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 2)$FAC_PER, na.rm=T)*100
nacional_mujeres_competencias_avanzadas_2022

#Hombres
#Sin competencias
nacional_hombres_competencias_sin_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 1 & (P6_3 %in% c(2,3,6)  | (P6_8_1==2 & P6_8_2==2 & P6_8_3==2 & P6_8_4==2 & P6_8_5==2 & P6_8_6==2 & P6_8_7==2 & P6_8_8==2 & P6_8_9==2)))$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 1)$FAC_PER, na.rm=T)*100
nacional_hombres_competencias_sin_2022
#Competencias básicas
nacional_hombres_competencias_basicas_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_1 == 1 & SEXO == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_2 == 1 & SEXO == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_3 == 1 & SEXO == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_4 == 1 & SEXO == 1)$FAC_PER), na.rm=T)/(4*sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 1)$FAC_PER, na.rm=T))*100
nacional_hombres_competencias_basicas_2022
#Competencias estándar
nacional_hombres_competencias_estandar_2022 <- sum(c(subset(tr_endutih_usuarios_anual_2022, P6_8_5 == 1 & SEXO == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_6 == 1 & SEXO == 1)$FAC_PER, subset(tr_endutih_usuarios_anual_2022, P6_8_7 == 1 & SEXO == 1)$FAC_PER), na.rm=T)/(3*sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 1)$FAC_PER, na.rm=T))*100
nacional_hombres_competencias_estandar_2022
#Competencias avanzadas
nacional_hombres_competencias_avanzadas_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, P6_8_9 == 1 & SEXO == 1)$FAC_PER, na.rm=T)/sum(subset(tr_endutih_usuarios_anual_2022, SEXO == 1)$FAC_PER, na.rm=T)*100
nacional_hombres_competencias_avanzadas_2022



#### Porcentaje de hogares y personas sin internet por entidad (Cuadro A.8)
#Hogares
endutih_hogar_anual_2021_int_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[P4_4 == 2], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2021_int_entidad , n=32)
c(28.44,20.86,27.45,38.17,28.60,26.30,69.22,31.30,14.94,44.17,37.91,44.46,45.31,29.02,23.08,40.55,30.72,36.90,22.46,60.98,44.16,25.91,27.61,38.24,23.06,13.79,47.28,30.25,44.29,48.86,33.62,36.76)-endutih_hogar_anual_2021_int_entidad$porcentaje_falta

endutih_hogar_anual_2022_int_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[P4_4 == 2], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2022_int_entidad , n=32)
c(24.15,16.90,24.57,34.98,30.03,23.64,61.57,28.92,14.04,40.02,35.00,46.43,41.72,24.30,24.06,43.15,30.17,36.19,18.49,52.58,41.95,22.63,22.52,34.94,19.75,19.37,42.22,31.46,44.45,46.07,34.13,31.44)-endutih_hogar_anual_2022_int_entidad$porcentaje_falta

#Personas
endutih_persona_anual_2021_int_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P7_2)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2021_int_entidad , n=32)
c(19.65,13.18,16.52,24.29,19.22,17.08,53.94,19.40,11.69,29.12,28.05,38.59,30.66,17.80,18.17,33.23,21.67,24.07,15.79,43.10,33.12,20.17,17.66,25.69,20.29,14.16,31.28,18.08,25.58,32.92,20.06,27.67)-endutih_persona_anual_2021_int_entidad$porcentaje_falta

endutih_persona_anual_2022_int_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
 summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P7_2)], na.rm=T)) %>%
 mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2022_int_entidad , n=32)
c(13.09,10.23,15.33,20.19,18.48,14.70,43.33,17.48,10.26,21.10,24.07,32.52,24.63,16.66,18.14,32.06,21.92,21.11,13.89,37.47,29.86,15.92,16.39,24.85,18.17,14.08,22.73,15.37,22.62,27.28,17.13,22.31)-endutih_persona_anual_2022_int_entidad$porcentaje_falta



#### Porcentaje de hogares y personas sin internet por localidad (Cuadro A.9)
#Hogares
endutih_hogar_anual_2021_int_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[P4_4 == 2], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2021_int_localidad , n=32)
c(19.09,30.21,44.67,59.53)-endutih_hogar_anual_2021_int_localidad$porcentaje_falta

endutih_hogar_anual_2022_int_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[P4_4 == 2], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2022_int_localidad , n=32)
c(17.28,28.32,42.03,57.32)-endutih_hogar_anual_2022_int_localidad$porcentaje_falta

#Personas
endutih_persona_anual_2021_int_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P7_2)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2021_int_localidad , n=32)
c(14.03,19.81,31.06,43.45)-endutih_persona_anual_2021_int_localidad$porcentaje_falta

endutih_persona_anual_2022_int_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P7_2)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2022_int_localidad , n=32)
c(12.44,19.29,25.57,37.67)-endutih_persona_anual_2022_int_localidad$porcentaje_falta

## Nacional
#Hogares
nacional_hogar_int_2021 <- sum(subset(tr_endutih_hogar_anual_2021, P4_4 == 2)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_int_2021
nacional_hogar_int_2022 <- sum(subset(tr_endutih_hogares_anual_2022, P4_4 == 2)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_int_2022

#Personas
nacional_persona_int_2021 <- sum(subset(tr_endutih_usuario_anual_2021, !is.na(P7_2))$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_int_2021
nacional_persona_int_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, !is.na(P7_2))$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_int_2022



#### Porcentaje de hogares y personas sin dispositivos por entidad (Cuadro A.10)
#Hogares
endutih_hogar_anual_2021_disp_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[!is.na(P4_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2021_disp_entidad , n=32)
c(50.24,44.75,46.85,55.24,51.81,51.15,77.83,47.91,32.96,56.38,59.48,70.72,63.07,51.17,52.35,61.38,53.83,59.47,45.51,71.58,62.89,47.99,53.12,57.30,54.10,48.84,65.00,53.62,63.31,67.34,55.31,63.24)-endutih_hogar_anual_2021_disp_entidad$porcentaje_falta

endutih_hogar_anual_2022_disp_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[!is.na(P4_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2022_disp_entidad , n=32)
c(48.00,42.13,48.12,58.47,54.96,51.69,75.48,51.43,35.92,56.61,62.13,72.34,62.02,50.36,53.61,66.15,57.76,58.84,43.83,72.09,63.27,50.06,55.21,59.90,54.87,51.12,65.61,55.19,61.55,68.12,55.34,60.71)-endutih_hogar_anual_2022_disp_entidad$porcentaje_falta

#Personas
endutih_persona_anual_2021_disp_entidad = tr_endutih_usuario_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P6_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2021_disp_entidad , n=32)
c(60.45,55.10,56.36,63.06,58.28,56.34,80.53,54.12,44.62,67.24,69.68,75.54,67.66,60.21,59.05,69.09,61.91,67.77,53.57,76.56,70.08,58.11,57.24,64.75,60.73,54.22,68.40,60.57,66.38,70.15,60.28,70.75)-endutih_persona_anual_2021_disp_entidad$porcentaje_falta

endutih_persona_anual_2022_disp_entidad = tr_endutih_usuarios_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P6_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2022_disp_entidad , n=32)
c(61.63,51.61,58.73,65.24,60.14,57.18,78.49,58.01,44.31,62.65,68.83,76.58,67.71,62.41,59.75,73.19,64.56,64.83,52.64,75.42,68.32,55.38,60.51,67.13,64.73,55.87,69.00,61.36,63.45,71.93,61.16,70.10)-endutih_persona_anual_2022_disp_entidad$porcentaje_falta

#### Porcentaje de hogares y personas sin dispositivos por localidad (Cuadro A.11)
#Hogares
endutih_hogar_anual_2021_disp_localidad = tr_endutih_hogar_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[!is.na(P4_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2021_disp_localidad , n=32)
c(42.10,53.31,65.30,77.89)-endutih_hogar_anual_2021_disp_localidad$porcentaje_falta

endutih_hogar_anual_2022_disp_localidad = tr_endutih_hogares_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), falta = sum(FAC_HOG[!is.na(P4_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_hogar_anual_2022_disp_localidad , n=32)
c(42.62,54.52,66.48,79.43)-endutih_hogar_anual_2022_disp_localidad$porcentaje_falta

#Personas
endutih_persona_anual_2021_disp_localidad = tr_endutih_usuario_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P6_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2021_disp_localidad , n=32)
c(50.62,60.70,70.61,82.24)-endutih_persona_anual_2021_disp_localidad$porcentaje_falta

endutih_persona_anual_2022_disp_localidad = tr_endutih_usuarios_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), falta = sum(FAC_PER[!is.na(P6_3)], na.rm=T)) %>%
  mutate(porcentaje_falta = falta/contador_pond*100)
print(endutih_persona_anual_2022_disp_localidad , n=32)
c(50.42,61.82,70.93,83.74)-endutih_persona_anual_2022_disp_localidad$porcentaje_falta

## Nacional
#Hogares
nacional_hogar_disp_2021 <- sum(subset(tr_endutih_hogar_anual_2021, !is.na(P4_3))$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_disp_2021
nacional_hogar_disp_2022 <- sum(subset(tr_endutih_hogares_anual_2022, !is.na(P4_3))$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_disp_2022

#Personas
nacional_persona_disp_2021 <- sum(subset(tr_endutih_usuario_anual_2021, !is.na(P6_3))$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_disp_2021
nacional_persona_disp_2022 <- sum(subset(tr_endutih_usuarios_anual_2022, !is.na(P6_3))$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_disp_2022



#### Porcentaje de hogares y personas sin celular por entidad (Cuadro A.12)
#Hogares
endutih_hogar_anual_2021_cel_entidad = tr_endutih_hogar_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_hogar_anual_2021_cel_entidad, n=32)
c(4.55,2.87,2.81,6.57,4.63,4.73,18.17,4.09,2.98,6.57,7.85,12.85,9.75,4.82,4.87,6.96,5.08,5.70,3.95,15.19,6.75,5.25,3.97,7.52,3.66,3.48,7.79,4.13,6.07,9.58,5.60,9.37)-endutih_hogar_anual_2021_cel_entidad$porcentaje_no_uso

endutih_hogar_anual_2022_cel_entidad = tr_endutih_hogares_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_hogar_anual_2022_cel_entidad, n=32)
c(5.07,3.35,2.01,6.31,4.09,4.45,14.19,4.58,3.29,6.16,7.69,7.10,7.28,5.30,5.13,7.32,5.01,6.48,3.54,13.10,8.49,4.62,5.18,8.19,3.92,4.03,8.03,3.04,5.96,9.09,5.16,7.21)-endutih_hogar_anual_2022_cel_entidad$porcentaje_no_uso

#Personas
endutih_persona_anual_2021_cel_entidad = tr_endutih_usuario2_anual_2021 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_uso = sum(FAC_PER[P8_1 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)  
print(endutih_persona_anual_2021_cel_entidad, n=32)
c(17.83,13.38,13.27,21.73,18.82,15.16,41.89,13.70,11.06,21.93,21.59,31.38,21.08,15.09,18.47,24.17,17.54,18.52,14.18,35.15,24.94,17.11,16.78,19.91,13.00,13.96,26.01,14.42,22.48,27.73,15.30,25.47)-endutih_persona_anual_2021_cel_entidad$porcentaje_no_uso

endutih_persona_anual_2022_cel_entidad  = tr_endutih_usuarios2_anual_2022 %>% group_by(ENT) %>%
  summarise(contador_pond = sum(FAC_PER), no_uso = sum(FAC_PER[P8_1 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)  
print(endutih_persona_anual_2022_cel_entidad, n=32) 
c(16.05,9.09,11.81,23.88,15.88,13.78,37.73,13.93,11.68,19.59,20.97,32.43,25.60,15.32,15.85,24.49,14.33,18.49,13.69,34.50,27.39,17.47,17.69,23.95,15.34,12.10,21.45,12.75,25.12,25.84,19.05,24.49)-endutih_persona_anual_2022_cel_entidad$porcentaje_no_uso



#### Porcentaje de hogares y personas sin celular por localidad (Cuadro A.13)
#Hogares
endutih_hogar_anual_2021_cel_localidad = tr_endutih_hogresusu2_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_hogar_anual_2021_cel_localidad, n=32)
c(3.39,4.58,7.05,13.98)-endutih_hogar_anual_2021_cel_localidad$porcentaje_no_uso

endutih_hogar_anual_2022_cel_localidad = tr_endutih_hogresusu2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_HOG), no_uso = sum(FAC_HOG[P4_1_6 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)
print(endutih_hogar_anual_2022_cel_localidad, n=32)
c(3.34,4.43,7.02,12.82)-endutih_hogar_anual_2022_cel_localidad$porcentaje_no_uso

#Personas
endutih_persona_anual_2021_cel_localidad = tr_endutih_usuario2_anual_2021 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_uso = sum(FAC_PER[P8_1 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)  
print(endutih_persona_anual_2021_cel_localidad, n=32)
c(13.40,16.59,23.69,34.08)-endutih_persona_anual_2021_cel_localidad$porcentaje_no_uso

endutih_persona_anual_2022_cel_localidad = tr_endutih_usuarios2_anual_2022 %>% group_by(TLOC) %>%
  summarise(contador_pond = sum(FAC_PER), no_uso = sum(FAC_PER[P8_1 == 2], na.rm=T)) %>%
  mutate(porcentaje_no_uso = no_uso/contador_pond*100)  
print(endutih_persona_anual_2022_cel_localidad, n=32) 
c(12.56,17.65,21.76,33.62)-endutih_persona_anual_2022_cel_localidad$porcentaje_no_uso

##Nacional
#Hogares
nacional_hogar_cel_2021 <- sum(subset(tr_endutih_hogresusu2_anual_2021, P4_1_6 == 2)$FAC_HOG, na.rm=T)/pond_hogar_2021*100
nacional_hogar_cel_2021
nacional_hogar_cel_2022 <- sum(subset(tr_endutih_hogresusu2_anual_2022, P4_1_6 == 2)$FAC_HOG, na.rm=T)/pond_hogar_2022*100
nacional_hogar_cel_2022

#Personas
nacional_persona_cel_2021 <- sum(subset(tr_endutih_usuario2_anual_2021, P8_1 == 2)$FAC_PER, na.rm=T)/pond_usuario_2021*100
nacional_persona_cel_2021
nacional_persona_cel_2022 <- sum(subset(tr_endutih_usuarios2_anual_2022, P8_1 == 2)$FAC_PER, na.rm=T)/pond_usuario_2022*100
nacional_persona_cel_2022
