library(tidyverse)
library(sf)
library(lwgeom)

PROVINCIA <- read_csv("censo2010/prov.csv")
DPTO <- read_csv("censo2010/dpto.csv")
FRAC <- read_csv("censo2010/frac.csv")
RADIO <- read_csv("censo2010/radio.csv")
VIVIENDA <- read_csv("censo2010/vivienda.csv")
HOGAR <- read_csv("censo2010/hogar.csv")
PERSONA <- read_csv("censo2010/persona.csv")

radios_geo <- st_read("radios_censales/Codgeo_CABA_con_datos/cabaxrdatos.shp",crs=22183)
radios_geo <- st_transform(radios_geo, 4326)

delitos <- read_csv("delitos/delitos.csv", 
					col_types = cols(CANTIDAD_DE_VICTIMAS = col_integer(), 
									 FECHA_DEL_HECHO = col_date(format = "%Y-%m-%d"), 
									 FRANJA_24_HS = col_integer(), 
									 HORA_DEL_HECHO = col_time(format = "%H:%M:%S")))

delitos <- st_as_sf(delitos,coords = c("LONGITUD", "LATITUD"), crs = 4326)

rosetta <- PROVINCIA %>%
	inner_join(DPTO, by = 'PROV_REF_ID') %>%
	mutate(PROV_REF_ID = '02') %>%
	
	inner_join(FRAC, by = 'DPTO_REF_ID') %>%
	mutate(DPTO_REF_ID = formatC(DPTO_REF_ID, width = 3, format = "d", flag = "0")) %>%
	
	inner_join(RADIO, by = 'FRAC_REF_ID') %>%
	mutate(FRAC_REF_ID = formatC(FRAC_REF_ID, width = 2, format = "d", flag = "0")) %>%
	
	inner_join(VIVIENDA, by = 'RADIO_REF_ID') %>%
	mutate(RADIO_REF_ID = formatC(RADIO_REF_ID, width = 2, format = "d", flag = "0")) %>%
	
	inner_join(HOGAR, by = 'VIVIENDA_REF_ID') %>%
	inner_join(PERSONA, by = 'HOGAR_REF_ID') %>%
	
	select(PROV_REF_ID, DPTO_REF_ID, FRAC_REF_ID, RADIO_REF_ID, VIVIENDA_REF_ID, HOGAR_REF_ID, PERSONA_REF_ID) %>%
	mutate(LINK = paste0(PROV_REF_ID, DPTO_REF_ID, FRAC_REF_ID, RADIO_REF_ID))
	
hist(PERSONA$P09, freq = FALSE, col = 'orange',
	 main = 'Histograma de nivel educativo (por persona)',
	 xlab = 'Nivel Educativo alcanzado')

hist(VIVIENDA$V01, freq = FALSE, col = 'blue',
	 main = 'Histograma de Tipo de Vivienda Particular',
	 xlab = 'Tipo de Vivienda Particular')


