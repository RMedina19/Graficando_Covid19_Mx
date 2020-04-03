# 1. Cargar paquetes ----
require(tidyverse)
require(readxl)
require(ggthemes)
require(lubridate)
require(geofacet)
require(RColorBrewer)

# 2. Establecer directorios ----
dir <- "~/GitHub/Graficando_Covid19_Mx"
setwd(dir)

inp <- "/01_datos/"
out <- "/03_gráficas/art_rm/"
fiuffi <- "Elaboración propia con datos de la Secretaría de Salud\n@guzmart_ | @regi_medina | @lolo7no"

# Función para reconocer acentos
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

# 3. Cargar datos ----
d <- read_excel(paste0(dir, inp, "covid_mex_20200401.xlsx"), 
                col_types = c("numeric", "text", "text", "numeric", "date", "text", "text", "date", "date", "numeric", "date", "numeric", "date")) %>% 
  mutate(ent = str_replace_all(ent, "\r", " "),
         ent = str_remove_all(ent, "\n"),
         ent = rm_accent(ent)) %>% 
  filter(!inconsistencia_omision==1)

load(paste0(dir, inp, "mxhexmap.RData"))
mxhexmap$state_abbr <- ifelse(mxhexmap$state_abbr=="DF",
                                  "CDMX",as.character(mxhexmap$state_abbr))


# 4. Transformaciones de los datos----
data_fecha <- d %>% 
  group_by(
    fecha_corte
  ) %>% 
  summarise(
    n = n()
  ) 

data_fecha_acumulado <- data_fecha %>% 
  mutate(
    n = cumsum(n),
    fecha_corte = ymd(fecha_corte), 
    pais = "México"
  ) 
  
data_fecha_sintomas <- d %>% 
  group_by(
    fecha_inicio
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  mutate(
    n_acumulada = cumsum(n)
  ) 

# DATA CURVA EPIDÉMICA COMPARADA

data_ent <- d %>% 
  group_by(
    ent
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  mutate(
    cve_ent = case_when(
      ent == "AGUASCALIENTES" ~ "01",
      ent == "BAJA CALIFORNIA" ~ "02",
      ent == "BAJA CALIFORNIA SUR" ~ "03",
      ent == "CAMPECHE" ~ "04",
      ent == "CHIAPAS" ~ "07",
      ent == "CHIHUAHUA" ~ "08",
      ent == "CIUDAD DE MEXICO" ~ "09",
      ent == "COAHUILA" ~ "05",
      ent == "COLIMA" ~ "06",
      ent == "DURANGO" ~ "10",
      ent == "GUANAJUATO" ~ "11",
      ent == "GUERRERO" ~ "12",
      ent == "HIDALGO" ~ "13",
      ent == "JALISCO" ~ "14",
      ent == "MEXICO" ~ "15",
      ent == "MICHOACAN" ~ "16",
      ent == "MORELOS" ~ "17",
      ent == "NAYARIT" ~ "18",
      ent == "NUEVO LEON" ~ "19",
      ent == "OAXACA" ~ "20",
      ent == "PUEBLA" ~ "21",
      ent == "QUERETARO" ~ "22",
      ent == "QUINTANA ROO" ~ "23",
      ent == "SAN LUIS POTOSI" ~ "24",
      ent == "SINALOA" ~ "25",
      ent == "SONORA" ~ "26",
      ent == "TABASCO" ~ "27",
      ent == "TAMAULIPAS" ~ "28",
      ent == "TLAXCALA" ~ "29",
      ent == "VERACRUZ" ~ "30",
      ent == "YUCATAN" ~ "31",
      ent == "ZACATECAS" ~ "32",
    )
  ) %>% 
  left_join(mxhexmap %>% rename(cve_ent=ent))


data_world <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  filter(`Country/Region`=="Spain" | `Country/Region`== "Italy" |
         `Country/Region`== "France" | `Country/Region` == "Germany" | `Country/Region`== "US") %>% 
  filter(is.na(`Province/State`)) %>% 
  select(-"Province/State", -Lat, -Long) %>% 
  rename("pais" = "Country/Region") %>% 
  pivot_longer(-pais,
               names_to = "fecha_corte", 
               values_to = "n") %>% 
  group_by(pais, fecha_corte) %>% 
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>% 
  mutate(fecha_corte = mdy(fecha_corte),
         pais = case_when(
           str_starts(pais, "Fr")~ "Francia",
           str_starts(pais, "Ger")~ "Alemania",
           str_starts(pais, "It")~ "Italia",
           str_starts(pais, "Sp")~ "España",
           str_starts(pais, "U")~ "EEUU",
         )) %>% 
  select(pais, fecha_corte, n)  %>% 
  bind_rows(data_fecha_acumulado) %>% 
  complete(fecha_corte, pais) %>% 
  replace(., is.na(.), 0) %>% 
  filter(n>0)%>%
  group_by(pais) %>% 
  mutate(Día = as.numeric(fecha_corte-min(fecha_corte)),
         max = max(n))%>%
  ungroup() 


# 5. Visualizaciones ----
fiuf <- "Número de casos confirmados de COVID-19 en México\npor fecha de corte"
fiuff <- paste0("Actualización: ", str_sub(max(data_fecha_acumulado$fecha_corte), end = -1))

# Total de nuevos casos confirmados por día 
ggplot(data_fecha_acumulado, 
       aes(x = as.Date(fecha_corte),
           y = n,
           label = n)) +
  geom_line() + geom_label(size=5) +
  scale_x_date(date_breaks = "1 day",
               limits = c(
                 min(as.Date(data_fecha_acumulado$fecha_corte)-0.7),
                 max(as.Date(data_fecha_acumulado$fecha_corte)+0.8)
               ),
               expand = c(0,0)) +
  theme_minimal() + 
  labs(title=fiuf, 
       subtitle=fiuff,
       caption=fiuffi,
       x="",
       y="Número de casos acumulados") +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15))
ggsave(filename = paste0(dir, out, 
                  str_replace_all(str_sub(max(data_fecha_acumulado$fecha_corte), end = -1), "-", "_"), 
                  "_01_acumulados.png"), width = 15, height = 10, dpi = 100)


# Total de personas con primeros síntomas por día
fiuf <- "Número de casos confirmados de COVID-19 en México\npor fecha de síntomas"
ggplot(data_fecha_sintomas, 
       aes(x = as.Date(fecha_inicio),
           y = n_acumulada,
           label = ifelse(
             fecha_inicio==max(data_fecha_sintomas$fecha_inicio), n_acumulada, ""
           ))) +
  geom_text(size=6, vjust = -0.5, hjust = 1) + geom_line() + 
  scale_x_date(date_breaks = "1 day",
               limits = c(
                 min(as.Date(data_fecha_sintomas$fecha_inicio)-0.7),
                 max(as.Date(data_fecha_sintomas$fecha_inicio)+0.7)
               ),
               expand = c(0,0)) +
  theme_minimal() + 
  labs(title=fiuf, 
       subtitle=fiuff,
       caption=fiuffi,
       x="Fecha de inicio de síntomas",
       y="Número de casos acumulados") +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15))

ggsave(filename = paste0(dir, out, 
                         str_replace_all(str_sub(max(d$fecha_corte), end = -1), "-", "_"), 
                         "_02_sintomas.png"), width = 15, height = 10, dpi = 100)



# Casos acumulados por entidad en mapa de calor
fiuf <- "Número de casos confirmados de COVID-19 en México\npor entidad federativa"

ggplot(data_ent,
       aes(long, lat, group=ent,
           fill=as.integer(n))) +
  geom_polygon(color = "gray") +
  geom_text(aes(label=paste0(state_abbr, "\n[ ",n, " ]"),
                x=cent_x,y=cent_y)) +
  theme_void() +
  scale_fill_gradient("",
                      low = brewer.pal(n = 9, "Reds")[1],
                      high = brewer.pal(n = 9, "Reds")[9]) + 
  labs(title=fiuf, 
       subtitle=fiuff,
       caption=fiuffi,
       x="",
       y="") +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  coord_fixed()

ggsave(filename = paste0(
  out, str_replace_all(str_sub(max(d$fecha_corte), end = -1), "-", "_"), "_03_ent.png"
), width = 15, height = 10, dpi = 100)


fiuf <- "Número de casos acumulados en distintos países\ndesde el primer caso confirmado en el país"
ggplot(data_world, 
            aes(x = Día,
                y = n,
                color = pais,
                label = ifelse(
                  fecha_corte==max(fecha_corte), 
                  paste0(pais, "\n", 
                         format(max, big.mark = ",")), ""
                ))) +
  geom_line(lineend = "round",
            size = 1) +
  geom_text_repel(color="black") +
  labs(title = fiuf,
       subtitle = fiuff,
       caption = fiuffi,
       colour = "",
       x = "Días desde el primer caso confirmado en el país",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 15, vjust = 0.5),
        axis.text.y = element_text(size = 13),
        legend.position = "none",
        legend.text = element_text(size = 18))

ggsave(filename = paste0(dir, out, 
                         str_replace_all(str_sub(max(d$fecha_corte), end = -1), "-", "_"), 
                         "_04_mundial.png"), width = 15, height = 10, dpi = 100)

