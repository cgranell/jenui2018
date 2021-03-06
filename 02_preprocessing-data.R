
# pacman package management tool to install and load the package dependencies.
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# Load and install packages from CRAN
p_load(
  "tidyverse",
  "here",
  "lubridate",
  "highcharter",
  "knitr",
  "kableExtra", 
  "scales",
  "viridis"
)

file_name <- "datos-practica-uji.csv"
data_path <- here::here("data", file_name)

tb <- read_csv(data_path)

tb <- tb %>%
  mutate(fecha_final = lubridate::dmy(fecha), 
           # date= as.Date(fecha, format = "%d/%m/%Y"),
         anyo = year(fecha_final)) %>%
  select(-fecha) %>%
  rename(num_sesion = id, dur_total = duracion, dur1 = duracion_1, dur2 = duracion_2, dur3 = duracion_3) %>%
  arrange(fecha_final)


duracion_total <- round(sum(tb$dur_total))
duracion_media <- round(duracion_total/nrow(tb))
alumnos_total <- sum(tb$alumnos)
alumnos_media <- round(alumnos_total/nrow(tb))



tb_table <- tb %>%
  mutate(perfil = paste(cursos, "-", conocimiento)) %>%
  select(`Año-sesión` = num_sesion, 
         `Duración (min)` = dur_total, 
         `Num. alumnos `=alumnos, 
         `M` = chicos,
         `F` =chicas, 
         `Perfil alumnos`=perfil)

kable(tb_table)


# Add these packages in the latex source document
# \usepackage{graphicx}
# \usepackage{booktabs}
# \usepackage{colortbl}
# \usepackage[table]{xcolor}

# https://cran.r-project.org/web/packages/kableExtra/README.html

# Use the above command to generate the table in latex for the paper
kable(tb_table,
      format = "latex", 
      booktabs = TRUE, 
      escape = TRUE,
      caption = paste0("Lista sesiones. ",
                       "Total alumnos: ", alumnos_total, 
                       "; Media alumnos por sesión: ", alumnos_media,
                       "; Duración media sesión (minutos): ", duracion_media)) %>%
  kable_styling(position = "center", 
                font_size = 7,
                latex_options = c("striped","hold_position"))


##### SUMMARY TABLE: type of interventions per year

tb_intervenciones <- data.frame(
  secciones = c("A", "B", "C"),
  tangible2017 = c(
    "Sí",
    "Sí",
    "Sí"),
  social2017 = c(
    "indiv. + grupo",
    "grupo",
    "grupo"),
  tangible2018 = c(
    "Sí",
    "Sí",
    "Sí"),
  social2018 = c(
    "indiv. + grupo",
    "grupo  + roles",
    "grupo  + roles"),
  tangible2019 = c(
    "Sí",
    "Sí",
    "Sí"),
  social2019 = c(
    "indiv. + grupo",
    "grupo  + roles",
    "grupo  + roles")
)

tb_intervenciones_table <- tb_intervenciones %>%
  select(`Sección` = secciones, 
         `Tangible` = tangible2017, 
         `Social`=social2017,
         `Tangible` = tangible2018,
         `Social`=social2018,
         `Tangible` = tangible2019,
         `Social`=social2019)

kable(tb_intervenciones_table)

# Use the above command to generate the table in latex for the paper
kable(tb_intervenciones_table, 
      format = "latex", 
      booktabs = TRUE, 
      escape = TRUE,
      caption = "Tipo de intervenciones.") %>%
  add_header_above(c(" ", "2017" = 2, "2018" = 2, "2019" = 2)) %>%
  kable_styling(position = "center", 
              font_size = 7,
              latex_options = c("striped","hold_position"))





###### Temporal distribution of each section per session
tb_dur <- tb %>%
  select(-chicos, -chicas, -conocimiento, -cursos, -lugar) %>%
  select(num_sesion, fecha_final, anyo, everything())

tb_dur <- tb_dur %>%
  gather(dur1, dur2, dur3, key = "tipo_seccion", value = "dur_seccion")

tb_dur <- tb_dur %>%
  mutate (porcentaje = round(dur_seccion / dur_total, 2) * 100)

media_alumnos <- mean(tb_dur$alumnos)  
tb_dur <- tb_dur %>%
  group_by(num_sesion)  %>%
  # mutate (pos = cumsum(porcentaje) - (0.5 * porcentaje)) %>%
  mutate (asistencia = ifelse(alumnos > media_alumnos, "mayor", "menor")) %>%
  arrange(desc(fecha_final), tipo_seccion)
  
tb_dur$asistencia <- factor(tb_dur$asistencia, level = unique(tb_dur$asistencia),
                              labels = c("Inferior media", "Superior media"))

cols_asistencia <- c("Superior media" = "#a6611a", "Inferior media" = "#dfc27d")


tb_dur$tipo_seccion <- factor(tb_dur$tipo_seccion, level = unique(tb_dur$tipo_seccion),
                               labels = c("A - Explicación profesor", "B - Proyecto guiado", "C - Proyecto libre"))
  
# http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
ggplot(tb_dur, aes(x=num_sesion, y=porcentaje)) + 
  geom_bar(aes(fill = tipo_seccion), stat = "identity", position= position_stack(reverse = TRUE)) +
  geom_point(aes(x=num_sesion, y=alumnos, colour=asistencia)) +
  scale_fill_brewer(palette = "Set3") + theme_bw() +
  scale_colour_manual(values = cols_asistencia) +
  geom_text(aes(x=num_sesion, y=alumnos, label = alumnos), size=2, vjust = 1, nudge_y = -1) +
  geom_text(aes(x=num_sesion, y=porcentaje, label = ifelse(porcentaje>0, paste0(porcentaje, "%"), "")), size=3, 
            position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  theme(legend.position="bottom", 
        legend.direction="vertical",
        legend.title = element_blank()) +
  # coord_flip() +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix="")) +
  labs(x = "Sesiones (año - número)", y="Porcentaje (%)") +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5, hjust=0.5))
  # ggtitle("Ditribución temporal de secciones por sesión (%)")
  

# Save stacked bar chart
file_name <- "porcentaje_secciones.png"
data_path <- here::here("figs", file_name)
ggsave(data_path, dpi = 600)




#####################3


ggplot(evaldata_years_long, aes(year, value)) +
  geom_bar(aes(fill = variable), stat = "identity") +
  ylab("mean value") +

dur_year <- tb %>%
  group_by(year) %>%
  summarise( 
    count = n(),
    dur_med = mean(dur_total, na.rm = TRUE),
    dur1_per = sum(duracion_1) / sum(duracion),
    dur2_per = sum(duracion_2) / sum(duracion),
    dur3_per = sum(duracion_3) / sum(duracion)
    )




# https://cran.r-project.org/web/packages/kableExtra/README.html
kable(tb %>% select(-duracion_1, -duracion_2, -duracion_3, -chicos, -chicas, -conocimiento, -lugar, -date, -duracion_media),
      format = "latex", 
      booktabs = TRUE, 
      caption = "Sesiones") %>%
  kable_styling(full_width = T)
  #kable_styling(latex_options = "scale_down")
  #kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE)


