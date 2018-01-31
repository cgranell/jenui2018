
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

tb_table <- tb %>%
  mutate(perfil = paste(cursos, "-", conocimiento)) %>%
  select(`Año-sessión` = num_sesion, 
         `Duración (min)` = dur_total, 
         `Num. alumnos `=alumnos, 
         `Perfil alumnos`=perfil)

kable(tb_table)

# Add these packages in the latex source document
# \usepackage{graphicx}
# \usepackage{booktabs}
# \usepackage{colortbl}
# \usepackage[table]{xcolor}

# https://cran.r-project.org/web/packages/kableExtra/README.html
kable(tb_table,
      format = "latex", 
      booktabs = TRUE, 
      caption = "Descripción de las sesiones") %>%
  kable_styling(position = "center", 
                font_size = 7,
                latex_options = c("striped","hold_position"))


# Temporal distribtion of each section per session
tb_dur <- tb %>%
  select(-alumnos, -chicos, -chicas, -conocimiento, -cursos, -lugar) %>%
  select(num_sesion, fecha_final, anyo, everything())

tb_dur <- tb_dur %>%
  gather(dur1, dur2, dur3, key = "tipo_seccion", value = "dur_seccion") %>%
  arrange(fecha_final, tipo_seccion)
tb_dur

tb_dur2 <- tb_dur %>%
  mutate (dur_per = dur_seccion / dur_total * 100)

tb_dur2

# http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
ggplot(tb_dur2, aes(x=num_sesion, y=dur_per)) + 
  geom_bar(aes(fill = tipo_seccion), stat = "identity") +
  labs(x = "Sesiones", y="Duración (min)") +
  scale_fill_viridis(discrete=TRUE, option="viridis") + theme_bw() +
  coord_flip()



#####################3


scales::percent(tb_dur$dur1_per)


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


