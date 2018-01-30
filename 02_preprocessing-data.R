
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
  "scales"
)

file_name <- "datos-practica-uji.csv"
data_path <- here::here("data", file_name)

tb <- read_csv(data_path)
head(nrow(tb))


tb <- tb %>%
  mutate(date = lubridate::dmy(fecha), 
           # date= as.Date(fecha, format = "%d/%m/%Y"),
         year = year(date)) %>%
  select(-fecha)
tb


dur_year <- tb %>%
  group_by(year) %>%
  summarise( 
    count = n(),
    dur_media = mean(duracion, na.rm = TRUE),
    dur1_per = sum(duracion_1) / sum(duracion),
    dur2_per = sum(duracion_2) / sum(duracion),
    dur3_per = sum(duracion_3) / sum(duracion)
    )

scales::percent(dur_year$dur1_per)


# https://cran.r-project.org/web/packages/kableExtra/README.html
kable(tb %>% select(-duracion_1, -duracion_2, -duracion_3, -chicos, -chicas, -conocimiento, -lugar, -date, -duracion_media),
      format = "latex", 
      booktabs = TRUE, 
      caption = "Sesiones") %>%
  kable_styling(full_width = T)
  #kable_styling(latex_options = "scale_down")
  #kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE)


