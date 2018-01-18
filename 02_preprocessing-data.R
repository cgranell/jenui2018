
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
  "kableExtra"
)

file_name <- "datos-practica-uji.csv"
data_path <- here::here("data", file_name)

tb <- read_csv(data_path)
head(tb, 8)


tb <- tb %>%
  mutate(date = as.Date(fecha, format = "%d/%m/%Y"),
         year = year(ymd(date)),
         duracion_media = mean (duracion))

# https://cran.r-project.org/web/packages/kableExtra/README.html
kable(tb %>% select(-duracion_1, -duracion_2, -duracion_3, -chicos, -chicas, -conocimiento, -lugar, -date, -duracion_media),
      format = "latex", 
      booktabs = TRUE, 
      caption = "Sesiones") %>%
  #kable_styling(latex_options = "scale_down")
  kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE)


