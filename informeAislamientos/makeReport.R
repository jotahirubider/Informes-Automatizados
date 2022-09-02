# Load packages
pacman::p_load(here,lubridate,tcltk)
# Set Paths
setwd(here())
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
# Set names
filename <- paste0(today(),"-Informe-FIRMA.pdf")
output_error_file <- "log/error.txt"
condition_date <- as_datetime(paste0(as.character(today()),"14:00:00 CEST"))
now <- as_datetime(now()) + hours(2)
if (now > condition_date) {
  filename <- paste0(today(),"-Informe-Aislamientos.pdf")
} else {
  filename <- paste0(today(),"-Informe-Aislamientos-BORRADOR.pdf")
}
filename_cleaning <- paste0(today(),"-Limpieza-Habitaciones.pdf")
# Empty error file
try(invisible(file.remove(output_error_file)),silent = T)
writeLines("",con=output_error_file)
# Render
try(
  expr = rmarkdown::render(
    input = "base/0-isolationReport.Rmd",
    output_format = "pdf_document",
    output_file = filename,
    output_dir = "K:/AISLAMIENTOS/",
    intermediates_dir = "K:/AISLAMIENTOS/temp",
    encoding = "UTF-8",
    clean = TRUE,
    quiet = TRUE
  ),
  silent = F,
  outFile = output_error_file
)
try(
  expr = rmarkdown::render(
    input = "base/0-cleaningReport.Rmd",
    output_format = "pdf_document",
    output_file = filename_cleaning,
    output_dir = "K:/AISLAMIENTOS/",
    intermediates_dir = "K:/AISLAMIENTOS/temp",
    encoding = "UTF-8",
    clean = TRUE,
    quiet = TRUE
  ),
  silent = F,
  outFile = output_error_file
)
# If error show messagebox and pause console
error <- ifelse(readLines("log/error.txt")=="",F,T)
if (error) {
  tkmessageBox(title="Error de renderización",
               message="Se ha producido un error al generar el informe.\n\nRevisa la consola de comandos para resolverlo o contactar con el administrador.\n\nContacto: juaneda.juan@gmail.com",
               icon= "error")
  shell("pause")
}

# https://datacornering.com/how-to-run-r-script-from-another-r-script-and-use-as-a-source/
# https://bookdown.org/yihui/rmarkdown-cookbook/rmarkdown-render.html