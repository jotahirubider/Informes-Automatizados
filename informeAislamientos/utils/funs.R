format_number <- function(number, format="double", decimals=0) {
  number <- sprintf(fmt = str_glue("%2.{decimals}f"),number)
  if (format=="percent") {number <- paste0(number,"%")}
  return(number)
}
