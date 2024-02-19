#pdftools::pdf_subset("../../../OneDrive/eBooks/MilBal/Vol_101(1)_The Military Balance/03-NATO and non_NATO Europe.pdf", pages = 48:52, "uk2001.pdf"

mround <- function(x,base){
  base*round(x/base)
}


pdf_tbls <- pdftools::pdf_data("uk2001.pdf", font_info = TRUE)                    
page  <- pdf_tbls[[2]]

# Identity Midpoint
page |>
   dplyr::summarise((max(x)-min(x))/2) |>
   dplyr::pull()

midpoint <- 255
library(ggplot2) 
ggplot(page) + geom_bar(aes(x)) + geom_vline(aes(xintercept = 255), linetype = 2) 

  
page <- page |> dplyr::filter(y != min(y))
page <- page |> 
  dplyr::mutate(font_face = purrr::map_chr(stringr::str_split(font_name, "-"), ~.x[length(.x)])) 
 

col_1 <- page |> dplyr::filter(x < midpoint)
col_2 <- page |> dplyr::filter(x > midpoint)

col_1 |> dplyr::filter(stringr::str_detect(text, "EQUIPMENT"))



# Land Equipment by Row -----

land_equipment_1 <- col_1 |> dplyr::filter(y > 400) |>
    dplyr::mutate(y = mround(y, 5)) |>  
    dplyr::group_by(y) |>
    dplyr::summarise(text = paste(text, collapse = " "),
                     font = paste(substr(font_face, 1, 2), collapse = ", "),
                     min_x = min(x),
                     avg_font_size = mean(font_size)) |>
    dplyr::mutate(row = cumsum(substr(font, 1,2)== "Bo")) |>
    dplyr::group_by(row) |>
    dplyr::summarise(text = paste(text, collapse = " "),
                     font = paste(font, collapse = ", "),
                     indent = as.factor(mround(min(min_x),2)),
                     avg_font_size = mean(avg_font_size))  |>
  dplyr::mutate(indent = as.numeric(indent))


land_equipment_2 <- col_2 |> dplyr::filter(y <= 39) |>
  dplyr::mutate(y = mround(y, 5)) |>
  dplyr::group_by(y) |>
  dplyr::group_by(y) |>
  dplyr::summarise(text = paste(text, collapse = " "),
                   font = paste(substr(font_face, 1, 2), collapse = ", "),
                   min_x = min(x),
                   avg_font_size = mean(font_size)) |>
  dplyr::mutate(row = cumsum(substr(font, 1,2)== "Bo")) |>
  dplyr::group_by(row) |>
  dplyr::summarise(text = paste(text, collapse = " "),
                   font = paste(font, collapse = ", "),
                   indent = as.factor(mround(min(min_x),2)),
                   avg_font_size = mean(avg_font_size))  |>
  dplyr::mutate(indent = as.numeric(indent)) |>
  dplyr::mutate(row = 15:16)

land_equipment <- dplyr::bind_rows(land_equipment_1, land_equipment_2)

# Extract Headings and Equipment
land_equipment |>
  dplyr::mutate(text = stringr::str_split(text, "\\s"),
                font = stringr::str_split(font, ", ")) |>
  tidyr::unnest(c(text, font)) |>
  dplyr::group_by(row) |>
  dplyr::mutate(font_change = font != dplyr::lag(font)) |>
  tidyr::replace_na(list(font_change = TRUE)) |>
  dplyr::mutate(font_change = cumsum(font_change)) |>
  dplyr::group_by(row, font_change, font, indent) |>
  dplyr::summarise(text = paste(text, collapse = " ")) |>
  dplyr::mutate(text = dplyr::case_when(
    font == "Bo" ~ paste0("<b>", text, "</b>"),
   # font == "It" ~ paste0("<e>", text, "</e>"), 
    TRUE ~ text
  )) |>
  dplyr::group_by(row, indent) |>
  dplyr::summarise(text = paste(text, collapse = " ")) |>
  dplyr::mutate(heading = stringr::str_extract(text, "(?<=\\<b\\>)(.*?)(?=\\<)")) |>
  dplyr::mutate(text = stringr::str_remove(text, "(?<=\\<b\\>)(.*?)(?=\\<)")) |>
  dplyr::mutate(text = stringr::str_remove(text, "\\<b\\>\\</b\\>\\s")) |>
  dplyr::mutate(text = stringr::str_remove(text, "\\<b\\>")) |>
  dplyr::mutate(text = stringr::str_remove(text, "\\</b\\>")) |>
  dplyr::mutate(text = stringr::str_replace(text, "^([0-9]),([0-9])", "\\1\\2")) |>
  dplyr::mutate(text = stringr::str_split(text, ";|,|:")) |>
  tidyr::unnest(text) |>
  View()
