
library(ggplot2)

pdf_data <- pdftools::pdf_data("data-raw/2011_United Kingdom_06-Chapter Four_ Europe.pdf", font_info = TRUE)


page <- pdf_data[[2]] 

min_y <- page |>
  dplyr::summarise(min(y)) |>
  dplyr::pull()

midpoint <- page |>
  dplyr::summarise(min(x) + (max(x) - min(x))/2) |>
  dplyr::pull()


# Remove Header
page <- page |>
  dplyr::mutate(font_face = purrr::map_chr(stringr::str_split(font_name, "-"), ~.x[length(.x)])) 


mround <- function(x,base){
  base*round(x/base)
}

headings <- "EQUIPMENT BY TYPE"


extract_qty <- function(x, chr){
  loc_1 <- stringr::str_locate(x, chr)[,"end"]
  split_1 <- trimws(substr(x, loc_1+1, nchar(x)))
  stringr::str_split(split_1, "\\s") |> 
    unlist() |>
    purrr::pluck(1)
}


# By Row
processed <- dplyr::filter(page, y != min_y, x < midpoint) |>
  dplyr::filter(y >= 116, y < 445) |> 
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
                   min_x = min(min_x),
                   avg_font_size = mean(avg_font_size))



processed |>
  dplyr::filter(!text %in% headings) |>
  dplyr::mutate(text = stringr::str_split(text, "\\s")) |>
  dplyr::mutate(font = stringr::str_split(font, ", ")) |>
  tidyr::unnest(c(text, font)) |>
  dplyr::group_by(row) |>
  dplyr::mutate(font_change = font != dplyr::lag(font)) |>
  tidyr::replace_na(list(font_change = TRUE)) |>
  dplyr::mutate(font_change = cumsum(font_change)) |>
  dplyr::group_by(row, font_change, min_x, font) |>
  dplyr::summarise(text = paste(text, collapse = " "))|>
  dplyr::ungroup() |>
  dplyr::mutate(indent = as.numeric(factor(min_x))) |>
  dplyr::mutate(text = dplyr::case_when(
    font == "It" ~ paste0("<e>", text),
    font == "Bo" ~ paste0("<b>", text, "</b>"),
    TRUE ~ text
  )) |>
  dplyr::group_by(row, indent) |>
  dplyr::summarise(text = paste(text, collapse = " ")) |>
  dplyr::mutate(text  = stringr::str_split(text, ":|;")) |>
  tidyr::unnest(text) |>
  dplyr::mutate(text = trimws(text)) |>
  dplyr::mutate(equipment = substr(text, stringr::str_locate(text, "<e>")[,"end"]+1, nchar(text))) |>
  dplyr::mutate(qty = dplyr::case_when(
    stringr::str_detect(text, "^[0-9]") ~ substr(text, 1, stringr::str_locate(text, "\\s")[,"start"]-1),
    stringr::str_detect(text, "^<b>") ~ extract_qty(text, "</b>"),
    TRUE ~ NA
  )) |>
  dplyr::group_by(row) |>
  dplyr::mutate(classification = stringr::str_extract_all(text, "(?<=\\<b\\>)(.*?)(?=\\<)")) |>
  dplyr::mutate(classification = purrr::map_chr(classification, ~paste(.x, collapse = "/"))) |>
  dplyr::mutate(classification = ifelse(classification == "", NA, classification)) |>
  tidyr::fill(classification, .direction = "down") |>
  dplyr::ungroup()  |>
  dplyr::mutate(level_1 = ifelse(indent == 1, row != dplyr::lag(row), FALSE)) |>
  tidyr::replace_na(list(level_1 = TRUE)) |>
  dplyr::mutate(level_1 = cumsum(level_1)) |>
  dplyr::group_by(level_1) |>
  dplyr::mutate(classification_path = ifelse(indent == 2, paste(classification[1], classification, sep = "/"), classification)) |>
  dplyr::mutate(classification_path = ifelse(!is.na(equipment), paste(classification_path, equipment, sep = "/"), classification_path)) |>
  dplyr::ungroup() |>
  dplyr::mutate(text = trimws(stringr::str_remove_all(text, glue::glue("<b>|</b>|<e>|{qty}|{equipment}|{classification}")))) |>
  View()







# Split by Column


