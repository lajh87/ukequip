# pdftools::pdf_subset(
#   file.path(
#     "../../../OneDrive/eBooks/MilBal/", 
#     "Vol_091(1)_The Military Balance", 
#     "04597229108460029.pdf"
#     ),
#   pages = 31:35, 
#   "data-raw/uk1991.pdf"
# )

library(ggplot2)

mround <- function(x,base){
  base*round(x/base)
}

pdf_tbls <- pdftools::pdf_data("data-raw/uk1991.pdf", font_info = TRUE)                    

# Remove Headers
page  <- pdf_tbls[[2]] |>
  dplyr::filter(x > min(x), y > min(y))  |> 
  dplyr::mutate(
    font_face = purrr::map_chr(
      stringr::str_split(font_name, "-"), 
      ~.x[length(.x)])
    ) 


# Identify Midpoint
midpoint <- page |> dplyr::summarise(min(x) + (max(x)-min(x))/2) |> dplyr::pull()

page |>
  ggplot() + geom_bar(aes(x)) +
  geom_vline(aes(xintercept = midpoint), linetype = 2)

# Identify where equipment starts
equipment <- page |>
  dplyr::filter(x < midpoint) |>
  dplyr::filter(text == "EQUIPMENT:") |>
  dplyr::pull(y)

land_equipment_col_1 <- page |> dplyr::filter(x < midpoint, y> equipment) |>
  dplyr::mutate(y = mround(y, 5)) |>  
  dplyr::group_by(y) |>
  dplyr::summarise(text = paste(text, collapse = " "),
                   font = paste(substr(font_face, 1, 2), collapse = ", "),
                   min_x = min(x),
                   avg_font_size = mean(font_size))  |>
  dplyr::mutate(row = cumsum(substr(font, 1,2)== "Bo" | stringr::str_detect(text, "[A-Z]{3}:"))) |>
  dplyr::group_by(row) |>
  dplyr::summarise(text = paste(text, collapse = " "),
                   font = paste(font, collapse = ", "),
                   indent = as.factor(mround(min(min_x),2)),
                   avg_font_size = mean(avg_font_size))  |>
  dplyr::mutate(indent = as.numeric(indent)) 

land_equipment_col_2 <- page |> dplyr::filter(x > midpoint, y < 271) |>
  dplyr::mutate(y = mround(y, 2)) |>  
  dplyr::group_by(y) |>
  dplyr::summarise(text = paste(text, collapse = " "),
                   font = paste(substr(font_face, 1, 2), collapse = ", "),
                   min_x = min(x),
                   avg_font_size = mean(font_size))  |>
  dplyr::mutate(row = cumsum(substr(font, 1,2)== "Bo" | 
                               stringr::str_detect(text, "[A-Z]{3}:") |
                               stringr::str_detect(text, "MRL\\s"))) |>
  dplyr::group_by(row) |>
  dplyr::summarise(text = paste(text, collapse = " "),
                   font = paste(font, collapse = ", "),
                   indent = mround(min(min_x),2),
                   avg_font_size = mean(avg_font_size)) |>
  dplyr::mutate(indent = factor(indent, levels = sort(unique(indent))) |>
                  as.numeric()) |>
  dplyr::mutate(row = 8:17)


land_equipment <- dplyr::bind_rows(land_equipment_col_1, land_equipment_col_2)

for(i in 1:nrow(land_equipment)){
  if(i == 1){
    v[i] <- 1
  } else{
    if(land_equipment$indent[i] == 1) v[i] <- v[i-1] + 1 else v[i] <- v[i-1]
  }
}

land_equipment$level_1 <- v

land_equipment <- land_equipment |>
  dplyr::group_by(level_1) |>
  dplyr::mutate(level_2 = 1:dplyr::n()-1) |>
  dplyr::ungroup()

land_1991 <- land_equipment |>
  dplyr::mutate(heading = stringr::str_extract(text, "^(.*?)(?=:)")) |>
  dplyr::mutate(text = stringr::str_remove(text, "^(.*?):\\s")) |>
  dplyr::mutate(text = stringr::str_split(text, ":\\s|:|;\\s")) |>
  dplyr::select(level_1, level_2, text, indent, heading) |>
  tidyr::unnest(text) |>
  dplyr::group_by(level_1) |>
  dplyr::mutate(type = ifelse(1:dplyr::n() == 1 & dplyr::n()>1, "Subtotal", "Equipment")) |>
  dplyr::ungroup() |>
  dplyr::group_by(level_2) |>
  dplyr::mutate(type = ifelse(1:dplyr::n() == 1 & dplyr::n()>1, "Subtotal", type)) |>
  dplyr::mutate(text = stringr::str_split(text, ", ")) |>
  tidyr::unnest(text) |>
  dplyr::mutate(some = stringr::str_detect(text, "some")) |>
  dplyr::mutate(text = stringr::str_remove(text, "some ")) |> 
  dplyr::mutate(text = stringr::str_remove_all(text, ",|\\.")) |>
  dplyr::mutate(notes = stringr::str_extract_all(text, "\\((.*?)\\)")) |>
  dplyr::mutate(notes = purrr::map_chr(notes, ~paste(.x, collapse = ", "))) |>
  dplyr::mutate(text = trimws(stringr::str_remove_all(text, "\\((.*?)\\)"))) |>
  dplyr::mutate(qty = as.numeric(stringr::str_extract(text, "^[0-9](.*?)(?=\\s)|^[0-9]*[0-9]$"))) |>
  dplyr::mutate(text = trimws(stringr::str_remove(text, "^[0-9](.*?)(?=\\s)|^[0-9]*[0-9]$"))) |>
  dplyr::mutate(text = ifelse(text == "", heading, text))

write.csv(land_1991, "data-raw/uk-land-1991.csv", row.names = FALSE)
