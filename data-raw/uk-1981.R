# pdftools::pdf_subset(
#   file.path(
#     "../../../OneDrive/eBooks/MilBal/",
#     "Vol_081(1)_The Military Balance",
#     "04597228108459913.pdf"
#     ),
#   pages = 13:15,
#   "data-raw/uk1981.pdf"
# )

mround <- function(x,base){
  base*round(x/base)
}

pdf_tbls <- pdftools::pdf_data("data-raw/uk1981.pdf", font_info = TRUE)
page <- pdf_tbls[[1]] |>
  dplyr::mutate(y = mround(y, 5)) |>
  dplyr::filter(x != min(x), y != min(y))

midpoint <- page |>
  dplyr::summarise(min(x) + (max(x) - min(x))/2) |>
  dplyr::pull()

library(ggplot2)
ggplot(page) + geom_bar(aes(x)) + geom_vline(aes(xintercept = midpoint), linetype = 2)

col_1 <- page |> dplyr::filter(x < midpoint)
col_2 <- page |> dplyr::filter(x > midpoint)


start <- col_2 |> dplyr::filter(stringr::str_detect(text, "AFV")) |> dplyr::pull(y)
end <- col_2 |> dplyr::filter(stringr::str_detect(text, "Spartan")) |> dplyr::pull(y)

land_1 <- col_2 |> dplyr::filter(y>=start, y <= end)  |>
  dplyr::summarise(text = paste(text, collapse =  " "), 
                   min_x = min(x)) |>
  dplyr::mutate(heading = stringr::str_extract(text, "^[A-Z]*\\:")) |>
  dplyr::mutate(text = stringr::str_remove(text, "^[A-Z]*\\:\\s")) |>
  dplyr::mutate(text = stringr::str_split(text, "; ")) |>
  tidyr::unnest(cols = text) |>
  dplyr::mutate(text = stringr::str_replace(text, "([0-9]),([0-9])", "\\1\\2")) |>
  dplyr::mutate(text = stringr::str_remove(text, "- ")) |>
  dplyr::mutate(text = stringr::str_split(text, ", ")) |>
  tidyr::unnest(text) |>
  dplyr::mutate(heading = substr(heading, 1, nchar(heading)-1)) |>
  dplyr::mutate(qty = as.numeric(trimws(stringr::str_extract(text, "^[0-9]*\\s")))) |>
  dplyr::mutate(text = stringr::str_remove(text, "^[0-9]*\\s")) |>
  dplyr::select(-min_x) 

land_1

page2 <- pdf_tbls[[2]] |>
  dplyr::mutate(y = mround(y, 5)) |> 
  dplyr::mutate(x = mround(x, 2)) |>
  dplyr::filter(x != min(x), y != min(y))

midpoint2 <- page2 |>
  dplyr::summarise(min(x) + (max(x) - min(x))/2) |>
  dplyr::pull()

library(ggplot2)
ggplot(page2) + geom_bar(aes(x)) + geom_vline(aes(xintercept = midpoint2), linetype = 2)

col_1_2 <- page2 |> dplyr::filter(x < midpoint2)
col_2_2 <- page2 |> dplyr::filter(x > midpoint2)

start2 <- col_1_2|> dplyr::filter(stringr::str_detect(text, "Arty")) |> dplyr::pull(y)
end2 <- (col_1_2 |> dplyr::filter(stringr::str_detect(text, "hel.")) |> dplyr::pull(y))[1]

land_2 <- col_1_2 |> dplyr::filter(y>=start2, y <= end2) |>
  dplyr::group_by(y) |>
  dplyr::summarise(text = paste(text, collapse =  " "), 
                   min_x = min(x)) |>
  dplyr::mutate(heading = stringr::str_extract(text, "^[A-Z]*:|^[A-Z][a-z]*:")) |>
  dplyr::mutate(heading = substr(heading, 1, nchar(heading)-1)) |>
  tidyr::fill(heading, .direction = "down") |>
  dplyr::mutate(text = stringr::str_remove(text, "^[A-Z]*:\\s|^[A-Z][a-z]*:\\s")) |>
  dplyr::group_by(heading) |>
  dplyr::summarise(text = paste(text, collapse = " ")) |>
  dplyr::mutate(text = stringr::str_replace(text, "([0-9]),([0-9])", "\\1\\2")) |>
  dplyr::mutate(text = stringr::str_split(text, "; |, ")) |>
  tidyr::unnest(text) |>
  dplyr::mutate(qty = trimws(stringr::str_extract(text, "^[0-9]*\\s"))) |>
  dplyr::mutate(text = stringr::str_remove(text, "[0-9]*\\s")) |>
  dplyr::mutate(qty = as.numeric(qty))

land_1981 <- land_1 |>
  dplyr::bind_rows(land_2) |>
  dplyr::mutate(text = stringr::str_remove(text, "\\.$")) |>
  dplyr::select(heading, equipment = text, qty)

land_1981
write.csv(land_1981, "data-raw/uk-land-1981.csv", row.names = FALSE)
