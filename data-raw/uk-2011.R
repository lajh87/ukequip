d <- "data-raw/2011_United Kingdom_06-Chapter Four_ Europe/"

layout <- readr::read_csv(file.path(d, "layout.csv")) 



tables <- list.files(d, pattern = "table") |>
  purrr::map(~{
    tbl <- readr::read_csv(
      file.path(d, .x), 
      col_names = FALSE,
      name_repair = "minimal"
    ) |>
      dplyr::slice(1:(which(stringr::str_detect(dplyr::pick(1)[[1]], "Confidence"))-2)) 
    tbl |> dplyr::select(1:(ncol(tbl)-1))
  })

tables

layout |>
  janitor::clean_names() |>
  dplyr::mutate(text = substr(text, 2, nchar(text))) |>
  dplyr::mutate(text = ifelse(is.na(text), layout, text)) |>
  dplyr::mutate(text = ifelse(stringr::str_detect(layout, "header"), paste("##", text), text)) |>
  dplyr::summarise(text = paste(text, collapse = "\n\n")) |>
  dplyr::pull() |>
  write("data-raw/uk-2011.md")


