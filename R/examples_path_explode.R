write_flipbook_setup <- function(){

"
---

`r chunk_reveal('funct', title = 'funct')`

```{r funct, include = F}
code
```

"
}



examples_path_flipbooks <- function(path = "R/", flipbook_setup = write_flipbook_setup()){

fs::dir_ls(path, pattern = "\\.R") %>%
  tibble::tibble(file = .) %>%
  dplyr::mutate(file = as.character(file)) %>%
  dplyr::mutate(funct = stringr::str_remove_all(file, paste0(path, "|\\.R"))) %>%
  dplyr::mutate(text = purrr::map(file, readLines)) %>%
  tidyr::unnest() %>%
  dplyr::filter(text %>% stringr::str_detect("^\\#\\' ")) %>%
  dplyr::group_by(file, funct) %>%
  dplyr::mutate(start_example = text %>% stringr::str_detect("^\\#\\' @examples")) %>%
  dplyr::mutate(example = ifelse(start_example, T, NA)) %>%
  tidyr::fill(example) %>%
  dplyr::filter(example, !start_example) %>%
  dplyr::mutate(text = text %>% stringr::str_remove("^\\#\\' ")) %>%
  dplyr::summarise(text = paste(text, collapse = "\n")) %>%
  dplyr::mutate(breaks = flipbook_setup) %>%
  dplyr::mutate(breaks = stringr::str_replace_all(breaks, "funct", funct)) %>%
  dplyr::mutate(breaks = stringr::str_replace(breaks, "code", text)) %>%
  dplyr::pull(breaks) %>%
  cat()

}


write_xaringan_yaml <- function(){

'---
title: "Example Exploder"
subtitle: "With flipbookr and xaringan"
author: "You!"
output:
  xaringan::moon_reader:
    lib_dir: libs
    css: [default, hygge, ninjutsu]
    nature:
      ratio: 16:9
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---'



}
