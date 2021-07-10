write_flipbook_setup <- function(){

"
---

`r flipbookr::chunk_reveal('funct', title = 'funct')`

```{r funct, include = F}
code
```

"
}



examples_path_flipbooks <- function(path = "R/", flipbook_setup = write_flipbook_setup()){

fs::dir_ls(path, pattern = "\\.R") %>%
  tibble::tibble(file = .) %>%
  dplyr::mutate(file = as.character(.data$file)) %>%
  dplyr::mutate(funct = stringr::str_remove_all(.data$file, paste0(path, "|\\.R"))) %>%
  dplyr::mutate(text = purrr::map(file, readLines)) %>%
  tidyr::unnest() %>%
  dplyr::filter(.data$text %>% stringr::str_detect("^\\#\\' ")) %>%
  dplyr::group_by(.data$file, .data$funct) %>%
  dplyr::mutate(start_example = .data$text %>% stringr::str_detect("^\\#\\' @examples")) %>%
  dplyr::mutate(example = ifelse(.data$start_example, T, NA)) %>%
  tidyr::fill(.data$example) %>%
  dplyr::filter(.data$example, !.data$start_example) %>%
  dplyr::mutate(text = .data$text %>% stringr::str_remove("^\\#\\' ")) %>%
  dplyr::summarise(text = paste(.data$text, collapse = "\n")) %>%
  dplyr::mutate(breaks = flipbook_setup) %>%
  dplyr::mutate(breaks = stringr::str_replace_all(.data$breaks, "funct", .data$funct)) %>%
  dplyr::mutate(breaks = stringr::str_replace(.data$breaks, "code", .data$text)) %>%
  dplyr::pull(breaks)

}


write_xaringan_yaml <- function(){

'---
title: "Example Exploder"
subtitle: "With flipbookr and xaringan"
author: ""
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

#'
#'
#' @param path
#' @param flipbook_setup
#' @param yaml
#' @param rmd
#' @param render
#'
#' @return
#' @export
#'
#' @examples
examples_path_write_exploded <- function(path = "R/",
                                    flipbook_setup = write_flipbook_setup(),
                                    yaml = write_xaringan_yaml(),
                                    rmd = "docs/exploded_examples.Rmd",
                                    render = T){

  examples_flipbook <- examples_path_flipbooks(path, flipbook_setup = flipbook_setup)

  paste(yaml, examples_flipbook, sep = "\n\n") %>%
    writeLines(con = rmd)

  if(render){

    rmarkdown::render(input = rmd)

  }

}


