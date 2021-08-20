#' @importFrom rlang .data

write_flipbook_setup <- function(){

"
---

`r flipbookr::chunk_reveal('funct_num_example', title = '### funct')`

```{r funct_num_example, include = F}
code
```

"
}

examples_path_flipbooks <- function(package_path = ".",
  functions_path = "./R/", flipbook_setup = write_flipbook_setup()){

fs::dir_ls(functions_path, pattern = "\\.R") %>%
  tibble::tibble(file = .) %>%
  dplyr::mutate(file = as.character(.data$file)) %>%
  dplyr::mutate(funct = stringr::str_remove_all(.data$file, paste0(functions_path, "|\\.R"))) %>%
  dplyr::mutate(text = purrr::map(file, readLines)) %>%
  tidyr::unnest() %>%
  dplyr::filter(.data$text %>% stringr::str_detect("^\\#\\' ")) %>%
  dplyr::mutate(file = forcats::fct_inorder(.data$file)) %>%
  dplyr::mutate(funct = forcats::fct_inorder(.data$file)) %>%
  dplyr::group_by(.data$file, .data$funct) %>%
  dplyr::mutate(start_example = .data$text %>% stringr::str_detect("^\\#\\' @examples")) %>%
  dplyr::mutate(example = ifelse(.data$start_example, T, NA)) %>%
  tidyr::fill(.data$example) %>%
  dplyr::filter(.data$example, !.data$start_example) %>%
  dplyr::mutate(subexample = stringr::str_extract(text, "^#' #.+")) %>%
  tidyr::fill(.data$subexample) %>%
  dplyr::group_by(.data$file, .data$funct, .data$subexample) %>%
  dplyr::mutate(num_example = dplyr::cur_group_id()) %>%
  dplyr::group_by(.data$file, .data$funct, .data$subexample, .data$num_example) %>%
  dplyr::mutate(text = .data$text %>% stringr::str_remove("^\\#\\' ")) %>%
  dplyr::summarise(text = paste(.data$text, collapse = "\n")) %>%
  # prepare breaks for each section
  dplyr::mutate(breaks = flipbook_setup) %>%
  dplyr::mutate(breaks = stringr::str_replace_all(.data$breaks, "funct", .data$funct)) %>%
  dplyr::mutate(breaks = stringr::str_replace(.data$breaks, "code", .data$text)) %>%
  dplyr::mutate(breaks = stringr::str_replace_all(.data$breaks, "num_example", as.character(.data$num_example))) %>%
  dplyr::pull(.data$breaks)

}


write_xaringan_yaml <- function(package_name){

paste0(
'---
title: ', package_name,'
subtitle: ""
author: "Exploded Examples"
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
)

}

write_setup_code_chunk <- function(package_name){

paste0(
'```{r setup, echo = F}
knitr::opts_chunk$set(echo = TRUE, message = F, warning = F, fig.height = 6, comment = "")
options(knitr.duplicate.label = "allow")
library(',
package_name
,')
```')

}



#' Returns a flipbook built from package function examples
#'
#' @param flipbook_setup
#' @param yaml
#' @param rmd
#' @param render
#' @param package_name
#' @param package_path
#' @param functions_path
#' @param setup_code_chunk
#'
#' @return
#' @export
#'
#' @examples
package_examples_exploded <- function(package_name,
                                        package_path = ".",
                                         functions_path = "./R/",
                                    flipbook_setup = write_flipbook_setup(),
                                    yaml = write_xaringan_yaml(package_name),
                                    setup_code_chunk = write_setup_code_chunk(package_name),
                                    rmd = "docs/exploded_examples.Rmd",
                                    render = T){

  examples_flipbook <- examples_path_flipbooks(functions_path = functions_path,
                                               flipbook_setup = flipbook_setup)

  paste(examples_flipbook, collapse = "\n") %>%
  paste(yaml, setup_code_chunk, ., sep = "\n\n") %>%
    writeLines(con = rmd)

  if(render){

    rmarkdown::render(input = rmd)

  }

}


