manufacture_ScorzProfile <- function(x,
                                     custom_args_ls = NULL,
                                     custom_fn = NULL,
                                     domain_pfxs_1L_chr = character(0),
                                     what_1L_chr = "domains",
                                     ...){
  object_xx <- NULL
  if(what_1L_chr == "domains"){
    if(is.null(custom_fn)){
      if(!identical(domain_pfxs_1L_chr, character(0))){
        object_xx <- purrr::map_chr(names(dplyr::select(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, dplyr::starts_with(domain_pfxs_1L_chr))),
                                    ~ {
                                      domain_1L_chr <- eval(parse(text=paste0("attributes(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb$",.x,")$label")))
                                      domain_1L_chr <- ifelse(!stringr::str_detect(domain_1L_chr," - "),
                                                              domain_1L_chr,
                                                              stringr::str_sub(domain_1L_chr,
                                                                               start = stringi::stri_locate_last_fixed(domain_1L_chr," - ")[1,1] %>%
                                                                                 unname() + 2))
                                      domain_1L_chr
                                    })


      }
    }else{
      object_xx <-  rlang::exec(custom_fn, !!!custom_args_ls)
    }
  }
  return(object_xx)
}
manufacture_ScorzAqol6 <- function(x,
                                   what_1L_chr = "domains",
                                   ...){
  object_xx <- NULL
  if(what_1L_chr == "domains"){
    object_xx <- c("Independent Living", "Relationships", "Mental Health", "Coping", "Pain", "Senses")
  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
manufacture_ScorzEuroQol5 <- function(x,
                                      what_1L_chr = "domains",
                                      ...){
  object_xx <- NULL
  if(what_1L_chr == "domains"){
    object_xx <- x@itm_labels_chr
  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
