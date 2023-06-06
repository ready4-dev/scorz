#' 
#' Manufacture a new object
#' @name manufacture-ScorzAqol6
#' @description manufacture method applied to ScorzAqol6
#' @param x An object of class ScorzAqol6
#' @param what_1L_chr What (a character vector of length one), Default: 'domains'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,ScorzAqol6-method
#' @export 
#' @importFrom methods callNextMethod
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "ScorzAqol6", function (x, what_1L_chr = "domains", ...) 
{
    object_xx <- NULL
    if (what_1L_chr == "domains") {
        object_xx <- c("Independent Living", "Relationships", 
            "Mental Health", "Coping", "Pain", "Senses")
    }
    else {
        object_xx <- methods::callNextMethod()
    }
    return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-ScorzEuroQol5
#' @description manufacture method applied to ScorzEuroQol5
#' @param x An object of class ScorzEuroQol5
#' @param what_1L_chr What (a character vector of length one), Default: 'domains'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,ScorzEuroQol5-method
#' @export 
#' @importFrom methods callNextMethod
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "ScorzEuroQol5", function (x, what_1L_chr = "domains", ...) 
{
    object_xx <- NULL
    if (what_1L_chr == "domains") {
        object_xx <- x@itm_labels_chr
    }
    else {
        object_xx <- methods::callNextMethod()
    }
    return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-ScorzProfile
#' @description manufacture method applied to ScorzProfile
#' @param x An object of class ScorzProfile
#' @param custom_args_ls Custom arguments (a list), Default: NULL
#' @param custom_fn Custom (a function), Default: NULL
#' @param domain_pfxs_1L_chr Domain prefixes (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'domains'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,ScorzProfile-method
#' @export 
#' @importFrom purrr map_chr
#' @importFrom dplyr select starts_with
#' @importFrom stringr str_detect str_sub
#' @importFrom stringi stri_locate_last_fixed
#' @importFrom rlang exec
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "ScorzProfile", function (x, custom_args_ls = NULL, custom_fn = NULL, domain_pfxs_1L_chr = character(0), 
    what_1L_chr = "domains", ...) 
{
    object_xx <- NULL
    if (what_1L_chr == "domains") {
        if (is.null(custom_fn)) {
            if (!identical(domain_pfxs_1L_chr, character(0))) {
                object_xx <- purrr::map_chr(names(dplyr::select(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
                  dplyr::starts_with(domain_pfxs_1L_chr))), ~{
                  domain_1L_chr <- eval(parse(text = paste0("attributes(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb$", 
                    .x, ")$label")))
                  domain_1L_chr <- ifelse(!stringr::str_detect(domain_1L_chr, 
                    " - "), domain_1L_chr, stringr::str_sub(domain_1L_chr, 
                    start = stringi::stri_locate_last_fixed(domain_1L_chr, 
                      " - ")[1, 1] %>% unname() + 2))
                  domain_1L_chr
                })
            }
        }
        else {
            object_xx <- rlang::exec(custom_fn, !!!custom_args_ls)
        }
    }
    return(object_xx)
})
