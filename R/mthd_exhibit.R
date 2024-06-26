#' 
#' Exhibit features of model module data by printing them to the R console
#' @name exhibit-ScorzProfile
#' @description exhibit method applied to ScorzProfile
#' @param x An object of class ScorzProfile
#' @param type_1L_chr Type (a character vector of length one), Default: 'ds'
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @aliases exhibit,ScorzProfile-method
#' @export 
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", "ScorzProfile", function (x, type_1L_chr = "ds", ...) 
{
    if (type_1L_chr %in% c("ds", "dict")) {
        y <- procureSlot(procureSlot(x, "a_YouthvarsProfile"), 
            "a_Ready4useDyad")
    }
    if (type_1L_chr == "characterize") {
        y <- procureSlot(x, "a_YouthvarsProfile")
    }
    ready4::exhibit(y, type_1L_chr = type_1L_chr, ...)
})
