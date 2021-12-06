exhibit_ScorzProfile <- function(x,
                                 type_1L_chr = "ds",
                                 ...){
  if(type_1L_chr %in% c("ds","dict")){
    y <- procureSlot(procureSlot(x,"a_YouthvarsProfile"),
                     "a_Ready4useDyad")
  }
  if(type_1L_chr == "characterize"){
    y <- procureSlot(x,"a_YouthvarsProfile")
  }
  ready4::exhibit(y,
                  type_1L_chr = type_1L_chr,
                  ...)

}
