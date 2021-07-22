pch_to_glyph <- function(pch, alpha = NULL) {

  len <- length(pch)
  if(len == 0) return(loon::l_getOption("glyph"))

  switchPch <- function(pch) {


    switch(
      as.character(pch),
      "19" = "circle" ,
      "1" = "ocircle",
      "21" = "ccircle",
      "15" = "square",
      "0" = "osquare",
      "22" = "csquare",
      "17" = "triangle",
      "2" = "otriangle",
      "24" = "ctriangle",
      "18" = "diamond",
      "5" = "odiamond",
      "23" = "cdiamond",
      {
        loon::l_getOption("glyph")
      }
    )
  }

  vapply(seq(len),
         function(i) {
           if(is.na(alpha[i]) || is.null(alpha[i])){
             switchPch(pch[i])
           } else {
             if(alpha[i] < 0.5 ){
               switch(
                 as.character( pch[i] ),
                 "19" = "ocircle" ,
                 "1" = "ocircle",
                 "21" = "ocircle",
                 "15" = "osquare",
                 "0" = "osquare",
                 "22" = "osquare",
                 "17" = "otriangle",
                 "2" = "otriangle",
                 "24" = "otriangle",
                 "18" = "odiamond",
                 "5" = "odiamond",
                 "23" = "odiamond",
                 {
                   "ocircle"
                 }
               )
             } else {
               switchPch(pch[i])
             }
           }
         }, character(1))
}
