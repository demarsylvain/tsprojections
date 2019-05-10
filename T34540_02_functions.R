# ---------------------------------------------------------------------------- #
# printProgress                                                           ----
# ---------------------------------------------------------------------------- #
# fonction pour afficher des messages lorsqu'un long script roule
# s'adapte dans un shiny aussi

printProgress <- function(message, shiny, expr){
  if (shiny) {
    withProgress(message = message, value = 0, expr = expr)
  } else {
    print(message)
    eval(expr)
  }
}


# ---------------------------------------------------------------------------- #
# rename_own                                                              ----
# ---------------------------------------------------------------------------- #

rename_own <- function(df, cols, new_cols) { 
  rename_(df, .dots = setNames(cols, new_cols)) 
}






