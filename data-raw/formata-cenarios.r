devtools::load_all()
arq <- system.file("extdata/cenarios.csv", package = "clustena")
cenariosdummy <- learqcenarios(arq)
usethis::use_data(cenariosdummy, overwrite = TRUE)
