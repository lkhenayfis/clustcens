devtools::load_all()

learqcenarios <- function(arq, pat_data = "%m/%Y") {

    cenario <- data <- bacia <- anoref <- NULL

    temdia <- grepl("\\%d", pat_data)
    pat_data_2 <- ifelse(temdia, pat_data, paste0("%d__", pat_data))

    dat <- fread(arq)

    coldata <- as.Date(paste0("01__", colnames(dat)), format = pat_data_2)
    colnames(dat)[!is.na(coldata)] <- as.character(coldata[!is.na(coldata)])

    dat[, cenario := seq(.N), by = c("bacia", "anoref")]

    dat <- melt(dat, id.vars = colnames(dat)[is.na(coldata)], value.name = "ena", variable.name = "data")
    dat[, data := as.Date(as.character(data))]
    setorder(dat, bacia, anoref, cenario)
    colnames(dat) <- c("cenario", "grupo", "anoref", "indice", "valor")
    setcolorder(dat, c("anoref", "grupo", "cenario", "indice", "valor"))

    dat <- split(dat, by = "anoref", keep.by = FALSE)
    dat <- lapply(dat, as.cenarios)

    dat[[1]]
}

arq <- "data-raw/cenarios.csv"
cenariosdummy <- learqcenarios(arq)
usethis::use_data(cenariosdummy, overwrite = TRUE)
