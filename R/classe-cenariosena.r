########################## FUNCOES PARA IMPORTACAO E MANIPULACAO DOS DADOS #########################

#' Leitura Do Arquivo De Cenarios
#' 
#' Funcao auxiliar que le arquivo texto e formata como objeto da classe \code{cenariosena}
#' 
#' Espera-se um arquivo com formato especifico. As tres primeiras colunas devem conter o indice do 
#' cenario, bacia e ano de referencia, respectivamente. As demais colunas devem corresponder aos
#' passos de tempo para os quais os cenarios foram gerados. Por exemplo, um arquivo contendo cinco 
#' cenarios gerados para 2022-01, 2022-02 e 2022-03 teria a forma
#' 
#' | cenario | bacia | anoref | 1/2022 | 2/2022 | 3/2022 |
#' | :-----: | :---: | :----: | :----: | :----: | :----: |
#' | 1       | SIN   | AA     | XXXX   | XXXX   | XXXX   | 
#' | 2       | SIN   | AA     | XXXX   | XXXX   | XXXX   | 
#' | 3       | SIN   | AA     | XXXX   | XXXX   | XXXX   |
#' | 4       | SIN   | AA     | XXXX   | XXXX   | XXXX   |
#' | 5       | SIN   | AA     | XXXX   | XXXX   | XXXX   |
#' 
#' XXXX   e um placeholder para os valores de ena e AA um placeholder para os valores de ano de 
#' referencia. Nao existe um padrao para os anos, podendo ser qualquer tipo de dado (i.e. "A1", 
#' "ano_1", 2022 e etc).
#' 
#' O nome das colunas de cenarios nao precisa seguir esta formato, sendo possivel informar o padrao
#' de nome pelo argumento \code{pat_data}, que deve corresponder a uma string de formato de data.
#' Neste caso, \code{pat_data = "%m/$Y"}
#' 
#' @param arq caminho do arquivo a ser lido com os cenarios. Formato deve ser compativel com o 
#'     descrito em Detalhes
#' @param pat_data string indicando o formato de data dos nomes das colunas de cenario
#' 
#' @return objeto da classe \code{cenariosena} contendo dados do arquivo lido
#' 
#' @export

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

    new_cenariosena(dat)
}

new_cenariosena <- function(dat) {

    out <- list(cenarios = dat[, .SD, .SDcols = c("anoref", "bacia", "cenario", "data", "ena")])

    class(out) <- "cenariosena"

    attr(out, "bacias") <- unique(dat$bacia)
    attr(out, "datas")  <- unique(dat$data)
    attr(out, "anos")   <- unique(dat$anoref)
    attr(out, "ncen")   <- nrow(dat) / with(attributes(out), length(bacias) * length(datas) * length(anos))

    return(out)
}

#' Conversao Para \code{cenariosena}
#' 
#' Converte um data.table em um objeto da classe \code{cenariosena}
#' 
#' \code{dat} deve ter a seguinte estrutura
#' 
#' | anoref | bacia | cenario | indice | valor |
#' | :----: | :---: | :-----: | :----: | :---: |
#' | AA     | SIN   | 1       | 1      | XXXX  | 
#' | AA     | SIN   | 1       | 2      | XXXX  | 
#' | AA     | SIN   | 1       | 3      | XXXX  |
#' | AA     | SIN   | 2       | 1      | XXXX  |
#' | AA     | SIN   | 2       | 2      | XXXX  |
#' 
#' @param dat data.table para converter
#' 
#' @return objeto da classe \code{cenariosena}
#' 
#' @export

as.cenariosena <- function(dat) new_cenariosena(dat)

# METODOS ------------------------------------------------------------------------------------------

#' Subset De \code{cenariosena}
#' 
#' Extrai subsets de objetos \code{cenariosena} e adequa os atributios
#' 
#' @param x objeto da classe \code{cenariosena}
#' @param i escalar ou vetor de anos de referencia para extrair do dado
#' @param j escalar ou vetor de bacias para extrair do dado
#' @param k escalar ou vetor de indices de cenarios para extrair do dado
#' @param l escalar ou vetor de datas para extrair do dado
#' @param ... nao possui uso -- existe apenas para consistencia com a generica
#' 
#' @return objecto \code{cenariosena} contendo apenas os valores especificados
#' 
#' @export

`[.cenariosena` <- function(x, i, j, k, l, ...) {

    anoref <- bacia <- cenario <- data <- NULL

    dat <- copy(x$cenarios)

    if(missing(i)) {
        i <- attr(x, "anos")
    }
    if(missing(j)) {
        j <- attr(x, "bacias")
    } else if(is.numeric(j)) {
        j <- attr(x, "bacias")[j]
    }
    if(missing(k)) {
        k <- seq(attr(x, "ncen"))
    }
    if(missing(l)) {
        l <- attr(x, "datas")
    } else if(is.numeric(l)) {
        l <- attr(x, "datas")[l]
    } else {
        l <- as.Date(l)
    }

    dat <- dat[(anoref %in% i) & (bacia %in% j) & (cenario %in% k) & (data %in% l)]

    new_cenariosena(dat)
}
