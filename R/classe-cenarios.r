########################## FUNCOES PARA IMPORTACAO E MANIPULACAO DOS DADOS #########################

#' Construtor Interno De \code{cenarios}
#' 
#' Funcao interna para instanciar objetos \code{cenarios}, nao deve ser chamada diretamente
#' 
#' @param dat um data.table contendo as colunas \code{c("grupo", "cenario", "indice", "valor")}
#' 
#' @return objeto \code{cenarios}, uma lista de um elemento chamado "cenarios" que e um
#'     \code{data.table} com as colunas
#' 
#' \describe{
#' \item{\code{grupo}}{grupo ao qual o cenario corresponde}
#' \item{\code{cenario}}{indice do cenario com respeito a bacia e ano de referencia}
#' \item{\code{indice}}{indice sequencial de valores do cenario}
#' \item{\code{valor}}{valor do cenario}
#' }
#' 
#' Adicionalmente contem os atributos
#' 
#' \itemize{
#' \item{\code{grupos}: }{vetor de caracters contendo as grupos presentes no dado}
#' \item{\code{indices}: }{vetor contendo os indices para os quais foram gerados cenarios}
#' \item{\code{ncen}: }{inteiro indicando numero de cenarios por grupo}
#' }

new_cenarios <- function(dat) {

    out <- list(cenarios = dat[, .SD, .SDcols = c("grupo", "cenario", "indice", "valor")])

    class(out) <- "cenarios"

    attr(out, "grupos")  <- unique(dat$grupo)
    attr(out, "indices") <- unique(dat$indice)
    attr(out, "ncen")    <- nrow(dat) / with(attributes(out), length(grupos) * length(indices))

    return(out)
}

#' Conversao Para \code{cenarios}
#' 
#' Converte um \code{data.frame} ou \code{data.table} em um objeto da classe \code{cenarios}
#' 
#' \code{dat} deve ter uma estrutura especifica, de quatro conlunas nomeadas 
#' \code{c("grupo", "cenario", "indice", "valor")}, nao necessariamente nesta ordem.
#' 
#' \code{grupo} pode ser uma coluna de inteiros, fatores, strings ou qualquer outro tipo de dado,
#' indicando de alguma forma grupos aos quais os cenarios pertencem como, por exemplo, usinas ou 
#' bacias. \code{cenario} deve ser uma coluna de inteiros indicando o indice do cenario. 
#' \code{indice} deve ser uma coluna de inteiros, datas ou qualquer outro tipo de dado, indicando de
#' alguma forma a sequencia temporal dos valores no cenario. Por fim, \code{valor} deve ser uma
#' coluna numerica indicando o valor simulado.
#' 
#' Um exemplo de \code{dat} e dado abaixo
#' 
#' | grupo | cenario | indice     | valor |
#' | :---: | :-----: | :--------: | :---: |
#' | SIN   | 1       | 2022-01-01 | XXXX  | 
#' | SIN   | 1       | 2022-01-02 | XXXX  | 
#' | SIN   | 1       | 2022-01-03 | XXXX  |
#' | SIN   | 2       | 2022-01-01 | XXXX  |
#' | SIN   | 2       | 2022-01-02 | XXXX  |
#' 
#' @param dat data.table para converter
#' 
#' @return objeto da classe \code{cenarios} -- veja \code{\link{new_cenarios}} para mais detalhes
#' 
#' @export

as.cenarios <- function(dat) {

    setDT(dat)
    datcols <- colnames(dat)

    if(all(datcols %in% c("grupo", "cenario", "indice", "valor"))) {
        setcolorder(dat, c("grupo", "cenario", "indice", "valor"))
    } else if(ncol(dat) == 4) {
        warning("Nomes das colunas de 'dat' nao sao o esperado -- serao forcados para transformacao")
        colnames(dat) <- c("grupo", "cenario", "indice", "valor")
    } else {
        stop("'dat' nao possui colunas com os nomes esperados e/ou numero de colunas diferente de 4")
    }

    new_cenarios(dat)
}

# METODOS ------------------------------------------------------------------------------------------

#' Subset De \code{cenarios}
#' 
#' Extrai subsets de objetos \code{cenarios} e adequa os atributios
#' 
#' @param x objeto da classe \code{cenarios}
#' @param i escalar ou vetor de grupos para extrair do dado
#' @param j escalar ou vetor de indices de cenarios para extrair do dado
#' @param k escalar ou vetor de datas para extrair do dado
#' @param ... nao possui uso -- existe apenas para consistencia com a generica
#' 
#' @return objeto \code{cenarios} contendo apenas os valores especificados
#' 
#' @export

`[.cenarios` <- function(x, i, j, k, ...) {

    grupo <- cenario <- data <- NULL

    dat <- copy(x$cenarios)

    if(missing(i)) {
        i <- attr(x, "grupos")
    } else if(is.numeric(i)) {
        i <- attr(x, "grupos")[i]
    }
    if(missing(j)) {
        j <- seq(attr(x, "ncen"))
    }
    if(missing(k)) {
        k <- attr(x, "indices")
    } else if(is.numeric(k)) {
        k <- attr(x, "indices")[k]
    }

    dat <- dat[(grupo %in% i) & (cenario %in% j) & (indice %in% k)]

    new_cenarios(dat)
}
