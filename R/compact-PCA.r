##################################### METODO DE COMPACTACAO PCA ####################################

#' Compactacao Por PCA
#' 
#' Reduz dimensionalidade dos cenarios via PCA mantendo um minimo de variacao total
#' 
#' A matriz de dados considerada para reducao de dimensionalidade tem em cada linha o cenario e,
#' em cada coluna, os passos a frente de simulacao por bacia. Desta forma, cem cenarios 15 passos a
#' frente de dois grupos corresponde a uma matriz de dados 100 por 30. Serao selecionadas as n 
#' primeiras componentes principais que representem no minimo \code{vartot} por cento da variacao
#' total. Deve ser observado que o dado e normalizado para media zero e variancia um antes da 
#' compactacao.
#' 
#' @param cenarios objeto da classe \code{cenarios} contendo apenas uma bacia e ano de referencia
#' @param vartot percentual em formato decimal de variacao total mininima
#' 
#' @return objeto da classe \code{compactcen} contendo o dado em dimensao reduzida. Este e uma lista
#'     de um elemento nomeado \code{"compact"}, um \code{data.table} com as colunas
#' 
#' \describe{
#' \item{\code{grupo}}{grupo a qual o cenario corresponde}
#' \item{\code{cenario}}{indice do cenario com respeito a grupo}
#' \item{\code{ind}}{indice do elemento no vetor de dimensao reduzida}
#' \item{\code{valor}}{valor do elemento no vetor de dimensao reduzida}
#' }
#' 
#' Adicionalmente contem os atributos
#' 
#' \itemize{
#' \item{\code{metodo}: }{string do nome da funcao chamada para compactacao}
#' \item{\code{teminv}: }{booleano indicando se a compactacao possui inversa}
#' \item{\code{invfunc}: }{caso tenha inversa, a funcao que recebe vetores no espaco compactado e 
#'     retorna no espaco original}
#' }
#' 
#' @examples 
#' 
#' # usando o dado exemplo do pacote
#' 
#' # Compactando apenas os cenarios relativos ao SIN completo
#' cens_compact <- PCAcens(cenariosdummy["SIN"])
#' \dontrun{
#' plot(cens_compact)
#' }
#' 
#' @seealso \code{\link{plot.compactcen}} para visualizacao das compactacoes
#' 
#' @export

PCAcens <- function(cenarios, vartot = .8) {

    ind <- grupo <- cenario <- NULL

    if(vartot > 1) vartot <- vartot / 100

    dat <- copy(cenarios$cenarios)
    dat[, cenario := factor(cenario, levels = unique(cenario))]

    dat <- dcast(dat, cenario ~ grupo + indice, value.var = "valor")
    pca <- prcomp(dat[, -1], scale = TRUE)

    if(vartot < 0) {
        importance <- 1
    } else {
        importance <- which(summary(pca)$importance[3, ] >= vartot)[1]
    }

    monta_out_PCA(pca$x, pca$rotation, importance, dat[[1]], attr(cenarios, "grupos"),
        list(pca$center, pca$scale))
}

#' Compactacao De Novas Observacoes
#' 
#' Compacta novos cenarios utilizando a mesma funcao ja estimada para o objeto \code{x}
#' 
#' @param x objeto \code{compactcen} contendo uma compactacao por PCA
#' @param newcens objeto \code{cenarios} contendo novos valores a compactar
#' 
#' @return objeto da classe \code{compactcen} contendo o novo dado em dimensao reduzida
#' 
#' @export

predict.PCAcens <- function(x, newcens, ...) {

    SIGMA <- attr(x, "SIGMA")
    importance <- attr(x, "importance")
    escala <- attr(x, "escala")

    dat <- copy(newcens$cenarios)
    dat[, cenario := factor(cenario, levels = unique(cenario))]

    dat <- dcast(dat, cenario ~ grupo + indice, value.var = "valor")
    cens <- dat[[1]]
    dat  <- dat[, -1]

    dat[] <- mapply(dat, escala[[1]], escala[[2]], FUN = function(d, s1, s2) (d - s1) / s2, SIMPLIFY = FALSE)
    compdat <- data.matrix(dat) %*% SIGMA

    monta_out_PCA(compdat, SIGMA, importance, cens, attr(newcens, "grupos"), escala)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Monta Objeto \code{compactcens} Com PCA
#' 
#' Wrapper de codigo comum entre \code{PCAcens} e seu metodo \code{predict}
#' 
#' @param compact matriz contendo a transformacao completa do dado em PCAs
#' @param importance inteiro indicando quantas componentes reter
#' @param cens nomes dos cenarios originalmente passados no objeto sendo compactado
#' 
#' @return objeto da classe \code{compactcen} contendo o dado em dimensao reduzida

monta_out_PCA <- function(compdat, SIGMA, importance, cenarios, grupos, escala) {

    compdat <- compdat[, seq(importance), drop = FALSE]
    compdat <- cbind(cenario = cenarios, as.data.table(compdat))
    compdat <- melt(compdat, id.vars = "cenario", variable.name = "ind", value.name = "valor")
    compdat[, ind := as.numeric(sub("[[:alpha:]]*", "", ind))]

    out <- cbind(grupo = paste0(grupos, collapse = "."), compdat)
    setorder(out, grupo, cenario, ind)
    out[, cenario := as.character(cenario)]

    new_compactcen(out, "PCAcens", importance = importance, SIGMA = SIGMA, escala = escala)
}