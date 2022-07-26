############################# METODO DE COMPACTACAO ACUMULACAO PARCIAL #############################

#' Compactacao Por Valor Acumulado
#' 
#' Reducao de dimensao por valor acumulada, possivelmente em partes crescentes
#' 
#' Esta funcao reduz a dimensao de um vetor de cenarios calculando a soma acumulada do vetor,
#' possivelmente em partes. Deve ser notado que, antes da compactacao, os cenarios sao escalonados
#' para o intervalo \eqn{[0, 1]} de modo a obter valores de menor variancia. Esta regularizacao e feita 
#' por bacia individualmente. Isto objetiva determinar um espaco reduzido que capture principalmente
#' o perfil de cada cenario em cada regiao, independentemente da magnitude.
#' 
#' Se \code{quebras = 3L}, por exemplo, a funcao quebra o cenario em tres partes iguais e calcula a
#' soma de cada um. O vetor em dimensao reduzida e, entao, a soma acumulada destas partes. Para 
#' cenarios de um ano, indo de janeiro a dezembro, o uso de \code{quebras = 3L} representa o calculo
#' do acumulado ate abril, agosto e dezembro (por construcao um vetor de valores crescentes).
#' 
#' @param cenarios objeto da classe \code{cenarios} contendo apenas uma bacia e ano de referencia
#' @param quebras ou um inteiro indicando em quantas partes iguais separar o dado ou um vetor de 
#'     inteiros indicando as posicoes nas quais separar
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
#' cens_compact <- acumulacens(cenariosdummy["SIN"], quebras = 2L)
#' \dontrun{
#' plot(cens_compact)
#' }
#' 
#' @seealso \code{\link{plot.compactcen}} para visualizacao das compactacoes
#' 
#' @export

acumulacens <- function(cenarios, quebras = 1L) {

    valor <- valor2 <- NULL

    if(quebras < 0) quebras <- length(attr(cenarios, "datas"))

    dat <- copy(cenarios$cenarios)

    # por algum motivo inexplicavel, valor := da erro mas assim funciona
    dat[, valor2 := (valor - min(valor)) / diff(range(valor)), by = "grupo"]
    dat[, valor := valor2]
    dat[, valor2 := NULL]

    out <- dat[, list(ind = seq(quebras), valor = stepcumsum(valor, quebras)),
        by = c("grupo", "cenario")]

    new_compactcen(out, "acumulacens", NULL)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Soma Acumulada Por Degraus
#' 
#' Calcula a soma acumulada de um vetor em partes
#' 
#' @param x vetor do qual calcular soma
#' @param qbr escalar ou vetor de inteiros indicando quantas partes ou onde separar as partes
#' 
#' @return vetor de ENA acumulada por partes

stepcumsum <- function(x, qbr) {
    if(length(qbr) == 1) qbr <- floor(seq(1, length(x), length.out = qbr + 1)[-1])
    out <- sapply(qbr, function(q) sum(x[seq(q)]))
    out <- unname(out)
    return(out)
}
