#' Dado Exemplo Do Pacote
#' 
#' Dado contendo um exemplo de cenarios gerados
#' 
#' \code{cenariosdummy} e um objeto da classe \code{cenarios}, uma lista de um emento chamado 
#' cenarios, sendo este um \code{data.table} em formato regular com as colunas
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
"cenariosdummy"