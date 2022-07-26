############################# FUNCOES PARA REDUCAO DE DIMENSIONALIDADE #############################

#' Funcoes de compactacao
#' 
#' Wrappers de metodos de compactacao para uso nos processos desse pacote
#' 
#' Durante a execucao das funcoes de selecao e/ou extracao de cenarios, o objeto \code{cenarios}
#' passa por uma reducao de dimensionalidade e posterior clusterizacao. As funcoes para compactacao
#' do dado podem implementar qualquer metodo, mas devem respeitar alguns padroes de argumentos de 
#' entrada e caracteristicas na saida.
#' 
#' Quanto as entradas, qualquer funcao implementada com este proposito deve receber na primeira 
#' posicao o argumento \code{cenarios}, um objeto da classe \code{cenarios}. Outros argumentos
#' especificos a cada metodo de compactacao podem seguir o primeiro, com qualquer nome (estes 
#' serao passados a chamada via o argumento \code{compact_args} das funcoes de selecao/extracao).
#' 
#' Os objetos retornados pelas funcoes de compactacao devem ser da classe \code{compactcen}. A 
#' geracao destes objetos e feita pela funcao \code{\link{new_compactcen}}. Essencialmente deve 
#' receber um datatable com estrutura igual a de objetos \code{cenarios}, exceto pela coluna 
#' \code{data} que muda para \code{ind}, indicando o indice da variavel compactada para cada 
#' cenario. Consulte a pagina de ajuda do construtor para mais detalhes.
#' 
#' Atualmente o pacote fornece duas opcoes:
#' 
#' \itemize{
#' \item{\code{\link{PCAcens}}}
#' \item{\code{\link{acumulacens}}}
#' }
#' 
#' As paginas de help de cada uma das funcoes contem detalhes a respeito dos argumentos de cada uma
#' (que podem ser passados as funcoes de selecao atraves de \code{compact_args} naquelas chamadas).
#' 
#' @seealso Funcoes \code{\link{PCAcens}} e \code{\link{acumulacens}} para compactacao de dados
#' 
#' @name compact_funs
NULL
