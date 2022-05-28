############################### WRAPPERS DE FUNCOES DE CLUSTERIZACAO ###############################

#' Funcoes De Clusterizacao
#' 
#' Wrappers de metodos de clusterizacao para uso nos processos do pacote
#' 
#' Durante a execucao das funcoes de selecao e/ou extracao de cenarios, o objeto \code{cenariosena}
#' passa por uma reducao de dimensionalidade e posterior clusterizacao. As funcoes para clusterizar
#' o dado podem implementar qualquer metodo, mas devem respeitar alguns padroes de argumentos de 
#' entrada e caracteristicas na saida.
#' 
#' Quanto as entradas, todas as funcoes de clusterizacao precisam receber o primeiro argumento 
#' chamado \code{compact}, correspondente a um objeto \code{compactcen} observacoes a serem
#' clusterizadas. Alem dele, o segundo argumento deve ser \code{nc}, recebendo um inteiro indicando
#' o numero de clustes desejados. Os demais argumentos sao livres, porem independentemente deles
#' deve existir \code{...} para consistencia com as demais funcoes.
#' 
#' Nao ha restricoes ou especificacoes para o objeto de saida, porem devem ser definidos metodos das 
#' genericas \code{getclustclass} e \code{getclustmeans}, que extraem do objeto de saida as 
#' classificacoes de cada obsevacao e centroides dos clusters respectivamente, para a classe dos 
#' objetos retornados. Por exemplo, \code{clustkmeans} retorna um objeto com classe \code{kmeans},
#' de modo que ha implementadas no pacote \code{getclustmeans.kmeans} e \code{getclustclass.kmeans},
#' que recebem objetos dessa classe e extraem centroides e classficacoes respectivamente.
#' 
#' Atualmente o pacote fornece duas opcoes:
#' 
#' \itemize{
#' \item{\code{\link{clustkmeans}}}
#' \item{\code{\link{clustEM}}}
#' }
#' 
#' As paginas de help de cada uma das funcoes contem detalhes a respeito dos argumentos de cada uma
#' (que podem ser passados as funcoes de selecao atraves de \code{...} naquelas chamadas).
#' 
#' @seealso Funcoes \code{\link{clustkmeans}} e \code{\link{clustEM}} para clusterizacao
#' 
#' @name clust_funs
NULL

# GENERICAS ----------------------------------------------------------------------------------------

#' Genericas Para Classes Cluster
#' 
#' Funcoes genericas para extracao das classes ajustadas e centroides de cada classe
#' 
#' @param clust objeto com classe \code{clustcens} e subclasse especifica do metodo
#' 
#' @return Para \code{getclustmeans}, os centroides; para \code{getclustclass} as classificacoes
#' 
#' @name getclust
NULL

#' @export 
#' 
#' @rdname getclust

getclustmeans <- function(clust) UseMethod("getclustmeans")

#' @rdname getclust

getclustmeans.default <- function(clust) {
    stop(paste0("Metodo 'getclustmeans' nao implementado para a classe ", class(clust)))
}

#' @export 
#' 
#' @rdname getclust

getclustclass <- function(clust) UseMethod("getclustclass")

#' @rdname getclust

getclustclass.default <- function(clust) {
    stop(paste0("Metodo 'getclustclass' nao implementado para a classe ", class(clust)))
}

# KMEANS -------------------------------------------------------------------------------------------

#' Clusteriza Dado Por Kmeans
#' 
#' Wrapper da funcao \code{\link[stats]{kmeans}} para uso neste pacote
#' 
#' @param compact objeto \code{compactcen} contendo cenarios compactados para clusterizar
#' @param nc numero de clusters
#' @param nstart numero de sementes para testar o kmeans
#' @param ... demais parametros passados a funcao \code{\link[stats]{kmeans}} exceto \code{nstart}
#' 
#' @return objeto \code{kmeans} -- Veja \code{\link[stats]{kmeans}} para mais detalhes
#' 
#' @importFrom stats kmeans
#' 
#' @export

clustkmeans <- function(compact, nc, nstart = 30, ...) {

    mat <- extracdims(compact)
    clusters <- kmeans(mat, nc, nstart = nstart, ...)
    return(clusters)
}

#' @param clust objeto da classe \code{kmeans}
#' 
#' @rdname clustkmeans

getclustclass.kmeans <- function(clust) clust$cluster

#' @param clust objeto da classe \code{kmeans}
#' 
#' @rdname clustkmeans

getclustmeans.kmeans <- function(clust) clust$centers

# KMEDOIDES ----------------------------------------------------------------------------------------

#' Clusteriza Dado Por Kmedoides
#' 
#' Wrapper da funcao \code{\link[cluster]{pam}} para uso neste pacote
#' 
#' @param compact objeto \code{compactcen} contendo cenarios compactados para clusterizar
#' @param nc numero de clusters
#' @param nstart numero de sementes para testar o kmeans
#' @param ... demais parametros passados a funcao \code{\link[cluster]{pam}} exceto \code{nstart}
#' 
#' @return objeto \code{kmeans} -- Veja \code{\link[cluster]{pam}} para mais detalhes
#' 
#' @export

clustkmedoids <- function(compact, nc, nstart = 30, ...) {

    if(!requireNamespace("cluster", quietly = TRUE)) {
        stop("Clusterizacao por k-medoides requer o pacote 'cluster'")
    }

    mat <- extracdims(compact)
    clusters <- cluster::pam(mat, nc, nstart = nstart, ...)
    return(clusters)
}

#' @param clust objeto da classe \code{pam}
#' 
#' @rdname clustkmedoids

getclustclass.pam <- function(clust) clust$clustering

#' @param clust objeto da classe \code{pam}
#' 
#' @rdname clustkmedoids

getclustmeans.pam <- function(clust) clust$medoids

# EM MIXTURE GUASS ---------------------------------------------------------------------------------

#' Clusteriza Dado Por Mistura Gaussiana
#' 
#' Wrapper da funcao \code{\link[mclust]{Mclust}} para uso neste pacote
#' 
#' @param compact objeto \code{compactcen} contendo cenarios compactados para clusterizar
#' @param nc numero de clusters
#' @param ... demais parametros passados a funcao \code{\link[mclust]{Mclust}}
#' 
#' @return objeto \code{Mclust} -- Veja \code{\link[mclust]{Mclust}} para mais detalhes
#' 
#' @export

clustEM <- function(compact, nc, ...) {
    if(!requireNamespace("mclust", quietly = TRUE)) {
        stop("Clusterizacao por EM requer o pacote 'mclust'")
    }

    mat <- extracdims(compact)

    # tem algum problema de implementacao do mclust que a funcao mclustBIC nao e encontrada a nao
    # ser que o pacote seja explicitamente importado no NAMESPACE. Como ele e uma dependencia
    # opcional isso se torna um problema
    # Para contorna-lo, e feito o bind do nome mclustBIC localmente a funcao homonima no namespace
    # do mclust
    # E uma maracturaia mas resolve o problema
    mclustBIC <- mclust::mclustBIC

    clusters <- mclust::Mclust(mat, G = nc, ...)
    return(clusters)
}

#' @param clust objeto da classe \code{Mclust}
#' 
#' @rdname clustEM

getclustclass.Mclust <- function(clust) clust$classification

#' @param clust objeto da classe \code{Mclust}
#' 
#' @rdname clustEM

getclustmeans.Mclust <- function(clust) t(clust$parameters$mean)

# HCLUST -------------------------------------------------------------------------------------------

#' Clusteriza Dado Por Metodo Aglomerativo Hierarquico
#' 
#' Wrapper da funcao \code{\link[stats]{hclust}} para uso neste pacote
#' 
#' @param compact objeto \code{compactcen} contendo cenarios compactados para clusterizar
#' @param nc numero de clusters
#' @param distfun uma funcao que receba a matriz de dados e retorne uma medida de dissimilaridade. 
#'     Por padrao \code{\link[stats]{dist}}
#' @param ... demais parametros passados a funcao \code{\link[stats]{hclust}}
#' 
#' @return objeto \code{hclust_aug}, que e um objeto \code{hclust} com dois atributos extras: as
#'     classificacoes do dado cortando o dendograma em \code{nc} clusters os respectivos centroides.
#'     Para mais detalhes acerca da classe \code{hclust}, veja \code{\link[stats]{hclust}}
#' 
#' @export

clusthierarq <- function(compact, nc, distfun = dist, ...) {

    mat <- extracdims(compact)
    dx  <- distfun(mat)
    clusters <- hclust(dx, ...)

    classes <- cutree(clusters, nc)
    medias  <- do.call(rbind, lapply(split(mat, classes), colMeans))

    class(clusters) <- c("hclust_aug", class(clusters))
    attr(clusters, "classes") <- classes
    attr(clusters, "medias")  <- medias

    return(clusters)
}

#' @param clust objeto da classe \code{hclust_aug}
#' 
#' @rdname clusthierarq

getclustclass.hclust_aug <- function(clust) attr(clust, "classes")

#' @param clust objeto da classe \code{hclust_aug}
#' 
#' @rdname clusthierarq

getclustmeans.hclust_aug <- function(clust) attr(clust, "medias")

# HELPERS ------------------------------------------------------------------------------------------

extracdims <- function(x) {
    x <- x$compact
    x <- dcast(x, cenario ~ grupo + ind, value.var = "valor")[, -1]
    return(x)
}