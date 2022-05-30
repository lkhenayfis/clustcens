######################### FUNCOES PARA CLUSTERIZAR E/OU SELECIONAR CENARIOS ########################

#' Selecao De Cenarios Por Quantil
#' 
#' Seleciona cenarios representativos buscando aqueles mais proximos de quantis especificados
#' 
#' A selecao por quantis requer que cada cenario seja reduzido a apenas uma dimensao (de modo que 
#' seja possivel calcular quantis unicos). A compactacao pode ser feita por qualquer funcao que
#' retorne objetos da classe \code{compactcen}, informada atraves do argumento \code{compact_fun}. A
#' funcao sera chamada com \code{cenarios} como primeiro arumento; \code{compact_args} permite 
#' informar demais parametros a serem utilizados por \code{compact_fun}, na forma de uma lista 
#' nomeada.
#' 
#' O argumento \code{cenarios} deve ser um objeto da classe \code{cenarios}. \code{quantis} deve ser
#' um vetor indicando quais quantis estao sendo buscados. Por construcao e impossivel extrair
#' exatamente estes quantis, de modo que serao retornados os indices dos cenarios associados aos 
#' quantis mais proximos. 
#' 
#' @param cenarios objeto da classe \code{cenarios} do qual selecionar cenarios representativos
#' @param quantis vetor de quantis para selecionar
#' @param compact_fun funcao para compactacao dos cenarios. Ver \code{\link{compact_funs}}
#' @param compact_args lista nomeada informando argumentos (alem do primeiro) a \code{compact_fun}
#' 
#' @return lista contendo vetor de inteiros indicando indices dos cenarios selecionados e resultado 
#'     da compactacao
#' 
#' @examples 
#' 
#' # usando o dado exemplo do pacote
#' 
#' # execucao simples
#' selec <- selecporquantil(cenariosdummy)
#' 
#' # pegando quantis c(.33, .5, .67)
#' selec <- selecporquantil(cenariosdummy, c(.33, .5, .67))
#' 
#' @seealso \code{\link{plot.cenarios}} para visualizacao dos cenarios selecionados
#' 
#' @export

selecporquantil <- function(cenarios, quantis = c(.25, .5, .75), compact_fun = acumulacens,
    compact_args = list()) {

    compact_call <- c(list(compact_fun, cenarios), compact_args)
    compact <- eval(as.call(compact_call))

    dat <- compact$compact
    dat <- dcast(dat, cenario ~ ind, value.var = "valor")[, -1]

    if(ncol(dat) > 1) stop("A compactacao para selecao por quantil deve ser em apenas uma dimensao")

    dat_quant <- quantile(dat[[1]], quantis)
    maisprox  <- unname(sapply(dat_quant, function(dq) which.min((dat[[1]] - dq)^2)))

    out <- list(maisprox, compact)

    return(out)
}

#' Selecao Ou Extracao De Cenarios Por Clusterizacao
#' 
#' Seleciona ou extrai cenarios baseado nos centroides da clusterizacao de cenarios
#' 
#' O procedimento desta funcao e aplicar a reducao de dimensionalidade em \code{cenarios} e 
#' clusterizar as variaveis no espaco reduzido em \code{nc} clusters.
#' 
#' A compactacao pode ser feita por qualquer funcao que retorne objetos da classe \code{compactcen},
#' informada atraves do argumento \code{compact_fun}. A funcao sera chamada com \code{cenarios} como
#' primeiro arumento; \code{compact_args} permite informar demais parametros a serem utilizados por
#' \code{compact_fun}, na forma de uma lista nomeada.
#' 
#' A clusterizacao em si e realizada por uma chamada a funcao passada pelo argumento 
#' \code{clust_fun}, uma funcao que deve receber como primeiro argumento um objeto \code{cenarios} e 
#' como segundo argumento o numero de clusters desejado \code{nc}, e retornar um objeto com os 
#' metodos \code{getclustclass} e \code{getclustmeans}, para retornar as classificacoes de cada 
#' cenario e as medias dos centroides. A funcao passada sera usada numa chamada da forma 
#' \code{clust_fun(cenarios, nc, ...)}. \code{compact_fun} e \code{compact_args}.
#' 
#' Por fim, \code{transforma} controla o que sera retornado. Se \code{FALSE}, a funcao entrega 
#' apenas os indices dos cenarios mais proximos, por distancia euclidiana, dos centroides de cada
#' cluster. Se \code{TRUE}, o centroide de cada cluster (no espaco reduzido) sera transformado de
#' volta para o espaco original. Isso so e possivel caso a compactacao utilizada permita este tipo
#' de operacao: por exemplo, a reducao de dimensionalidade por acumulo de enas parciais e uma 
#' transformacao que nao permite retornar para a escala original. Outras como PCA e autoencoder sim.
#' 
#' @param cenarios objeto da classe \code{cenarios} do qual selecionar cenarios representativos
#' @param nc numero de clusters
#' @param clust_fun funcao para clusterizacao. Veja \code{\link{clust_funs}} para mais detalhes
#' @param ... demais parametros passados a \code{clust_fun}
#' @param transforma booleano indicando se devem ser retornados indices dos cenarios mais proximos
#'     do centroide de cada cluster (\code{FALSE}) ou se o cetroide deve ser transformado de volta
#'     para a escala original (\code{TRUE}). Ver Detalhes
#' @param compact_fun funcao para compactacao dos cenarios. Ver \code{\link{compact_funs}}
#' @param compact_args lista nomeada informando argumentos (alem do primeiro) a \code{compact_fun}
#' 
#' @return lista contendo vetor de inteiros indicando indices dos cenarios selecionados, resultado 
#'     da compactacao e resultado da clusterizacao
#' 
#' @examples
#' 
#' # usando o dado exemplo do pacote
#' 
#' # a selecao pode ser realizada para uma unica regiao ou multiplas de forma multivariada
#' 
#' # Usando somente o SIN completo
#' cens <- cenariosdummy["SIN"]
#' 
#' # Usando SUL e SE juntos
#' cens <- cenariosdummy[c("SUL", "SE")]
#' 
#' # execucao simples pegando 3 clusters
#' selec <- selecporcluster(cens, 3)
#' 
#' # usando diferentes funcoes de compactacao
#' selec <- selecporcluster(cens, 3, compact_fun = acumulacens, compact_args = list(quebras = 3))
#' selec <- selecporcluster(cens, 3, compact_fun = PCAcens, compact_args = list(vartot = .7))
#' 
#' # usando diferentes funcoes de clusterizacao
#' selec <- selecporcluster(cens, 3, clust_fun = clusthierarq, method = "single")
#' 
#' # os dois metodos seguintes precisam que os pacotes 'mclust' e 'cluster', respectivamente, 
#' # estejam instalados
#' \dontrun{
#' selec <- selecporcluster(cens, 3, clust_fun = clustEM, modelNames = c("EEE", "VVV"))
#' selec <- selecporcluster(cens, 3, clust_fun = clustkmedoids, metric = "manhattan")
#' }
#' 
#' # Por fim, pode ser feita a visualizacao da escolha
#' selec <- selecporcluster(cens, 3)
#' \dontrun{
#' plot(cens, cens[, , selec[[1]]])
#' }
#' 
#' @seealso \code{\link{plot.cenarios}} para visualizacao dos cenarios selecionados
#' 
#' @export

selecporcluster <- function(cenarios, nc, clust_fun = clustkmeans, ..., transforma = FALSE,
    compact_fun = PCAcens, compact_args = list()) {

    compact_call <- c(list(compact_fun, cenarios), compact_args)
    compact <- eval(as.call(compact_call))

    cluster_call <- c(list(clust_fun, compact, nc), list(...))
    cluster <- eval(as.call(cluster_call))

    if(attr(compact, "teminv") & transforma) {
        stop("Transformacao inversa ainda nao implementada")
    } else {

        # calcula elementos mais proximos de cada centroide

        dat <- extracdims(compact)

        novaordem <- split(seq(nrow(dat)), getclustclass(cluster))

        dat_clust <- split(dat, getclustclass(cluster))
        clstmeans <- getclustmeans(cluster)
        clstmeans <- lapply(seq(dat_clust), function(i) clstmeans[i, ])

        maisprox <- mapply(dat_clust, clstmeans,
            FUN = function(dc, cm) rowSums(mapply(dc, cm, FUN = "-")^2), SIMPLIFY = FALSE)
        maisprox <- lapply(maisprox, which.min)
        maisprox <- mapply(novaordem, maisprox, FUN = function(v, i) v[i])

        out <- list(maisprox, compact, cluster)

        return(out)
    }
}