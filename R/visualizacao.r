####################################### VISUALIZACAO DE DADOS ######################################

#' Visualizacao De \code{cenarios}
#' 
#' Plot em grade de objetos \code{cenarios}
#' 
#' O argumento \code{...} permite que sejam passados mais outros objetos \code{cenarios} contendo
#' um numero menor de cenarios selecionados para plot por cima do completo.
#' 
#' Esta funcao exige a dependencia opcial \code{ggplot2}, sem a qual retorna um erro.
#' 
#' @param x objeto da classe \code{cenarios}
#' @param ... objetos \code{cenarios} opcionais com cenarios a serem plotados por cima
#' @param print booleano indicando se o plot deve ser exibido ou retornado invisivelmente
#' 
#' @return objeto \code{ggplot} contendo o plot em grade dos cenarios em \code{x}
#' 
#' @examples
#' 
#' # toda a informacao em x sera plotada, incluindo anos de referencia e bacias. O controle pode
#' # ser feito usando o subset de objetos cenariosena
#' 
#' \dontrun{
#' plot(cenariosdummy)
#' plot(cenariosdummy[c("SUL", "SE")])
#' plot(cenariosdummy[, seq(10, 15)])
#' plot(cenariosdummy[, , seq(as.Date("2022-09-01"), as.Date("2023-05-01"), by = "month")])
#' }
#' 
#' # o uso do argumento ... permite realcar alguns cenarios
#' 
#' # por exemplo, realcando os cenarios indice 10, 15 e 20 em todos os grupos
#' \dontrun{
#' plot(cenariosdummy, cenariosdummy[, c(10, 15, 20)])
#' }
#' 
#' # realcando 1 a 10 apenas no SUL
#' \dontrun{
#' plot(cenariosdummy, cenariosdummy["SUL", seq(10)])
#' }
#' 
#' @export

plot.cenarios <- function(x, ..., print = TRUE) {

    grupo <- cenario <- indice <- valor <- acum <- tipo <- NULL

    if(!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Visualizacao de cenarios requer o pacote 'ggplot2'")
    }

    nbacias <- length(attr(x, "grupos"))

    x <- copy(x$cenarios)

    highlight <- list(...)
    highlight <- lapply(highlight, function(y) {
        y <- copy(y$cenarios)
        y[, acum := sum(valor), by = c("grupo", "cenario")]
        y[, tipo := factor(acum, labels = paste0("Cen.", seq(unique(cenario)))), by = "grupo"]
        y[, seq_along(unique(cenario)), by = c("grupo")]
        y
    })

    if(length(highlight) == 0) {
        highlight <- cbind(x[is.na(grupo)], tipo = numeric(0))
    } else {
        highlight <- rbindlist(highlight)
    }

    g <- ggplot2::ggplot() +
        ggplot2::geom_line(data = x, ggplot2::aes(indice, valor, group = cenario), color = "grey80", alpha = .4) +
        ggplot2::geom_line(data = highlight, ggplot2::aes(indice, valor, group = cenario, color = tipo)) +
        ggplot2::scale_color_discrete(name = "") +
        ggplot2::scale_x_date(name = "Data", breaks = "1 month", date_labels = "%b/%Y") +
        ggplot2::labs(y = "Valor") +
        ggplot2::facet_wrap(~ grupo, ncol = 1, scales = "free_y") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 50),
            panel.grid.minor = ggplot2::element_line(color = NA))

    if(print) print(g)

    invisible(g)
}

#' Visualizacao De \code{compactcen}
#' 
#' Plot do dado compactado em ate tres dimensoes, possivelmente colorido por clusters estimados
#' 
#' Esta funcao permite a visualizacao da compactacao de cenarios e resultado da clusterizacao. Deve
#' ser notado que sua funcionalidade esta prevista apenas para compactacoes em ate tres dimensoes e
#' retornara um erro caso seja passado \code{x} com quatro ou mais.
#' 
#' A visualizacao de compactacoes em uma ou duas dimensoes exige a dependencia opcional 
#' \code{ggplot2}, enquanto em tres dimensoes exige \code{plotly}. A ausencia da dependencia 
#' relevante retornara um erro.
#' 
#' @param x objeto da classe \code{compactcen} gerado por uma das funcoes de compactacao
#' @param clusters argumento opcional, objeto retornado por uma das funcoes de clusterizacao
#' @param print booleano indicando se o plot deve ser exibido ou retornado invisivelmente
#' @param ... nao possui utilidade, existe apenas para consistencia com a generica
#' 
#' @return O plot das variaveis compactadas. Isto sera um objeto \code{ggplot} caso a compactacao 
#'     tenha sido feita em uma ou duas dimensoes ou um objeto \code{plot_ly} caso em tres.
#' 
#' @examples 
#' 
#' # o tipo de saida depende do numero de dimensoes compactadas
#' \dontrun{
#' plot(acumulacens(cenariosdummy["SIN"])) # plot em 1d
#' plot(acumulacens(cenariosdummy["SIN"], 2)) # plot em 2d
#' plot(acumulacens(cenariosdummy["SIN"], 3)) # plot em 3d
#' }
#' 
#' # adicionando classes de clusterizacao
#' cens_compact <- acumulacens(cenariosdummy["SIN"], 2)
#' clusters     <- clustkmeans(cens_compact, 3)
#' \dontrun{
#' plot(cens_compact, clusters)
#' }
#' 
#' @export

plot.compactcen <- function(x, clusters, print = TRUE, ...) {

    Cluster <- Dim1 <- Dim2 <- NULL

    dat <- copy(x$compact)
    dat <- dcast(dat, cenario ~ ind, value.var = "valor")[, -1]
    colnames(dat) <- paste0("Dim", seq(ncol(dat)))
    dim <- ncol(dat)

    if(dim <= 2 & !requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Visualizacao de compactacoes em uma ou duas dimensoes requer o pacote 'ggplot2'")
    } else if(dim == 3 & !requireNamespace("plotly", quietly = TRUE)) {
        stop("Visualizacao de compactacoes em tres dimensoes requer o pacote 'plotly'")
    }

    if(!missing("clusters")) {
        dat[, Cluster := getclustclass(clusters)]
        dat[, Cluster := factor(Cluster, labels = paste0("Clust.", unique(Cluster)), ordered = TRUE)]
        gp <- ggplot2::geom_point(ggplot2::aes(color = Cluster))
        lycol <- list(color = ~Cluster)
    } else {
        gp <- ggplot2::geom_point()
        lycol <- list()
    }

    if(dim == 1) {
        dat[, Dim2 := rep(0, .N)]
        pp <- ggplot2::ggplot(dat, ggplot2::aes(Dim1, Dim2)) + gp + ggplot2::theme_bw()
    } else if(dim == 2) {
        pp <- ggplot2::ggplot(dat, ggplot2::aes(Dim1, Dim2)) + gp + ggplot2::theme_bw()
    } else if(dim == 3) {
        call <- c(list(quote(plotly::plot_ly), quote(dat), x = ~Dim1, y = ~Dim2, z = ~Dim3,
            type = "scatter3d", mode = "markers"), lycol)
        call <- as.call(call)
        pp <- eval(call)
    } else {
        stop("Visualizacao de 'compactcen' so funciona com ate tres dimensoes")
    }

    if(print) print(pp)

    invisible(pp)
}