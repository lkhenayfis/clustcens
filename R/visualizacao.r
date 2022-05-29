####################################### VISUALIZACAO DE DADOS ######################################

#' Visualizacao De \code{cenariosena}
#' 
#' Plot em grade de objetos \code{cenariosena}
#' 
#' O argumento \code{...} permite que sejam passados mais outros objetos \code{cenariosena} contendo
#' um numero menor de cenarios selecionados para plot por cima do completo. A intencao de uso deste
#' argumento e passar \code{cenariosena} para cada ano dos quais se tiram cenarios, fazendo a 
#' selecao do mesmo numero de cenarios a cada ano de referencia. Assim, se existem tres anos de 
#' referencia, \code{...} corresponde a tres objetos \code{cenariosena} contendo cada um um unico
#' ano de referencia e o mesmo numero N de cenarios selecionados.
#' 
#' @param x objeto da classe \code{cenariosena}
#' @param ... objetos \code{cenariosena} opcionais com cenarios a serem plotados por cima
#' @param print booleano indicando se o plot deve ser exibido ou retornado invisivelmente
#' 
#' @importFrom ggplot2 ggplot aes geom_line facet_grid theme theme_bw scale_x_date
#' @importFrom ggplot2 scale_color_discrete element_line element_text labs
#' 
#' @export

plot.cenariosena <- function(x, ..., print = TRUE) {

    bacia <- cenario <- data <- ena <- acum <- tipo <- NULL

    nbacias <- length(attr(x, "bacias"))
    nanos   <- length(attr(x, "anos"))
    #if(nbacias <= nanos) form <- anoref ~ bacia else form <- bacia ~ anoref
    form <- bacia ~ anoref

    x <- copy(x$cenarios)

    highlight <- list(...)
    highlight <- lapply(list(...), function(y) {
        y <- copy(y$cenarios)
        y[, acum := sum(ena), by = c("anoref", "bacia", "cenario")]
        y[, tipo := factor(acum, labels = paste0("Cen.", seq(unique(cenario)))), by = c("anoref", "bacia")]
        y[, seq_along(unique(cenario)), by = c("anoref", "bacia")]
        y
    })

    if(length(highlight) == 0) {
        highlight <- cbind(x[is.na(bacia)], tipo = numeric(0))
    } else {
        highlight <- rbindlist(highlight)
    }

    g <- ggplot() +
        geom_line(data = x, aes(data, ena, group = cenario), color = "grey80", alpha = .4) +
        geom_line(data = highlight, aes(data, ena, group = cenario, color = tipo)) +
        scale_color_discrete(name = "") +
        scale_x_date(name = "Data", breaks = "1 month", date_labels = "%b/%Y") +
        labs(y = "ENA") +
        facet_grid(form, scales = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = 1, angle = 50),
            panel.grid.minor = element_line(color = NA))

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
#' @param x objeto da classe \code{compactcen} gerado por uma das funcoes de compactacao
#' @param clusters argumento opcional, objeto retornado por uma das funcoes de clusterizacao
#' @param print booleano indicando se o plot deve ser exibido ou retornado invisivelmente
#' @param ... nao possui utilidade, existe apenas para consistencia com a generica
#' 
#' @return O plot das variaveis compactadas. Isto sera um objeto \code{ggplot} caso a compactacao 
#'     tenha sido feita em uma ou duas dimensoes ou um objeto \code{plot_ly} caso em tres.
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_color_discrete theme_bw
#' 
#' @export

plot.compactcen <- function(x, clusters, print = TRUE, ...) {

    Cluster <- Dim1 <- Dim2 <- NULL

    dat <- copy(x$compact)
    dat <- dcast(dat, cenario ~ ind, value.var = "ena")[, -1]
    colnames(dat) <- paste0("Dim", seq(ncol(dat)))
    dim <- ncol(dat)

    if(dim == 3 & !requireNamespace("plotly", quietly = TRUE)) {
        stop("Visualizacao de compactacoes em tres dimensoes requer o pacote 'plotly'")
    }

    if(!missing("clusters")) {
        dat[, Cluster := getclustclass(clusters)]
        dat[, Cluster := factor(Cluster, labels = paste0("Clust.", unique(Cluster)), ordered = TRUE)]
        gp <- geom_point(aes(color = Cluster))
        lycol <- list(color = ~Cluster)
    } else {
        gp <- geom_point()
        lycol <- list()
    }

    if(dim == 1) {
        dat[, Dim2 := rep(0, .N)]
        pp <- ggplot(dat, aes(Dim1, Dim2)) + gp + theme_bw()
    } else if(dim == 2) {
        pp <- ggplot(dat, aes(Dim1, Dim2)) + gp + theme_bw()
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