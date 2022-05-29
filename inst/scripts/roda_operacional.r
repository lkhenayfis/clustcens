library(clustena)

main <- function() {

    ARGS <- commandArgs(trailingOnly = TRUE)

    if(!requireNamespace("jsonlite", quietly = TRUE)) stop("Execucao do script necessita pacote 'jsonlite'")

    arq_conf <- ARGS[grep("jsonc?$", ARGS)]
    if(length(arq_conf) == 0) {
        arq_conf <- system.file("conf/roda_oper_default.jsonc", package = "clustena")
    } else {
        ARGS <- ARGS[-grep("jsonc?$", ARGS)]
    }

    CONF <- jsonlite::read_json(arq_conf, TRUE)

    # DADO DE CENARIOS -----------------------------------------------------------------------------

    # a essa altura so deveria ter sobrado o arquivo de cenarios mesmo nos argumentos de linha de
    # comando
    arq_cen <- CONF$arq_cenarios
    if(arq_cen == "") arq_cen <- ARGS[1]
    if(is.na(arq_cen)) arq_cen <- system.file("extdata/cenarios.csv", package = "clustena")

    cat("Arquivo de configuracao: ", arq_conf, "\n")
    cat("Arquivo de cenarios:     ", arq_cen, "\n")

    cens <- learqcenarios(arq_cen)

    attach(CONF)
    if(length(anos) != 0)   cens <- cens[anos]
    if(length(bacias) != 0) cens <- cens[, bacias]
    if(length(corte_datas) != 0) cens <- cens[, , , -sapply(corte_datas, grep, attr(cenarios, "datas"))]
    detach(CONF)

    CONF$seleciona <- lapply(CONF$seleciona, function(l) {
        whichfun <- names(l)[grep("fun$", names(l))]
        l[whichfun] <- lapply(whichfun, function(n) as.name(l[[n]]))
        as.call(l)
    })

    selec_ano <- lapply(CONF$seleciona, function(cc) {
        l_selec  <- lapply(attr(cens, "anos"), function(ano) {
            cc$cenarios <- cens[ano, CONF$bacia_ref]
            eval(cc)
        })
        names(l_selec) <- as.character(attr(cens, "anos"))
        lapply(l_selec, "[[", 1)
    })

    l_plots <- lapply(names(selec_ano), function(modo) {
        anos <- attr(cens, "anos")
        selecs <- selec_ano[[modo]]
        highlights <- mapply(anos, selecs, FUN = function(ano, selec) cens[ano, , selec], SIMPLIFY = FALSE)
        cc <- as.call(c(list(plot, cens, print = FALSE), highlights))
        gg <- eval(cc)
        gg <- gg + ggplot2::labs(title = modo)
        gg
    })
    names(l_plots) <- names(selec_ano)

    outdir <- CONF$outdir
    if(outdir == "") outdir <- getwd()

    for(gg in names(l_plots)) {
        ggplot2::ggsave(file.path(outdir, paste0(gg, ".pdf")), l_plots[[gg]], width = 16, height = 12)
    }
}

main()