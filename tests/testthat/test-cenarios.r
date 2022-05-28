test_that("Testes de leitura e classe cenarios", {

    # Testes da classe cenarios

    testacenarios <- function(cens) {
        expect_equal(class(cens), "cenarios")
        expect_equal(colnames(cens$cenarios), c("grupo", "cenario", "indice", "valor"))
        expect_equal(attr(cens, "grupos"), c("G1", "G2", "G3"))
        expect_equal(attr(cens, "indices"), seq(5))
        expect_equal(attr(cens, "ncen"), 6)
    }

    # usando dado qualquer
    dat <- data.table(
        grupo = rep(paste0("G", seq(3)), each = 30),
        cenario = rep(rep(seq(6), each = 5), 3),
        indice = rep(seq(5), 18),
        valor = rnorm(90)
    )

    cens <- as.cenarios(dat)
    testacenarios(cens)

    cens <- as.cenarios(dat[, .SD, .SDcols = c(2, 4, 1, 3)])
    testacenarios(cens)

    dat2 <- copy(dat)
    colnames(dat2) <- paste0("V", 1:4)
    expect_warning(as.cenarios(dat2))

    dat3 <- cbind(dat, erro = NA)
    expect_error(as.cenarios(dat3))

    dat3 <- cbind(dat, erro = NA)
    expect_error(as.cenarios(dat3))

    # Testes de subset

    cc <- cens[c("G1", "G2"), 2:4, 4:5]

    expect_equal(attr(cc, "grupos"), c("G1", "G2"))
    expect_equal(attr(cc, "indices"), 4:5)
    expect_equal(attr(cc, "ncen"), 3)

    #gg <- plot(cc, print = FALSE)
    #expect_equal(class(gg), c("gg", "ggplot"))

    #gg <- plot(cc, cc[c(55, 60, 65)], cc["G2", c(50, 55, 60)], print = FALSE)
    #expect_equal(class(gg), c("gg", "ggplot"))
})