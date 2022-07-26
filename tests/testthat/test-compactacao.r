test_that("Teste de compactacao: PCA", {

    compac <- PCAcens(cenariosdummy["SIN"], vartot = 1)
    expect_equal(colnames(compac$compact), c("grupo", "cenario", "ind", "valor"))
    expect_equal(class(compac), c("PCAcens", "compactcen"))
    expect_true(all(c("importance", "SIGMA", "escala") %in% names(attributes(compac))))
    expect_true(all(dim(attr(compac, "SIGMA")) == c(16, 16)))
    expect_true(length(attr(compac, "escala")[[1]]) == 16)
    expect_true(length(attr(compac, "escala")[[2]]) == 16)

    compac <- PCAcens(cenariosdummy[c("SIN", "SUL")], vartot = 1)
    expect_equal(colnames(compac$compact), c("grupo", "cenario", "ind", "valor"))
    expect_equal(class(compac), c("PCAcens", "compactcen"))
    expect_true(all(c("importance", "SIGMA", "escala") %in% names(attributes(compac))))
    expect_true(all(dim(attr(compac, "SIGMA")) == c(32, 32)))
    expect_true(length(attr(compac, "escala")[[1]]) == 32)
    expect_true(length(attr(compac, "escala")[[2]]) == 32)

    # Testando manutencao da ordem dos cenarios
    cenarios2 <- cenariosdummy[c("SIN", "SE")]
    cenarios2$cenarios[, cenario := rep(rep(outer(letters, letters, paste0)[131:32], each = 16), 2)]
    compac <- PCAcens(cenarios2)

    expect_equal(unique(cenarios2$cenarios$cenario), unique(compac$compact$cenario))

    # Testes de plots
    compac <- PCAcens(cenariosdummy["SIN"])

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    compac <- PCAcens(cenariosdummy["SIN"], .75)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))
})

test_that("Teste de compactacao: ACUMULADO", {

    testaacumulacens <- function(compac) {
        expect_equal(colnames(compac$compact), c("grupo", "cenario", "ind", "valor"))
        expect_equal(class(compac), c("acumulacens", "compactcen"))

        expect_error(plot(compac, print = FALSE))
    }

    compac <- acumulacens(cenariosdummy["SIN"], quebras = 5L)
    testaacumulacens(compac)

    compac <- acumulacens(cenariosdummy["SIN"], 3L)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    compac <- acumulacens(cenariosdummy["SIN"], 2L)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))
})