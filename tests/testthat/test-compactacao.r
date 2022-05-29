test_that("Testes das funcoes de compactacao", {

    # PCA

    testaPCAena <- function(compac) {
        expect_equal(colnames(compac$compact), c("anoref", "bacia", "cenario", "ind", "ena"))
        expect_equal(class(compac), "compactcen")
        expect_equal(attr(compac, "metodo"), "PCAena")
        expect_true(attr(compac, "teminv"))

        reverse <- unname(attr(compac, "invfunc")(compac$compact[cenario == 1, ena]))[1, ]
        expect_snapshot_value(reverse, style = "deparse")

        expect_error(plot(compac, print = FALSE))
    }

    compac <- PCAena(cenariosdummy["A1", "SIN"], vartot = 1)
    testaPCAena(compac)

    compac <- PCAena(cenariosdummy["A1", c("SIN", "SUL")], vartot = 1)
    testaPCAena(compac)

    compac <- PCAena(cenariosdummy["A1", "SIN"])

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    compac <- PCAena(cenariosdummy["A1", "SIN"], .75)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))

    # ACUMULADA

    testaacumulaena <- function(compac) {
        expect_equal(colnames(compac$compact), c("anoref", "bacia", "cenario", "ind", "ena"))
        expect_equal(class(compac), "compactcen")
        expect_equal(attr(compac, "metodo"), "acumulaena")
        expect_true(!attr(compac, "teminv"))

        expect_error(plot(compac, print = FALSE))
    }

    compac <- acumulaena(cenariosdummy["A1", "SIN"], quebras = 5L)
    testaacumulaena(compac)

    compac <- acumulaena(cenariosdummy["A1", "SIN"], 3L)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    compac <- acumulaena(cenariosdummy["A1", "SIN"], 2L)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))
})