test_that("Testes das funcoes de compactacao", {

    # PCA

    testaPCAcens <- function(compac) {
        expect_equal(colnames(compac$compact), c("grupo", "cenario", "ind", "valor"))
        expect_equal(class(compac), "compactcen")
        expect_equal(attr(compac, "metodo"), "PCAcens")
        expect_true(attr(compac, "teminv"))

        reverse <- unname(attr(compac, "invfunc")(compac$compact[cenario == 1, valor]))[1, ]
        expect_snapshot_value(reverse, style = "deparse")

        expect_error(plot(compac, print = FALSE))
    }

    compac <- PCAcens(cenariosdummy["SIN"], vartot = 1)
    testaPCAcens(compac)

    compac <- PCAcens(cenariosdummy[c("SIN", "SUL")], vartot = 1)
    testaPCAcens(compac)

    compac <- PCAcens(cenariosdummy["SIN"])

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    compac <- PCAcens(cenariosdummy["SIN"], .75)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))

    # ACUMULADA

    testaacumulacens <- function(compac) {
        expect_equal(colnames(compac$compact), c("grupo", "cenario", "ind", "valor"))
        expect_equal(class(compac), "compactcen")
        expect_equal(attr(compac, "metodo"), "acumulacens")
        expect_true(!attr(compac, "teminv"))

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