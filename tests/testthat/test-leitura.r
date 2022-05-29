test_that("Testes de leitura e classe cenariosena", {

    # Testes da classe cenariosena

    testacenariosena <- function(cens) {
        expect_equal(class(cens), "cenariosena")

        expect_equal(class(cens[[1]]), c("data.table", "data.frame"))
        expect_equal(colnames(cens[[1]]), c("anoref", "bacia", "cenario", "data", "ena"))

        expect_equal(attr(cens, "bacias"), c("N", "NE", "SE", "SIN", "SUL"))
        expect_equal(attr(cens, "datas"), seq(as.Date("2022-09-01"), as.Date("2023-12-01"), "month"))
        expect_equal(attr(cens, "anos"), paste0("A", seq(4)))
        expect_equal(attr(cens, "ncen"), 100)
    }

    # usando arquivo interno
    arq  <- system.file("extdata/cenarios.csv", package = "clustena")
    cens <- learqcenarios(arq)
    testacenariosena(cens)

    cens2 <- as.cenariosena(cens$cenarios)
    testacenariosena(cens2)

    expect_true(identical(cens, cens2))

    # Testes de subset

    cc <- cens[c("A1", "A4"), c("SUL", "NE"), 40:70, c("2022-11-01", "2023-03-01", "2023-07-01")]

    expect_equal(attr(cc, "bacias"), c("NE", "SUL"))
    expect_equal(attr(cc, "datas"), as.Date(c("2022-11-01", "2023-03-01", "2023-07-01")))
    expect_equal(attr(cc, "anos"), c("A1", "A4"))
    expect_equal(attr(cc, "ncen"), 31)

    gg <- plot(cc, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))

    gg <- plot(cc, cc["A1", "SUL", c(55, 60, 65)], cc["A4", "NE", c(50, 55, 60)], print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))
})