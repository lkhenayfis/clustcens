test_that("Testes de clusterizacao", {

    set.seed(1234)

    cens <- cenariosdummy["SIN"]
    compac <- PCAcens(cens)

    # KMEANS

    clust <- clustkmeans(compac, 5)

    expect_equal(class(clust), "kmeans")
    expect_snapshot_value(getclustclass(clust), style = "deparse")
    expect_snapshot_value(getclustmeans(clust), style = "deparse")

    clust <- clustkmeans(compac, 4)
    clust2 <- addnewobs(clust, compac)
    expect_equal(getclustclass(clust), getclustclass(clust2)[-seq(attr(cens, "ncen"))])

    gg <- plot(compac, clust, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    # EM

    clust <- clustEM(compac, 5, verbose = FALSE)

    expect_equal(class(clust), "Mclust")
    expect_snapshot_value(getclustclass(clust), style = "deparse")
    expect_snapshot_value(getclustmeans(clust), style = "deparse")

    clust <- clustEM(compac, 3)
    clust2 <- addnewobs(clust, compac)
    expect_equal(getclustclass(clust), getclustclass(clust2)[-seq(attr(cens, "ncen"))])

    gg <- plot(compac, clust, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    # HCLUST

    clust <- clusthierarq(compac, 5)

    expect_equal(class(clust), c("hclust_aug", "hclust"))
    expect_snapshot_value(getclustclass(clust), style = "deparse")
    expect_snapshot_value(getclustmeans(clust), style = "deparse")

    gg <- plot(compac, clust, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    # KMEDOIDS

    clust <- clustkmedoids(compac, 5)

    expect_equal(class(clust), c("pam", "partition"))
    expect_snapshot_value(getclustclass(clust), style = "deparse")
    expect_snapshot_value(getclustmeans(clust), style = "deparse")

    gg <- plot(compac, clust, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))
})