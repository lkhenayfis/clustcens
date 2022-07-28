# clustcens 1.1.3

## Misc

* Torna `ggplot2` uma dependencia opcional assim como `plotly`. Assim, toda a parte de visualizacao
  do pacote fica opcional, reduzindo a carga de instalacao

# clustcens 1.1.2

## New features

* Inclui metodo de `predict` para objetos `compactcen`
  * apenas objetos compactados por `PCAcens` sao suportados por enquanto
* Inclui generica `addnewobs` para inclusao de novos dados em objetos de cluster ja estimados
  * apenas clusters realizados via `clustkmeans` ou `clustEM` sao suportados por enquanto

### Minor

* Adiciona exemplos nas documentacoes

## Bug fixes

* Atualiza documentacao de `clust_funs` que estava defasada, sem mencao aos metodos hierarquico e
  kmedoides
* Corrige um erro em `selecporcluster` que ocorria quando clusters tinham apenas um elemento. O 
  calculo do elemento mais proximo dava erro em funcao da simplificacao de matriz para vetores
* `PCAcens` agora mantem o nome e ordem da coluna `cenarios` passada para compactacao

# clustcens 1.0

Este pacote prove funcoes para selecao de cenarios de variaveis genericas, independentemente da
metodologia utilizada para gera-los. Alem dos metodos para selecao de cenarios sao fornecidas
ferramentas para visualizacao da escolha. Fork do projeto `clustena` em b11cad9
