# master

## New features

### Minor

* Adiciona exemplos nas documentacoes

## Bug fixes

* Atualiza documentacao de `clust_funs` que estava defasada, sem mencao aos metodos hierarquico e
  kmedoides
* Corrige um erro em `selecporcluster` que ocorria quando clusters tinham apenas um elemento. O 
  calculo do elemento mais proximo dava erro em funcao da simplificacao de matriz para vetores

# clustcens 1.0

Este pacote prove funcoes para selecao de cenarios de variaveis genericas, independentemente da
metodologia utilizada para gera-los. Alem dos metodos para selecao de cenarios sao fornecidas
ferramentas para visualizacao da escolha. Fork do projeto `clustena` em b11cad9
