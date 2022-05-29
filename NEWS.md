# master

## New features

* Inclui diretorios `conf` e `scripts` em `inst`, contendo script para rodada operacional da selecao
  de cenarios e arquivo de configuracao default do mesmo, respectivamente
* `jsonlite` foi adicionado aos Suggests para realizar a leitura dos arquivos de configuracao
* Introduz funcao `as.cenariosena` para conversao de `data.table`s ja lidos em `cenariosena`
* Funcoes de compactacao agora tratam todas as bacias inclusas no dado de forma multivariada:
  * `acumulaena` faz a soma acumulada de cada bacia em cada cenario, retornando uma dimensao 
    compactada igual a `Nbacias * Nquebras`
  * `PCAena` concatena os trajetos de cada bacia em uma observacao `tamanho_cen * Nbacias` e usa
    isso como dado para compactar
* `extracdims` agora considera a possibilidade de multiplas bacias no dado. A compactacao por PCA
  naturalmente ja devolvia os cenarios no espaco reduzido considerando todas as bacias originais,
  mas a acumulacao ainda devolvia um espaco compactado por bacia. Isto causava um erro
* Introduz a funcao `clusthierarq` para clusterizacao na selecao/extracao de cenarios por metodos
  aglomerativos
* Introduz a funcao `clustkmedoids` para clusterizacao na selecao/extracao de cenarios por 
  k-medoides

## Bug fixes

### Minor

* Corrige documentacao dos metodos `getclustclass` e `getclustmeans` para `Mclust` e `kmeans`

# clustena 1.0

Este pacote prove funcoes para selecao de cenarios de Energia Natural Afluente, independentemente da
metodologia utilizada para gera-los. Alem dos metodos para selecao de cenarios sao fornecidas
ferramentas para visualizacao da escolha.
