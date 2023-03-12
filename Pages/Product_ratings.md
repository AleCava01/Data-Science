Product ratings
================

Il dataset Product ratings contiene dati su una serie di prodotti nel
catalogo di un noto marketplance online. Per ogni mese e ogni prodotto è
elencato il totale delle review che hanno ottenuto da 5 stelle a 1
stella e tutte le volte che il prodotto è stato acquistato ma non
valutato.

Lettura e visualizzazione del dataset

``` r
product_data = read.csv("../Datasets/Product_ratings.txt", sep="\t")
head(product_data)
```

    ##   Month   Products Stars_5 Stars_4 Stars_3 Stars_2 Stars_1 not_rated
    ## 1   Jan Product 12     255    5733    3878     351     504      1489
    ## 2   Feb Product 12     277    6613    3816     352     380      1617
    ## 3   Mar Product 12     272    7278    4518     523     475      1949
    ## 4   Apr Product 12     181    5311    3550     383     330      1853
    ## 5   May Product 12     301    5885    3231     396     421      2050
    ## 6   Jun Product 12     259    5436    3115     456     301      2273

Scatterplot con ggally per vedere se ci sono correlazioni ed è quindi
possibile procedere alla riduzione dimensionale

![](Product_ratings_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Sussiste una correlazione tra le variabili =\> ha senso procedere con
l’analisi delle componenti principali

------------------------------------------------------------------------

## PCA Steps

1.  Normalizzazione dei dati
2.  Matrice di covarianza (correlazione per dati standardizzati)
3.  Calcolare le componenti principali (loadings)
4.  Selezione componenti principali
5.  Proiezione dei dati nel nuovo spazio e analisi

------------------------------------------------------------------------

### Step 1

Estraggo solo le variabili numeriche dal dataset e normalizzo i dati

``` r
product_data_num = product_data[,-(1:2)]
head(product_data_num)
```

    ##   Stars_5 Stars_4 Stars_3 Stars_2 Stars_1 not_rated
    ## 1     255    5733    3878     351     504      1489
    ## 2     277    6613    3816     352     380      1617
    ## 3     272    7278    4518     523     475      1949
    ## 4     181    5311    3550     383     330      1853
    ## 5     301    5885    3231     396     421      2050
    ## 6     259    5436    3115     456     301      2273

``` r
product_data_scaled = scale(product_data_num)
head(product_data_scaled)
```

    ##          Stars_5     Stars_4     Stars_3     Stars_2    Stars_1 not_rated
    ## [1,]  0.20868970  0.01397475  0.09114066 -0.29293603  0.5201322 0.1247660
    ## [2,]  0.29424616  0.16848570  0.07093516 -0.29033860  0.1037466 0.2203068
    ## [3,]  0.27480151  0.28524681  0.29971358  0.15382199  0.4227517 0.4681159
    ## [4,] -0.07909111 -0.06012027 -0.01575296 -0.20981826 -0.0641508 0.3964602
    ## [5,]  0.38758048  0.04066301 -0.11971352 -0.17605167  0.2414225 0.5435035
    ## [6,]  0.22424542 -0.03817269 -0.15751736 -0.02020584 -0.1615313 0.7099536

### Step 2

Calcolo e visualizzazione della matrice di covarianza dei dati
standardizzati = matrice di correlazione dei dati numerici

![](Product_ratings_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Si nota una forte correlazione tra tutte le variabili (come si notava
già prima con lo scatterplot)

### Step 3 - Calcolo componenti principali

``` r
principal_components <- princomp(corr_matrix)
summary(principal_components)
```

    ## Importance of components:
    ##                           Comp.1    Comp.2     Comp.3      Comp.4       Comp.5
    ## Standard deviation     0.1622537 0.0651416 0.03006493 0.013547263 0.0047106001
    ## Proportion of Variance 0.8310238 0.1339496 0.02853284 0.005793322 0.0007004498
    ## Cumulative Proportion  0.8310238 0.9649734 0.99350623 0.999299550 1.0000000000
    ##                              Comp.6
    ## Standard deviation     3.761893e-10
    ## Proportion of Variance 4.467221e-18
    ## Cumulative Proportion  1.000000e+00

Si osserva che oltre il 96% della varianza è spiegata dalle prime due
componenti.

Ottendo le componenti principali (versori/loadings):

``` r
principal_components$loadings
```

    ## 
    ## Loadings:
    ##           Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6
    ## Stars_5    0.602  0.199  0.338  0.284  0.241  0.587
    ## Stars_4    0.292  0.422  0.300  0.180 -0.652 -0.435
    ## Stars_3   -0.101  0.515         0.163  0.684 -0.474
    ## Stars_2   -0.443  0.339  0.569 -0.527         0.290
    ## Stars_1   -0.545  0.302 -0.206  0.641 -0.197  0.346
    ## not_rated  0.221  0.558 -0.651 -0.414         0.194
    ## 
    ##                Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6
    ## SS loadings     1.000  1.000  1.000  1.000  1.000  1.000
    ## Proportion Var  0.167  0.167  0.167  0.167  0.167  0.167
    ## Cumulative Var  0.167  0.333  0.500  0.667  0.833  1.000

### Step 4 - Selezione delle componenti principali

Utilizzo il criterio del gomito

``` r
plot(cumsum(principal_components$sd^2)/sum(principal_components$sd^2), type="b", axes =F, xlab="numero di componenti", ylab="contributo alla varianza totale", ylim=c(0,1))
abline(h=1, col="blue")
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(product_data_num),labels = 1:ncol(product_data_num),las=2)
```

![](Product_ratings_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Analizzo anche gli scores attraverso un boxplot

``` r
boxplot(principal_components$scores, col="gold")
```

![](Product_ratings_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

=\> Scelgo le prime due componenti

``` r
par(mfrow = c(2,1))
for(i in 1:2) barplot(principal_components$loadings[,i], ylim = c(-1,1))
```

![](Product_ratings_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
