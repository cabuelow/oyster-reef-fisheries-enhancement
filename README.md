## Modelling fisheries enhancement from restored oyster reefs

This repository provides code to estimate fisheries enhancement from restored oyster reefs.

### TODO

-   [x] CAB to set up code for model
-   [x] What is 'theoretical age when length is 0'? - This is a scaling parameter to standardise growth curves of different fish species and directly compare them. Can estimate using the von Bertalanffy growth model if you have length at various points in time, growth rate, and the asymptotic (i.e., maximum) length of a species.

`L(t) = L_inf * (1 - exp(-K*(t - t_0)))`

L_inf = asymptotic length; K = growth rate; t_0 = theoretical age at length 0

-   [x] CAB to provide Jas with spreadsheet template for inputting data
