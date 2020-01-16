# SF12toSF6D

This package provides calculators to estimate the group-level distribution of SF-6D profiles, mean SF-6D utility values, and corresponding uncertainty intervals from group-level summary statistics (mean and standard deviation) of SF-12 Physical Component Summary (PCS) and Mental Component Summary (MCS) scores in osteoarthritis patients.

The package can be installed via github:

```
devtools::install_github("uo-cmor/SF12toSF6D")
```

## Examples

The function `SF12_to_SF6D` provides estimates of sample mean SF-6D utility values from mean and standard deviation of SF-12 PCS and MCS scores:

```r
SF12_to_SF6D(means = c(PCS = 44, MCS = 53), SDs = c(PCS = 10, MCS = 9))
#>   utility 
#> 0.7467391
```

Uncertainty intervals around these estimates can be generated, as a function of the sample size from which PCS and MCS were obtained, using `SF6D_uncertainty`:

```r
SF6D_uncertainty(N = 73)
#>    utility 
#> 0.01302616
```

The dimension-level distributions of SF-6D outcomes can also be predicted:

```r
SF12_to_PF(means = c(PCS = 44, MCS = 53), SDs = c(PCS = 10, MCS = 9))
#> [1] 0.54015105 0.37864838 0.08120056
```

For further description and help on each of these functions, see the included package help files (`?SF12toSF6D`).
