# Preliminary Analyses
- These include checking the assumptions of path analysis (linearity). The assumption of normality is not met given that categorical variables are included.
- These also include a correlation heatmap to examine the correlations between all variables included in models.

## Checking Linearity of Endogenous Variables

```r
#scatterplot matrix to visualise linearity
scatterplotMatrix(~DatingViolenceTotal + adultBRIEFave + BRIEFave, data = averages_master_comp,
                  diagonal = FALSE,             # Remove kernel density estimates
                  regLine = list(col = "green", # Linear regression line color
                                 lwd = 3),      # Linear regression line width
                  smooth = list(col.smooth = "red",   # Non-parametric mean color
                                col.spread = "blue"))

```


![image](https://user-images.githubusercontent.com/68326791/213329868-e05e7d77-0478-49bb-a474-e46c96f9ae96.png)


## Correlations 

```r
## reading in cleaned data 
averages_master_comp <- read.csv("C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\cleaned data\\master_data_5.csv")

## scaling ctqtotal and birthweight
averages_master_comp$birthweight <- scale(averages_master_comp$birthweight)
averages_master_comp$ctqtotal <- scale(averages_master_comp$ctqtotal)

### correlation matrix
## saving only variables of interest
averages_master_comp <- averages_master_comp[c(3,4,5,16,23,24,25,26,27,28,31,33)]

## library for making correlation heatmaps and correlation matrices
library(Hmisc)
library(knitr) 
library(tidyverse, warn.conflict=F)

## matrix
master_cor <- Hmisc::rcorr(as.matrix(averages_master_comp))
data.frame(master_cor$r) %>% head() %>% kable() # r values
data.frame(master_cor$P) %>% head() %>% kable() # p values

## function to turn all three matrices into a df
cors <- function(df) { 
  # turn all three matrices (r, n, and P into a data frame)
  M <- Hmisc::rcorr(as.matrix(df))
  # return the three data frames in a list return(Mdf)
  Mdf <- map(M, ~data.frame(.x))
}

## function to turn the df into a table of correlation values and pvalues
formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>% 
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA)) 
}

## correlation heatmap
formatted_cors(averages_master_comp) %>% 
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations", subtitle="Only significant Pearson's correlation coefficients shown") + scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))


```

![image](https://user-images.githubusercontent.com/68326791/213329626-181594d5-8938-4cda-b4d9-cbc75ffcad00.png)

- BRIEFave (early adolescent EF) and adultBRIEF (late adolescent EF) are correlated
- BRIEFave correlated with female gender, DV, ctq
- Caucasian negatively correlated with cocaine exposure and positive correlated with no substance exposure
- African American correlated with cocaine exposure and negatively correlated with no substance exposure
- cocaine is correlated with lower birthweight
- Hispanic correlated with DV
