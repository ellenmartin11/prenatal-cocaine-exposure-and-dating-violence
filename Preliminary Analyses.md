# Preliminary Analyses
- These include checking the assumptions of path analysis (linearity). The assumption of normality is not met given that categorical variables are included.
- These also include a correlation heatmap to examine the correlations between all variables included in models.

## Checking Linearity of Endogenous Variables

```r
#scatterplot matrix to visualise linearity
scatterplotMatrix(~scale_DatingViolenceTotal + scale_combinedBRIEF + scale_ctqtotal, data = averages_master_clean,
                  diagonal = FALSE,             # Remove kernel density estimates
                  regLine = list(col = "green", # Linear regression line color
                                 lwd = 3),      # Linear regression line width
                  smooth = list(col.smooth = "red",   # Non-parametric mean color
                                col.spread = "blue")) # Non-parametric variance color

```


![image](https://user-images.githubusercontent.com/68326791/222259044-b5ef8c2e-5016-432b-8752-ffae32f6bd16.png)

## Multivariate Normality

```r
library(knitr)
library(MVN)

# multivariate normality tests
vars <- as.data.frame((master[,c("scale_DatingViolenceTotal","scale_combinedBRIEF","scale_ctqtotal")]))
mvn_res <- mvn(vars, mvnTest = "royston")

# qq plots
qqnorm(master_data_6$DatingViolenceTotal, pch = 1, frame = FALSE)
qqline(master_data_6$DatingViolenceTotal, col = "steelblue", lwd = 2) #DV is approximately normal

qqnorm(master_data_6$adultBRIEFave, pch = 1, frame = FALSE)
qqline(master_data_6$adultBRIEFave, col = "steelblue", lwd = 2) #kind of normal

qqnorm(master_data_6$ctqtotal, pch = 1, frame = FALSE)
qqline(master_data_6$ctqtotal, col = "steelblue", lwd = 2) #mostly normal

```
|Test    |        H| p value|MVN |
|:-------|--------:|-------:|:---|
|Royston | 58.61056|       0|NO  |

|Test             |Variable                  |Statistic |p value |Normality |
|:----------------|:-------------------------|:---------|:-------|:---------|
|Anderson-Darling |scale_DatingViolenceTotal |1.8511    |1e-04   |NO        |
|Anderson-Darling |scale_combinedBRIEF       |1.7805    |1e-04   |NO        |
|Anderson-Darling |scale_ctqtotal            |2.8878    |<0.001  |NO        |

|                          |   n| Mean| Std.Dev|     Median|       Min|      Max|       25th|      75th|      Skew|   Kurtosis|
|:-------------------------|---:|----:|-------:|----------:|---------:|--------:|----------:|---------:|---------:|----------:|
|scale_DatingViolenceTotal | 106|    0|       1| -0.1772346| -1.388118| 2.722733| -0.8833503| 0.6197672| 0.6536847| -0.4777039|
|scale_combinedBRIEF       | 106|    0|       1| -0.2197222| -1.334765| 2.529794| -0.8335153| 0.6889341| 0.5084574| -0.7765632|
|scale_ctqtotal            | 106|    0|       1| -0.2377597| -1.119330| 3.962662| -0.7044732| 0.4363820| 1.4047547|  2.6286437|

### QQ Plot for DatingViolenceTotal
![image](https://user-images.githubusercontent.com/68326791/222259153-642ee477-6f8b-436b-950e-28771e57201b.png)

### QQ Plot for combinedBRIEF
![image](https://user-images.githubusercontent.com/68326791/222259205-e2b3c912-cf8d-485c-9616-dde07ac79bdd.png)

### QQ Plot for CTQtotal
![image](https://user-images.githubusercontent.com/68326791/222259342-70d7504b-9380-414d-b1ff-7e01967aff49.png)


## Correlations 

```r

#saving only variables of interest
master_data_6_cl <- averages_master_dummy[c(3,9,10,11,c(13:24))]
master_data_7 <- master_data_6_cl[c(c(1:11),13,15)]

#renaming variable names
colnames(master_data_7)[1] = "childhood maltreatment"
colnames(master_data_7)[2] = "young adult IPV"
colnames(master_data_7)[3] = "late adolescent EF"
colnames(master_data_7)[5] = "prenatal cocaine exposure"
colnames(master_data_7)[6] = "no prenatal exposure"
colnames(master_data_7)[7] = "exposure to non-cocaine substance"
colnames(master_data_7)[8] = "African American (mother)"
colnames(master_data_7)[9] = "Caucasian (mother)"
colnames(master_data_7)[10] = "Hispanic (mother)"
colnames(master_data_7)[11] = "highshool education (mother)"
colnames(master_data_7)[13] = "early adolescent substance use"

#creates a correlation matrix using the Hmisc package - specifies spearman correlation, but pearson can be specified
master_cor <- Hmisc::rcorr(as.matrix(master_data_7), type = "spearman")
data.frame(master_cor$r) %>% kable() 
data.frame(master_cor$P) %>% kable()

#function that computes all your info
cors <- function(df) { 
  # turn all three matrices (r, n, and P into a data frame)
  M <- Hmisc::rcorr(as.matrix(df), type = "spearman")
  # return the three data frames in a list return(Mdf)
  Mdf <- map(M, ~data.frame(.x))
}

formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>% 
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA)) 
}

#output is a nicely formatted correlation table
formatted_cors(master_data_7) %>% head() %>% kable()

#exporting corrmat (exports P values in a csv and correlation coefficients in another csv)
write.csv(master_cor$P, file = "C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\cleaned data\\corrP.csv")
write.csv(master_cor$r,  file = "C:\\Users\\Ellen Martin\\OneDrive\\Desktop\\Yale Stover Lab\\Thesis\\data\\cleaned data\\corrR.csv")

#VISUALISATION
#using corrplot package - simple heatmap 
#method is color, but can be changed to circle/square - only upper triangle is shown
#FPC clusters it based on which factors seem to co-occur (e.g., Caucasian seems to clsuter alongside higher birthweigt, no prenatal exposure, highschool education)
M <- as.matrix(cor(master_data_7, method = "spearman"))
corrplot(M, method = 'color', order = 'FPC', type = 'upper', diag = FALSE)

#national parks palette! (relies on installing the nat parks palette)
pal <- natparks.pals("Acadia", 20, type = "continuous")

## add significant level stars and an interesting color palette
corrplot(M, p.mat = master_cor$P, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'FPC', col = pal)

```
### Correlation Heatmap

![image](https://user-images.githubusercontent.com/68326791/233662855-47282b91-f8b8-42af-b4b6-4067b41ecc51.png)

### Correlation Coefficients (Spearman)

|                    |   ctqtotal| DatingViolenceTotal| adultBRIEFave| birthweight|    cocaine| nosubstance| noncocainesubstance| AfricanAmerican|  Caucasian|   Hispanic| highschool| belowhighschool|     female|       male|         SU|       noSU|
|:-------------------|----------:|-------------------:|-------------:|-----------:|----------:|-----------:|-------------------:|---------------:|----------:|----------:|----------:|---------------:|----------:|----------:|----------:|----------:|
|ctqtotal            |  1.0000000|           0.0272014|     0.1871421|  -0.0397014|  0.0572121|   0.0116927|          -0.0816486|       0.0288041| -0.0270183| -0.0087384|  0.0608566|      -0.0608566|  0.0666919| -0.0666919|  0.0833162| -0.0833162|
|DatingViolenceTotal |  0.0272014|           1.0000000|     0.3622180|  -0.0188453| -0.0265621|   0.0442341|          -0.0145883|       0.0981247| -0.1826950|  0.1192727|  0.0905082|      -0.0905082| -0.0363872|  0.0363872|  0.1189867| -0.1189867|
|adultBRIEFave       |  0.1871421|           0.3622180|     1.0000000|  -0.0634297|  0.0386045|  -0.0367997|          -0.0078546|       0.0713216| -0.1406591|  0.0996281|  0.0604439|      -0.0604439|  0.2170719| -0.2170719|  0.3030692| -0.3030692|
|birthweight         | -0.0397014|          -0.0188453|    -0.0634297|   1.0000000| -0.3527031|   0.2894544|           0.1211893|      -0.1205812|  0.1512802| -0.0261806|  0.0716764|      -0.0716764| -0.0006167|  0.0006167| -0.0337649|  0.0337649|
|cocaine             |  0.0572121|          -0.0265621|     0.0386045|  -0.3527031|  1.0000000|  -0.6186961|          -0.5571009|       0.3171509| -0.3212287| -0.0571853| -0.3254280|       0.3254280| -0.1133893|  0.1133893|  0.1989428| -0.1989428|
|nosubstance         |  0.0116927|           0.0442341|    -0.0367997|   0.2894544| -0.6186961|   1.0000000|          -0.3077466|      -0.2714837|  0.3217441| -0.0279420|  0.2625730|      -0.2625730| -0.0216523|  0.0216523| -0.1219561|  0.1219561|
|noncocainesubstance | -0.0816486|          -0.0145883|    -0.0078546|   0.1211893| -0.5571009|  -0.3077466|           1.0000000|      -0.0971282|  0.0489393|  0.0987923|  0.1165714|      -0.1165714|  0.1602120| -0.1602120| -0.1120247|  0.1120247|
|AfricanAmerican     |  0.0288041|           0.0981247|     0.0713216|  -0.1205812|  0.3171509|  -0.2714837|          -0.0971282|       1.0000000| -0.8418974| -0.4613798|  0.1335305|      -0.1335305|  0.0482243| -0.0482243| -0.0367698|  0.0367698|
|Caucasian           | -0.0270183|          -0.1826950|    -0.1406591|   0.1512802| -0.3212287|   0.3217441|           0.0489393|      -0.8418974|  1.0000000| -0.0903336| -0.0109410|       0.0109410| -0.0270666|  0.0270666| -0.0219690|  0.0219690|
|Hispanic            | -0.0087384|           0.1192727|     0.0996281|  -0.0261806| -0.0571853|  -0.0279420|           0.0987923|      -0.4613798| -0.0903336|  1.0000000| -0.2284453|       0.2284453| -0.0444994|  0.0444994|  0.1039780| -0.1039780|
|highschool          |  0.0608566|           0.0905082|     0.0604439|   0.0716764| -0.3254280|   0.2625730|           0.1165714|       0.1335305| -0.0109410| -0.2284453|  1.0000000|      -1.0000000| -0.0404226|  0.0404226| -0.2684427|  0.2684427|
|belowhighschool     | -0.0608566|          -0.0905082|    -0.0604439|  -0.0716764|  0.3254280|  -0.2625730|          -0.1165714|      -0.1335305|  0.0109410|  0.2284453| -1.0000000|       1.0000000|  0.0404226| -0.0404226|  0.2684427| -0.2684427|
|female              |  0.0666919|          -0.0363872|     0.2170719|  -0.0006167| -0.1133893|  -0.0216523|           0.1602120|       0.0482243| -0.0270666| -0.0444994| -0.0404226|       0.0404226|  1.0000000| -1.0000000| -0.1229797|  0.1229797|
|male                | -0.0666919|           0.0363872|    -0.2170719|   0.0006167|  0.1133893|   0.0216523|          -0.1602120|      -0.0482243|  0.0270666|  0.0444994|  0.0404226|      -0.0404226| -1.0000000|  1.0000000|  0.1229797| -0.1229797|
|SU                  |  0.0833162|           0.1189867|     0.3030692|  -0.0337649|  0.1989428|  -0.1219561|          -0.1120247|      -0.0367698| -0.0219690|  0.1039780| -0.2684427|       0.2684427| -0.1229797|  0.1229797|  1.0000000| -1.0000000|
|noSU                | -0.0833162|          -0.1189867|    -0.3030692|   0.0337649| -0.1989428|   0.1219561|           0.1120247|       0.0367698|  0.0219690| -0.1039780|  0.2684427|      -0.2684427|  0.1229797| -0.1229797| -1.0000000|  1.0000000|

### P-values

|                    |  ctqtotal| DatingViolenceTotal| adultBRIEFave| birthweight|   cocaine| nosubstance| noncocainesubstance| AfricanAmerican| Caucasian|  Hispanic| highschool| belowhighschool|    female|      male|        SU|      noSU|
|:-------------------|---------:|-------------------:|-------------:|-----------:|---------:|-----------:|-------------------:|---------------:|---------:|---------:|----------:|---------------:|---------:|---------:|---------:|---------:|
|ctqtotal            |        NA|           0.7819442|     0.0547434|   0.6861657| 0.5602110|   0.9053070|           0.4053823|       0.7694452| 0.7833755| 0.9291597|  0.5354537|       0.5354537| 0.4969790| 0.4969790| 0.3958261| 0.3958261|
|DatingViolenceTotal | 0.7819442|                  NA|     0.0001359|   0.8479454| 0.7869458|   0.6525381|           0.8820091|       0.3169747| 0.0608661| 0.2233075|  0.3561710|       0.3561710| 0.7111513| 0.7111513| 0.2244277| 0.2244277|
|adultBRIEFave       | 0.0547434|           0.0001359|            NA|   0.5183074| 0.6943996|   0.7080241|           0.9363086|       0.4675197| 0.1503877| 0.3095806|  0.5382297|       0.5382297| 0.0254097| 0.0254097| 0.0015890| 0.0015890|
|birthweight         | 0.6861657|           0.8479454|     0.5183074|          NA| 0.0002088|   0.0026169|           0.2159041|       0.2182342| 0.1216269| 0.7899349|  0.4653023|       0.4653023| 0.9949941| 0.9949941| 0.7311420| 0.7311420|
|cocaine             | 0.5602110|           0.7869458|     0.6943996|   0.0002088|        NA|   0.0000000|           0.0000000|       0.0009247| 0.0007866| 0.5603954|  0.0006643|       0.0006643| 0.2471459| 0.2471459| 0.0409101| 0.0409101|
|nosubstance         | 0.9053070|           0.6525381|     0.7080241|   0.0026169| 0.0000000|          NA|           0.0013314|       0.0048773| 0.0007705| 0.7761616|  0.0065443|       0.0065443| 0.8256321| 0.8256321| 0.2129914| 0.2129914|
|noncocainesubstance | 0.4053823|           0.8820091|     0.9363086|   0.2159041| 0.0000000|   0.0013314|                  NA|       0.3219377| 0.6183534| 0.3136772|  0.2340436|       0.2340436| 0.1008927| 0.1008927| 0.2529173| 0.2529173|
|AfricanAmerican     | 0.7694452|           0.3169747|     0.4675197|   0.2182342| 0.0009247|   0.0048773|           0.3219377|              NA| 0.0000000| 0.0000006|  0.1723788|       0.1723788| 0.6234985| 0.6234985| 0.7082511| 0.7082511|
|Caucasian           | 0.7833755|           0.0608661|     0.1503877|   0.1216269| 0.0007866|   0.0007705|           0.6183534|       0.0000000|        NA| 0.3571035|  0.9113686|       0.9113686| 0.7829982| 0.7829982| 0.8231228| 0.8231228|
|Hispanic            | 0.9291597|           0.2233075|     0.3095806|   0.7899349| 0.5603954|   0.7761616|           0.3136772|       0.0000006| 0.3571035|        NA|  0.0185059|       0.0185059| 0.6505907| 0.6505907| 0.2888243| 0.2888243|
|highschool          | 0.5354537|           0.3561710|     0.5382297|   0.4653023| 0.0006643|   0.0065443|           0.2340436|       0.1723788| 0.9113686| 0.0185059|         NA|       0.0000000| 0.6807720| 0.6807720| 0.0053979| 0.0053979|
|belowhighschool     | 0.5354537|           0.3561710|     0.5382297|   0.4653023| 0.0006643|   0.0065443|           0.2340436|       0.1723788| 0.9113686| 0.0185059|  0.0000000|              NA| 0.6807720| 0.6807720| 0.0053979| 0.0053979|
|female              | 0.4969790|           0.7111513|     0.0254097|   0.9949941| 0.2471459|   0.8256321|           0.1008927|       0.6234985| 0.7829982| 0.6505907|  0.6807720|       0.6807720|        NA| 0.0000000| 0.2091469| 0.2091469|
|male                | 0.4969790|           0.7111513|     0.0254097|   0.9949941| 0.2471459|   0.8256321|           0.1008927|       0.6234985| 0.7829982| 0.6505907|  0.6807720|       0.6807720| 0.0000000|        NA| 0.2091469| 0.2091469|
|SU                  | 0.3958261|           0.2244277|     0.0015890|   0.7311420| 0.0409101|   0.2129914|           0.2529173|       0.7082511| 0.8231228| 0.2888243|  0.0053979|       0.0053979| 0.2091469| 0.2091469|        NA| 0.0000000|
|noSU                | 0.3958261|           0.2244277|     0.0015890|   0.7311420| 0.0409101|   0.2129914|           0.2529173|       0.7082511| 0.8231228| 0.2888243|  0.0053979|       0.0053979| 0.2091469| 0.2091469| 0.0000000|        NA|

