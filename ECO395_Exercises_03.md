# ECO 395M: Exercises 3

## What causes what?

**1. Why can’t I just get data from a few different cities and run the
regression of “Crime” on “Police” to understand how more cops in the
streets affect crime? (“Crime” refers to some measure of crime rate and
“Police” measures the number of cops in a city.)**

Higher crime cities are already incentivized to hire more police. And
when crime is increased, the response is to hire more police. To
actually find causality of extra police on crime rate, you need to find
examples of where extra police get hired for reasons uncorrelated with
crime. Examples of this include using the terrorism alert system as
mentioned in the podcast.

**2. How were the researchers from UPenn able to isolate this effect?
Briefly describe their approach and discuss their result in the “Table
2” below, from the researchers’ paper.**

The researchers were able to isolate this effect by implementing a dummy
variable for ‘high alert’ that signifies the terror alert in Washington
D.C. is turned to orange. When that happens, extra police is put on the
street for terrorism related reasons. This is important because in this
case, the extra level of police is uncorrelated with crime, so you can
make a better test of the causal effect of police on crime.

**3. Why did they have to control for Metro ridership? What was that
trying to capture?**

They controlled for Metro ridership because an orange terror alert level
might actually reduce the amount of potential crime victims in
Washington D.C. during high alert times. So they accounted for that in
the second model to see if would change their results.

**4. Below I am showing you “Table 4” from the researchers’ paper. Just
focus on the first column of the table. Can you describe the model being
estimated here? What is the conclusion?**

The model measures the reduction in crime in Washington D.C. during high
alert days. The model also uses a dummy for crime incidents in District
1 where there are a lot of important political and historic buildings,
as well as a dummy for the rest of the city. It also controls for Metro
ridership. The model concludes that on high alert days, the reduction in
crime in the first district of Washington D.C. is much higher than it is
for the rest of the districts.

## Tree modeling: dengue cases

``` r
dengue = read.csv("dengue.csv")

dengue = drop_na(dengue)
dengue$city = factor(dengue$city)
dengue$season = factor(dengue$season)

dengue_split = initial_split(dengue, prop = 0.8)
dengue_train = training(dengue_split)
dengue_test = testing(dengue_split)
```

### CART

``` r
dengue.tree = rpart(total_cases ~ season + city + specific_humidity + precipitation_amt + tdtr_k + precip_amt_kg_per_m2  + dew_point_temp_k + relative_humidity_percent, data=dengue_train, control = rpart.control(cp = 0.002, minsplit=30))

prune_1se = function(my_tree) {
  out = as.data.frame(my_tree$cptable)
  thresh = min(out$xerror + out$xstd)
  cp_opt = max(out$CP[out$xerror <= thresh])
  prune(my_tree, cp=cp_opt)
}

dengue.tree_prune = prune_1se(dengue.tree)
rpart.plot(dengue.tree, digits=-5, type=4, extra=1)
```

![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
rpart.plot(dengue.tree_prune, digits=-5, type=4, extra=1)
```

![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-2-2.png)

### Random forest

``` r
dengue.forest = randomForest(total_cases ~ season + city + specific_humidity + precipitation_amt + tdtr_k + precip_amt_kg_per_m2  + dew_point_temp_k + relative_humidity_percent, data=dengue_train, importance = TRUE)
```

### Gradient boost

``` r
dengue_boost = gbm(total_cases ~ season + city + specific_humidity + precipitation_amt + tdtr_k + precip_amt_kg_per_m2  + dew_point_temp_k + relative_humidity_percent, distribution="gaussian", data=dengue_train, interaction.depth=4, n.trees=500, shrinkage=.05)
```

### Results

| Model            |     RMSE |
|:-----------------|---------:|
| Tree             | 29.68468 |
| Pruned Tree      | 28.59820 |
| Random Forest    | 27.55274 |
| Gradient Boosted | 28.70655 |

The best performing model was the random forest model. Below are partial
dependence plots for `specific_humidity`,
`precipitation_amt`,`dew_point_temp_k`.

![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-6-1.png)![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-6-2.png)![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-6-3.png)

## Predictive model building: green certification

I started off by creating the `revenue` variable by multiplying `Rent`
with `leasing_rate`. Then I split my data into the train-test sets.

### Linear Models

``` r
lm_green = lm(revenue ~ . - CS_PropertyID - Rent - leasing_rate - LEED - Energystar, data=green_train)
```

I originally regressed revenue on the entire dataset taking out the
`Rent`, `leasing rate`, and `CS_PropretyID`. I opted to use the combined
`green_rating` variable, so I took out the `LEED` and `Energystar` as
well.

``` r
lm_green_modified = lm(revenue ~ . - CS_PropertyID - Rent - leasing_rate - LEED - Energystar - Gas_Costs:cd_total_07 + Electricity_Costs:hd_total07 + Gas_Costs:class_a + Gas_Costs:class_b + Gas_Costs:City_Market_Rent + Gas_Costs:stories + Gas_Costs:amenities + Electricity_Costs:class_a + Electricity_Costs:class_b + Electricity_Costs:age + Electricity_Costs:age  + Electricity_Costs:City_Market_Rent + Electricity_Costs:stories + Electricity_Costs:amenities, data=green_train)
```

However, I wanted to improve the model by feature engineering by way of
interactions. I figured that electricity and gas costs would vary widely
between building types, which would affect rent and space combinations
that people would want to lease at.The idea is that some buildings would
vary in efficiency for energy usage based on building class types,
amount of amenities offered, age, etc. I incorporated these interactions
into the linear model and tested to see if it would linear model would
improved.

### CART

``` r
green.tree = rpart(revenue ~ . - Rent - leasing_rate - LEED - Energystar, data=green_train, control = rpart.control(cp = 0.002, minsplit=30))

prune_1se = function(my_tree) {
  out = as.data.frame(my_tree$cptable)
  thresh = min(out$xerror + out$xstd)
  cp_opt = max(out$CP[out$xerror <= thresh])
  prune(my_tree, cp=cp_opt)
}

green.tree_prune = prune_1se(green.tree)
```

![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-11-1.png)![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-11-2.png)

### Random forest

``` r
green.forest = randomForest(revenue ~ . - CS_PropertyID - Rent - leasing_rate - LEED - Energystar, data = green_train, importance = TRUE, na.action = na.omit)
```

### Gradient boost

``` r
green_boost = gbm(revenue ~ . - CS_PropertyID - Rent - leasing_rate - LEED - Energystar, distribution="gaussian", data = green_train, interaction.depth=4, n.trees=500, shrinkage=.05)
```

### Results

| Model                 |      RMSE |
|:----------------------|----------:|
| Linear Model          | 1075.1764 |
| Modified Linear Model | 1063.7568 |
| Tree                  | 1080.2975 |
| Pruned Tree           | 1081.8893 |
| Random Forest         |  789.0855 |
| Gradient Boosted      |  955.5421 |

## Predictive model building: California housing

    ## Distribution not specified, assuming gaussian ...

    ## [1] 76610.08

    ## [1] 76008.57

    ## [1] 78063.36

    ## [1] 78153.32

    ## [1] 65628.33

    ## [1] 66650.12

![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-15-1.png)![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-15-2.png)![](ECO395_Exercises_03_files/figure-markdown_github/unnamed-chunk-15-3.png)
