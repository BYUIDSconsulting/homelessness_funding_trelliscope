Trelliscope Links
================

``` r
library(tidyverse)
library(trelliscopejs)
library(gapminder)
```

## Example Data

We will use the gapminder data set of different countries life
expectancy to demonstrate how you can create links within a trelliscope.

``` r
dat <- gapminder::gapminder
head(dat)
```

    ## # A tibble: 6 x 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

## Countries Life Expectancy Trelliscope

``` r
point_plot <- function(dat){
  ggplot(dat) +
    geom_point(aes(x = year, y = lifeExp)) +
    theme_bw()
}  

dat_count <- dat %>% 
  nest(-c(country,continent))

dat_count <- dat_count %>% 
  mutate(dat_plot = map_plot(data, point_plot))

dat_count %>% 
  trelliscope(name="Life Expectancy by Country",
              path = "~lifeExpectancy",
              thumb = T,
              nrow = 1, 
              ncol = 2
  )
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<div id="htmlwidget-7e45de0c18ad2815d275" style="width:900px;height:550px;" class="trelliscopejs_widget html-widget"></div>
<script type="application/json" data-for="htmlwidget-7e45de0c18ad2815d275">{"x":{"id":"29985206","config_info":"'~lifeExpectancy/appfiles/config.jsonp'","self_contained":false,"latest_display":{"name":"Life_Expectancy_by_Country","group":"common"},"spa":false,"in_knitr":true,"in_shiny":false,"in_notebook":false},"evals":[],"jsHooks":[]}</script>

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
