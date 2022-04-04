Sankey Charts
================
Tyler Jones
3/17/2022

### Load in Libraries

``` r
library(tidyverse)
library(trelliscopejs)
library(plotly)
```

### Format our Data

First we load in our data and choose our categorical columns in the
order that we want them to appear in our sankey charts.

Since the order we wanted for our sankey chart is

-   Department
-   Agency
-   Program

We have ordered our columns

-   `name_department_award`
-   `name_agency_award`
-   `name_program`

``` r
testdata <- read.csv("C:/Users/tymax/Downloads/fsr_input (1).tsv.gz", sep = ',', header = TRUE)
dat <- testdata
dat <- dat[,c("name_department_award",  "name_agency_award", "name_program","amount_award", "state_company")]
```

We want our trelliscope to wrap based on state so we will nest our
dataframe based on the `state_company` variable.

``` r
dat <- nest(dat, -"state_company")
```

Now we create a function that can be applied to each state that will
create a sankey chart.

``` r
sankey_plot_func <- function(dat1){
  
#This will tell us which column is the quantitative variable. We Set 'amount_award' as the qunatitative variable in the dataframe and retrieve the column number
  quant_var <- which(colnames(dat1) == "amount_award")

#We set the quantitative variable column to 'amount' for standardization 
  colnames(dat1)[quant_var] <- "amount"
  
#Get a dataframe that has only the variables that will be made into nodes in the sankey chart
  dat_nodes <- dat1[,-quant_var]

#Create a list of nodes with their level in the sankey chart and assign them an ID
  nodes <- c()
  levels <- c()
  n <- 1
  for (i in colnames(dat_nodes)) {
    vals <- unique(dat_nodes[,i])[[1]]
    nodes <- append(nodes,  vals)
    levels <- append(levels, rep(n, length(vals)))
    n <- n+1
    
  }
  node_df <- data.frame(nodes, levels)
  node_df$ID <- seq(from = 0 , nrow(node_df)-1)
  
  
#We now create the dataframe with the appropriate colors for the sankey nodes
  node_colors <- RColorBrewer::brewer.pal(n = 12, name = "Set3")
  add_color_df <- data.frame(node_colors, 1:12)
  colnames(add_color_df)[2] <- "ID"
  
#add the colors to our node dataframe
  node_df <- left_join(node_df, add_color_df, by = c("levels" = "ID"))
  
#Go through all of our columns and retrieve the values connecting nodes
  source <- c()
  target <- c()
  amount <- c()
  for (i in 1:(length(colnames(dat_nodes)) - 1) ) {
    col1 <- colnames(dat_nodes)[i]
    col2 <- colnames(dat_nodes)[i+1]
    
    dat2 <- dat1 %>% 
      group_by(.dots = c(col1,col2)) %>% 
      summarise(amount = sum(amount))
    
    colnames(dat2)[1] <- "nodes"
    dat2 <- left_join(dat2, node_df[node_df$levels==i,c(1,3)], by = "nodes")
    colnames(dat2)[colnames(dat2)=="ID"] <- "Source"
    dat2 <- dat2[,2:4]
    colnames(dat2)[1] <- "nodes"
    dat2 <- left_join(dat2, node_df[node_df$levels==(i+1),c(1,3)], by = "nodes")
    colnames(dat2)[colnames(dat2)=="ID"] <- "Target"
    dat2 <- dat2[,2:4]
    
    source <- append(source, dat2$Source)
    target <- append(target, dat2$Target)
    amount <- append(amount, dat2$amount)
    
  }
  
#With the node df and the connections we can now plot our sankey chart
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    
    node = list(
      label = node_df$nodes,
      color = node_df$node_colors,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    link = list(
      source = source,
      target = target,
      value =  amount
    )
  )
  fig <- fig %>% layout(
    title = "",
    font = list(
      size = 10
    )
  )
  fig
}
```

With out sankey chart function we will apply it to every states
dataframe

``` r
dat <- dat %>% 
  mutate(dat_plot = map_plot(data, sankey_plot_func))
```

Drop the NA state and create the trelliscope.

``` r
trel_sankey <- dat %>% 
  drop_na() %>% 
  trelliscope(name="Sankey Charts",
              path = "~Sankey",
              thumb = T,
              nrow = 1, 
              ncol = 2
  )
```
