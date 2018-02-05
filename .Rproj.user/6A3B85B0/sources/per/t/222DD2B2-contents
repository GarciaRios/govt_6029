# Lab 2: Graphing and Data Manipulation
Jeffrey B. Arnold and Sergio Garcia-Rios  
April 10, 2015  

The objectives of this lab are

1. Understand how R treats missing values and use functions to identify and remove those missing values
2. Manipulate data using the verbs in the **dplyr**
3. Use the pipe operator `%>%` to simplify complicated code by chaining expressions together
4. Create plots using the ggplot2 package.

We will be using the following libraries in this `dplyr` and `ggplot2` so make sure to install and load those libraries:


```r
library("dplyr")
library("ggplot2")
library("gapminder")
```

## Missing Data

Misssing data is particularly important


```r
foo <- c(1, 2, NA, 3, 4)
```


**Challenge**

1.  What is the result of `2 + NA`
1.  What is the result of `mean(foo)`
1.  Look at the documentation of `mean` to change how that function handles missing values.
1.  How does `median(foo)` work?
1.  `foo > 2`. Are all the entries `TRUE` and `FALSE`?
1.  What does `is.na(foo)` do? What about `! is.na(foo)` ?
1.  What does `foo[! is.na(foo)]` do?

The function `na.omit` is particularly useful.
It removes any row in a dataset with a missing value in *any* column.
For example,

```r
dfrm <- data.frame(x = c(NA, 2, NA, 4), y = c(NA, NA, 7, 8))
na.omit(dfrm)
```

```
##   x y
## 4 4 8
```


# Gapminder Data

You will be using the gapminder data again.
In the first lab, you loaded the data from a csv file to show how to load from a csv.
In this lab, you will be using the same data, but as it is distributed in **gapminder** package.

To load a dataset included with an R package, use the `data()` function.
You can see which datasets are included in a package,

```r
data(package = "gapminder")
```


**challenge**

- Which dataset in the **gapminder** package is country data?
- Load that data using the `data` function
- Pull up the help page for the "gapminder" dataset

Load the gapminder data

```r
data("gapminder")
```

# Introduction `dplyr`

**dplyr** is a package for data manipulation.
It provides a few core functions data manipulation.
Most data manipulations can be done by combining these verbs together --- something which becomes even easier with the `%>%` operator.

-  `filter()`: subset observations by logical conditions
-  `slice()`: subset observations by row numbers
-  `arrange()`: sort the data by variables
-  `select()`: select a subset of variables
-  `rename()`: rename variables
-  `distinct()`: keep only distict rows
-  `mutate()` and `transmute()`: adds new variables
-  `group_by()`: group the data according to variables
-  `summarise()`: summarize multiple values into a single value
-  `sample_n()` and `sample_frac()`: select a random sample of rows

**dplyr** also offers the function `glimpse` to quickly view the data

```r
glimpse(gapminder)
```

```
## Observations: 1,704
## Variables: 6
## $ country   (fctr) Afghanistan, Afghanistan, Afghanistan, Afghanistan,...
## $ continent (fctr) Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asi...
## $ year      (int) 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992...
## $ lifeExp   (dbl) 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.8...
## $ pop       (int) 8425333, 9240934, 10267083, 11537966, 13079460, 1488...
## $ gdpPercap (dbl) 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 78...
```
or `tbl_df` - which will print out the data frame more similarly to `head`

```r
tbl_df(gapminder)
```

```
## Source: local data frame [1,704 x 6]
## 
##        country continent  year lifeExp      pop gdpPercap
##         (fctr)    (fctr) (int)   (dbl)    (int)     (dbl)
## 1  Afghanistan      Asia  1952  28.801  8425333  779.4453
## 2  Afghanistan      Asia  1957  30.332  9240934  820.8530
## 3  Afghanistan      Asia  1962  31.997 10267083  853.1007
## 4  Afghanistan      Asia  1967  34.020 11537966  836.1971
## 5  Afghanistan      Asia  1972  36.088 13079460  739.9811
## 6  Afghanistan      Asia  1977  38.438 14880372  786.1134
## 7  Afghanistan      Asia  1982  39.854 12881816  978.0114
## 8  Afghanistan      Asia  1987  40.822 13867957  852.3959
## 9  Afghanistan      Asia  1992  41.674 16317921  649.3414
## 10 Afghanistan      Asia  1997  41.763 22227415  635.3414
## ..         ...       ...   ...     ...      ...       ...
```

## Exploring our Data

We are ready to begin exploring our data set in more depth.

We want to explore the relationship between life expectancy and GDP.
Let's use some `dplyr` verbs to explore our data.
For you Stata users missing "if statements" let's begin with `filter()`


```r
filter(gapminder, lifeExp < 29)
filter(gapminder, country == "Rwanda")
filter(gapminder, country %in% c("Rwanda", "Afghanistan"))
```

You can combine filter statements.
Including multiple logical statements is equivalent to combining them with "and".
This will give all observations in "Africa", before 1966, and which have
a life expectancy less than 40.

```r
filter(gapminder, continent == "Africa", year < 1966, lifeExp < 40)
```
That is equivalent to

```r
filter(gapminder, continent == "Africa" & year < 1966 & lifeExp < 40)
```
To combine logical statements with "or" you need to explicitly use `|`.
To find observations from Afghanistan or Albania,

```r
filter(gapminder, country == "Afghanistan" | country == "Albania")
```

## %>%

Before we go any further, we should exploit the new pipe operator that `dplyr` imports from the **magrittr** package.
This is going to change your data analytical life.


## Use `select()` to subset the data on variables or columns.

Most of the times we don't need to see all the variables and are often interested in just a few of them. Here’s a conventional call:

```r
select(gapminder, year, lifeExp)
```

**Challenge**

Using a combination of `filter`, `select` and and `slice` show only year and life expectancy of Cambodia for the first two observations


```r
gapminder %>%
  filter(country == "Cambodia") %>%
  select(year, lifeExp) %>%
  slice(1:2)
```

```
## Source: local data frame [2 x 2]
## 
##    year lifeExp
##   (int)   (dbl)
## 1  1952  39.417
## 2  1957  41.366
```

**Challenge** Use `mutate()` to add  new variables

Imagine we wanted to recover each country’s GDP. We do have data for population and GDP per capita. what do we do?

-  Yes we multiply, let's create a new variable called GDP that brings back the gross amount


```r
gapminder <- gapminder %>%
mutate(gdp = pop * gdpPercap)

gapminder %>% glimpse
```

```
## Observations: 1,704
## Variables: 7
## $ country   (fctr) Afghanistan, Afghanistan, Afghanistan, Afghanistan,...
## $ continent (fctr) Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asi...
## $ year      (int) 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992...
## $ lifeExp   (dbl) 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.8...
## $ pop       (int) 8425333, 9240934, 10267083, 11537966, 13079460, 1488...
## $ gdpPercap (dbl) 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 78...
## $ gdp       (dbl) 6567086330, 7585448670, 8758855797, 9648014150, 9678...
```

So... GDP is almost useless because it doesn't give a base line and that is why we often use per capita, but a baseline is often more useful, how about comparing it to another country sat, USA? Yes USA, USA, USA!

Let's  create first an object containing only US data, we use `filter` here

```r
just_usa <- gapminder %>%
  filter(country == "United States") %>%
  select(year, gdpPercap) %>%
  rename(usa_gdpPercap = gdpPercap)
```

We can join (or merge) the dataset to the gapminder data using the `left_join` function.
There are are several ways to merge datasets with **dplyr** (left join, right join, inner join, and outer join which differ in which oberservations it matches and keep).

```r
gapminder <- left_join(gapminder, just_usa, by = c("year")) %>%
  mutate(gdpPercapRel = gdpPercap / usa_gdpPercap)
```

Now, summarize the relative GDP

```r
gapminder %>%
  select(gdpPercapRel) %>%
  summary
```

```
##   gdpPercapRel     
##  Min.   :0.006168  
##  1st Qu.:0.052423  
##  Median :0.146476  
##  Mean   :0.278566  
##  3rd Qu.:0.390212  
##  Max.   :7.746863
```

Nice, now we can do something like this:

Look at the GDP per capita  of Mexico and Canada relative to US by year

```r
gapminder %>%
  filter(country %in% c("United States", "Canada", "Mexico")) %>%
  select(country, year, gdpPercap, usa_gdpPercap, gdpPercapRel) %>%
  glimpse()
```

```
## Observations: 36
## Variables: 5
## $ country       (fctr) Canada, Canada, Canada, Canada, Canada, Canada,...
## $ year          (int) 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, ...
## $ gdpPercap     (dbl) 11367.161, 12489.950, 13462.486, 16076.588, 1897...
## $ usa_gdpPercap (dbl) 13990.48, 14847.13, 16173.15, 19530.37, 21806.04...
## $ gdpPercapRel  (dbl) 0.8124925, 0.8412368, 0.8323975, 0.8231586, 0.86...
```


## `arrange`

Because the world is not always ordered the way we want it

```r
gapminder %>%
  arrange(year, country) %>%
  glimpse()
```

```
## Observations: 1,704
## Variables: 9
## $ country       (fctr) Afghanistan, Albania, Algeria, Angola, Argentin...
## $ continent     (fctr) Asia, Europe, Africa, Africa, Americas, Oceania...
## $ year          (int) 1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952, ...
## $ lifeExp       (dbl) 28.801, 55.230, 43.077, 30.015, 62.485, 69.120, ...
## $ pop           (int) 8425333, 1282697, 9279525, 4232095, 17876956, 86...
## $ gdpPercap     (dbl) 779.4453, 1601.0561, 2449.0082, 3520.6103, 5911....
## $ gdp           (dbl) 6567086330, 2053669902, 22725632678, 14899557133...
## $ usa_gdpPercap (dbl) 13990.48, 13990.48, 13990.48, 13990.48, 13990.48...
## $ gdpPercapRel  (dbl) 0.05571254, 0.11443895, 0.17504816, 0.25164324, ...
```

```r
gapminder %>%
  filter(year == 2007) %>%
  arrange(- lifeExp) %>%
  glimpse()
```

```
## Observations: 142
## Variables: 9
## $ country       (fctr) Japan, Hong Kong, China, Iceland, Switzerland, ...
## $ continent     (fctr) Asia, Asia, Europe, Europe, Oceania, Europe, Eu...
## $ year          (int) 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, ...
## $ lifeExp       (dbl) 82.603, 82.208, 81.757, 81.701, 81.235, 80.941, ...
## $ pop           (int) 127467972, 6980412, 301931, 7554661, 20434176, 4...
## $ gdpPercap     (dbl) 31656.068, 39724.979, 36180.789, 37506.419, 3443...
## $ gdp           (dbl) 4.035135e+12, 2.772967e+11, 1.092410e+10, 2.8334...
## $ usa_gdpPercap (dbl) 42951.65, 42951.65, 42951.65, 42951.65, 42951.65...
## $ gdpPercapRel  (dbl) 0.7370163, 0.9248766, 0.8423608, 0.8732241, 0.80...
```

** Challenge

What about life expectancy? Create a relative to life expectancy variable, compare the three NAFTA countries US, Canada and Mexico


# Plotting with ggplot2

We will be using the graphics package **ggplot2**, which is one of the most popular, but it is only one of several graphics packages in R.[^1]

Unlike many other graphics systems, functions in **ggplot2** do not correspond to separate types of graphs.
There are not scatterplot, histogram, or line chart functions per se.
Instead plots are built up from component functions.

1. Data
2. Aesthetics: Maps variables in the data to visual properties: position, color, size, shape, line type ...
3. Geometric objects: The specific shapes that are drawn: points, lines,
4. scales: How variables values map to "computer" values.
5. stat: summarize or transform the data. e.g. bin data and count in histogram; run a regression to get a line.
5. facet: create mini-plots of data subsets

Let's continue using the gapminder data, take another look at it

```r
glimpse(gapminder)
```

```
## Observations: 1,704
## Variables: 9
## $ country       (fctr) Afghanistan, Afghanistan, Afghanistan, Afghanis...
## $ continent     (fctr) Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia,...
## $ year          (int) 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, ...
## $ lifeExp       (dbl) 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, ...
## $ pop           (int) 8425333, 9240934, 10267083, 11537966, 13079460, ...
## $ gdpPercap     (dbl) 779.4453, 820.8530, 853.1007, 836.1971, 739.9811...
## $ gdp           (dbl) 6567086330, 7585448670, 8758855797, 9648014150, ...
## $ usa_gdpPercap (dbl) 13990.48, 14847.13, 16173.15, 19530.37, 21806.04...
## $ gdpPercapRel  (dbl) 0.05571254, 0.05528699, 0.05274798, 0.04281523, ...
```

Great, let the plotting begin:


```r
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
```
This gives an error message because there is nothing to plot yet!

This just initializes the plot object, it is better if you assign it to an object, `p` is good enough

```r
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
```

No we can add `geoms`

```r
p + geom_point()
```

![](lab2_files/figure-html/unnamed-chunk-22-1.png) 

That look okay but it would probably look be better if we log transform

```r
p_l <- ggplot(gapminder, aes(x = log10(gdpPercap), y = lifeExp))
p_l + geom_point()
```

![](lab2_files/figure-html/unnamed-chunk-23-1.png) 

A better way to log transform

```r
p + geom_point() + scale_x_log10()
```

![](lab2_files/figure-html/unnamed-chunk-24-1.png) 

Let's make that stick

```r
p <- p + scale_x_log10()
```

Common workflow: gradually build up the plot you want,  re-define the object 'p' as you develop "keeper" commands.
Note that in the reassigning we excluded the `geom`.


Now, let continent variable to aesthetic color

```r
p + geom_point(aes(color = continent))
```

![](lab2_files/figure-html/unnamed-chunk-26-1.png) 

In full detail, up to now:

```r
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()
```

![](lab2_files/figure-html/unnamed-chunk-27-1.png) 
Let's address over-plotting: SET alpha transparency and size to a value

```r
p + geom_point(alpha = (1 / 3), size = 3)
```

![](lab2_files/figure-html/unnamed-chunk-28-1.png) 
Add now a fitted curve or line

```r
p + geom_point() + geom_smooth()
```

![](lab2_files/figure-html/unnamed-chunk-29-1.png) 

```r
p + geom_point() + geom_smooth(lwd = 2, se = FALSE)
```

![](lab2_files/figure-html/unnamed-chunk-29-2.png) 

```r
p + geom_smooth(lwd = 1, se = FALSE, method = "lm") + geom_point()
```

![](lab2_files/figure-html/unnamed-chunk-29-3.png) 
That's great but I actually want to revive our interest in continents!

```r
p + aes(color = continent) + geom_point() + geom_smooth(lwd = 3, se = FALSE)
```

![](lab2_files/figure-html/unnamed-chunk-30-1.png) 
Facetting: another way to exploit a factor

```r
p + geom_point(alpha = (1 / 3), size = 3) + facet_wrap(~ continent)
```

![](lab2_files/figure-html/unnamed-chunk-31-1.png) 
Still want lines? Let's add them

```r
p + geom_point(alpha = (1 / 3), size = 3) + facet_wrap(~ continent) +
  geom_smooth(lwd = 2, se = FALSE)
```

![](lab2_files/figure-html/unnamed-chunk-32-1.png) 

**Challenge**

* plot lifeExp against year
* make mini-plots, split out by continent
* add a fitted smooth and/or linear regression, w/ or w/o faceting




```r
# plot lifeExp against year
# y <- ggplot(gDat, aes(x = year, y = lifeExp)) + geom_point()

# make mini-plots, split out by continent
# y + facet_wrap(~ continent)

# add a fitted smooth and/or linear regression, w/ or w/o facetting
#y + geom_smooth(se = FALSE, lwd = 2) +
#  geom_smooth(se = FALSE, method ="lm", color = "orange", lwd = 2)

# y + geom_smooth(se = FALSE, lwd = 2) +
#  facet_wrap(~ continent)
```

What if I am only interrested in the US?

```r
ggplot(filter(gapminder, country == "United States"),
       aes(x = year, y = lifeExp)) +
  geom_line() +
  geom_point()
```

![](lab2_files/figure-html/unnamed-chunk-34-1.png) 

Let just look at five countries


```r
some_countries <- c("United States", "Canada", "Rwanda", "Cambodia", "Mexico")
ggplot(filter(gapminder, country %in% some_countries),
       aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point()
```

![](lab2_files/figure-html/unnamed-chunk-35-1.png) 


So what's up with Mexico?

* Nafta? Higher GDP?

Not really...

```r
ggplot(subset(gapminder, country %in% some_countries),
       aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point(aes(size=gdpPercap))
```

![](lab2_files/figure-html/unnamed-chunk-36-1.png) 

You can change the way the plot looks overall using `theme`


```r
ggplot(subset(gapminder, country %in% some_countries),
       aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point(aes(size=gdpPercap)) +
  theme_minimal()
```

![](lab2_files/figure-html/unnamed-chunk-37-1.png) 

In addition to the themes included with ggplot, several other themes are available in the [ggthemes](http://cran.r-project.org/web/packages/ggthemes/index.html) package.

## References

- Hadley Wickham's tutorials from useR 2014: Video [Part I](https://www.youtube.com/watch?v=8SGif63VW6E), [Part II](https://www.youtube.com/watch?v=Ue08LVuk790), [Slides and Tutorial](https://www.dropbox.com/sh/i8qnluwmuieicxc/AAAgt9tIKoIm7WZKIyK25lh6a).
- The vignettes for [dplyr](http://cran.rstudio.com/web/packages/dplyr/) and [tidyr](http://cran.rstudio.com/web/packages/dplyr/)


[^1]: You may encounter these other packages in other classes, or code samples online.

    - base graphics: included with R. Functions like `plot`, `barplot`, `hist`. See http://www.statmethods.net/graphs/index.html
    - lattice graphics: Functions including `barchart`, `densityplot`, `dotplot`, `xyplot`, `histogram`. See http://www.statmethods.net/advgraphs/trellis.html
    - - Other graphics packages designed for the web: [ggvis](http://ggvis.rstudio.com/), [rcharts](http://rcharts.io/), [plotly](https://plot.ly/)
    - - [tile](http://faculty.washington.edu/cadolph/?page=60) Chris Adolph's graphics package.

[^2]:

* * *
- Derived from Jennifer Bryan, "hello ggplot2!", https://github.com/jennybc/ggplot2-tutorial/blob/master/ggplot2-tutorial-slides.pdf. License: CC-BY-NC
- Derived from Karthik Ram, "A quick introduction to ggplot2", [Speakerdeck](http://inundata.org/2013/04/10/a-quick-introduction-to-ggplot2/), [github](https://github.com/karthik/ggplot-lecture). License: [CC-BY](http://creativecommons.org/licenses/by/2.0/)
-->


<!--  LocalWords:  'Lab Manipulation' html dplyr summarise frac gd df
 -->
<!--  LocalWords:  ggplot2 gapminder url gdf delim str tbl gapminder Stata
 -->
<!--  LocalWords:  lifeExp magrittr gdp gdpPercap usa gdpPercapRel gg
 -->
<!--  LocalWords:  NAFTA geoms aes colour ggplot log10 workflow 'p'
 -->
<!--  LocalWords:  lwd se lm Facetting gDat
 -->
