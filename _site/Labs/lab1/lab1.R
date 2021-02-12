

Now We will use the following libraries in this `dplyr` and `ggplot2` section so make sure to install and load those libraries:
  
  ```{r, libraries, warning = FALSE, message = FALSE}
library("dplyr")
library("ggplot2")
library("gapminder")
```


# Gapminder Data

You will be using the gapminder data again.
In the first lab, you loaded the data from a csv file to show how to load from a csv.
In this lab, you will be using the same data, but as it is distributed in **gapminder** package.

To load a dataset included with an R package, use the `data()` function.
You can see which datasets are included in a package,
```{r eval=FALSE}
data(package = "gapminder")
```


**challenge**
  
  - Which dataset in the **gapminder** package is country data?
  - Load that data using the `data` function
- Pull up the help page for the "gapminder" dataset

Load the gapminder data
```{r}
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
```{r}
glimpse(gapminder)
```
or `tbl_df` - which will print out the data frame more similarly to `head`
```{r}
tbl_df(gapminder)
```

## Exploring our Data

We are ready to begin exploring our data set in more depth.

We want to explore the relationship between life expectancy and GDP.
Let's use some `dplyr` verbs to explore our data.
For you Stata users missing "if statements" let's begin with `filter()`

```{r results='hide'}
filter(gapminder, lifeExp < 29)
filter(gapminder, country == "Rwanda")
```

You can combine filter statements.
Including multiple logical statements is equivalent to combining them with "and".
This will give all observations in "Africa", before 1966, and which have
a life expectancy less than 40.
```{r results='hide'}
filter(gapminder, continent == "Africa", year < 1966, lifeExp < 40)
```
That is equivalent to
```{r results='hide'}
filter(gapminder, continent == "Africa" & year < 1966 & lifeExp < 40)
```
To combine logical statements with "or" you need to explicitly use `|`.
To find observations from Afghanistan or Albania,
```{r results='hide'}
filter(gapminder, country == "Afghanistan" | country == "Albania")
```

## %>%

Before we go any further, we should exploit the new pipe operator that `dplyr` imports from the **magrittr** package.
This is going to change your data analytical life.


## Use `select()` to subset the data on variables or columns.

Most of the times we don't need to see all the variables and are often interested in just a few of them. Here’s a conventional call:
```{r results='hide'}
select(gapminder, year, lifeExp)
```

**Challenge**

Using a combination of `filter`, `select` and and `slice` show only year and life expectancy of Cambodia for the first two observations

```{r,}
gapminder %>%
filter(country == "Cambodia") %>%
select(year, lifeExp) %>%
slice(1:2)

```

**Challenge** Use `mutate()` to add  new variables

Imagine we wanted to recover each country’s GDP. We do have data for population and GDP per capita. what do we do?


-  Yes we multiply, let's create a new variable called GDP that brings back the gross amount

```{r, echo=FALSE}
gapminder <- gapminder %>%
  mutate(gdp = pop * gdpPercap)

gapminder %>% glimpse
```

So... GDP is almost useless because it doesn't give a base line and that is why we often use per capita, but a baseline is often more useful, how about comparing it to another country say, USA? Yes USA, USA, USA!

Let's  create first an object containing only US data, we use `filter` here
```{r}
just_usa <- gapminder %>%
  filter(country == "United States") %>%
  select(year, gdpPercap) %>%
  rename(usa_gdpPercap = gdpPercap)
```

We can join (or merge) the dataset to the gapminder data using the `left_join` function.
There are are several ways to merge datasets with **dplyr** (left join, right join, inner join, and outer join which differ in which oberservations it matches and keep).
```{r}
gapminder <- 
  left_join(gapminder, just_usa, by = c("year")) %>%
  mutate(gdpPercapRel = gdpPercap / usa_gdpPercap)
```

Now, summarize the relative GDP
```{r}
gapminder %>%
  select(gdpPercapRel) %>%
  summary

```

Nice, now we can do something like this:
  
  Look at the GDP per capita  of Mexico and Canada relative to US by year
```{r}
gapminder %>%
  filter(country %in% c("United States", "Canada", "Mexico")) %>%
  select(country, year, gdpPercap, usa_gdpPercap, gdpPercapRel) %>%
  glimpse()
```


## `arrange`

Because the world is not always ordered the way we want it
```{r}
gapminder %>%
  arrange(year, country) %>%
  glimpse()


gapminder %>%
  filter(year == 2007) %>%
  arrange(- lifeExp) %>%
  glimpse()

```

** Challenge

What about life expectancy? Create  life expectancy variable relative to the US, compare the three NAFTA countries US, Canada and Mexico


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
```{r}
glimpse(gapminder)
```

Great, let the plotting begin:

```{r eval=FALSE}
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
```
This gives an error message because there is nothing to plot yet!

This just initializes the plot object, it is better if you assign it to an object, `p` is good enough
```{r}
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
```

No we can add `geoms`
```{r}
p + geom_point()
```

That look okay but it would probably look be better if we log transform
```{r}
p_l <- ggplot(gapminder, aes(x = log10(gdpPercap), y = lifeExp))
p_l + geom_point()
```

A better way to log transform
```{r}
p + geom_point() + scale_x_log10()
```

Let's make that stick
```{r}
p <- p + scale_x_log10()

```

Common workflow: gradually build up the plot you want,  re-define the object 'p' as you develop "keeper" commands.
Note that in the reassigning we excluded the `geom`.


Now, let continent variable to aesthetic color
```{r}
p + geom_point(aes(color = continent))

```

In full detail, up to now:
  ```{r}
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()
```
Let's address over-plotting: SET alpha transparency and size to a value
```{r}
p + geom_point(alpha = (1 / 3), size = 3)
```
Add now a fitted curve or line
```{r}
p + geom_point() + geom_smooth()
p + geom_point() + geom_smooth(lwd = 2, se = FALSE)
p + geom_smooth(lwd = 1, se = FALSE, method = "lm") + geom_point()
```
That's great but I actually want to revive our interest in continents!
  ```{r}
p + aes(color = continent) + geom_point() + geom_smooth(lwd = 3, se = FALSE)
```
Facetting: another way to exploit a factor
```{r}
p + geom_point(alpha = (1 / 3), size = 3) + facet_wrap(~ continent)
```
Still want lines? Let's add them
```{r}
p + geom_point(alpha = (1 / 3), size = 3) + facet_wrap(~ continent) +
geom_smooth(lwd = 2, se = FALSE)
```

**Challenge**

* plot lifeExp against year
* make mini-plots, split out by continent
* add a fitted smooth and/or linear regression, w/ or w/o faceting



```{r}
# plot lifeExp against year
y <- ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_point()

# make mini-plots, split out by continent
y + facet_wrap(~ continent)

# add a fitted smooth and/or linear regression, w/ or w/o facetting
y + geom_smooth(se = FALSE, lwd = 2) +
geom_smooth(se = FALSE, method ="lm", color = "orange", lwd = 2)

y + geom_smooth(se = FALSE, lwd = 2) +
facet_wrap(~ continent)
```

What if I am only interrested in the US?
```{r}
ggplot(filter(gapminder, country == "United States"),
aes(x = year, y = lifeExp)) +
geom_line() +
geom_point()

```

Let just look at five countries

```{r}
some_countries <- c("United States", "Canada", "Rwanda", "Cambodia", "Mexico")
ggplot(filter(gapminder, country %in% some_countries),
aes(x = year, y = lifeExp, color = country)) +
geom_line() +
geom_point()
```


So what's up with Mexico?
  
  * Nafta? Higher GDP?
  
  ```{r}
ggplot(subset(gapminder, country %in% some_countries),
       aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point(aes(size=gdpPercap))
```

You can change the way the plot looks overall using `theme`

```{r}
ggplot(subset(gapminder, country %in% some_countries),
       aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point(aes(size=gdpPercap)) +
  theme_bw()
```

* * *
  
  Some text and the data set used in this are taken from Jenny Bryant, [R basics, workspace and working directory, RStudio projects](https://stat545-ubc.github.io/block002_hello-r-workspace-wd-project.html), licensed under [CC BY-NC 3.0](http://creativecommons.org/licenses/by-nc/3.0/) 

Now We will use the following libraries in this `dplyr` and `ggplot2` section so make sure to install and load those libraries:
  
  ```{r, libraries, warning = FALSE, message = FALSE}
library("dplyr")
library("ggplot2")
library("gapminder")
```


# Gapminder Data

You will be using the gapminder data again.
In the first lab, you loaded the data from a csv file to show how to load from a csv.
In this lab, you will be using the same data, but as it is distributed in **gapminder** package.

To load a dataset included with an R package, use the `data()` function.
You can see which datasets are included in a package,
```{r eval=FALSE}
data(package = "gapminder")
```


**challenge**
  
  - Which dataset in the **gapminder** package is country data?
  - Load that data using the `data` function
- Pull up the help page for the "gapminder" dataset

Load the gapminder data
```{r}
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
```{r}
glimpse(gapminder)
```
or `tbl_df` - which will print out the data frame more similarly to `head`
```{r}
tbl_df(gapminder)
```

## Exploring our Data

We are ready to begin exploring our data set in more depth.

We want to explore the relationship between life expectancy and GDP.
Let's use some `dplyr` verbs to explore our data.
For you Stata users missing "if statements" let's begin with `filter()`

```{r results='hide'}
filter(gapminder, lifeExp < 29)
filter(gapminder, country == "Rwanda")
```

You can combine filter statements.
Including multiple logical statements is equivalent to combining them with "and".
This will give all observations in "Africa", before 1966, and which have
a life expectancy less than 40.
```{r results='hide'}
filter(gapminder, continent == "Africa", year < 1966, lifeExp < 40)
```
That is equivalent to
```{r results='hide'}
filter(gapminder, continent == "Africa" & year < 1966 & lifeExp < 40)
```
To combine logical statements with "or" you need to explicitly use `|`.
To find observations from Afghanistan or Albania,
```{r results='hide'}
filter(gapminder, country == "Afghanistan" | country == "Albania")
```

## %>%

Before we go any further, we should exploit the new pipe operator that `dplyr` imports from the **magrittr** package.
This is going to change your data analytical life.


## Use `select()` to subset the data on variables or columns.

Most of the times we don't need to see all the variables and are often interested in just a few of them. Here’s a conventional call:
```{r results='hide'}
select(gapminder, year, lifeExp)
```

**Challenge**

Using a combination of `filter`, `select` and and `slice` show only year and life expectancy of Cambodia for the first two observations

```{r,}
gapminder %>%
filter(country == "Cambodia") %>%
select(year, lifeExp) %>%
slice(1:2)

```

**Challenge** Use `mutate()` to add  new variables

Imagine we wanted to recover each country’s GDP. We do have data for population and GDP per capita. what do we do?


-  Yes we multiply, let's create a new variable called GDP that brings back the gross amount

```{r, echo=FALSE}
gapminder <- gapminder %>%
  mutate(gdp = pop * gdpPercap)

gapminder %>% glimpse
```

So... GDP is almost useless because it doesn't give a base line and that is why we often use per capita, but a baseline is often more useful, how about comparing it to another country say, USA? Yes USA, USA, USA!

Let's  create first an object containing only US data, we use `filter` here
```{r}
just_usa <- gapminder %>%
  filter(country == "United States") %>%
  select(year, gdpPercap) %>%
  rename(usa_gdpPercap = gdpPercap)
```

We can join (or merge) the dataset to the gapminder data using the `left_join` function.
There are are several ways to merge datasets with **dplyr** (left join, right join, inner join, and outer join which differ in which oberservations it matches and keep).
```{r}
gapminder <- 
  left_join(gapminder, just_usa, by = c("year")) %>%
  mutate(gdpPercapRel = gdpPercap / usa_gdpPercap)
```

Now, summarize the relative GDP
```{r}
gapminder %>%
  select(gdpPercapRel) %>%
  summary

```

Nice, now we can do something like this:
  
  Look at the GDP per capita  of Mexico and Canada relative to US by year
```{r}
gapminder %>%
  filter(country %in% c("United States", "Canada", "Mexico")) %>%
  select(country, year, gdpPercap, usa_gdpPercap, gdpPercapRel) %>%
  glimpse()
```


## `arrange`

Because the world is not always ordered the way we want it
```{r}
gapminder %>%
  arrange(year, country) %>%
  glimpse()


gapminder %>%
  filter(year == 2007) %>%
  arrange(- lifeExp) %>%
  glimpse()

```

** Challenge

What about life expectancy? Create a  life expectancy variable relative to the US, compare the three NAFTA countries US, Canada and Mexico


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
```{r}
glimpse(gapminder)
```

Great, let the plotting begin:

```{r eval=FALSE}
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
```
This gives an error message because there is nothing to plot yet!

This just initializes the plot object, it is better if you assign it to an object, `p` is good enough
```{r}
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
```

No we can add `geoms`
```{r}
p + geom_point()
```

That look okay but it would probably look be better if we log transform
```{r}
p_l <- ggplot(gapminder, aes(x = log10(gdpPercap), y = lifeExp))
p_l + geom_point()
```

A better way to log transform
```{r}
p + geom_point() + scale_x_log10()
```

Let's make that stick
```{r}
p <- p + scale_x_log10()

```

Common workflow: gradually build up the plot you want,  re-define the object 'p' as you develop "keeper" commands.
Note that in the reassigning we excluded the `geom`.


Now, let continent variable to aesthetic color
```{r}
p + geom_point(aes(color = continent))

```

In full detail, up to now:
  ```{r}
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()
```
Let's address over-plotting: SET alpha transparency and size to a value
```{r}
p + geom_point(alpha = (1 / 3), size = 3)
```
Add now a fitted curve or line
```{r}
p + geom_point() + geom_smooth()
p + geom_point() + geom_smooth(lwd = 2, se = FALSE)
p + geom_smooth(lwd = 1, se = FALSE, method = "lm") + geom_point()
```
That's great but I actually want to revive our interest in continents!
  ```{r}
p + aes(color = continent) + geom_point() + geom_smooth(lwd = 3, se = FALSE)
```
Facetting: another way to exploit a factor
```{r}
p + geom_point(alpha = (1 / 3), size = 3) + facet_wrap(~ continent)
```
Still want lines? Let's add them
```{r}
p + geom_point(alpha = (1 / 3), size = 3) + facet_wrap(~ continent) +
geom_smooth(lwd = 2, se = FALSE)
```

**Challenge**

* plot lifeExp against year
* make mini-plots, split out by continent
* add a fitted smooth and/or linear regression, w/ or w/o faceting



```{r}
# plot lifeExp against year
y <- ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_point()

# make mini-plots, split out by continent
y + facet_wrap(~ continent)

# add a fitted smooth and/or linear regression, w/ or w/o facetting
y + geom_smooth(se = FALSE, lwd = 2) +
geom_smooth(se = FALSE, method ="lm", color = "orange", lwd = 2)

y + geom_smooth(se = FALSE, lwd = 2) +
facet_wrap(~ continent)
```

What if I am only interrested in the US?
```{r}
ggplot(filter(gapminder, country == "United States"),
aes(x = year, y = lifeExp)) +
geom_line() +
geom_point()

```

Let just look at five countries

```{r}
some_countries <- c("United States", "Canada", "Rwanda", "Cambodia", "Mexico")
ggplot(filter(gapminder, country %in% some_countries),
aes(x = year, y = lifeExp, color = country)) +
geom_line() +
geom_point()
```


So what's up with Mexico?
  
  * Nafta? Higher GDP?
  
  ```{r}
ggplot(subset(gapminder, country %in% some_countries),
       aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point(aes(size=gdpPercap))
```

You can change the way the plot looks overall using `theme`

```{r}
ggplot(subset(gapminder, country %in% some_countries),
       aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  geom_point(aes(size=gdpPercap)) +
  theme_bw()
```

* * *
  
  Some text and the data set used in this are taken from Jenny Bryant, [R basics, workspace and working directory, RStudio projects](https://stat545-ubc.github.io/block002_hello-r-workspace-wd-project.html), licensed under [CC BY-NC 3.0](http://creativecommons.org/licenses/by-nc/3.0/) 