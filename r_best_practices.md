<!-- 
.. title: Best practices for using R
-->

## Style Guide {#style}

It is important to follow a consistent and clear style when writing R.

Follow the Hadley Wickham's *Advanced R* [Style guide](http://adv-r.had.co.nz/Style.html).

Writing code in a  consistent (and good) style will make is easier for others and you to read your code.
It will also result fewer bugs.

For, example, if you do not put spaces around the assignment operator (`<-`),
```r
x<-1
```
do you mean
```r
x <- 1
```
or
```r
x < -1
```
Off the top of your head, do you know which one R is going to do?
Yes, I've seen bugs due to this in real life.

## Best Practices {#best-practices}

These are some best practices for writing R code suggested by [Software Carpentry](https://github.com/swcarpentry/r-novice-inflammation). Many of these are applicable, although some are more specific to putting your analyses in R scripts rather than when writing your analyses in R Markdown documents.

1. Start your code with a description of what it is:

```r
#This is code to replicate the analyses and figures from my 2014 Science paper.
#Code developed by Sarah Supp, Tracy Teal, and Jon Borelli
```

2. The first code should all of your import statments (`library`)

```r
library("ggplot2")
library("reshape")
library("vegan")
```

Followed by any sourced documents
```r
source("foo.R")
```

3. Set your working directory before running a script, or start `R` inside your project folder:

  You should exercise caution when using `setwd()`.
  Changing directories inside your script can limit reproducibility:

	* `setwd()` will throw an error if the directory you're trying to change to doesn't exit, or the user doesn't have the correct permissions to access it. This becomes a problem when sharing scripts between users who have organized their directories differently.
	* If/when your script terminates with an error, you might leave the user in a different directory to where they started, and if they call the script again this will cause further problems. If you must use `setwd()`, it is best to put it at the top of the script to avoid this problem.

   The following error message indicates that R has failed to set the working directory you specified:

	```
	Error in setwd("~/path/to/working/directory") : cannot change working directory
	```

	Consider using the convention that the user running the script should begin in the relevant directory on their machine and then use relative file paths.
	That means you should avoid using `setwd()` *within* a script; instead you should use `setwd()` in the console to move into the correct directory before running a script.

4. Use `#` or `#-` to set off sections of your code so you can easily scroll through it and find things.

5. Do **not** use `attach()`. Ever.

6. If you have only one or a few functions, put them at the top of
   your code, so they are among the first things run. If you written
   many functions, put them all in their own .R file, and `source`
   them. Source will run all of these functions so that you can use
   them as you need them.

	```r
	source("my_genius_fxns.R")
	```

7. Keep your code modular. If a single function or loop gets too long,
   consider breaking it into smaller pieces.

8. Don't repeat yourself. Automate! If you are repeating the same
   piece of code on multiple objects or files, use a loop or a
   function to do the same thing. The more you repeat yourself, the
   more likely you are to make a mistake.

9. Manage all of your source files for a project in the same
   directory. Then use relative paths as necessary. For example, use

	```r
	dat <- read.csv(file = "/files/dataset-2013-01.csv", header = TRUE)
	```

    rather than:

	```r
	dat <- read.csv(file = "/Users/Karthik/Documents/sannic-project/files/dataset-2013-01.csv", header = TRUE)
	```

10. Don't save a session history (the default option in R, when it
    asks if you want an `RData` file). Instead, start in a clean
    environment so that older objects don't contaminate your current
    environment. This can lead to unexpected results, especially if
    the code were to be run on someone else's machine.

11. Collaborate. Grab a buddy and practice "code review". We do it for
    methods and papers, why not code? Our code is a major scientific
    product and the result of a lot of hard work!


* * *

Copywrite  © [Software Carpentry](http://software-carpentry.org/), [link to the license][cc-by-human]. Page derived from "[Best practices for using R and designing programs](https://raw.githubusercontent.com/swcarpentry/r-novice-inflammation/gh-pages/06-best-practices-R.Rmd"), with only minor edits.
