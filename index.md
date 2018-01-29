<!--
.. title: POLS/CS&SS 503: Advanced Quantitative Political Methodology
.. slug: index
.. date: 2015-03-25 21:36:15 UTC-07:00
.. description: Syllabus for Jeffrey Arnold, POLS/CS&SS 503: Advanced Quantitative Political Methodology, Spring 2015, University of Washington.
-->

<p class="lead">
University of Washington, Spring 2015
</p>

<table>
<tr>
<td><a href="http://www.polisci.washington.edu/"><img src="uw-deptPoliSci_273@2x.png" height="74px"></a></td>
<td><a href="https://www.csss.washington.edu/"><img src="CSSStranMD@2x.png" height="95opx"></a></td>
</tr>
</table>

# Teaching Team

<table class = "table table-striped table-hover">
<tr>
<td>Professor </td>
<td> <a href="http://jrnold.me">Jeffrey Arnold</a> </td>
<td> <a href="mailto:jrnold@uw.ed">jrnold@uw.edu</a> </td>
</tr>
<tr>
<td>TAs</td>
<td><a href="http://www.sergiogarciarios.com/">Sergio García-Rios</a> </td>
<td><a href="mailto:sigarcia@uw.edu">sigarcia@uw.edu</a> </td>
</tr>
<tr>
<td></td>
<td><a href="http://staff.washington.edu/csjohns/">Carolina Johnson</a> </td>
<td><a href="mailto:csjohns@uw.edu">csjohns@uw.edu</a> </td>
</tr>
</table>

# Class Meetings

<table class = "table table-striped table-hover">
<td>Class </td>
<td>Tues </td>
<td> 4:30-7:20 pm </td>
<td> SAV 158 </td>
</tr>
<tr>
<td>Lab </td>
<td>Fri </td>
<td>1:30-3:30 pm </td>
<td> SAV 117 </td>
</tr>
</table>

## Office Hours

<table class = "table table-striped table-hover">
<tr>
<td>Jeffrey Arnold</td>
<td>Th 3:00--5:00 pm</td>
<td>Smith 221B </td>
</tr>
<tr>
<td>Carolina Johnson</td>
<td>Tu 3:30--4:30 pm </td>
<td> SAV 119 </td>
</tr>
<tr>
<td>Sergio García-Rios </td>
<td>F 12:30--1:30 pm </td>
<td> SAV 119 </td>
</tr>
</table>

# Overview and Class Goals

<!-- Begin from Chris Adolph syllabus -->

This course continues the graduate sequence in quantitative political methodology, focused particularly on fitting, interpreting, and refining the linear regression model.

Our agenda includes gaining familiarity with statistical programming via the popular R environment, developing clear and informative graphical representations of regression results, and understanding regression models in matrix form.

<!-- End from Chris Adolph syllabus -->

# Prerequisites

<!-- Begin from Chris Adolph syllabus -->

It is desirable for students to have taken the introductory course in the
sequence (Political Science 501), but any prior course on basic social statistics and linear regression should suffice.

<!-- End from Chris Adolph syllabus -->

# Assessment and Evaluation

## Problem Sets

Problem sets assigned weekly or bi-weekly. These problem sets will include programming problems with an emphasis on writing understandable, reproducible code.

The assignments and due dates will be distributed during the quarter.
<!-- These assignments may involve peer review / grading. -->

Assignments will be *both* submitted digitally through Canvas at the due date and a paper copy to the TA at the next lab section.

## Final Paper

A 15--20 page original report on an original quantitative analysis or replication-and-extension of a published article.
The quantitative analysis should be conducted in R and reproducible.
Students may work in pairs on the final paper with instructor permission.

The final paper is due on **June 9, 2015 15:00 PDT**.

For details, see the [Guidelines for the Final Papers](posts/final-papers.html).

## Email & Canvas: 

The teaching team will send announcements regularly by email.

Any non-personal questions related to the material in the course should be posted as a Canvas discussion.
Reserve email for personal or administrative matters.
Before posting, check that the question has not been asked and answered already.

It is often more efficient to answer questions in person, so try to ask them attend office hours.

## Resources

There are a couple places on campus that you can go to get additional statistical conulting

- CSSCR has a drop-in statistical consulting center in Savery 119. They provide consulting on statistical software, e.g. R. http://csscr.washington.edu/consulting.html
- CSSS Statistical Consulting provides general statistical consulting (questions about your research project).
  You can find their hours and locations on thier [site](https://www.csss.washington.edu/Consulting/).


# Texts

### Required

- Fox, John. 2008. *Applied Regression Analysis and Generalized Linear Models*. 2nd edition. Los Angeles: SAGE Publications, Inc.
- Gelman, Andrew, and Jennifer Hill. 2006. *Data Analysis Using Regression and Multilevel/Hierarchical Models*. 1st edition. Cambridge ; New York: Cambridge University Press.
- Angrist, Joshua D., and Jörn-Steffen Pischke. 2009. *Mostly Harmless Econometrics: An Empiricist’s Companion*. 1st edition. Princeton: Princeton University Press. (UW library eBook available)

### Optional

- Zuur, Alain, Elena N. Ieno, and Erik Meesters. 2009. *A Beginner’s Guide to R*. Springer. (UW library eBook available)

# Schedule and Topics

**Warning:** The uncertainty on this schedule is high.
Given that this is my first time teaching this course, I expect many adjustments to be made.

## Week 1: Introduction to 503 and R, Math Review

### Tuesday, March 31

- [Deck 1](lectures/Lecture_01_handout.pdf
)
- In-class R Markdown example analysis: [Rmd source](docs/Challenger_Analysis.Rmd), [html output](docs/Challenger_Analysis.html)
- Readings
    - Gelman and Hill, Ch 2
    - Fox, Ch 2, 3 (review)

### Friday, April 3

- Lab document: [.Rmd](labs/lab1.Rmd), [html](labs/lab1.html)
- Data: [gapminder.csv](labs/gapminder.csv)
- Readings:

    - Data Camp "Introduction to R", <https://www.datacamp.com/courses/free-introduction-to-r>
    - Wickham, Hadley. 2010. ``A Layered Grammar of Graphics.'' *Journal of Computational and Graphical Statistics* 19(1): 3–28. <http://dx.doi.org/10.1198/jcgs.2009.07098>

## Week 2: Assumptions & Properties of the Linear Regression Model, Part I

### Tuesday, April 7

- [Deck 2](lectures/Lecture_02_handout.pdf)
- Readings: 
    - Matrix algebra readings. Read **any** of the following
        - Moore, Will H., and David A. Siegel. 2013. *A Mathematics Course for Political and Social Research*. 1st edition. Princeton, NJ: Princeton University Press, Chapter 12 (on Canvas).
        - Kevin Quinn's matrix algebra [handout](docs/matrix.pdf)
        - [CSSS Math Camp Lectures](http://www.csss.washington.edu/MathCamp/Lectures/) Section 4
	- Fox, Ch. 5, 9.1--9.2

### Friday, April 10


- Readings:

    - Hadley Wickham, [Introduction to dplyr](https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)

## Week 3: Assumptions & Properties of the Linear Regression Model, Part II

### Tuesday, April 14

- [Deck 3](lectures/Lecture_03_handout.pdf)
- [Sampling Distribution of linear regression example](https://raw.githubusercontent.com/POLS503/pols_503_sp15/master/lectures/Sampling_Distribution_Linear_Regression.Rmd)
- [Multiple regression coefficient anatomy](https://github.com/POLS503/pols_503_sp15/blob/master/lectures/multiple_regression_anatomy.R)
- Readings:

    - Fox, Ch. 6, 9.3
	
### Friday, April 17

[Problem Set 1](hw/hw1.html) Due

- Lab document: [.Rmd](labs/lab3.Rmd), [html](labs/lab3.html)
- Readings

    - Wickham, Hadley. 2014. ``Tidy Data.'' *Journal of Statistical Software* 59(10). <http://www.jstatsoft.org/v59/i10/>.
    - Hadley Wickham, [tidyr vignette](https://cran.rstudio.com/web/packages/tidyr/vignettes/tidy-data.html)

## Week 4: Statistical Inference / Interpretation of the Linear Model

### Tuesday, April 21

- [Deck 4](lectures/Lecture_04_handout.pdf)
- Using R with the Duncan occupational prestige data [.Rmd](docs/Duncan_regression_example.Rmd), [.html](docs/Duncan_regression_example.html)
- Readings:

    - Fox, Ch. 6, 9.3


### Friday, April 24

- Lab document: [.Rmd](labs/lab4.Rmd), [html](labs/lab4.html)
- Data: [ross_2012.csv](data/ross_2012.csv)
- Readings

    - **stargazer** [vignette](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf)
	- **broom** [vignette](https://cran.r-project.org/web/packages/broom/vignettes/broom.html)

## Week 5: Model Fitting and Data Transformation

### Tuesday, Apr 28

[Problem Set 2](hw/hw2.html) Due

- More on p-values and significance testing

    - Nuzzo, Regina. 2014. ``Scientific Method: Statistical Errors.'' *Nature* 506(7487): 150–52. <http://www.nature.com/doifinder/10.1038/506150a>

- Omitted Variable Bias

    - [Omitted Variable Bias handout](handout/OVB_Measurement_Error.pdf)
    - Fox, Ch. 6.3
    - Agrist and Pischke, Ch. 3.2.2


- Heteroskedasticity and misspecification

    - [OLS Residuals deck](lectures/Lecture_OLS_Residuals_presentation.pdf)
    - Fox, Ch 12.1--12.3
    - Angrist and Pischke, Ch. 8
	- King, Gary, and Margaret E. Roberts. 2015. ``How Robust Standard Errors Expose Methodological Problems They Do Not Fix, and What to Do About It.'' Political Analysis 23(2): 159–79. <http://pan.oxfordjournals.org/content/23/2/159>

- Mispecification, Transforming Covariates

    - See Christopher Adolph's slides from 503 last year <http://faculty.washington.edu/cadolph/503/topic5.pw.pdf>
    - Fox, Ch. 4, Ch 17.1--17.3
	- Gelman and Hill, Ch. 4.1--4.3, 4.5-4.6
	- Brambor, Thomas, William Roberts Clark, and Matt Golder. 2006. “Understanding Interaction Models: Improving Empirical Analyses.” Political Analysis 14(1): 63–82. <http://pan.oxfordjournals.org/content/14/1/63>
	
### Friday, May 1

- Lab document: [.Rmd](labs/lab5.Rmd), [html](labs/lab5.html)

## Week 6: Measurement Error, Transformations

### Tuesday, May 5

We covered substantive vs. statistical influence; transformations; and measurement error.
You will need to cover collinearity, unusual and influential data, and robust regression on your own.

- The perils of stargazing:
    - Christopher Adolph, "Inference and Interpretation of Linear Regression", POLS 503, Spring 2014. <http://faculty.washington.edu/cadolph/503/topic4.pw.pdf#page=77>, pg. 77--.

- Transformations of Variables

    - Lecture deck: [.pdf](lectures/Lecture_Transformation_handout.pdf)

- Measurement error

    - Lecture deck: [.pdf](lectures/Lecture_Measurement_Error_handout.pdf)
    - Fox. Ch. 6.4
	
- Collinearity

    - Fox Ch 13 (13.1, skim the rest)


### Friday, May 8

Lab notes: [.html](http://UW-POLS503.github.io/pols_503_sp15/labs/lab6.html), [.Rmd](http://pols503.github.io/pols_503_sp15/labs/lab6.Rmd)

## Week 7: Interpretation, Model Selection

### Tuesday, May 12

[Problem Set 3](hw/hw3.html) Due

- [Deck on Model Specification and Fit](lectures/Lecture_Model_Specification_handout.pdf)
- Life Expectancy Example: [.html](docs/Life_Expectancy_Example.html), [.Rmd](docs/Life_Expectancy_Example.Rmd)
- Interpretation of regression

    - King, Gary, Michael Tomz, and Jason Wittenberg. 2000. ``Making the Most of Statistical Analyses: Improving Interpretation and Presentation." *American Journal of Political Science* 44(2): 347–61. <http://www.jstor.org/stable/2669316>.

- Bootstrapping

    - Fox, Ch 21

- Model Selection and Cross-Validation

    - Fox, Ch 22

### Friday, May 15

Lab notes: [.html](labs/lab7.html), [.Rmd](labs/lab7.Rmd)

## Week 8: Causal Inference I

### Tuesday, May 19

- Unusual and Influential Data and Robust Regression

    - Worked Example [Rmd](docs/Outliers_Robust_Regression.Rmd), [html](docs/Outliers_Robust_Regression.html)
    - Christopher Adolph, "Outliers and Robust Regression Techniques", POLS 503, Spring 2014. <http://faculty.washington.edu/cadolph/503/topic6.pw.pdf>
	- Readings
	
        - Fox, Ch 11, 19

- Missing Data

    - Slides on Missing Data [pdf](lectures/Lecture_Missing_Data_handout.pdf)
	- Worked Example [Rmd](docs/Imputing_Missing_Data.Rmd), [html](docs/Imputing_Missing_Data.html)
	- Readings:
	
        - King, Gary, James Honaker, Anne Joseph, and Kenneth Scheve. 2001. "Analyzing Incomplete Political Science Data: An Alternative Algorithm for Multiple Imputation." American Political Science Review 95: 49–69. Copy at http://j.mp/1zTTZUT

- Miscellaneous Thoughts on the state of quantitative analysis in political science. *We did not talk about this, but these are good readings*

    - Schrodt, Philip A. 2014. ``Seven Deadly Sins of Contemporary Quantitative Political Analysis.'' *Journal of Peace Research* 51(2): 287–300. <http://jpr.sagepub.com/content/51/2/287>
	- Achen, Christopher H. 2002. ``Toward a New Political Methodology: Microfoundations and ART.'' *Annual Review of Political Science* 5(1): 423–50. <http://dx.doi.org/10.1146/annurev.polisci.5.112801.080943>. Read the part on a "Rule of Three"; skim other parts.
	- Achen, Christopher H. 2005. ``Let’s Put Garbage-Can Regressions and Garbage-Can Probits Where They Belong.'' *Conflict Management and Peace Science* 22(4): 327–39. <http://cmp.sagepub.com/content/22/4/327> *skim*


### Friday, May 22

Lab document: [html](labs/lab8.html), [Rmd](labs/lab8.Rmd)

## Week 9: Causal Inference II

### Tuesday, May 26

**Problem Set 4 Due**

- Limited Dependent Variables

    - Slides on the Linear Probability Model and Logit [pdf](lectures/Lecture_Binary_Dependent_Variables_handout.pdf)

- Potential outcomes framework, regression, matching

    - Slides on Casual Inference [pdf](lectures/Lecture_Causal_Inference_handout.pdf)
    - Readings

		- Gelman and Hill, Ch 9--10. 
		- Angrist and Pischke, Ch 1--3.
		- Angrist, Joshua D., and Jorn-Steffen Pischke. 2010. ``The Credibility Revolution in Empirical Economics: How Better Research Design Is Taking the Con out of Econometrics.'' *Journal of Economic Perspectives* 24(2): 3–30. <http://www.aeaweb.org/articles.php?doi=10.1257/jep.24.2.3> *For background reading*


### Friday, May 29

Lab document: [Rmd](labs/lab9.Rmd), [html](labs/lab9.html)

## Week 10: Panel Data 

### Tuesday, June 2

- Slides on Panel data: [pdf](lectures/Lecture_Panel_Data_handout.pdf)
- Example with R code: [html](docs/panel.html), [Rmd](docs/panel.Rmd)
- Readings

    - Angrist and Pischke, Ch. 5, 8.2
    - Peter Kennedy, *A Guide to Econometrics*, 6th edition, Chapter 18, "Panel Data". On Canvas.
	- Wooldridge, *Econometric Analysis of Cross Section and Panel Data*, Chapter 10, "Basic Linear Unobserved Effects Panel Data Models". On Canvas.
	- Beck, N., & Katz, J. N. (2011). Modeling Dynamics in Time-Series–Cross-Section Political Economy Data. *Annual Review of Political Science*, 14(1), 331–352. <http://doi.org/10.1146/annurev-polisci-071510-103222>


### Friday, June 5

Open office hours to answer questions about your papers


* * * 

Syllabus derived from Christopher Adolph. (Spring 2014). *POLS/CSSS 503: Advanced Quantitative Political Methodology* [Syllabus]. University of Washington. http://faculty.washington.edu/cadolph/503/503.pdf [CC-BY-SA](https://creativecommons.org/licenses/by-sa/2.0/).

<!--  LocalWords:  UTC td href TAs García SAV gelman angrist UW eBook
 -->
<!--  LocalWords:  zuur Brambor TBD CSSS img src 221B Tu faq html
 -->
<!--  LocalWords:  wickham brambor
 -->
