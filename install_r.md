<!--
.. title: Installing R and RStudio
-->

This course will require the following software:

1. R. The programming language and interpreter that actually runs the code
2. RStudio. An [IDE](http://en.wikipedia.org/wiki/Integrated_development_environment), i.e. a nice interface that makes working with R much easier.
3. Some tools to build R packages from source since we may be installing some bleeding edge packages.
3. LaTeX (optional). Needed to compile PDFs from R Markdown

# Installing R

The R website is ... special. Since you probably don't know or care what a mirror is, here are links to the 

- Ubuntu: `sudo apt-get install r-base-dev` (in order to be able to install other packages from source).


# Installing RStudio

You want to download the RStudio Desktop version. Download it from <http://www.rstudio.com/products/rstudio/download/>

# Installing Tools to Build R Packages from Scratch

Mature R packages are distributed over CRAN and installable in R using `install.packages` without any other assistance.
However, we may use some bleeding-edge R packages that require the packages to be built from source and installed using the [devtools](http://www.rstudio.com/products/rpackages/devtools/) package.
This requires some extra tools (unless you are using Linux, which is why you should be using Linux).

- Windows: Install the newest version of `Rtools*.exe` from http://cran.r-project.org/bin/windows/Rtools/
- Mac: Install Xcode from the Mac App Store.
- Ubuntu: The tools to build packages from source should have been installed with `r-base-dev`.

Or follow the instructions [here](http://www.rstudio.com/products/rpackages/devtools/).


# Installing LaTeX

LaTeX is needed to create PDFs from R Markdown files. Follow the instructions below:

- Windows: <http://miktex.org/download> 
- OSX: <https://tug.org/mactex/>
- Ubuntu: `apt-get install texlive` . This should have been installed with `r-base-dev`.

