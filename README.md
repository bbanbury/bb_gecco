GECCO -- Installing R Packages
==========
![](http://www.fredhutch.org/en/labs/phs/projects/cancer-prevention/projects/gecco/_jcr_content/par/textimage/image.img.jpg/1401223545236.jpg) 

A suite of R code for the GECCO working group. 


##Installing packages

GECCO is a private repository, so you will need to authorize your computer to be able to use it (or even see it). Before you can download, you will need Keith to add you as a user and then go to [github applications](https://github.com/settings/applications) to generate a personal access token (=PAT). Follow the instructions for generating a new token leaving the default check boxes. Then copy the token once it is generated, this is your new secret computer generated passcode.  You will now either need to pass this as a variable in your install, or you can set it as an environment variable for permanent use.  For example,
```r
Sys.setenv(GITHUB_PAT="yoursecretcode")
```

This variable will not save when you quit R (unless you load up previous workspaces), so you will need to access it on github or store it somewhere.  Alternatively, you could set it as a permanent key by creating a system preference file (see System()).  

Then you can install devtools (if you haven't already) and any of the packages. 
```r
install.packages("devtools")
devtools::install_github("FredHutch/GECCO", subdir="R-packages/gecco")
library(gecco)
```

Note that the above install only works if you have set your GITHUB\_PAT environmental variable. If you don't wish to do so, then you can still pass the token to install_github using the argument auth_token. 

