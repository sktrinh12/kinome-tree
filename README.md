# kinome-tree
The app is borrowed from the Phanistel lab at UNC
Running app - http://phanstiel-lab.med.unc.edu/CORAL/
Code repo - https://github.com/dphansti/CORAL


setup difference libraries for different users/apps - https://shiny.rstudio.com/articles/libraries.html

create .Rprofile in /home/username/ direcotry with .libPaths("/path/to/libs")
```
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cloud.r-project.org"
    options(repos = r)
})
```

- currently now, another `.Rprofile` in the kinome-tree app directory that has `.libPaths("/usr/lib/R/addn-lib")` to point to the depdencies
- this was originally setup in order to prevent conflicts in library installs for different R-Shiny apps

#### location of the app
/srv/shiny-server/kinome-tree

#### location of logs
/var/log/shiny-server

#### location of config file
/etc/shiny-server/shiny-server.conf 

#### different library paths 
- add route and set run as user
- doesn't seem to work; so the `.Rprofile` in the main directory of the app works
```
location /PK_plots {
      run_as shinykinome;
    }
```

At the moment `shinykinome` user is being used for both `PK_plots` and `kinome-tree` app. 
kinome-tree app has `.Rprofile` pointing to `"/usr/lib/R/addn-lib"` and `PK_plots` pointing to `"/home/shinykinome/R/library"`

`install.packages("PACKAGE_NAME", lib = "/home/shinykinome/R/library")` for any new packages


#### Docker container
To run in a docker container and use of `renv` package to manage all
depedencies, ensure to ignore following packages:
```
renv::settings$ignored.packages(c("rJava", "renv", "DBI", "RJDBC"), persist = FALSE)
renv::snapshot()
```
Run this in RStudio within the working directory. This way, `renv` will ignore installing these when restoring, then the
`Dockerfile` will install these packages manually. At the time of developing this
workflow, this was the only work-around since installing using `renv` failed for
these packages. `.Rprofile` is not longer needed since each shiny app will be
self-contained within its own environment. 
