# team_quandt_internal
repository for our internal workshop

# folder structure and guidelines
I propose the following folder structure and guidelines:
- continuous numbering by order of workshops (e.g. the first workshop folder starts with '01-...' etc.) plus a short descriptive name (e.g. 01-intro for the first workshop folder etc.)
- within the workshop folder a folder for each programming language used and scripts within that
- a 'data' folder for every workshop
- create short data examples within the script and use publicly available/downloadable datasets wherever possible to keep the repo size down (e.g. iris data set for machine learning or cars dataset for regression)
- if larger datasets are required (e.g. for images or large text corpora), host them on cloud services (please respect copyrights) or distribute them beforehand
- feel free to add something to the README.md file or send it to me and I will add it

# 12.06.2023
## prerequisites:
### first read the steps below
- if you want to continue to use R/Python I recommend installing everything below (but you can also totally do that after the workshop :) )
  - for the workshop I recommend to install R/R-Studio but to use colab for python (because we focus on R)
- if you want to install stuff later and want to start coding directly look at the colab instructions
  - be aware of the following caveats regarding colab:
    - it is a google product, you need a google account
    - it can be quite slow compared to a local install
    - it requires a stable internet connection
- If you have any questions on the installation or want to troubleshoot, feel free to contact me beforehand or afterwards

### if you **don't** yet have R and RStudio, start *here*
- download and install R from [CRAN](https://cran.r-project.org/):
  - MacOS ([main download page](https://cran.r-project.org/bin/macosx/)):
    - [Apple Silicon (M1/M2)](https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.3.0-arm64.pkg)
    - [older intel macs](https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.3.0-x86_64.pkg)
  - Windows ([main Download page](https://cran.r-project.org/bin/windows/base/)):
    - [R for windows](https://cran.r-project.org/bin/windows/base/R-4.3.0-win.exe)
- download and install R-Studio from [posit](https://posit.co/downloads/):
  - [MacOS](https://download1.rstudio.org/electron/macos/RStudio-2023.03.1-446.dmg) (this should work for all newer MacOS distributions)
  - [Windows](https://download1.rstudio.org/electron/windows/RStudio-2023.03.1-446.exe)

### if you **don't** yet have Python and Anaconda, continue *here*
- download and install Anaconda from [here](https://www.anaconda.com/download#downloads)
  - MacOS:
    - [M1](https://repo.anaconda.com/archive/Anaconda3-2023.03-1-MacOSX-arm64.pkg)
    - [older intel macs](https://repo.anaconda.com/archive/Anaconda3-2023.03-1-MacOSX-x86_64.pkg)
  - [Windows](https://repo.anaconda.com/archive/Anaconda3-2023.03-1-Windows-x86_64.exe)

### using colab instead:
- if you don't have one, create a google account
- links to the colabs used in the workshop will be posted here later

## workshop structure
1. we will start with very basic R programming to get everyone up to speed and/or repeat the basics (esp. functions an loops) and get through that according to the groups proficiency with R
2. A short introduction into the "tidyverse", a widely used data handling environment package
3. Some extended work with dataframes follows, including inspection, cleaning, filtering and light visualization
4. We do the equivalents of the above in Python
5. If we have time, we will look into the basics of git and discuss possible workflows and where to go from here
