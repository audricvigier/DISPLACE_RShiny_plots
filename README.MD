# Contents

This repository is a fork of frabas/DISPLACE_RShiny_plots. It contains scripts to read outputs of DISPLACE simulations (RTI Celtic Sea application). data, info, output and www repositories were present in the original repository, but are not used here. For more info on the study case, instead of browsing info, please read the documentation: https://www.overleaf.com/read/nrryshjnjcnd . For any other request, please contact me: audric.vigier@mailo.com

All scripts of use are in R-scripts and main directories. Most of my work was done on app.R, useDisplacePlots.R, and R-scripts/getXXX.R. All other scripts were provided by François, some of which are still used. R-scripts/getXXX.R provide R functions to condition DISPLACE outputs, and create some plots. They are called (among others) by useDisplacePlots.R (conditionning) and app.R (plotting in the R Shiny app)

# How to use

These scripts should sed once the DISPLACE simulation runs are done. Scripts of fork audricvigier/displaceplot are also called (minor alterations compared to the original repo). In our study case, we wanted to have a deterministic model (though that's not the case yet), and hence this app was designed for deterministic outputs. 2 scripts are to be executed by the user:

-1/ Use useDisplacePlots.R . In the general object, say for which simulations the outputs should be conditionned. Depending on whether it was for a calibration procedure or a single run, the file names are not the same, hence, it has to be stated in the calib argument.
2 scripts : useD to condition the outputs, and app, with the Shiny app. Some functions of the first conditionning chunk are not functionnal anymore, so don't worry too much about it. Because the outputs are stored in big files, heavy objects have to be manipulated in this script, which increases the computation time. Regarding other conditionning chunks, they are wirtten as for loops. For 11-year runs, depending on your computer, running these chunks maybe challenging. Either improve the way outputs are processed, or execute the code iteartion by iteration (with a clean-up of the workspace between each). For 3-year runs, the execution of the code should be fine.

-2/ Once this is done, app.R, containing the R Shiny app, can be executed (set up its general object according to what you want to plot in the Shiny app). If you want to further develop it, be careful about the time needed to generate a map, especially at the node scale (sometimes only RTI and ICES rectangles scales are proposed as node scale maps are just too long to generate).