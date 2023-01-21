# SleepApp
Useful application to suggest wake/sleep times for adolscents. Functions of this application include mechanisms for visualizing given data and the use of inforgraphics (such as bar graphs). 

The minimum R version required is 3.0 (R >= 3.0). Currently (1/21/23), this is supported by RStudio and related IDEs. R 4.2.1 is compatible as well. Several packages are used:

1. shiny
2. shinythemes
3. suncalc
4. ggplot2
5. maptools
6. lubridate
7. lutz
8. tidygeocoder
9. tibble
10. dplyr
11. deSolve
12. sf
13. tidyverse
14. scales
15. plotly

Several trials and runs have shown unefficient time complexity with a runtime excedding 5 mins or so. It is likely (for sample.R) that time complexity surpasses O(2n^2). For this reason, a progess bar will be implemented in the newest updates. Shiny "reactivity" may also have some problems. 


