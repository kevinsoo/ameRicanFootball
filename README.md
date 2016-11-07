# ameRicanFootball
#### *Author: Kevin Soo*

This is a project (written in `R`) for scraping, visualizing, and analyzing NFL data. The data used here comes from the [Pro Football Reference](http://www.pro-football-reference.com/). I don't know much about American Football (I prefer [soccer](https://github.com/kevinsoo/socceRstuff)), so I'm getting help from [Evelyn Yarzebinski](https://github.com/evementen) to ask and answer questions of interest.

## QB data
`scrapeQB.R` is the script used to scrape QB data, which is then saved as a data frame; `QBStats.Rda`. I've included only passing stats. For simplicity, I only included data from QBs in the last 20 seasons, and who played for at least 3 seasons. Provided the source is updated, running the script at any given time should give you up-to-date statistics on active quarterbacks.
