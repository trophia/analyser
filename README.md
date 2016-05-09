# analyser

### What and why

A R package for analysis of New Zealand fisheries catch and effort data. There is nothing particularly sophisticated here; it just helps streamline the workflow for taking data, analysing it and presenting the results. 

`analyser` is made up of a number of `Worker` modules that do certain jobs. Although there is a usual "template" for how these workers are arranged in an analysis, this modular approach allows for flexibility and customization of analyses. 

`analyser` is specifically designed to be used for New Zealand catch and effort data (e.g. expects a certain data structure and variable names) but some of the modules may be useful for other data. It is often used in conjunction with [`groomer`](https://github.com/trophia/groomer).

### History

`analyser` was initially developed in 2008 and became part of the "Tanga" family of tools used by [Trophia](http://www.trophia.com) for analyses of catch and effort data, (the other tools being `tangaviewer` and `tangagroomer`). It has been used extensively by some Trophia associates but has not had any major development since 2012. In May 2016, `tangaanalyser` was renamed `analyser`, switched from [Mercurial](http://mercurial.selenic.com/) to [Git](http://git-scm.com), and open sourced under the [GPLv3 licence](LICENCE.txt). At that time we began a complete rewrite of the code to make use of modern R packages. The original code has been retained for posterity in the `legacy` branch and is borrowed from where appropriate.
