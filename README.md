==nwac_tool==
A lil' script written in R/shiny to plot the telemetry data from multiple stations concurrently.

==todo==
*Automagically clean up noisy data source (eg, total snow depth).  I think we'll have to do some model-based clustering and pick the more reasonable (closer to 100 inches) if more than one cluster is selected.  Some stations report the ultrasonic offset more often than they report the true depth.
For other series, just killing the spikes by computing 8 hour median absolute deviations and looking for outliers might do the trick.

*Maybe eventually it will include Snowtel data as well.

Contributions welcome.
