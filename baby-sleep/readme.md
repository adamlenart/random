# Visualizing sleeping times of a 6-8 month old infant



Sharing the bed again with my six-month-old son increased his night time
sleeping hours and reduced the wake up counts. He slept especially
poorly between four and six months of age. My wife and I tried different
strategies recommended by experts on infant sleep. To measure the
efficiency of these strategies, we decided to record his sleeping times, and implemented them in an interrupted time series
design.

![sleeptime](https://github.com/adamlenart/rstuff/blob/master/baby-sleep/images/sleep.png)

Here I will concentrate only on the impact of co-sleeping on the time
spent sleeping and awake during the night.

|Statistic| Days |Mean | St. Dev.| Min| Max|
|---------|------|-----|---------|----|----|
|night (hours) | 76 | 9.345 | 1.038 | 7.52 | 11.85 |
|total (hours) | 76 | 13.080 | 1.235 | 9.96 | 15.77 |
|night wake ups (count) | 76 | 5.947 | 2.343 | 2 | 12|


After 58 days of trial and error, we still did not see any improvement in his sleeping time as the series did not show either a positive and significant drift or trend term. Moreover, the autocorrelation and partial autocorrelation functions showed that an ARIMA(0,0,0) model, or
white noise, would provide the best fit to the night time sleep data. Interestingly, the log-ratio of total hours of sleep to total wakeful hours during the whole day, showed a negative autocorrelation at the first lag, hinting at that in general, even if the night time sleeping hours were independent of the hours slept during the night the day before, my son might have offset short night time sleep with longer naps the day after.

On the day, we started to share our bed with him, and immediately felt an improvement. This impression was corroborated by the the following 17 days of records which showed that the ratio of hours slept to hours spent awake during the night increased on average 2.33 times

![nightday-table](https://github.com/adamlenart/rstuff/blob/master/baby-sleep/tables/table_nightday.png)


and the number of wake up times decreased by 56%.

![night-wakeups](https://github.com/adamlenart/rstuff/blob/master/baby-sleep/tables/table_wakeups.png)



#References
John Aitchison. The statistical analysis of compositional data. Journal of the Royal Statis-
tical Society. Series B (Methodological), pages 139–177, 1982.

Vera Pawlowsky-Glahn and Juan José Egozcue. Compositional data and their analysis: an introduction. Geological Society, London, Special Publications, 264(1):1–10, 2006.


[^1]: For simplicity, define night as the span of time between 6.30 pm
    and 6.30 am. Due to this definition, night time sleep and wakeful
    time during the night have a bounded support on $[0,12] $ and sum up
    to 12. By taking the logarithm of the ratio of night time sleep to
    wakeful time these two components are transformed to a univariate
    time series defined in $(-\infty,\infty)$. In this case, a logit
    transformation would have also worked because we have only a
    two-part composition but Aitchison’s solution
    (Aitchison 1982) is valid for higher dimensional data as
    well (Pawlowsky-Egozcue 2006).
