# Plot a word cloud for speeches given before the year 1800
textplot_wordcloud(dfm_trimmed[docvars(dfm_trimmed, "year") < 1800, ])

# Plot a word cloud for speeches given after the year 2000
textplot_wordcloud(dfm_trimmed[docvars(dfm_trimmed, "year") > 2000, ])
