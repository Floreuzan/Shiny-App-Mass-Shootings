# Data

`Mass Shootings in America` is the focus of the Stanford Mass Shootings of America (MSA) data project that began in 2012, in reaction to the mass shooting in Sandy Hook, CT. The dataset contains a non-exhaustive list of US Mass Shootings from 1966 to 2016. The definition of mass shooting used for the Stanford database is 3 or more shooting victims (not necessarily fatalities), not including the shooter. The shooting must not be identifiable gang, drug, or organized crime related.

# Visualization

<li> The Shiny_App_1 and Shiny_App 2 attempt to identify the profile of the shooter and their personal motivation. The profile of the shooter is defined by its age, its sex, its race, if they have military experience and any history of mental illness. </li> 

<li> The first shiny app gives the bar charts of the type of place and the possible motivation by shooter’s profile. The barcharts can either be in group or stacked barmode. </li>

<li> The second shiny app displays at the relationship of the characteristics of the shooter profile. For each plot, you can select different indicators: Number of Victims or Number of Fatalities.  </li>

<li> For the parallel coodinate plot, we add some noise in order to add some variance and spread the point around each response. The data can be can standardized on different scales (globalminmax, std, uniminmax). You can hover over the points to view a tooltip showing more detailed information.</li>

<li> The interaction boxplot can be ordered by values. You can select the shooter's profile, the characteristic of the mass shooting and the indicator.</li>

<li> The pie chart diplays the Type of Place, the Motivation or Relationship to Incident of the selected indicator.</li>

<li> Finally, the scatter plot gives the number of mass shooting in the US over the year for a selected characteristic of the shooter's profile.  You can hover over the points to view a tooltip showing the Date, Location, Total Victims, Profile. </li>

# Conclusions

<li> California has the strictest firearm laws in the nation but also the highest number of mass shooting incidents, but if we also consider the population of the state, the number of mass shooting deaths per population in California is not that high. </li>

<li> Semi-Automatic handguns have been involved in mass shootings, and if we ignore spree kills, more people die from semi-automatic handguns - especially in FMS or family murder suicides. </li>

<li> The profile of the murderer is a White Male around 31 years with history of mental illness and with military experience. The main motivation is after a dispute, which mostly occur in a residential area. </li>
