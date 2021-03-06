# Data

`Mass Shootings in America` is the focus of the Stanford Mass Shootings of America (MSA) data project that began in 2012, in reaction to the mass shooting in Sandy Hook, CT. The dataset contains a non-exhaustive list of US Mass Shootings from 1966 to 2016. The definition of mass shooting used for the Stanford database is 3 or more shooting victims (not necessarily fatalities), not including the shooter. The shooting must not be identifiable gang, drug, or organized crime related.

# Visualization

* Shiny_App_1 and Shiny_App 2 attempt to identify the profile of the shooter and their personal motivation. The profile of the shooter is defined by its age, its sex, its race, if they have military experience and any history of mental illness.

  * The first shiny app gives the bar charts of the type of place and the possible motivation by a selected characteristic of the shooter’s profile. The barcharts can either be in grouped or stacked barmode.
  * The second shiny app displays the relationships of the characteristics of the shooter's profile. For each plot, you can select different indicators: Number of Victims or Number of Fatalities.

     * For the parallel coodinate plot, we add some noise in order to add some variance and spread the point around each response. The data can be can standardized on different scales (globalminmax, std, uniminmax). You can hover over the points to view a tooltip showing more detailed information.
     * The interaction boxplot can be ordered by values. You can select the shooter's profile, the characteristic of the mass shooting and the indicator.
     * The pie chart diplays the Type of Place, the Motivation or Relationship to Incident of the selected indicator.
     * Finally, the scatter plot gives the number of mass shooting in the US over the year for a selected characteristic of the shooter's profile.  You can hover over the points to view a tooltip showing the Date, Location, Total Victims, Profile.

* Shiny_App_3 is a visualization of the weapon analysis. 
 
  * The barchart gives the number of Type of Gun by Semi-Automatic and Non Semi-Automatic Guns. 
  * The line charts displays the Type of Gun Semi-Automatic and Non Semi-Automatic Guns over the years. You can the select a window in the bottom to zoom on a period of time.
  * When hovering over a category over the barchart, the summary statistics for Number of Fatalities from Mass Shooting Events by Gun Types is displayed in the table. You can reset it by double clicking in the barchart. It is server-side linking with shiny and plotly.
  
  
* Finally, we compare the number of incidents and fatalities of Mass Shooting deaths with gun deaths in general and the number of gun laws per state using cloropeths and barplots in plotly.

# Conclusions

<li> California has the strictest firearm laws in the nation but also the highest number of mass shooting incidents, but if we also consider the population of the state, the number of mass shooting deaths per population in California is not that high. </li>

<li> Semi-Automatic handguns have been involved in mass shootings, and if we ignore spree kills, more people die from semi-automatic handguns - especially in FMS or family murder suicides. </li>

<li> The profile of the murderer is a White Male around 31 years with history of mental illness and with military experience. The main motivation is after a dispute, which mostly occur in a residential area. </li>

