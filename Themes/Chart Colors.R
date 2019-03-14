####Themes, colors and other such values#####

##Colors####

##Blue spectrum DPAP Year, sperate agencies
 
cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=5))

## Spectrum One specific topic by DPAP by Year
    #Health IT
cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))



##Government-Wide quarter by quarter, Civ vs Def

values <- brewer.pal(9, "YlOrRd")[c(1,3,5,7)]

##Quarter by quarter, Products vs Services

values <- brewer.pal(9, "Blues")[c(1,3,5,7)]

##Quarter by quarter Agency Comparisons

    #Health Agencies
values <- brewer.pal(9, "GnBu")[c(1,3,5,7)]