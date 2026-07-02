
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAIVEtools

<!-- badges: start -->

<!-- badges: end -->

The goal of DAIVEtools is to support Decision Analysis for Intervention
Value Efficiency (DAIVE) by providing methods easily format data,
visualize results, and identify frontiers of efficiency for factorial
optimization trial data.

## Installation

You can install the development version of DAIVEtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("setheaton/DAIVEtools")
```

## Example

This is a basic example which shows how DAIVEtools can format posterior
draws data into a table of expected value:

``` r
library(DAIVEtools)

## combine draws dataframes into a list object
draws <- list(gpaDraws, actDraws)

## define a list of components and outcomes labels
components <- c("A", "B", "C")
outcomes <- c("GPA", "ACT")

## generate a frame with effects codes
codes <- DAIVEtools::get_codes(components)

## generate value summary table
expectedValue <- DAIVEtools::values_table(draws, outcomes, components, codes)
expectedValue
#>    A  B  C   names        GPA GPA.scale      ACT ACT.scale
#> 1 -1 -1 -1 All Off -0.6798174 0.4997223 19.48335 0.4811386
#> 2  1 -1 -1       A  0.6116712 0.6514479 20.93811 0.5412324
#> 3 -1  1 -1       B -0.8555110 0.4790816 23.88989 0.6631660
#> 4  1  1 -1      AB -0.1651793 0.5601826 27.24404 0.8017211
#> 5 -1 -1  1       C -2.0651978 0.3369662 15.63841 0.3223100
#> 6  1 -1  1      AC  2.1327076 0.8301410 14.69324 0.2832665
#> 7 -1  1  1      BC -1.5802862 0.3939342 25.54174 0.7314017
#> 8  1  1  1     ABC  0.1622224 0.5986461 24.08650 0.6712878
```

Values tables can be used to easily combine outcomes with value
functions and identify frontiers of efficiency:

``` r
## define weights for outcomes
weights <- c(100/160, 60/160)

## calculate a value function as a weighted sum
expectedValue$valueFunc <- DAIVEtools::weight_outcomes(expectedValue,
                                                    c("GPA.scale", "ACT.scale"), 
                                                    weights)
expectedValue
#>    A  B  C   names        GPA GPA.scale      ACT ACT.scale valueFunc
#> 1 -1 -1 -1 All Off -0.6798174 0.4997223 19.48335 0.4811386 0.4927534
#> 2  1 -1 -1       A  0.6116712 0.6514479 20.93811 0.5412324 0.6101171
#> 3 -1  1 -1       B -0.8555110 0.4790816 23.88989 0.6631660 0.5481133
#> 4  1  1 -1      AB -0.1651793 0.5601826 27.24404 0.8017211 0.6507595
#> 5 -1 -1  1       C -2.0651978 0.3369662 15.63841 0.3223100 0.3314701
#> 6  1 -1  1      AC  2.1327076 0.8301410 14.69324 0.2832665 0.6250631
#> 7 -1  1  1      BC -1.5802862 0.3939342 25.54174 0.7314017 0.5204845
#> 8  1  1  1     ABC  0.1622224 0.5986461 24.08650 0.6712878 0.6258867

## calculate alternative costs using component costs
expectedValue$cost <- rowSums(expand.grid(A=c(0, 70), B=c(0, 60), C=c(0,20)))

## identify a frontier
DAIVEtools::frontier(expectedValue, "valueFunc", "cost")
#>     names cost valueFunc
#> 1 All Off    0 0.4927534
#> 2       A   70 0.6101171
#> 6      AC   90 0.6250631
#> 4      AB  130 0.6507595
```

Values tables can also be used to easily visualize frontiers of
efficiency:

``` r
DAIVEtools::DAIVE_plot(expectedValue, "valueFunc", "cost", 
                       outs=c("Scaled GPA", "Scaled ACT"), weights=weights)
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-4dff834d8d99d4c6280f" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-4dff834d8d99d4c6280f">{"x":{"data":[{"x":[0.54811326435475904,0.33147014161105132,0.52048452460346084,0.62588672039128435],"y":[60,20,80,150],"text":["Name: B<br>Outcome: 0.548<br>Cost: 60<br>On Frontier: No","Name: C<br>Outcome: 0.331<br>Cost: 20<br>On Frontier: No","Name: BC<br>Outcome: 0.52<br>Cost: 80<br>On Frontier: No","Name: ABC<br>Outcome: 0.626<br>Cost: 150<br>On Frontier: No"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(160,32,240,1)","opacity":1,"size":9.4488188976377963,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(160,32,240,1)"}},"hoveron":"points","name":"FALSE","legendgroup":"FALSE","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.49275339285798819,0.61011708257978237,0.65075952626447386,0.62506305417064656],"y":[0,70,130,90],"text":["Name: All Off<br>Outcome: 0.493<br>Cost: 0<br>On Frontier: Yes","Name: A<br>Outcome: 0.61<br>Cost: 70<br>On Frontier: Yes","Name: AB<br>Outcome: 0.651<br>Cost: 130<br>On Frontier: Yes","Name: AC<br>Outcome: 0.625<br>Cost: 90<br>On Frontier: Yes"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":9.4488188976377963,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)"}},"hoveron":"points","name":"TRUE","legendgroup":"TRUE","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.49275339285798819,0.61011708257978237,null,0.49275339285798819,0.61011708257978237,null,0.49275339285798819,0.61011708257978237,null,0.49275339285798819,0.61011708257978237,null,0.49275339285798819,0.61011708257978237,null,0.49275339285798819,0.61011708257978237,null,0.49275339285798819,0.61011708257978237,null,0.49275339285798819,0.61011708257978237],"y":[0,70,null,0,70,null,0,70,null,0,70,null,0,70,null,0,70,null,0,70,null,0,70],"text":["Name: All Off<br>Outcome: 0.493<br>Cost: 0<br>On Frontier: Yes","Name: All Off<br>Outcome: 0.493<br>Cost: 0<br>On Frontier: Yes",null,"Name: A<br>Outcome: 0.61<br>Cost: 70<br>On Frontier: Yes","Name: A<br>Outcome: 0.61<br>Cost: 70<br>On Frontier: Yes",null,"Name: B<br>Outcome: 0.548<br>Cost: 60<br>On Frontier: No","Name: B<br>Outcome: 0.548<br>Cost: 60<br>On Frontier: No",null,"Name: AB<br>Outcome: 0.651<br>Cost: 130<br>On Frontier: Yes","Name: AB<br>Outcome: 0.651<br>Cost: 130<br>On Frontier: Yes",null,"Name: C<br>Outcome: 0.331<br>Cost: 20<br>On Frontier: No","Name: C<br>Outcome: 0.331<br>Cost: 20<br>On Frontier: No",null,"Name: AC<br>Outcome: 0.625<br>Cost: 90<br>On Frontier: Yes","Name: AC<br>Outcome: 0.625<br>Cost: 90<br>On Frontier: Yes",null,"Name: BC<br>Outcome: 0.52<br>Cost: 80<br>On Frontier: No","Name: BC<br>Outcome: 0.52<br>Cost: 80<br>On Frontier: No",null,"Name: ABC<br>Outcome: 0.626<br>Cost: 150<br>On Frontier: No","Name: ABC<br>Outcome: 0.626<br>Cost: 150<br>On Frontier: No"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.61011708257978237,0.62506305417064656,null,0.61011708257978237,0.62506305417064656,null,0.61011708257978237,0.62506305417064656,null,0.61011708257978237,0.62506305417064656,null,0.61011708257978237,0.62506305417064656,null,0.61011708257978237,0.62506305417064656,null,0.61011708257978237,0.62506305417064656,null,0.61011708257978237,0.62506305417064656],"y":[70,90,null,70,90,null,70,90,null,70,90,null,70,90,null,70,90,null,70,90,null,70,90],"text":["Name: All Off<br>Outcome: 0.493<br>Cost: 0<br>On Frontier: Yes","Name: All Off<br>Outcome: 0.493<br>Cost: 0<br>On Frontier: Yes",null,"Name: A<br>Outcome: 0.61<br>Cost: 70<br>On Frontier: Yes","Name: A<br>Outcome: 0.61<br>Cost: 70<br>On Frontier: Yes",null,"Name: B<br>Outcome: 0.548<br>Cost: 60<br>On Frontier: No","Name: B<br>Outcome: 0.548<br>Cost: 60<br>On Frontier: No",null,"Name: AB<br>Outcome: 0.651<br>Cost: 130<br>On Frontier: Yes","Name: AB<br>Outcome: 0.651<br>Cost: 130<br>On Frontier: Yes",null,"Name: C<br>Outcome: 0.331<br>Cost: 20<br>On Frontier: No","Name: C<br>Outcome: 0.331<br>Cost: 20<br>On Frontier: No",null,"Name: AC<br>Outcome: 0.625<br>Cost: 90<br>On Frontier: Yes","Name: AC<br>Outcome: 0.625<br>Cost: 90<br>On Frontier: Yes",null,"Name: BC<br>Outcome: 0.52<br>Cost: 80<br>On Frontier: No","Name: BC<br>Outcome: 0.52<br>Cost: 80<br>On Frontier: No",null,"Name: ABC<br>Outcome: 0.626<br>Cost: 150<br>On Frontier: No","Name: ABC<br>Outcome: 0.626<br>Cost: 150<br>On Frontier: No"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.62506305417064656,0.65075952626447386,null,0.62506305417064656,0.65075952626447386,null,0.62506305417064656,0.65075952626447386,null,0.62506305417064656,0.65075952626447386,null,0.62506305417064656,0.65075952626447386,null,0.62506305417064656,0.65075952626447386,null,0.62506305417064656,0.65075952626447386,null,0.62506305417064656,0.65075952626447386],"y":[90,130,null,90,130,null,90,130,null,90,130,null,90,130,null,90,130,null,90,130,null,90,130],"text":["Name: All Off<br>Outcome: 0.493<br>Cost: 0<br>On Frontier: Yes","Name: All Off<br>Outcome: 0.493<br>Cost: 0<br>On Frontier: Yes",null,"Name: A<br>Outcome: 0.61<br>Cost: 70<br>On Frontier: Yes","Name: A<br>Outcome: 0.61<br>Cost: 70<br>On Frontier: Yes",null,"Name: B<br>Outcome: 0.548<br>Cost: 60<br>On Frontier: No","Name: B<br>Outcome: 0.548<br>Cost: 60<br>On Frontier: No",null,"Name: AB<br>Outcome: 0.651<br>Cost: 130<br>On Frontier: Yes","Name: AB<br>Outcome: 0.651<br>Cost: 130<br>On Frontier: Yes",null,"Name: C<br>Outcome: 0.331<br>Cost: 20<br>On Frontier: No","Name: C<br>Outcome: 0.331<br>Cost: 20<br>On Frontier: No",null,"Name: AC<br>Outcome: 0.625<br>Cost: 90<br>On Frontier: Yes","Name: AC<br>Outcome: 0.625<br>Cost: 90<br>On Frontier: Yes",null,"Name: BC<br>Outcome: 0.52<br>Cost: 80<br>On Frontier: No","Name: BC<br>Outcome: 0.52<br>Cost: 80<br>On Frontier: No",null,"Name: ABC<br>Outcome: 0.626<br>Cost: 150<br>On Frontier: No","Name: ABC<br>Outcome: 0.626<br>Cost: 150<br>On Frontier: No"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":34.59692818596929,"r":26.567040265670411,"b":52.669157326691582,"l":54.263179742631799},"font":{"color":"rgba(0,0,0,1)","family":"","size":17.268576172685762},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.31550567237838018,0.66672399549714501],"tickmode":"array","ticktext":["0.4","0.5","0.6"],"tickvals":[0.40000000000000002,0.5,0.60000000000000009],"categoryorder":"array","categoryarray":["0.4","0.5","0.6"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.3171440431714405,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.814860938148611},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.78493528057662576,"zeroline":false,"anchor":"y","title":{"text":"Value = 0.625 * (Scaled GPA) + 0.375 * (Scaled ACT)","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402241}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-7.5,157.5],"tickmode":"array","ticktext":["0","50","100","150"],"tickvals":[0,50.000000000000007,100,150],"categoryorder":"array","categoryarray":["0","50","100","150"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.3171440431714423,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.814860938148611},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.78493528057662576,"zeroline":false,"anchor":"x","title":{"text":"Cost","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402241}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":13.814860938148611}},"hovermode":"closest","barmode":"relative","annotations":[{"text":"All Off","x":0.49275339285798819,"y":0,"xanchor":"left","xshift":8,"showarrow":false,"font":{"size":12,"color":"black"}},{"text":"A","x":0.61011708257978237,"y":70,"xanchor":"left","xshift":8,"showarrow":false,"font":{"size":12,"color":"black"}},{"text":"AB","x":0.65075952626447386,"y":130,"xanchor":"left","xshift":8,"showarrow":false,"font":{"size":12,"color":"black"}},{"text":"AC","x":0.62506305417064656,"y":90,"xanchor":"left","xshift":8,"showarrow":false,"font":{"size":12,"color":"black"}}]},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"8601522cb54c":{"x":{},"y":{},"colour":{},"text":{},"type":"scatter"},"86017532bab9":{"x":{},"y":{},"colour":{},"text":{}},"8601558103cb":{"x":{},"y":{},"colour":{},"text":{}},"86019203048":{"x":{},"y":{},"colour":{},"text":{}}},"cur_data":"8601522cb54c","visdat":{"8601522cb54c":["function (y) ","x"],"86017532bab9":["function (y) ","x"],"8601558103cb":["function (y) ","x"],"86019203048":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
