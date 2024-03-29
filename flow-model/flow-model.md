# The Propensity to Cycle Tool: An open source online system for sustainable transport planning
Robin Lovelace (University of Leeds)  
Anna Goodman (London School of Hygiene and Tropical Medicine)  
Rachel Aldred (University of Westminster)  
Nikolai Berkoff (independent web developer)  
Ali Abbas (University of Cambridge)  
James Woodcock (University of Cambridge)  





# Abstract {-}

Getting people cycling is an increasingly common objective in transport planning institutions worldwide. 
A growing evidence base indicates that high quality infrastructure 
can boost local cycling rates.
Yet for infrastructure and other cycling measures to be effective,
it is important to intervene in the right places, such as along 'desire lines' of high latent demand.
This creates the need for tools and methods to help answer the question 'where to build?'. 
Following a brief review of the policy and research context related to this question,
this paper describes the design, features and potential applications of such a tool.
The Propensity to Cycle Tool (PCT) is an online, interactive planning support system which was
initially developed to explore and map cycling potential across England
(see [www.pct.bike](http://pct.bike/)).
Based on origin-destination data, it models and visualises
cycling levels at area, desire line, route and route network levels,
for current levels of cycling, and for scenario-based 'cycling futures'.
Four scenarios are presented, including 'Go Dutch' and 'Ebikes',
which
explore what would happen if English people cycled as much as Dutch people and the potential impact of electric cycles on cycling uptake.
The cost effectiveness of investment depends not only on the number of additional trips cycled, but on wider impacts such as
health and carbon benefits. The PCT reports these at area, desire line, and route level for each scenario.
The PCT is open source, facilitating the creation of additional scenarios 
and its deployment in new contexts.
We conclude that the PCT illustrates the potential of online tools to
inform transport decisions and raises the wider issue
of how models should be used in transport planning.

# Introduction

Cycling can play an important role in creating
sustainable and equitable transport systems.
Cycling already provides reliable, healthy, affordable,
and convenient mobility to millions of people each day [@komanoff_bicycling_2004] and
is one of the fastest growing modes of transport in cities such as London,
New York and Barcelona [@fishman_cycling_2016].
There is mounting evidence about the external costs of car-dominated
transport systems [e.g. @han_assessment_2008;@shergold_rural_2012],
and the benefits of cycling [@oja_health_2011;@de_nazelle_improving_2011;@tainio_can_2016].
In this context there is growing interest, and in some cases substantial investment,
in cycling infrastructure, including in countries
with historically low rates of cycling.

<!-- ^[ -->
<!-- Several examples -->
<!-- of multi-million Euro projects are provided by the -->
<!-- [European Cyclist' Federation](http://www.ecf.com/advocary/eu-funding-2/examples-of-cycling-projects-co-funded-by-the-eu/), the -->
<!-- [FIA Foundation](http://www.unep.org/transport/sharetheroad/PDF/SharetheRoadReportweb.pdf) and the -->
<!-- [World Cycling Atlas](http://www.worldwidecyclingatlas.com/). -->
<!-- Another illustration of increased interest in cycling investment is provided by -->
<!-- the proliferation of publicly subsidised 'bike share' -->
<!-- [@obrien_mining_2014], which can be viewed in an interactive map at -->
<!-- [bikes.oobrien.com](http://bikes.oobrien.com). -->
<!-- ] -->

Providing high-quality infrastructure can play a key role in promoting cycling uptake [@parkin_cycling_2012].
Off-road cycle paths, for example, have been found to be associated with an uptake of cycling for commuting [@heinen_changes_2015].
Overall there is growing evidence linking cycling infrastructure to higher rates of cycling [@buehler_bikeway_2016].
But where should this infrastructure be built?
The central purpose of this paper is highlight the potential of online, interactive and publicly accessible tools to improve the provision of locally specific evidence about where cycling has the greatest potential to grow for transport planning.
It does so with reference to the Propensity to Cycle Tool (PCT), an online planning support system funded by the UK's Department for Transport to map cycling potential (Department for Transport, 2015).
The PCT is open source. This differs from other tools for assessing cycling potential (see the next section) and has wider implications for how models can and should be used in the transport decision making process (see the Discussion).


<!-- The version described in the paper was deployed for England (PCT-England) but we envision additional versions being developed for other countries and cities. -->

<!-- And where are local policy interventions --- such as reducing speed limits, enforcing road traffic laws, creating cycle hire schemes, providing free cycle training and reallocating road space for walking and cycling --- likely to have the greatest impact? -->



# The Propensity to Cycle Tool in context

The PCT was developed alongside two branches of academic research:
a) methodological developments for estimating cycling potential and b) Planning Support Systems (PSS).
The subsequent overview of this policy and academic landscape places the PCT in its wider context and explains its key features.

## The policy context

A number of factors influence
the attractiveness of cycling for everyday trips [@pucher_infrastructure_2010;@parkin_planning_2015].
However, the intervention that has received the most attention has been the construction of new cycle paths.
In the UK context, devolved transport budgets mean that local authorities have some control over the design and implementation of cycling networks.
While much of the road network budget allocated centrally, through Highways England, cycle paths tend to be commissioned and designed locally due to funding streams such as the Local Sustainable Transport Fund, described in the context of modal shift to cycling by @chatterjee_triggers_2013.
This, and the shorter distances and more flexible routes (e.g. through parks and along disused railways) that cycle paths can take increases the importance of high resolution map-based tools for cycling compared with motorised modes.
Local transport budgets also increase demand for local cycling targets that open source tools such as the PCT could enable, as described in the Discussion.

Planning new cycle paths requires many decisions to be made, including in relation to the width [@wegman_urban_1979;@pikora_developing_2002], quality [@heath_effectiveness_2006], directness [@crow_design_2007] and geographic location of the paths.
Yet while much guidance has been produced regarding physical design [e.g. @transport_for_london_london_2015;@welsh_government_guidance_2014],
little work has explicitly tackled the question of where high quality infrastructure should be built
[@aultman-hall_analysis_1997;@minikel_cyclist_2012;@larsen_build_2013].
Within this policy context, the PCT focuses explicitly on the question of *where* to build rather than *what* to build, although it does provide evidence on potential capacity requirements across the route network.

<!-- A wider issue related to the broader sustainable urban mobility question is that the tools and concepts within the 'predict and provide' paradigm have limited capacity to inform 'sustainable mobility' policies [@naess_transport_2014]. -->

## Research into cycling potential 

There is an emerging literature exploring cycling rates and potential rates of growth.
Much of this work is practitioner-led, as detailed in an excellent overview of developments in the USA [@kuzmyak_estimating_2014].
<!-- This links to the question of 'where to build' because  -->
<!-- areas and routes with the highest potential are likely to be cost-effective places to invest. -->
With the notable exceptions of @larsen_build_2013 and @zhang_prioritizing_2014, the methods reviewed in this section are not simultaneously *route-based*, *systematic*, *quantitative* and *scalable*, some of the key features of the PCT.
The work can be classified in a number of ways, including by the main mechanism of the tool (e.g. facility demand or mode choice models), the format of its outputs (e.g. spreadsheet results, GIS-based map or on-line, interactive map) or by the main level of the input data used.
In these terms, the PCT is based on a mode choice model (representing the shift to cycling from other modes) operating primarily at the origin-destination level, which provides outputs in an online, interactive map.
For research into cycling potential (and the resulting models) to be useful for local transport planners, their spatial scale must coincide with those over which the planning process has some control. For this reason we focus on scale as the primary way of categorising research into cycling potential, highlighting the following prominent levels (the PCT best fits into the third). 

- Area-based measures are based primarily on data at the level of administrative
zones, which will vary depending on context.
Outputs from these measures can assist with the location of site-specific transport infrastructure such as cycle parking.

- Individual-based measures are based on survey data, typically a household travel survey. These are not always geographically specific and tend to be used to identify and categorise demographic groups in relation to cycling, such as near-market or as warranting tailored interventions, such as targeted cycle training schemes.

- Route-based measures use origin-destination data which can be used to create 'desire lines' and (using route allocation) estimates of existing and potential demand at each point on the road network.

This work is reviewed in relation to the PCT below and summarised in Table 1.

<!-- ### Area-based measures of cycling potential -->

@parkin_estimation_2008 presented an area-based measure of cycling potential using regression model to estimate the proportion of commuter trips cycled across wards in England and Wales. Factors associated with lower levels of cycling included road defects, high rainfall, hills and a higher proportion of ethnic minority and low-income inhabitants.  Parkin et al. concluded that policy makers must engage with a mixture of physical and social barriers to promote cycling effectively, with the implication that some areas have lower barriers to cycling --- and hence higher propensity to cycle --- than others.

@zhang_prioritizing_2014 created an individual-based model of cycling potential to prioritise where to build cycle paths to "achieve maximum impacts early on".
The outputs of this model were aggregated to the level of 67 statistical zones in the study area of Belo Horizonte, Brazil, and used to generate a 'usage intensity index' for potential cycle paths.
This, combined with survey data on cyclists' stated preferences on whether people would cycle were infrastructure provided along particular routes and origin-destination data on travel to work, was used to rank key routes in the city in terms of their cycling potential. 

@larsen_build_2013 created
an area-based 'prioritization index', for Montreal, Canada.
This was based on four variables: the area's current level of cycling,
its cycling potential (estimated based on the shortest path between the origin and destination of short car trips from a travel survey), the number of injuries to cyclists,
and locations prioritised by current cyclists for improvement  [@larsen_build_2013].
These four were aggregated to the level of evenly spread cells covering the study area.
The resulting heat map was used to recommend the construction or upgrade of cycle paths on specific roads.

<!-- ### Individiual-based measures of cycling potential -->
<!-- Another method that used individual-level to identify areas with high numbers of -->
<!-- 'potentially cycleable trips' funded by London's transport authority [@transport_for_london_analysis_2010]. -->
<!-- This method was based primarily on the London Travel Demand Survey and analysed -->
<!-- which segments of the population were most willing to cycle. The results were -->
<!-- presented as a static heatmap and used to prioritise where to build new cycle -->
<!-- paths and cycle hire points. -->

Although the methods presented in preceding three paragraphs were developed in an academic context,
most of the usable *tools* for estimating cycling potential are practitioner-led.
Many of these may never have been documented in the academic literature, so the subsequent overview should not be regarded as comprehensive
[see @kuzmyak_estimating_2014 and subsequent documents that cite this report].

The Analysis of Cycling Potential (ACP) tool was developed by @transport_for_london_analysis_2010 to produce heat maps of cycling potential across London, for all trip purposes. It is based a mode choice model of likelihood of trips being cycled and uses the characteristics of observed trips (e.g. time of day, characteristics of the traveller, distance) as the main input dataset.
The results of the ACP have informed local cycling schemes, such as where to build new cycle hire stations but does not use origin-destination data so is less relevant for route-based interventions.

A more localised approach is the Permeability Assessment Tool (PAT), which was developed by a transport consultancy @payne_removing_2014.
The PAT is based on the concept of 'filtered permeability', which means providing a more direct route to people cycling than driving [@melia_urban_2015].
The PAT works by combining geographical data, including the location of popular destinations
and existing transport infrastructure, with on-site audit data of areas that have
been short-listed. Unlike the prioritisation index of Larsen et al. (2013), which is primarily aimed at informing a city-wide strategic cycling network, the results of the PAT are designed to guide smaller, site specific interventions such as 'contraflow' paths and cyclist priority traffic signals.

The Santa Monica model is an example of a direct *facility demand* model, which are based on "observed counts and context-driven regression models" [@kuzmyak_estimating_2014].
The model uses environmental explanatory variables such as employment density and speed limits of surrounding roads to estimate the dependent variable: walking and cycling activity (e.g. as recorded by screenline count devices).
By adjusting regression parameters (e.g. based on areas that have experienced growth in cycling), such models can be used to "forecast demand levels for walk or 
bike at a point or intersection level" and "evaluate and prioritize projects" (ibid., p. 75).

<!-- A key feature of this tool was its integration of data sources from many different sources. -->
<!-- Unlike @larsen_build_2013, @zhang_prioritizing_2014  Using socio-demographic data, groups who were most likely to cycle were identified.  -->

<!-- Route-allocation of the desire lines. This facilitates the identification of specific segments of the transport network to be targeted for improvement. Such an approach has considerable potential for creating an evidence-base for prioritising investment in cycle paths locally (Broach et al., 2012; Ehrgott et al., 2012), for example by []. -->

<!-- \newpage -->

\begin{table}[ht]
\centering
\caption{Summary of tools and methods to explore the geographical distribution of cycling potential. Levels of analysis refer to whether data is analysed at point (P), area (A), origin-destination (OD), route (R), route network (RN) or individual (I) levels.} 
\begin{tabularx}{\textwidth}{p{2.5cm}|p{2cm}p{2cm}XXXp{1.8cm}}
 Tool & Scale & Coverage & Public access & Format of output & Levels of analysis & Software licence \\ 
  \midrule
Propensity to Cycle Tool & National & England & Yes & Online map & A, OD, R, RN & Open source \\ 
   \midrule
Prioritization Index & City & Montreal & No & GIS-based & P, A, R & Proprietary \\ 
   \midrule
Permeability Assessment Tool & Local & Parts of Dublin & No & GIS-based & A, OD, R & Proprietary \\ 
   \midrule
Usage intensity index & City & Belo Horizonte & No & GIS-based & A, OD, R, I & Proprietary \\ 
   \midrule
Bicycle share model & National & England, Wales & No & Static & A, R & Unknown \\ 
   \midrule
Cycling Potential Tool & City & London & No & Static & A, I & Unknown \\ 
   \midrule
Santa Monica model & City & Santa Monica & No & Static & P, OD, A & Unknown \\ 
   \bottomrule
\end{tabularx}
\end{table}

<!-- \newpage -->
<!-- \elandscape -->

## Planning support systems

The methods and tools for estimating cycling potential outlined in Table 1 were generally created with only a single study region in mind. The benefit of this is that they can respond context-specific to practitioner and policy needs.
However, the PCT aims to provide a *generalisable* and *scalable* tool,
in the tradition of Planning Support Systems (PSS).

PSS were developed to encourage evidence-based policy in land-use planning [@klosterman_what_1999].
The application of PSS to transport planning has been more recent, with a goal of
"systematically [introducing] relevant (spatial) information to a
specific process of related planning actions" [@te_brommelstroet_developing_2008].
The PCT is systematic in its use of national data for all parts of the study region
(in this case England) and relates to a specific planning process --- the creation of new and enhancement of existing cycle infrastructure.

PSS typically work by presenting evidence about the characteristics and needs
of the study region in an interactive map.
A central objective is to visualise
alternative scenarios of change, and explore their potential impacts.
The results of traditional scenario-based models 
are usually not locally specific
[@mccollum_achieving_2009;@lovelace_assessing_2011;@woodcock_public_2009].
Online PSS can overcome this issue by using interactive maps [@pettit_online_2013].
The emergence of libraries for web mapping  [@haklay_web_2008] has facilitated online PSS, offering the potential for public access to the planning process. Transparency is further enhanced by making PSS open source, in-line with a growing trend in transport modelling [@tamminga_design_2012;@novosel_agent_2015;@borning_urbansim:_2008].  In these ways, PSS can make evidence for transport planning more widely available, and tackle the issue that transport models are often seen as 'black boxes', closed to public scrutiny (Golub et al., 2013).

With reference to a publicly accessible online map-based PSS for strategic land use (and to a lesser extent transport) planning, @kahila-tani_let_2016 provide guidance on the wider public participation process: it should be *transparent*, *influence* real world decisions, be *representative* of citizens and allow *independent participation*. Although these criteria depend primarily on *how* the tool is deployed in practice (for example with regards to training resources and workshops facilitated by planning authorities to ensure that the tool the tool is used effectively), which are outside the scope of the current paper, the PCT certainly encourages transparency through its use of open data and open source software and allows independent participation through its publicly accessible online interface.


<!-- allowing public access to the planning process. -->
<!-- Linked with the proliferation of open source software in transport planning -->
<!-- [@tamminga_design_2012;@novosel_agent_2015;@borning_urbansim:_2008], -->
<!-- PSS can tackle the issue that transport models are often seen as -->
<!-- 'black boxes', closed to public scrutiny [@golub_making_2013], by making -->
<!-- evidence for transport planning more widely available. -->

<!--' Much has been written about the characteristics of an effective PSS -->
<!--' of relavence to the PCT. -->
<!--' Although PSS are primarily technical projects to *develop*, -->
<!--' it has been found that 'soft' factors can impede their utility for practitioners. -->
<!--' In a review of the effectiveness of PSS uptake in the Netherlands, -->
<!--' @brommelstroet_equip_2010 identified timing and limited options for scenario -->
<!--' developement as key 'bottlenecks'. -->
<!--' These insights from the PSS literature influenced the design of the PCT -->
<!--' and the critique of PSS that practitioners should be more involved in -->
<!--' their development is returned to in the Discussion. -->







## National context and features of the Propensity to Cycle Tool

In addition to the international policy and academic context,
the PCT was influenced by the national context.
It was commissioned by the UK's Department for Transport to identify "parts of [England] with the greatest propensity to cycle"  [@department_for_transport_response_2015].
Thus the aim was not to produce a full transport demand or land use model,
but to provide an evidence base to prioritise where to create and improve cycling infrastructure based on scenarios of change.

Local and national cycling targets are often based on a target mode share by a given date.^[The
local target in Bristol, for example, is for [20% of commuter trips to be cycled by 2020](http://www.bristolpost.co.uk/Bristol-City-Council-s-pound-35million-plan/story-21341172-detail/story.html). [Manchester](http://cycling.tfgm.com/Documents/CCAG2-Executive-Summary.pdf) (10% by 2025),
[Derbyshire](https://www.derbyshire.gov.uk/images/2015-07-07%20Item%207l%20Cycle%20Plan_tcm44-267216.pdf) (to double the number of people cycling by 2025) and [London](http://content.tfl.gov.uk/gla-mayors-cycle-vision-2013.pdf) (to 'double cycling' by 2025) provide further examples of local ambitious time-bound cycling
targets.]
However, there is little evidence about what this might mean for cycling volumes along specific routes.
The PCT tackles this issue by estimating rate of cycling locally under different scenarios and presenting the results on an interactive map. Its key features include:

- Estimation of cycling potential at area, 'desire line' and route network levels. 

- Route-allocation of OD (origin-destination) pairs by a routing algorithm specifically developed for cycling.
This was done by CycleStreets.net, a routing service developed by cyclists, for cyclists.

- Visualisation of outputs at multiple geographic levels. The interactive map enables users to examine cycling potential at a very local level (e.g. just a few streets) or at a more regional level (e.g. across a large metropolitan area). 

- Public accessibility of results and code. The tool is freely available online and developers are encouraged to modify the PCT (e.g. to create alternative scenarios) by provision of the source code underlying the PCT under the open source AGP License.

- The presentation of estimated health economic and carbon impacts under future scenarios, providing an evidence base that could be used in business cases for investment.

As with any tool, the PCT’s utility depends on people knowing how to use it.
For that reason training materials and a user manual are being developed
to show how the tool can be used
(see the 'Manual' tab in Figure 3 and [pct.bike/manual.html](http://pct.bike/manual.html)).

# Data and methods

This section describes the data and methods that generate the scenario data for the PCT,
as summarised in Figure 1.
The details of the model, which models the proportion of trips made by cycling per OD pair as a function of hilliness and distance, are described the Appendix.
The scenario-generation method is not included in the main text of the paper because it is the most context-specific aspect of the PCT: as outlined in the Discussion, we envision future uses of the PCT using different model parameters and even different functional forms relating distance, hilliness and other variables to the level of cycling, to generate scenarios of use for new contexts beyond the English case study described here.
Central to the PCT approach is origin-destination (OD) data recording the travel flow between administrative zones.^[The
size and uniformity of these depend on the country in question. In the UK the primary areal units for statistical data are output areas (OA), lower layer super output areas (LSOA) and middle layer super output areas (MSOA) (see [webarchive.nationalarchives.gov.uk](http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html)). The version of the PCT presented in this paper operates at the MSOA level.
]
Combined with geographical data on the coordinates of the population-weighted centroid of each zones,
these can be represented as
straight 'desire lines' or as routes allocated to the transport network.

<!-- The route-allocation stage (outlined below) provides -->
<!-- hilliness and route network distance as additional variables at the OD level. -->

<div class="figure">
<img src="161103_NatModel2_MainPaper_Fig1Fig2.pdf" alt="Flow diagram illustrating the input data and processing steps used to create the input data used by the PCT. The abbreviations are as follows: HEAT = Health Economic Assessment Tool, OD pair = origin-destination pair, MSOA = Middle-Layer Super Output Area" width="100%" />
<p class="caption">Flow diagram illustrating the input data and processing steps used to create the input data used by the PCT. The abbreviations are as follows: HEAT = Health Economic Assessment Tool, OD pair = origin-destination pair, MSOA = Middle-Layer Super Output Area</p>
</div>

```
## [1] TRUE
```

## Processing OD data

The central input dataset was a table of origin-destination (OD) pairs from the 2011 Census.
This was loaded from open access file `wu03ew_v2.csv`, provided by the [UK Data Service](http://wicid.ukdataservice.ac.uk/). This captures the number of commuters travelling between Middle Super Output Area zones (MSOAs, average commuter population: 3300), by mode of travel (see Table 2).
 This dataset was derived from responses to the following questions in the [2011 Census for England and Wales](http://www.ons.gov.uk/census/): "In your main job, what is the address of your workplace?" (question 40) and "How do you usually travel to work? (Tick one box only, for the longest part, by distance, of your usual journey to work)" (Question 41).
 This dataset was enhanced by merging in information on the gender composition of cyclists in each OD pair (Dataset 1 in Figure 1); data at the area level on the background mortality rate (Dataset 2); and data at the OD pair-level on route distance (km) and hilliness (average gradient, as a percentage) (Dataset 3).
OD data was assigned to the transport network using the R package stplanr (Lovelace et al., 2016). See the Appendix for further details.

\begin{table}[]
\centering
\caption{Sample of the OD (origin-destination) input dataset, representing the number of people who
commute from locations within and between administrative zones (MSOAs). Note `Car' refers to people who drive as their main mode of travel per OD pair, rather than people who travel to work as a passenger in a car.}
\label{my-label}
\begin{tabular}{@{}lllllll@{}}
\toprule
                                        &                                        & \multicolumn{5}{l}{Number of commuters by main mode}                                                                 \\ \midrule
\multicolumn{1}{l}{Area of residence} & \multicolumn{1}{l}{Area of workplace} & \multicolumn{1}{l}{Total} & \multicolumn{1}{l}{Cycle} & \multicolumn{1}{l}{Walk} & \multicolumn{1}{l}{Car} & \multicolumn{1}{l}{Other} \\ \midrule
\multicolumn{1}{l}{E02002361}         & \multicolumn{1}{l}{E02002361}         & \multicolumn{1}{l}{109}   & \multicolumn{1}{l}{2}     & \multicolumn{1}{l}{59}   & \multicolumn{1}{l}{39}  & \multicolumn{1}{l}{9}     \\ 
\multicolumn{1}{l}{E02002361}         & \multicolumn{1}{l}{E02002362}         & \multicolumn{1}{l}{7}     & \multicolumn{1}{l}{1}     & \multicolumn{1}{l}{0}    & \multicolumn{1}{l}{4}   & \multicolumn{1}{l}{2}     \\ 
\multicolumn{1}{l}{E02002361}         & \multicolumn{1}{l}{E02002363}         & \multicolumn{1}{l}{38}    & \multicolumn{1}{l}{0}     & \multicolumn{1}{l}{4}    & \multicolumn{1}{l}{24}  & \multicolumn{1}{l}{10}    \\ 
\multicolumn{1}{l}{E02002361}         & \multicolumn{1}{l}{E02002364}         & \multicolumn{1}{l}{15}  & \multicolumn{1}{l}{1}     & \multicolumn{1}{l}{0}    & \multicolumn{1}{l}{10}  & \multicolumn{1}{l}{4}     \\ 
\multicolumn{1}{l}{E02002361}         & \multicolumn{1}{l}{E02002366}   & 29                         & 1                          & 10                        & 11                       & 7                          \\ \bottomrule
\end{tabular}
\end{table}



<!-- Table: Sample of the zone level input dataset. The coordinates represent the population-weighted centroids of MSOA zones, whose codes are also presented in Table 1. -->

<!-- ```{r tcents, echo=FALSE, results='asis'} -->
<!-- # t2 <- xtable(as.data.frame(cents[1:3,-c(3,4)]), caption = "Sample of the 'cents' input dataset, representing the geographical location of the population-weighted centroids of MSOA zones described in Table~\\ref{tbl:flow}", label = "tbl:cents") -->
<!-- # print(t2, type = "latex", comment = FALSE, caption.placement = "top") -->
<!-- data("cents") -->
<!-- sel <- which(cents$geo_code == flow$`Area of residence`[1]) -->
<!-- cents1 <- as.data.frame(cents[c(1:3, sel),-c(3,4)]) -->
<!-- names(cents1) <- c( -->
<!--   "Area of residence", -->
<!--   "Area name", -->
<!--   "Longitude", -->
<!--   "Latitude") -->
<!-- kable(cents1, row.names = F) -->
<!-- ``` -->

<!-- A model estimating cycling uptake as a function of distance, via distance decay curves [@iacono_access_2008], -->
<!-- and hilliness was used to create plausible scenarios of cycling futures. -->
<!-- For the purposes of this study, model parameters were derived from -->
<!-- English and Dutch travel survey data (see the Appendix). -->
<!-- Changing the height and shape of distance decay curves -->
<!-- (and, for the 'Ebike scenario, 'hilliness decay curves') -->
<!-- based on a baseline from the English National Travel Survey -->
<!-- (illustrated in Figure 2) allowed estimates of the model split -->
<!-- of cycling to be generated for every route-allocated OD pair for each scenario. -->

<!-- It is important that even in high-cycling countries such as the Netherlands there -->
<!-- is a strong relationship between cycling levels and trip distance, -->
<!-- influencing the potential for mode shift in different regions. -->
<!-- Where many trips are long and/or hilly, interventions related to land-use planning -->
<!-- (e.g. the creation of local jobs and services) may be a prerequisite for substantial cycling uptake. -->
<!-- Where many are short, by contrast, improvements to the infrastructure -->
<!-- for cycling, in tandem with 'soft' interventions such as cycle training, -->
<!-- may be sufficient for cycling levels to grow. -->
<!-- The PCT focuses explicitly on the latter scenario as it is (currently) -->
<!-- based on current travel patterns. -->

## Modelling baseline propensity to cycle

The starting point for generating our scenario-based 'cycling futures' was to model baseline data on cycle commuting in England and Wales.  We did this using OD data from the 2011 Census, and modelling cycling commuting as a function of route distance and route hilliness.  We did so using logistic regression applied at the individual level, including squared and square-root terms to capture 'distance decay' --- the non-linear impact of distance on the likelihood of cycling [@iacono_access_2008] --- and including terms to capture the interaction between distance and hilliness. Model fit is illustrated in Figure 2; see the Appendix for details and for the underlying equations.
We also developed equations to estimate commuting mode share among groups not represented in the between-zone ('interzonal') OD data, e.g. those commuting within a specific MSOA (this is within-zone or 'intrazonal' travel), or those with no fixed workplace. 
This model of baseline propensity to cycle formed the basis of three of the four scenarios (Government Target, Go Dutch and Ebikes), as described in more detail in the next section.

<div class="figure">
<img src="fig1.png" alt="The relationship between distance (left) and hilliness (right) and cycling mode share based on data from the 2011 Census for England and Wales. The plots show actual (blue) vs predicted (red) prevalence of cycling to work among commuters travelling &lt;30km to work." width="100%" />
<p class="caption">The relationship between distance (left) and hilliness (right) and cycling mode share based on data from the 2011 Census for England and Wales. The plots show actual (blue) vs predicted (red) prevalence of cycling to work among commuters travelling <30km to work.</p>
</div>

```
## [1] TRUE
```




<!-- The raw OD data was preprocessed before being used in the PCT. -->
<!-- The R package **stplanr** was developed for this task [@lovelace_stplanr:_2016]. -->
<!-- Tables 1 and 2 illustrate the two tables that comprise the OD data. Figure 3 -->
<!-- shows a visualisation of the output, straight lines with attributes for -->
<!-- each OD pair in both directions. These are also referred to as 'desire lines' when -->
<!-- represented as straight lines on the map [@tobler_experiments_1987;@chan_multi-criteria_2003]. -->
<!-- The visualisation of the OD data builds -->
<!-- on published work on cartographic visualisation [@rae_spatial_2009;@wood_visualisation_2010]. -->
<!-- The model for England described here uses the following open datasets -->
<!-- (similar OD datasets are available for cities across the world): -->

<!-- - Flow data representing the number of trips between origin-destination (OD) pairs, -->
<!-- disaggregated by mode of travel. -->
<!-- We used the file `wu03ew_v2.csv`, obtained from the UK Data Service -->
<!-- (see Table 2 for a sample of this dataset)^[See [wicid.ukdataservice.ac.uk/cider/wicid/downloads.php](https://wicid.ukdataservice.ac.uk/cider/wicid/downloads.php).]  This dataset was derived by the Office for National Statistics from responses to questions 40 ("In your main job, what is the address of your -->
<!-- workplace?") and 41 ("How do you usually travel to work?") in the 2011 Census for England and Wales (see [ons.gov.uk/census](http://www.ons.gov.uk/census/2011census/howourcensusworks/howwetookthe2011census/howwecollectedtheinformation/questionnairesdeliverycompletionandreturn) for further information). -->

<!-- - The population-weighted centroids of -->
<!-- administrative zones (see Table 1). Middle Super -->
<!-- Output Areas (MSOAs) (average population: 7,800) -->
<!-- were used for both origins and destinations.^[This -->
<!-- data was provided under the UK's Open Government -->
<!-- Licence [data.gov.uk/dataset/lower-layer-super-output-areas-ew-2011-population-weighted-centroids](http://data.gov.uk/dataset/lower-layer-super-output-areas-ew-2011-population-weighted-centroids).] -->

<!-- - Route distance, generated by the `route_cyclestreet` function -->
<!-- in the **stplanr** R package. -->

<!-- - Hilliness of zones and routes. There are various ways to generate this data, -->
<!-- ranging from the simple (e.g. vertical displacement between origin and -->
<!-- destination) to the complex (e.g. total amount of climb along the route network -->
<!-- in both directions). After experimenting with area-based measures of hilliness -->
<!-- we settled on a route-allocated measure: the average gradient (%) of the 'fastest' -->
<!-- route. -->

<!-- We calculated mean gradient per MSOA zone using publicly available -->
<!-- digital -->
<!-- elevation model (DEM) data supplied by NASA.^[See -->
<!-- [srtm.csi.cgiar.org/](http://srtm.csi.cgiar.org/) for the data and the -->
<!-- [steepness.R](https://github.com/npct/pct/blob/master/loading-data/steepness.R) -->
<!-- file in the project's repository for the processing algorithm used. "Version 4" -->
<!-- of the dataset was used. To allocate this area-based hilliness metric to OD -->
<!-- pairs, we calculated the average hilliness of origin and destination zones. This -->
<!-- method has the disadvantage that accuracy decreases with increased trip -->
<!-- distance.] -->

<!-- We used the Census 2011 travel to work origin-destination dataset for its comprehensive coverage of -->
<!-- the population, high geographic resolution and assurances surrounding data -->
<!-- quality. The MSOA It is worth noting that -->
<!-- a variety of emerging sources could be used instead to -->
<!-- provide OD data. This includes 'Big Data' -->
<!-- [@kitchin_big_2013] sources such as: mobile -->
<!-- telephone service providers [@smoreda_spatiotemporal_2013]; -->
<!-- public transport data, which can highlights local demand short trips; -->
<!-- household travel surveys [@transport_for_nsw_household_2014] -->
<!-- and output from transport demand models controlled by local government. -->



## Scenarios of cycling uptake 

Four scenarios were developed to a range of
explore cycling futures.
These can be framed in terms of the removal of different infrastructural, cultural and technological barriers that currently prevent cycling being the natural mode of choice for trips of short to medium distances. 
They are not predictions of the future.
They are snapshots indicating how the spatial distribution
of cycling may shift as cycling grows based on current travel patterns.
At a national level, the first two could be seen as shorter-term and the 
second two more ambitious.
The choice of scenarios was informed by a government target to double the number of cycle trips
and evidence from overseas
about which trips *could* be made by cycling.
Summaries of the four scenarios are as follows (see the Appendix for full details):

- Government Target. This scenario represents a doubling of the level of cycling, in line with the government's target to double the number of 'stages' (trips of a single mode) cycled by 2025 [@department_for_transport_cycling_2014]. Although substantial in relative terms, the rate of cycling under this scenario (rising from 3% to 6% of English commuters) remains low compared with countries such as the Netherlands and Denmark. This scenario was generated by adding together a) the observed number of cyclists in each OD pair in the 2011 Census, and b) the modelled number of cyclists, as estimated using the baseline propensity to cycle equations described in the previous section. The result is that cycling overall doubles at the national level, but at the local level this growth is not uniform, in absolute or relative terms. Areas with many short, flat trips and a below-average current rate of cycling are projected to more than double. Conversely, areas with above-average levels of cycling and many long-distance hilly commuter routes will experience less than a doubling.

- Gender Equality. This scenario illustrates the increase in cycling
that would result if women were as likely as men to cycle a given trip.
Specifically, the scenario sets the proportion of female cycle commuters to be equal
to the current proportion of males in each OD pair.
The scenario is based on the observation that in places where cycling accounts for a high proportion of personal travel,
women cycle at least as much as men [@pucher_infrastructure_2010;@aldred_does_2016].
This scenario has the greatest relative impact in areas where
the rate of cycling is highly gender-unequal.
<!-- In absolute terms, cycling increases most in this scenario where cycling -->
<!-- is already a common mode of transport. -->

- Go Dutch.
While the Government Target and Gender equality scenarios model relatively modest increases in cycle commuting, Go Dutch represents what would happen if English people were as likely as Dutch people to cycle a trip of a given distance and level of hilliness. This scenario thereby captures the proportion of commuters that would be expected to cycle if all areas of England had the same infrastructure  and cycling culture as the Netherlands (but retained their hilliness and commute distance patterns).
This scenario was generated by taking baseline propensity to cycle (see previous section), and applying Dutch scaling factors --- parameters which increase the proportion of trips cycled above the baseline level of cycling per OD pair. These scaling factors were calculated through analysis of the British and Dutch National Travel Surveys. We parameterised the scaling factors as one fixed parameter (the main effect) plus one distance-dependent parameter (parametrised as an interaction between Dutch status and trip distance), to take into account the fact that the "Dutch multiplier" is greater for shorter trips compared to longer trips.  Note that the scenario level of cycling under Go Dutch is not affected by the current level of cycling, but is instead purely a function of the distance and hilliness of each OD pair --- i.e. the two characteristics that determine baseline propensity to cycle.

<!-- While Government Target and Gender Equality build on current cycling behaviour, -->
<!-- 'Go Dutch' focuses on long-term potential. -->
<!-- Go Dutch represents what would happen if English people were as likely as Dutch people to cycle a trip of a given distance and level of hilliness. -->
<!-- It works by assigning Dutch -->
<!-- distance decay parameters to English travel patterns. -->
<!-- The scenario represents the elimination of the -->
<!-- infrastructural and cultural constraints which currently hold back cycle -->
<!-- use in England, including all localised differences. -->
<!-- As such, whereas the Government Target and Gender Equality scenarios take current levels as a starting point, the predicted levels of cycling in  Go Dutch -->
<!-- are unrelated to current levels, and are constrained -->
<!-- only by local trip distance distributions and hilliness. -->

- Ebikes. This scenario models the additional increase
in cycling that would be achieved through the widespread uptake of electric
cycles ('ebikes').
This scenario is generated by taking baseline propensity to cycle, applying the Dutch scaling factors described above, and then additionally applying Ebike scaling factors.  The Ebikes scenario is thus currently implemented as an extension of Go Dutch but could in future be implemented as an extension of other scenarios.  The Ebike scaling factors were generated through analysis of the UK, Dutch and Swiss National Travel Surveys, in which we estimated how much more likely it was that a given commute trip would be cycled by Ebike owners versus cyclists in general.  We parameterised the scaling factors as varying according to trip distance and according to hilliness, by fitting interaction terms between these two characteristics and ebike ownership (Appendix Equation 1B).  We did this to take account of the fact that electric cycles enable longer journeys and reduce the barrier of hills

The Ebikes scenario is thus currently implemented as an extension of Go Dutch
but could be implemented as an extension of other scenarios.
The Ebike scaling factors were generated through analysis of the English, Dutch and Swiss National Travel Surveys, in which we estimated how much more likely it was that a given commute trip would be cycled by Ebike owners versus cyclists in general.  We parameterised the Ebike scaling factors as interactions with trip distance and with hilliness, to take account of the fact that electric cycles enable longer journeys and reduce the barrier of hills.

Additional scenarios could be developed (see Discussion). If deployed in other settings, the PCT will likely benefit from scenarios that relate to both the current policy context and long-term aspirations.

## Estimation of health and carbon impacts

Because the cost effectiveness of cycling investments are influenced by wider social impacts, estimated health economic and emissions impacts are presented in the PCT.

An approach based on the World Health Organization’s Health Economic Assessment Tool ([HEAT](http://www.heatwalkingcycling.org/)) was used to estimate the number of premature deaths avoided due to increased physical activity [@kahlmeier_health_2014]. To allow for the fact that cycling would in some cases replace walking trips, HEAT estimates of the increase in premature deaths due to the reduction in walking were also included. The change in walking was estimated based on the assumption that, within a given OD pair, all modes were equally likely to be replaced by cycling. Thus all the non-cycling modes shown in Table 2 experienced the same relative decrease.

Trip duration was estimated as a function of the 'fast' route distance and average speed. For walking and cycling we applied the standard HEAT approach. Ebikes are not specifically covered in HEAT Cycling but enable faster travel and require less energy from the rider than traditional bikes. Thus we estimated new speeds and intensity values for this mode, giving a smaller benefit for every minute spent using Ebikes than conventional cycles. For more details see the Appendix.

The risk of death varies by gender and increases rapidly with age.
This was accounted for using age and sex-specific mortality rates for each local authority in England.
For the baseline and Government Target scenario the age distribution of cyclists recorded in the 2011 Census was used. New cyclists under Go Dutch and Ebikes were assumed to have the age-gender profile of commuter cyclists in the Netherlands.
The inclusion of age specific parameters and mode shift from walking shows how the HEAT approach can generate nuanced health impact estimates using publicly available data.

The net change in the number of deaths avoided for each OD pair was estimated as the number of deaths avoided due to cycle commuting minus the number of additional deaths due to reduced walking.  Note that this approach means that for some OD pairs where walking made up a high proportion of trips, additional deaths were incurred. The monetary value of the mortality impact was calculated by drawing on the standard 'value of a statistical life' used by the Department for Transport.

We also estimated the reduction in transport carbon emissions resulting from decreased car driving in each scenario.  This again relied on the assumption that all modes were equally likely to be replaced by cycling.  The average CO^2^-equivalent emission per kilometre of car driving was taken as 0.186 kg, the 2015 value of an 'average' car [@defra_defra_2015].

## Visualisation, route allocation and network generation

The data analysis and preparation stages described in the previous sections were conducted using the national OD dataset for England as a whole.  By contrast, the stages described in this section were conducted using a region-by-region approach. Transport decisions tend to be made at local and regional levels [@gaffron_implementation_2003], hence the decision to display results on a per region basis.

<!-- This increased the scalability of the PCT, allowing nationwide data to be generated on -->
<!-- desktop computers, albeit with large (16 GB+) amounts of RAM. -->

Figure 3 shows the output: 'desire lines' lines with attributes for each OD pair aggregated
in both directions [@tobler_experiments_1987;@chan_multi-criteria_2003],
and visualised as centroid to centroid 'flows' [@rae_spatial_2009;@wood_visualisation_2010].

<div class="figure">
<img src="od-data-leeds.png" alt="Overview of the PCT map interface, showing area and OD-level data. The zone colour represents the number of residents who cycle to work. The lines represent the top 6 most cycled commuter routes in Leeds, with width proportional to the total number of cycle trips. Population-weighted centroids are represented by circles, the diameter of which is proportional to the rate of within-zone cycling." width="100%" />
<p class="caption">Overview of the PCT map interface, showing area and OD-level data. The zone colour represents the number of residents who cycle to work. The lines represent the top 6 most cycled commuter routes in Leeds, with width proportional to the total number of cycle trips. Population-weighted centroids are represented by circles, the diameter of which is proportional to the rate of within-zone cycling.</p>
</div>

```
## [1] TRUE
```

<!-- A potential limitation of this single regional geographic level is -->
<!-- that 'edge zones', straddling two -->
<!-- or more regions, can become marginalised. -->
<!-- To overcome this issue the 'build' scripts were written to -->
<!-- enable custom regions. London, for example, -->
<!-- was 'built' at 2 different levels, one covering the entirety of the city -->
<!-- and another which divided the city into 4 quadrants based on a zoning system -->
<!-- used by Transport for London -->
<!-- (see [pct.bike/london/](http://pct.bike/london/) and -->
<!-- [pct.bike/london-east/](http://pct.bike/london-east/) respectively) -->
<!-- Another solution to the issue of edge-zones that was tested -->
<!-- (but not implemented following user feedback) -->
<!-- was the creation of buffers around the regions. -->
<!-- This involves expanding the outline of a region, -->
<!-- creating an area of overlap between two regions that are side-by-side. -->








<!-- ## Route allocation and network generation -->

Desire lines allocated to the route network are illustrated in Figure 4.
This shows two route options: the 'fast' route,
which represents an estimate of the route taken by cyclists to minimise travel time
and the 'quiet' route that preferentially selects smaller, quieter roads and
off road paths.

<!-- Overlapping fast routes were aggregated to create the 'Route Network' layer (see Figure 5). -->
<!-- These stages were computationally intensive, leading to a number of steps -->
<!-- being taken to make them fast and scalable: -->

<div class="figure">
<img src="od-data-fast-quiet.png" alt="Illustration of desire lines shown in Figure 3 after they have been allocated to the road network by CycleStreets.net. Purple and green lines represent `fastest' and `quietest' routes generated by CycleStreets.net, respectively." width="100%" />
<p class="caption">Illustration of desire lines shown in Figure 3 after they have been allocated to the road network by CycleStreets.net. Purple and green lines represent `fastest' and `quietest' routes generated by CycleStreets.net, respectively.</p>
</div>

```
## [1] TRUE
```

Routes generated by CycleStreets.net
do not necessarily represent the paths that cyclists currently take;
route choice models based on GPS data
have been developed for this purpose [@broach_where_2012;@ehrgott_bi-objective_2012].
Of the available routes (see [cyclestreets.net/journey/help](https://www.cyclestreets.net/journey/help/howtouse/) for more information), the 'fastest' option was used.
This decision was informed by recommendations from @crow_design_2007, building on evidence of
cyclists' preference direct routes.

<!-- To generate the Route Network layer, overlapping fast routes were aggregated (see Figure 5). These stages were computationally intensive, leading to a number of steps being taken to make them fast and scalable: -->

<!-- - Adjustable selection criteria were used to sample the lines before route allocation. Parameter setting the Euclidean distance and minimum all-mode number of commutes were set for each build. -->
<!-- Note that only between-zone (interzonal) commutes were represented on the route network due to the difficulty of estimating the route taken for a trip that starts and ends in the same zone. -->
<!-- Similarly those with no fixed commute were included in area-level data but not in flow-level data. -->

<!-- After some experimentation, these were set to 15 km and more than 10 commuters. -->
<!-- ^[To illustrate the effectiveness of these selection criteria, -->
<!-- consider the example of Manchester. Setting `mflow` to 30 reduced the number of OD pairs by 85%, yet -->
<!-- still accounted for almost 70% of commuters. -->
<!-- Different values for were tested to reach a reasonable -->
<!-- balance between comprehensive coverage and speed; we settled on `mflow = 10` -->
<!-- for the national build outside London although we plan to test reducing this further. -->
<!-- We also experimented by reducing `mdist` and settled on -->
<!-- 15 km. This translates to around 20 km -->
<!-- on the route network assuming a *circuity* value of 1.3, as reported by -->
<!-- [@cole_quantitative_1968]. -->
<!-- Only 1.4% of cycle commutes recorded in the -->
<!-- National Travel Survey exceeded a distance of 20 km.] -->

<!-- - Pre-processing of route-allocated lines. This meant that CycleStreets.net was not called for every build. -->
<!-- Instead, a pre-generated file of downloaded routes was used. -->

<!-- - Parallel code. We used the R package **foreach**  to make the slowest process in the build script run on multiple computer cores. -->
<!-- This roughly halved the build times. -->

<!-- To reduce data processing and visualisation times -->
<!-- a sub-sample of OD pairs was used. -->
<!-- The aim was to reduce the number of OD pairs whilst retaining the overall -->
<!-- travel pattern. To do this a minimum number (labelled `mflow`) -->
<!-- of trips between OD pairs was specified. OD pairs with less than -->
<!-- `mflow` trips were removed from the analysis. -->
<!-- This decision was taken on the basis that the distribution of number of commuters per OD pair is skewed: -->
<!-- a relatively small number of OD pairs along major travel corridors -->
<!-- account for a disproportionately high proportion of travel. -->

The spatial distribution of cycling potential can be explored interactively by selecting the 'top n' routes with the highest estimated cycling demand (see the slider entitled "N. Lines (most cycled)" in Figures 3 and 4).
Information about the *aggregate cycling potential* on the road network is shown in the Route Network layer.
Because the layer is the result of aggregating overlapping 'fast' routes, and summing the level of cycling for each scenario (see Figure 5), it relates to the *capacity* that infrastructure may need to handle.
Cycling along Otley Road (highlighted in Figure 5), under the Go Dutch scenario, rises from 73 to 296 commuters
along a single route, but from 546 to 1780 in the Route Network.
Note that more confidence can be placed in the relative rather than the absolute size of these numbers: the Route Network layer excludes within-zone commuters, commuters with no fixed workplace, and commuters working in a different region
(see Figure 1). Route Network values also omit routes due to the adjustable selection criteria: maximum distance and minimum total numbers of all-mode commuters per OD pair. At the time of writing these were set to 20 km Euclidean distance and 10 commuters respectively.
Nationally, the Route Network layer under these settings accounts for around two thirds of cycle commuters.

<!-- The Route Network was generated with the R function `overline` [@lovelace_stplanr:_2016]. -->

<!-- ^[Thanks to Barry Rowlingson from the University of Lancaster for assistance developing this function.] -->

<div class="figure">
<img src="fastest-headingly.png" alt="Illustration of Fast Routes map layer (left) compared with Route Network layer (right). The Route Network was was produced by aggregating all overlapping lines in the Fast Routes layer using the stplanr R package." width="50%" /><img src="fastest-net-headingly.png" alt="Illustration of Fast Routes map layer (left) compared with Route Network layer (right). The Route Network was was produced by aggregating all overlapping lines in the Fast Routes layer using the stplanr R package." width="50%" />
<p class="caption">Illustration of Fast Routes map layer (left) compared with Route Network layer (right). The Route Network was was produced by aggregating all overlapping lines in the Fast Routes layer using the stplanr R package.</p>
</div>

```
## [1] TRUE
```

```
## [1] TRUE
```

<!-- # Scenarios -->

<!-- The scenarios presented in the PCT operate at the OD level: -->
<!-- change in the rate of cycling is estimated per desire line, and subsequently aggregated up to the level of areas -->
<!-- and, via steps outlined above, down to the Route Network level. -->
<!-- A wide range of models could be used to estimate cycling potential at this level and scenarios are constrained only by the imagination (as considered in the Discussion). -->
<!-- This section outlines the scenarios produced for the English version of the PCT. -->


<!-- This section deliberately avoids technical detail about -->
<!-- the functional form of the model and how model parameters -->
<!-- for each scenario were derived (see the Appendix for this technical information). -->
<!-- Instead, it provides a high-level overview of the scenarios and how they were developed, which should be of use to others developing alternative cycling scenarios to inform the transport planning process. -->

<!-- For all scenarios except *gender equality*, a regression model was used -->
<!-- to estimate the potential rate of cycling at the OD level. -->
<!-- The parameters were estimated using trips level data from the National Travel Survey -->
<!-- using a logistic regression model estimating the *dependent variable*, -->
<!-- the probability of a trip being made by cycling $pcycle$, based on -->
<!-- distance ($d$) and hilliness ($H$) -->
<!-- For aggregate data, this translates into the proportion of trips made by cycling. -->

<!-- It is well-known that the proportion of trips made by cycle (henceforth -->
<!-- $pcycle$) tends to decrease with increasing distance [@iacono_access_2008]. -->
<!-- Based on this work and exploratory analysis of the data we estimated -->
<!-- the $logit$ of $pcycle$ rather than $pcycle$ directly. -->
<!-- The logit function converts a logistic curve for values between 0 and 1 (typically a probability, $p$) -->
<!-- into a linear curve via the relation: -->

<!-- (@) $$ logit(p) = log(\frac{p}{1 - p}) $$ -->

<!-- Preliminary analysis found $h$ to have a linear relationship with $pcycle$, -->
<!-- and interaction with distance, such that routes that were both hilly and -->
<!-- length were disproportionately unlikely to be cycled, -->
<!-- based on data from the National Travel Survey (see Appendix). -->

<!-- After an iterative process of model testing, the formula chosen was: -->

<!-- (@) $$ logit(pcycle) = \alpha + \beta_1 d + \beta_2 d^{0.5} + \beta_3 d^2 + \gamma h + \delta_1 d h + \delta_2 d^{0.5} h $$ -->



<!-- where $d$ is distance (km, route distance between population weighted centroids) and -->
<!-- $h$ is the hilliness -->
<!-- per OD pair. The remaining values are scalar -->
<!-- coefficients to be estimated. -->
<!-- $\alpha$ represents the intercept (the rate of cycling very short -->
<!-- trips). $\beta_1$, $\beta_2$ and $\beta_3$ represent the rate of distance decay. -->
<!-- $\gamma$ represents the impact of hilliness on cycling and the $\delta$ coefficients represent -->
<!-- the interaction between distance and hilliness (see Appendix). -->

<!-- ## Zone Buffer -->

<!-- Running the PCT region-by-region means -->
<!-- ignoring all zones outside the region. If the region is a -->
<!-- self-contained transport system this makes sense. -->
<!-- If the region is part -->
<!-- of a larger travel network, however, this could be problematic. -->
<!-- To solve this problem, buffers were used to expand regions -->
<!-- deemed to be part of a wider system.^[Various criteria were considered -->
<!-- to decide whether or not a buffer was needed and to select the size of the -->
<!-- buffer, for example -->
<!-- whenever the number of MSOA zones fell below some threshold -->
<!-- and the amount of inter-region travel. Ultimately it was decided to -->
<!-- add buffers on an ad-hoc basis, where they were deemed necessary.] -->
<!-- To illustrate how the buffers worked, -->
<!-- buffer was used to select zones -->
<!-- outside the City of Manchester (see Fig. 7 below). -->
<!-- This protocol increased the sample size by including all -->
<!-- zones whose population-weighted centroid lies inside the -->
<!-- buffer, the width of which can also be set based on knowledge of -->
<!-- the local area.i -->

<!-- These scenarios are described more fully below. -->

<!-- ## Government Target -->

<!-- The Government Target scenario -->
<!-- is based on the UK government's proposed target -->
<!-- (as set out in its draft Cycling Delivery Plan [@department_for_transport_cycling_2014]) -->
<!-- to double cycling in England, from 0.8 billion stages currently to 1.6 billion stages by 2025. -->
<!-- However @department_for_transport_cycling_2014 says nothing about where these additional -->
<!-- trips would come from. The Government Target scenario is therefore based on our own -->
<!-- assumptions about this. It aims to assist transport -->
<!-- planners in identifying where new demand for cycling is likely to be greatest in -->
<!-- the near term. -->

<!-- The key point about this scenario is that cycling does not double in all -->
<!-- areas. Instead, the increase is related to the current -->
<!-- commuter trips and the trip distance. -->

<!-- At the heart of the Government Target scenario is the previously discussed -->
<!-- regression model (labelled $natmod$) estimating -->
<!-- the dependent variable, namely the percentage of commuters who cycle (*pcycle*). -->
<!-- The new percentage of commuters who cycle ($pcycle(govtarget)$) -->
<!-- for each desire line is calculated as the current rate of -->
<!-- cycling plus this model-based estimate: -->

<!-- (@) $$ pcycle(govtarget)_{ij} = (pcycle_{ij} + pcycle(natmod)_d) $$ -->

<!-- where $pcycle_{ij}$ is the 2011 Census proportion of commuters who cycle for an OD pair $ij$ -->
<!-- of distance $d$ apart and $pcycle(natmod)_d$ is the proportion of commuters expected to -->
<!-- cycle the distance $d$ based on the national-level regression model. -->
<!-- The sum of these values -->
<!-- can be multiplied by the total number of commuters for all modes $tflow_{ij}$ -->
<!-- to convert the proportion into a number of cyclists, i.e.: -->

<!-- (@) $$ SLC(govtarget)_{ij} = (pcycle_{ij} + pcycle(natmod)_d) * tflow_{ij} $$ -->

<!-- where $SLC(govtarget)_{ij}$ is the *Scenario-based Level of Cycling* for this scenario for the $ij$ OD pair. -->
<!-- An example of this scenario for an imaginary OD pair $ab$ with Euclidean -->
<!-- distance 4.5 km is as follows. -->
<!-- In a representative sample of OD pairs in the UK -->
<!-- of distance 4-5km, the average proportion of cycle commuters was 5%. -->
<!-- Under the 'national doubling', -->
<!-- assume an additional 5% of commuters now cycle for all -->
<!-- trips of 4.5km --- i.e. $pcycle(natmod)_{4.5} = 0.05$. -->
<!-- For our specific OD pair $ab$ in the Census there are -->
<!-- 200 commuters, of whom only 2 are cyclists ($pcycle_{ab} = 0.01$). -->
<!-- The scenario adds an additional 5% of commuters, i.e. an additional 10 cyclists which more than doubles the total number of cyclists from 2 to 12. -->
<!-- The same methodology is applied to all distances represented in the OD -->
<!-- matrix.^[Thus -->
<!-- this particular OD pair has gone from a current proportion of cyclists to 1% (five times lower than the national average of 5%) to having a scenario proportion of cyclists of 6% (just over half the national average of 10%).  This illustrates the point that this scenario tends to equalize, but does not remove, pre-existing differences in cycling levels. -->
<!-- ] -->

<!-- The approach assumes that cycling potential against a given national -->
<!-- increase is always a positive number. -->
<!-- When only modest increases in cycling are assumed -->
<!-- (e.g. from 3% to 6% as an overall mode share), -->
<!-- the pre-existing cycling demographic contributes considerably to the new demographic. -->
<!-- In more ambitious cycling scenarios the pre-existing distribution matters less. -->
<!-- This corresponds to the insight that, with sustained investment in overcoming the infrastructural and -->
<!-- cultural constraints which limit cycling, -->
<!-- the long-term potential becomes more important -->
<!-- than the current rate of cycling. -->

<!-- ## Gender equality -->

<!-- The next scenario to be discussed is Gender Equality. In this -->
<!-- scenario cycling tends to grow more in areas that already have a high -->
<!-- rate of cycling. The scenario -->
<!-- recognises that this disparity is reduced or absent in -->
<!-- countries with a high rate of cycling [@Fishman2015]. -->
<!-- The Gender Equality scenario builds on such insights -->
<!-- and is based *observed level of cycling* (*OLC*) from the 2011 Census. -->

<!-- On average in England around three quarters of cycle commuters -->
<!-- are male, although this varies geographically [@Aldred2015]. -->
<!-- Gender Equality assumes that gender equality is reached in cycling. -->
<!-- A prerequisite is a model-based estimate of the number of male -->
<!-- and female cyclists between origin and destinations for -->
<!-- the observed data. -->
<!-- This involves splitting the number of cyclists estimated by the model, -->
<!-- the *Scenario-based Level of Cycling*, into -->
<!-- male ($SLC(gendereq)_m$) and female ($SLC(gendereq)_f$) components: -->

<!-- (@) $$ SLC(gendereq) = SLC(gendereq)_m + SLC(gendereq)_f $$ -->

<!-- More males cycle to work than females in every Local Authority in England -->
<!-- (Fig. 4). For this reason, the Gender Equality scenario -->
<!-- is based on the assumption that the rate of cycling amongst -->
<!-- females increases to match the rate of cycling amongst males. -->
<!-- Under Gender Equality $SLC(gendereq)_m = OLC_m$, there are no additional male cyclists. -->
<!-- Note that this is not as simple as $SLC(gendereq)_f = SLC(gendereq)_m$, -->
<!-- as the absolute number of female and male cyclists will also depend on the gender split of the total commuting population within each OD pair.^[To -->
<!-- illustrate this point, consider an OD pair in which the total number of female -->
<!-- commuters is larger than the total number of male commuters. -->
<!-- In this case, the number of female cyclists would exceed the -->
<!-- number of male cyclists in the Gender Equality scenario. -->
<!-- ] -->
<!-- It is the *proportion* of males and females -->
<!-- per OD pair who cycle that becomes equal, as follows. -->

<!-- (@) $$ pcycle(gendereq)_f = pcycle_{m}$$ -->

<!-- (@) $$ \frac{SLC(gendereq)_f}{tflow_f} = \frac{OLC_m}{tflow_m} $$ -->

<!-- (@) $$ SLC(gendereq)_f = tflow_f * \frac{OLC_m}{tflow_m}$$ -->

<!-- $OLC_m$ is the observed number of male cycle commuters -->
<!-- (in the 2011 Census in this case), -->
<!-- $SLC(gendereq)_f$ is number of female cycle commuters in the gender equality scenario, and -->
<!-- $tflow_m$ and $tflow_f$ are the total numbers of males -->
<!-- and females in the OD pair respectively. -->

<!-- $tflow_m$ and $tflow_f$  are both available at the OD level in the 2011 Census, -->
<!-- as is the total number of cyclists ($OLC$).  The proportion of cyclists who are male -->
<!-- in each OD pair ($pmale_(cyclist)$) is not available in the published 2011 datasets. -->
<!-- The smallest level at which the gender breakdown of cyclists is currently available -->
<!-- is the zone level ('$pmale_{cyclist}(zone)$'), and we assume that all OD pairs have -->
<!-- this same proportion of male cyclists. This allows the estimation -->
<!-- the number of male cycle commuters as $OLC_m = OLC * pmale_{cyclist}(zone)$, so that -->

<!-- (@) $$ SLC(gendereq)_f = OLC * pmale_{cyclist}(zone) *  \frac{tflow_f}{tflow_m} $$ -->

<!-- and therefore the total number of trips for gender equality $SLC(gendereq)$ would be -->

<!-- (@) $$ SLC(gendereq) = OLC_m + SLC(gendereq)_f $$ -->

<!-- (@) $$ SLC(gendereq) = OLC * pmale_{cyclist}(zone) * (1 + \frac{tflow_f}{tflow_m}) $$ -->

<!-- ```{r, echo=FALSE, fig.cap="Cycling and the gender balance of cycling in England. The choropleth maps illustrate the spatial distribution of the two variables. The scatter plot illustrates the relationship between the two variables cycle commuting (x axis) against the proportion of commuter cyclists who are male (y axis) for all 326 Local Authorities (including Districts) in the UK."} -->
<!-- grid.raster(readPNG("../figures/las-gender-pcycle.png")) -->
<!-- ``` -->

<!-- To illustrate how this method works in practice, imagine an OD pair in which -->
<!-- 50 from a total of 500 people commute by cycle ($tflow = 500; OLC = 50$). -->
<!-- 300 of the total trips in the OD pair are made by males -->
<!-- ($tflow_m = 300$) and 200 by females ($tflow_f = 200$). -->
<!-- In addition, 70% of commuter cycling in the wider zone is by -->
<!-- males ($pmale_{cyclist}(zone) = 0.70$). This means that -->
<!-- an estimated $50 * 0.70 = 35$ cycle commuters are male ($OLC_m = 35$) and -->
<!-- 15 are female ($OLC_f = 15$). -->

<!-- Applying the formulae presented previously: -->

<!-- (@) $$ SLC(gendereq)_f = OLC * pmale_{cyclists}(zone) * (1 + \frac{tflow_f}{tflow_m}) $$ -->

<!-- (@) $$ SLC(gendereq) = 50 * 0.70 * (1 + \frac{200}{300} ) = 58.3 $$ -->

<!-- ```{r, echo=FALSE} -->
<!-- tflow <- 500 -->
<!-- olc <- 50 #  the currect rate of cycling between origin (o) and destination (d) -->
<!-- tflow_m <- 0.6 # the proportion of all trips o and d by males -->
<!-- pcyclez_m <- 0.75 # the proportion of cycle trips in the zone/region made by males -->
<!-- tflow_m <- tflow * tflow_m -->
<!-- tflow_f <- tflow * (1 - tflow_m) -->
<!-- olc_m <- olc * pcyclez_m -->
<!-- olc_f <- olc - olc_m -->
<!-- pmale_c <- olc_m / tflow_m -->
<!-- slc_gendereq_f <- tflow_f * pmale_c -->
<!-- # print(olc_f) -->
<!-- # print(slc_gendereq_f) -->
<!-- ``` -->

<!-- The increase from 50 cyclists to 58.3 represents an increase -->
<!-- of 17% from the observed rate of cycling -->
<!-- in total numbers of cyclists.  All of these extra 8.3 -->
<!-- cyclists are female, giving a new total of 15 + 8.3 = 23.3 female cyclists -->
<!-- (and still 35 male cyclists). -->
<!-- Gender equality in cycling has been reached, such that an estimated 11.7% -->
<!-- of commute trips are made by cycling among both men (35/300) -->
<!-- and women (23.3 / 200). -->

<!-- ## Go Dutch -->

<!-- The 'Go Dutch' scenario represents the rate of cycling that would occur -->
<!-- if people had the same propensity to cycle as the Dutch do, for trips of -->
<!-- the same length and hilliness. It is -->
<!-- important to note that this is not a 'top down' scenario in which the national -->
<!-- level of cycling is set to levels found in The Netherlands. The scenario is -->
<!-- 'bottom up' because the proportion of trips being cycled is set per OD pair -->
<!-- and the end result for any particular region depends on the local distribution -->
<!-- of trip distances. -->
<!-- Although the Dutch currently cycle far more frequently than the -->
<!-- English for short trips, their propensity to cycle still drops rapidly with -->
<!-- distance, with relatively few utility trips being made beyond around 15 km. -->

<!-- Based on these insights, the essence of the 'Go Dutch' scenario -->
<!-- is the application of distance decay parameters -->
<!-- found in the Netherlands to each OD pair in the study area. -->

<!-- In contrast to Government Target and Gender Equality scenarios, -->
<!-- Go Dutch is unrelated to the current rate of cycling. The scenario -->
<!-- thus represents the elimination of localised -->
<!-- constraints which inhibit cycling more in some area than others. -->
<!-- Local cycling in Go Dutch is therefore constrained -->
<!-- only by trip distances and hilliness. -->

<!-- ## E-bikes -->

<!-- This scenario aims to provide insight into how cycling -->
<!-- could shift with the uptake of electric cycles ('E-bikes'). -->
<!-- The scenario builds on 'Go Dutch' and represents -->
<!-- a reduction in the degree to which cycling is -->
<!-- constrained by trip distances and technology. -->
<!-- This is the most ambitious and speculative -->
<!-- scenario presented in this paper. -->

<!-- The results are -->
<!-- based on the decision to increase by a small amount the $\beta_1$ distance decay parameter, -->
<!-- which corresponds to distance as a linear term. -->
<!-- Specifically, we increased this value by 0.025, as we found -->
<!-- this to be sufficiently small to avoid generating an -->
<!-- implausibly high rate of cycling but sufficiently large to create a noticeable effect. -->
<!-- This allows us to illustrate the type of output that will -->
<!-- be possible in this model. In future work we plan to -->
<!-- update this scenario, basing the changes to the distance decay -->
<!-- parameters on real data from the Dutch National Travel Survey. -->
<!-- This will build on analysis of the influence of E-bikes on propensity to cycle -->
<!-- in the Netherlands, research that is being undertaken in parallel to the work -->
<!-- presented in this paper. -->

# Outputs of the Propensity to Cycle Tool

This section describes and illustrates some outputs from the PCT, alongside discussion of how these outputs could be used in transport planning.  Note that some details of the graphics in the online version may evolve as the PCT develops.

## Model output tabs

Tabs are panels within the PCT that reveal new information when clicked (see the top of Figure 3). Of these, the first four provide region-specific information:

- **Map**: This interactive map is the main component of the PCT, and is the default tab presented to users. It shows cycling potential at area, desire-line, route and route network levels under different scenarios of change, as described throughout this paper. 'Popups' appear when zones, desire lines or segments on the Route Network are clicked, presenting quantitative information about the selected element.

- **Lines**: When lines are displayed on the interactive map, this tab provides raw data on a sample of the variables as a table at the OD pair level.

- **Areas**: This tab is the equivalent of the 'Lines' tab, but with data at the area level.

- **Model output**: This tab provides key statistics, diagnostic plots and model results for each region. The document is produced by a 'dynamic document' which runs embedded code for each regional dataset.
Diagnostic plots include the distribution of cycling by trip distance under each scenario (see Figure 6), providing insight into local travel patterns and how they relate to cycling potential in the region overall.

## Trip distance distributions

Figure 6 shows how the proportion of trips made by cycling varies as a function of distance in two regions currently, and under the PCT's four scenarios of change. The frequency of all mode trips by distance band (the red lines) illustrates the two regions have different spatial structures. Oxfordshire has a high proportion short (under 5km) trips, helping to explain the relatively high level of cycling there. West Yorkshire (Figure 6, left), by contrast, has a higher proportion of longer distance commutes and a lower level of cycling than Oxford.
Note that under Go Dutch and Ebikes scenarios, regional differences in the rate of cycling diminish, however, illustrating
that these scenarios are not influenced by the current level of cycling.

<!-- These results (presented in the Model Output tab in the PCT) suggest that there is high cycling potential in both regions but perhaps imply that West Yorkshire should prioritise the 'low hanging fruit' of short trips not currently cycled whereas in Oxfordshire extra cycling potential is more evenly distributed across all cyclable distances. -->



<div class="figure">
<img src="dd-west-yorks.png" alt="Modal share of trips made by cycling in West Yorkshire (left) and Oxfordshire (right) by distance, currently and under 4 scenarios of change." width="50%" /><img src="dd-ox.png" alt="Modal share of trips made by cycling in West Yorkshire (left) and Oxfordshire (right) by distance, currently and under 4 scenarios of change." width="50%" />
<p class="caption">Modal share of trips made by cycling in West Yorkshire (left) and Oxfordshire (right) by distance, currently and under 4 scenarios of change.</p>
</div>

```
## [1] TRUE
```

```
## [1] TRUE
```

<!-- In Manchester the Government Target scenario -->
<!-- has a considerably higher rate of cycling than the Gender Equality -->
<!-- scenario, whereas in Norwich these scenarios are very similar. This is because -->
<!-- Manchester is starting from a lower baseline, so a doubling nationwide -->
<!-- results in a relatively high absolute increase in cycling locally. In Norwich, -->
<!-- by contrast, the current rate of cycling is considerably greater than the -->
<!-- national average, so the Government Target scenario represents less than a doubling in -->
<!-- cycling. -->

## The shifting spatial distribution of cycling and associated impacts

The spatial distribution of cycling potential differs markedly between scenarios, as illustrated in Figure 7 for the city of Leeds, West Yorkshire. The top 50 OD pairs in Leeds under Government Target are strongly influenced by the current distribution of cycling trips, concentrated in the North of the city (see Figure 3 for comparison with the baseline).
Under the Go Dutch scenario, by contrast, the pattern of cycling shifts substantially to the South.
The cycling patterns under the Go Dutch scenario
are more representative of short-distance trips across the city overall.
In both cases the desire lines are focussed on Leeds city centre: the region
has a mono-centric regional economy, making commute trips beyond around 5 km from the
centre much less likely to be made by cycling.

<div class="figure">
<img src="od-scens-leeds.png" alt="Model output illustrating the top 50 most cycled OD pairs in Leeds under the Government Target and Go Dutch scenarios." width="100%" />
<p class="caption">Model output illustrating the top 50 most cycled OD pairs in Leeds under the Government Target and Go Dutch scenarios.</p>
</div>

```
## [1] TRUE
```

<!-- The equivalent results are shown for the city of Manchester in Figure 8. This shows that -->
<!-- Manchester has a poly-centric structure, favouring the construction of cycle -->
<!-- routes between the various sub-centres, not just in radial routes to a single -->
<!-- centre. As shown in the scale bar up the left-hand side, -->
<!-- the absolute level of cycling is much lower in Government Target -->
<!-- (which represents only a doubling nationwide) -->
<!-- than in the much more ambitious Go Dutch scenario. -->

The same scenario is illustrated in Figure 8 with the Route Network layer.
This shows how the number of commuter cyclist using different road segments
could be expected to change.
The number using York Road, highlighted in Figure 8, for example more than triples (from 88 to 318)
under Government Target and increases more than 10 fold under Go Dutch (from 88 to 1426).
This contrasts with Otley Road (highlighted in Figure 5), which 'only' triples under the Go Dutch scenario.
These outputs suggest that the geographical distribution of cycling may shift if the proportion of trips cycled increases in the city.
The results also suggest that cycle paths built to help achieve ambitious targets, as represented by the Go Dutch scenario, should be of sufficient width to accommodate the estimated flows.

<!-- These results suggest that as cycling grows, policy interventions in Leeds should shift from routes in the Northwest of the city, the area with the highest current level of cycling, to routes that have low current rates of cycling but high potential, such as York Road to the East.  -->

<div class="figure">
<img src="leeds-rnet-gov-go.png" alt="The Route Network layer illustrating the shifting spatial distribution of cycling flows in Leeds under Government Target (top) and Go Dutch (bottom) scenarios." width="100%" />
<p class="caption">The Route Network layer illustrating the shifting spatial distribution of cycling flows in Leeds under Government Target (top) and Go Dutch (bottom) scenarios.</p>
</div>

```
## [1] TRUE
```

Another output that can be highly policy relevant is the difference between
'fast' and 'quiet' routes.
Figure 9 illustrates this by showing the Fast & Quiet Routes layer in Manchester with the highest cycling
potential under the Government Target scenario.
The 'quiet' route is longer: 2.8 km
(as shown by clicking on the line) compared with the more direct
fast route which is 2.3 km. However, the 'fast' route may not currently be attractive for cycling as it passes
along a busy dual carriage way.
The Euclidean distance associated with this OD pair is 1.6 km
(this can be seen by clicking on a line illustrated from the 'Straight Lines' layer in the PCT's interface),
resulting in 'circuity' [see @iacono_access_2008], values of
1.6 and 1.4 for 'quiet' and 'fast' routes respectively.

Dutch guidance suggests that circuity values
"for cycle provision should be 1.2" [@crow_design_2007].
Evidence indicates that women and older people have a greater preference for off-road and shorter routes [@garrard_promoting_2008;@woodcock_national_2016]. This suggests the 'fast route' option, if built to a high standard, may be favourable from an equity perspective in this context.

<div class="figure">
<img src="man-trinity-way.png" alt="Close-up of a 'fast' and 'quiet' route in the PCT under the Government Target scenario in Manchester. This provides an indication of the local 'quietness diversion factor'." width="100%" />
<p class="caption">Close-up of a 'fast' and 'quiet' route in the PCT under the Government Target scenario in Manchester. This provides an indication of the local 'quietness diversion factor'.</p>
</div>

```
## [1] TRUE
```

Three basemap options are worth highlighting in addition to the grey default basemap.
These were selected to provide insight into how the geographical distribution of latent demand for cycling relates to current cycle infrastructure and socio-demographics:
'OpenCycleMap' indicates where cycle provision is (and is not) currently; 'Index of Deprivation' illustrates the spatial distribution of social inequalities; and the 'Satellite' basemap can help identify opportunities for re-allocating space away from roads and other land uses for cycle and walking paths by providing visual information on road widths and land uses along desire lines. The 'Satellite' basemap option is illustrated in Figure 10, which shows a section of Trinity Way (as it crosses the River Irwell). This shows there are 4 lanes of traffic, a central paved area and wide pavements on both sides of the road, suggesting there may be space for a cycle path, especially if road space were re-allocated away from motorised traffic.

<div class="figure">
<img src="man-trinity-way-satellite.png" alt="Zoomed-in section of Trinity way (see Figure 9) using the Satellite basemap to show road width and the number of lanes allocated to motorised traffic." width="100%" />
<p class="caption">Zoomed-in section of Trinity way (see Figure 9) using the Satellite basemap to show road width and the number of lanes allocated to motorised traffic.</p>
</div>

```
## [1] TRUE
```

<!-- ## The Model Output tab -->



<!-- ## Visualising Health and Carbon Benefits -->

Another feature of the user interface worth highlighting is addition of a dropdown menu to enable the 'top n' routes to be selected not only based on the level of cycling, but also based on estimated health and carbon impacts, under each scenario.
The reason for this addition was the finding that health benefits do not always rise in simply in proportion to the number of people cycling:
longer trips lead to a greater health benefit than short ones do, and this is now represented by shifting distribution of lines when the "HEAT Value" option is selected from the "Order lines/flows by" dropdown menu (this menu only appears when lines are shown *and* a scenario other than the Census 2011 is selected).

This feature is illustrated in Figure 5, economic value of health benefits reported for the 4.1 km route in Figure 5 is estimated to be £70785. When health benefits are the main criteria for policy evaluation, OD pairs with low current rates of walking would be favoured for intervention.
When emissions are the main criteria, OD pairs with a high baseline level of car use are also favoured.
The exploration of these considerations is facilitated in the PCT by allowing users to select the top routes ranked by health and carbon benefits.

# Discussion

We have outlined a method for modelling and visualising the spatial
distribution of cycling flows, currently and under various scenarios of 'cycling futures'.
Inspired by previous approaches to estimating cycling potential [@larsen_build_2013;@zhang_prioritizing_2014]
and by online, interactive planning support systems (PSS) [@pettit_online_2013], the
PCT tackles the issue
of how to generate an evidence base to decide where new cycle paths and other localised pro-cycling interventions should be prioritised.
By showing potential health-related benefits the tool provides various metrics for transport planners, going beyond the number of additional trips.
Illustrative uses of the PCT demonstrated the potential utility of the tool, for example
by showing settings in which the spatial distribution
of cycling demand is likely to shift as cycling grows. 

<!-- For example, the output of the PCT suggests that the 'centre of gravity' for cycling in the city of Leeds will shift towards areas with high numbers of short distance trips but low current rates of cycling. -->

In addition to creating an evidence base for planning specific routes and area-based interventions, the long-term Go Dutch and Ebikes scenarios could be used for envisioning different transport futures [@hickman_transitions_2011].
The PCT could also: help translate national targets into local aspirations (as illustrated by the Government Target scenario); inform local targets (e.g. by indicating what the potential in one region is relative to neighbouring regions); support business cases (by showing that there is high cycling potential along proposed routes); and help plan for cycle capacity along the route network via the network analysis layer.
Ongoing case study work with stakeholders will be needed to establish and develop these uses. 
Future developments will be facilitated by the open source code-base underlying the PCT (see [github.com/npct](https://github.com/npct)) [@lima_coding_2014].


<!-- The underlying code is open source (see [github.com/npct](https://github.com/npct)), -->
<!-- making it easy for others use the project as a basis for further work [@lima_coding_2014]. -->
<!-- This flexibility -->
<!-- has been demonstrated by the tool's ability to be deployed in any -->
<!-- Local Authority (or other administrative area) -->
<!-- in England. -->
<!-- <!-- Extensibility has been demonstrated by the addition of --> 
<!-- <!-- new scenarios, and the addition of new features to the tool following user feedback.  --> 

<!-- An indication of how the -->
<!-- tool could be used was illustrated -->
<!-- with maps showing the shifting distribution of cling -->
<!-- under 'Government Target' and 'Go Dutch'  -->
<!-- scenarios in Leeds and Manchester. These outputs suggested -->
<!-- the 'centre of gravity' for gravity will shift, towards areas with high -->
<!-- numbers of short distance trips (and therefore high latent demand) but -->
<!-- low current rates of cycling. -->
<!-- The scenarios suggest the PCT could be used for 'visioning' transport futures futures [@tight_visions_2011]. -->

<!-- Such developments could further enhance the PCT's ability to provide an evidence base to support local knowledge. -->
<!-- Future (more sophisticated) versions of the PCT could provide a visual evidence base to help answer the following question [see @Sloman2014; @Aldred2014]:   -->
<!-- Should cycling investment prioritise -->
<!-- areas of relatively low current propensity but high potential, or those of relatively -->
<!-- high current propensity but lower potential? -->

<!-- In the Netherlands (representing long-term ambition), -->
<!-- cycling is equally popular -->
<!-- among males and females and across -->
<!-- different socio-economic and age groups, -->
<!-- although some ethnic and religious differences exist [@Fishman2015]. -->
<!-- Demographics should therefore play less of a -->
<!-- role in estimating cycling potential for strategic purposes -->
<!-- than for identifying 'quick-win' policies based on current propensities. -->

<!-- Based on this understanding, the PCT has the potential to represent spatial -->
<!-- *and* demographic shifts in cycling as it grows. -->

<!-- An evaluation of the PCT *after* it has been launched (July 2016) and used by practitioners will be needed to assess the extent to which the approach assists transport-land use planners. -->
<!-- This will test our hypotheses that the tool can help translate national targets into local aspirations (as illustrated by the Government Target scenario), set local targets (by indicating what the potential is relative to other regions), -->
<!-- support business cases (by showing that there is high cycling potential along proposed routes) and plan -->
<!-- for capacity increases in cycling along the route network via the network analysis layer. -->
<!-- Above all, post-launch evaluation will be able to assess the extent to which the approach can -->
<!-- help design strategic cycling networks. -->

<!-- ^[Targets have proliferated in recent years. For instance, an official -->
<!-- target to reach 10% of trips made by bicycle has been -->
<!-- made by authorities in Dublin, Leicester and across -->
<!-- all of Scotland over various time-scales [@beatley_green_2012]. A mode share of -->
<!-- "20% by 2020" has been set for several cities including San Francisco and -->
<!-- Orlando.] -->


As with any modelling tool, the approach presented in this paper has
limitations: the reliance on Census origin-destination (OD) data from 2011 means that the results are limited to commuting and may not encapsulate recent shifts in travel behaviour, and the user interface is constrained to a few, discrete, scenarios.
These limitations suggest directions for future work, most notably the use of new sources of OD data.

<!-- ; and versions of the model to represent the impact of specific improvements to the route network (e.g. by modifying the 'quietness diversion factor'). Beyond these changes, further extensions of the PCT could include: -->

There is often a tension between transparency and complexity in the design of tools for transport planning. Excessive complexity can result in tools that are 'black boxes' [@saujot_making_2016].
In a context of limited time, expertise, and resources, Saujot et al. caution against investing
in ever more complex models.
Instead, they suggest models should be more user focussed.
The PCT’s open source, freely available nature will, we believe, facilitate the future development of the PCT organically to meet the needs of its various users.
To encourage others to use the outputs of the PCT project, we make the data underlying the online map available to download (via the Zones and Lines tabs in the tool's online interface).
We envision stakeholders in local government modifying scenarios for their own purposes, and that academics in relevant fields may add new features and develop new use cases of the PCT. Such enhancements could include:

- Additional scenarios to illustrate a wider range of 'cycling futures', including medium-term and local targets.

- Use of individual level data to estimate cycling potential and impacts.
The use of synthetic 'spatial microdata', for example, could enable specification of scenarios and
analysis of outcomes by a much wider range of predictors [@lovelace_spatial_2014].

<!-- - The incorporation of changes in land-use and transport since the Census 2011, -->
<!-- to explore the dynamics of cycling potential, such as the impact of new residential and commercial developments. -->
<!-- This work could build on recently developed GIS for transport planning [@farrell_evaluating_2015]. -->

- Additional purposes of trips in the model. An 'education layer' would enable prioritisation of 'safe routes to school', building on methods analysing 'school commute' data [@singleton_gis_2014].
Other data sources to include more trip types include mobile telephone providers [@alexander_validation_2015] and outputs from transport models.

- Deployment of the PCT for new cities, regions or countries. This will depend on the availability of appropriate OD data, perhaps from sources mentioned in the previous point, and routing services that can estimate cyclable routes based on globally available data, e.g. using the function `route_graphhopper` from **stplanr** package.
Such work could also facilitate international comparisons of cycling potential.

Transport planning is a complex and contested field [@banister_sustainable_2008].
When it comes to sustainable mobility, policy, politics, leadership and vision are key ingredients that computer models alone cannot supply [@melia_urban_2015].
The approach described here can, however, assist in this wider context by providing new tools for exploring the evidence at high geographical resolution and envisioning transformational change in travel behaviours.

By providing transport authorities, campaign groups and the public with access to the same evidence base, we hypothesise that tools such as the PCT can encourage informed and rigorous debate, as advocated by @golub_making_2013. In conclusion, the PCT provides an accessible evidence base to inform the question of where to prioritise interventions for active travel and raises more fundamental questions about how models should be used in transport planning.

<!-- limitations: the reliance on 2011 Census OD data means that the results are -->
<!-- not up-to-date; -->
<!-- there are no scenarios representing specific infrastructure -->
<!-- interventions; and the user interface is constrained to a few discrete scenarios. -->
<!-- These limitations open-up the potential for future work, including: -->
<!-- using more up-to-date sources of OD data; creating a version of the model -->
<!-- to represent the impact of specific improvements to the route network -->
<!-- (e.g. by modifying the 'quietness diversion factor', described above); -->
<!-- and the implementation of continuous variables to define future scenarios. -->
<!-- Further extensions of the PCT  -->
<!-- could include: -->

<!-- - Additional scenarios to illustrate a wider range of 'cycling futures', including medium-term and local targets such as 'Go York' (where 12% of commuters cycle to work). -->

<!-- - Additional 'output tabs' in the PCT's user interface, to estimate the -->
<!-- quantitative benefits of cycling uptake at the local (and potentially -->
<!-- route-allocated) level. Benefits estimated could include -->
<!-- health gains due to increased physical activity, as estimated using models such as -->
<!-- the Health Economic Assessment Tool -->
<!-- (HEAT), endorsed by the World Health Organisation [@fraser_cycling_2011]. -->
<!-- The tool could also be used as a basis for estimating and communicating the local energy and -->
<!-- carbon savings of cycling uptake, previously estimated at city levels [e.g. @lovelace_assessing_2011]. -->

<!-- - Deployment of the PCT for entire countries. -->
<!-- This would depend on having appropriate OD data and could -->
<!-- build on emerging 'Big Data' sources for origin-destination -->
<!-- flows [@alexander_validation_2015]. -->

<!-- - International comparisons of cycling potential. -->
<!-- This could include an exploration of -->
<!-- the relationship between places of high potential and investment. -->
<!-- We have already begun this by using Dutch distance decay functions in an English -->
<!-- context, but more could be done by fully implementing the model in different -->
<!-- country contexts. -->

<!-- - Additional purposes of trips in the model. -->
<!-- An 'education layer' would enable prioritisation of 'safe routes to school', -->
<!-- building on methods analysing 'school commute' data [@singleton_gis_2014]. -->

<!-- - Use of alternative input data data, such as the location of new developments and road traffic speed and volumes. This work could build on recently developed GIS-based methods for transport planning [@farrell_evaluating_2015]. -->

<!-- The PCT's open source license allows others to -->
<!-- modify it for their own needs. , -->
<!-- to modify the scenarios, input data and display of the results to suit -->
<!-- local contexts. This could, for example, help to visualise city-level targets for -->
<!-- the proportion cycling by a certain year and -->
<!-- which will vary considerably from place to place in ways not yet well understood. -->
<!-- Modifying the code base would also allow transport planners -->
<!-- to decide on and create the precise set of online tools that are most useful for their work. -->
<!-- Building on participatory models at the macro-level [@Macmillan2014], extensions -->
<!-- to the model could include using the PCT methodology to -->
<!-- enable public engagement in the strategic planning process around -->
<!-- sustainable transport. -->

<!-- Transport planning is a -->
<!-- complex and contested field [@banister_sustainable_2008]. -->
<!-- Policy, politics, leadership and vision are key ingredients for sustainable -->
<!-- urban mobility that computer models alone cannot supply [@melia_urban_2015]. -->
<!-- The approach described here can assist in this wider context by providing -->
<!-- new tools for exploring the best available evidence. -->
<!-- By providing transport authorities, campaign groups and the public with access to the same evidence base, -->
<!-- we hypothesise that tools such as the PCT can encourage informed and rigorous debate, -->
<!-- as advocated by @golub_making_2013. -->
<!-- In conclusion, although the PCT aims to inform the specific issue of where to construct -->
<!-- new infrastructure for cycling, it relates to the wider -->
<!-- question of how to create an evidence-base for designing sustainable transport plans. -->

<!-- Moreover, by highlighting the importance of 'arterial' routes to key destinations, -->
<!-- the PCT can help rejuvenate long-standing debates -->
<!-- such as the re-allocation of road space away from -->
<!-- private cars [@Jones2014;@Sharples2009;@Black1992]. -->



<!-- The flexibility of the approach outlined in this paper means that the -->
<!-- PCT can be seen not only as a tool but as a framework for strategic -->
<!-- transport planning. -->
<!-- Under this interpretation the case study of cycling in England is -->
<!-- just one of many potential applications. -->
<!-- Still, a number of the lessons learned throughout the -->
<!-- development and user testing of the tool are generalisable internationally. -->
<!-- Indeed, one of the major motivations for writing this paper is to showcase the method -->
<!-- for use by others to avoid 'reinventing the wheel' to help answer the common question of where to build. -->



# Author contribution statement {-}

The PCT was built as a collaborative effort. The Principal Investigator of the project was JW, and the initial concept for the project came from JW and RL in response to a call from the Department of Transport. AG led the creation of the model underlying the PCT, and the generation of estimates of cycling levels, health gains and carbon impacts in each scenario.  JW and AG led the development of the methods for calculating health impacts.  JW, RL, AG, and RA contributed to the development of the cycling uptake rules. RL led the processing of these modelled estimates for spatial visualisation in the online tool and coordinated the development of the online tool.  RA led the coordination of policy implementation and collection of practitioner feedback. AA and NB led on the user interface, with contributions from all authors. NB led the deployment of the PCT on a public facing server. AG led the writing of the Appendix. RL led the writing of this manuscript, with input from all authors.

# Acknowledgements {-}

We would like to thank the following people for comments on earlier versions of the manuscript and the development of the PCT:
Roger Geffen (Cycling UK), Tom Gutowski (Sustrans),
Helen Bowkett (Welsh Government),
John Parkin (University of the West of England), Nikée Groot and Phil Tate.
Thanks to Simon Nutall and Martin Lucas-Smith for access to and instructions
on the use of the CycleStreets.net API.
Thanks to Barry Rowlingson from the University of Lancaster for developing functionality in the **stplanr** package that enabled the creation of the Route Network layer.
We are grateful to Matthew Tranter at DfT for merging additional geographic data into the UK National Travel Survey, and to Eva Heinen, Rick Prins and Thomas Götschi for their help in analysing Dutch and Swiss Travel Survey data.
Thanks to Alvaro Ullrich for contributions to the code.
Thanks to developers of open source software we have been able to make
the PCT free and open to the world.
We would also like to thank Brook Lyndhurst for assistance with the user testing,
and all participants in the user testing sessions.
We would also like to thank Shane Snow and other staff at the DfT for
specifying the project's aims and providing
feedback on early versions of the tool.

# Funding {-}

The work presented was funded by the Department for Transport (contract no. RM5019SO7766: "Provision of Research Programme into Cycling: Propensity to Cycle"), with contract facilitation and project management by Brook Lyndhurst in Phase 1, and by Atkins in Phase 2.
RL’s contribution was
supported by the Consumer Data Research Centre (ESRC grant number ES/L011891/1).
JW's contribution was supported by an MRC
Population Health Scientist Fellowship. JW’s and AA’s contributions were supported
by the Centre for Diet and Activity Research (CEDAR), a UKCRC Public Health
Research Centre of Excellence funded by the British Heart Foundation, Cancer
Research UK, Economic and Social Research Council, Medical Research Council, the
National Institute for Health Research (NIHR), and the Wellcome Trust. AG’s
contribution was supported by an NIHR post-doctoral fellowship. The views
reported in this paper are those of the authors and do not necessarily represent
those of the DfT, Brook Lyndhurst, Atkins the NIHR, the NHS or the Department for
Health.



# References



