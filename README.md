# Statistical Consulting Project

Add here some ideas :) 

1. Only confirmed cases from https://github.com/CSSEGISandData/COVID-19/tree/master/who_covid_19_situation_reports;
2. Hospital bed as variable from OECD (data folder);
3. Reference lockdown across states: 
    - https://www.bsg.ox.ac.uk/sites/default/files/2020-05/BSG-WP-2020-032-v5.0_0.pdf
    - https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic
    - https://www.politico.eu/article/europes-coronavirus-lockdown-measures-compared/
    - https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker
    - https://www.businessinsider.com/countries-on-lockdown-coronavirus-italy-2020-3?IR=T
    - https://en.wikipedia.org/wiki/Stay-at-home_order#2020_coronavirus_pandemic
    - https://en.wikipedia.org/wiki/Curfews_and_lockdowns_related_to_the_2019%E2%80%9320_coronavirus_pandemic
    - https://www.cdc.gov/coronavirus/2019-ncov/php/risk-assessment.html
    - Lockdown policies analysis: https://www.mckinsey.com/~/media/McKinsey/Business%20Functions/Risk/Our%20Insights/COVID%2019%20Implications%20for%20business/COVID%2019%20April%2013/COVID-19-Facts-and-Insights-April-24.ashx
4. State Ranking of cases: 
    - https://www.worldometers.info/coronavirus/
 
   Data:
    - https://covidtracker.bsg.ox.ac.uk/
    - https://github.com/OxCGRT/covid-policy-tracker/
    - NEW!!!! https://www.commonwealthfund.org/blog/2020/how-us-compares-other-countries-responding-covid-19-populations-risk-health-system (health and pop variables, only 23 countries)
 5. Mobility Data: https://www.google.com/covid19/mobility/data_documentation.html?hl=en (in data folder)
 6. Updating policies lockdown reference: https://www.imf.org/en/Topics/imf-and-covid19/Policy-Responses-to-COVID-19
 7. References stringency index: https://www.bsg.ox.ac.uk/sites/default/files/Calculation%20and%20presentation%20of%20the%20Stringency%20Index.pdf; https://www.bsg.ox.ac.uk/sites/default/files/2020-05/BSG-WP-2020-032-v5.0_0.pdf
Some comments about McKinsey report:

 1. **Temporal correlation between R0 and changing of mobility**: It seems that a mobiity reduction during lockdowns is roughly correlated with reductions in transmission, in general implementation of public health measures leading to reduced mobility (e.g. closure of public spaces, lock downs, closure schools) have successsgully reduced COVID-19 transmission. However, in SOUTH KOREA they implement robust testing and tracing than reducing in mobility, in ITALY large portion of population was infected before lockdown measures were enforced, making trasmission more difficult to control even after lockdown, and in NORWAY the geographical and environmental factors contribute to naturally low rates of spread despite limited control measures. THe seemingly low drop in Rt corresponds to a 40% decrease from baseline. 
 2. **Plot incremental COVID cases and conducted tests**: Early testing and tracking capacity has enabled some countries to contain incremental cases. Evidence of how the confirmed cases decrease as the countries expandeded to a mass testing approach;

**References Functional Mixed model**:
 1. https://www.ece.uvic.ca/~bctill/papers/mocap/Guo_2002.pdf
 2. https://www.stat.tamu.edu/~carroll/ftp/2006.papers.directory/rssb_b6165.pdf

**References contact lockdown**:
 1. https://www.ecdc.europa.eu/sites/default/files/documents/Contact-tracing-Public-health-management-persons-including-healthcare-workers-having-had-contact-with-COVID-19-cases-in-the-European-Union%E2%80%93second-update_0.pdf
 2. https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-use-face-masks-community.pdf
 3. https://www.ted.com/talks/the_ted_interview_adam_kucharski_on_what_should_and_shouldn_t_worry_us_about_the_coronavirus 
 **References Italian Region**:
 1. https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-05-04-COVID19-Report-20-Italian.pdf
 
 **Estimate R0**:
 1. https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/#estimating-changes-in-the-effective-reproduction-number
 2. https://www.sciencedirect.com/science/article/pii/S1201971220301193
 3. https://github.com/annecori/EpiEstim/blob/master/vignettes/demo.Rmd
 4. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3816335/
 5. https://github.com/timchurches/blog/blob/master/_posts/2020-03-01-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-2/analysing-covid-19-2019-ncov-outbreak-data-with-r-part-2.Rmd
 6. https://timchurches.github.io/blog/posts/2020-03-01-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-2/#modelling-epidemic-trajectory-in-hubei-province-using-log-linear-models
 7.https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/
 8. https://cmmid.github.io/topics/covid19/global-time-varying-transmission.html

Some simulated analysis about intervantion policies:
 1. https://timchurches.github.io/blog/posts/2020-03-18-modelling-the-effects-of-public-health-interventions-on-covid-19-transmission-part-2/
 2. https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/#estimating-changes-in-the-effective-reproduction-number
