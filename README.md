# eprivacy-sro
Replication material for the paper "The closing educational gap in e-privacy management in European perspective", in published in Sociological Research Online [doi](https://doi.org/10.1177/13607804211023524).

## Main syntax
The stata syntax Syntax_manuscript_revision.do replicates all the analyses and most of the plots included in the manuscript. 

In order to run the syntax, the following datasets have to be downloaded from their original sources:
- Individual level data: European Commission and European Parliament, Brussels (2017): Eurobarometer 87.1 (2017). TNS opinion, Brussels [producer]. GESIS Data Archive, Cologne. ZA6861 Data file Version 1.2.0, [doi.org/10.4232/1.12922](https://doi.org/10.4232/1.12922)
- DESI: European Commission (2019): Digital Economy and Society Index, reference period: 2017. Digital Scoreboard [link](https://digital-agenda-data.eu/charts/desi-composite#chart={%22indicator%22:%22desi_sliders%22,%22breakdown%22:{%22desi_1_conn%22:5,%22desi_2_hc%22:5,%22desi_3_ui%22:3,%22desi_4_idt%22:4,%22desi_5_dps%22:3},%22unit-measure%22:%22pc_desi_sliders%22,%22time-period%22:%222017%22}). Data extracted on 12 Nov 2019
- GDP: Eurostat (2019): GDP per capita in PPS (TEC00114) [link](https://ec.europa.eu/eurostat/databrowser/view/TEC00114/default/table)

## DESI map
The map is produced with an R syntax (see desi-2017.R) and runs on an excerpt of the DESI dataset exported from the Stata syntax. 
