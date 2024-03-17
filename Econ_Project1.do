*Who becomes an inventor today?-Experimental analysis-Do-file**

/////////////////////////////////////////////////////////////////////////////
//////////////////////////////Introductory setup/////////////////////////////
/////////////////////////////////////////////////////////////////////////////

clear mata
capture log close
cd "D:\Sorbonne\Econometrics\Project PSME1\Datasets"
clear

/////////////////////////////////////////////////////////////////////////////
/////////////////////////Data import and manipulation////////////////////////
/////////////////////////////////////////////////////////////////////////////

*0) Y0=i--->Regional innovation rate (PCT patent applications per million inhabitants)
*-------------------------------------------------------------------------------------
*Data Source: OECD (https://stats.oecd.org/Index.aspx?DataSetCode=REGION_INNOVATION)
import delimited RIRi
describe
*save in Stata format 
save dfi, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
keep value territoryleve~y reg_id region year
*equally, another specification must be introduced, i.e., we are only interested in country's level of regional innovation (patent application density per million inhabitants), given our data availability on macro variables 
keep if territorylevelandtypology == "Country"
keep value reg_id year
*keep only target countries (OECD members)
keep if reg_id == "AUS" | reg_id == "AUT" | reg_id == "BEL" | reg_id == "CAN" | reg_id == "CHL" | reg_id == "CZE" | reg_id == "DNK" | reg_id == "EST" | reg_id == "FIN" | reg_id == "FRA" | reg_id == "DEU" | reg_id == "GRC" | reg_id == "HUN" | reg_id == "ISL" | reg_id == "IRL" | reg_id == "ISR" | reg_id == "ITA" | reg_id == "JPN" | reg_id == "KOR" | reg_id == "LVA" | reg_id == "LTU" | reg_id == "LUX" | reg_id == "MEX" | reg_id == "NLD" | reg_id == "NZL" | reg_id == "NOR" | reg_id == "POL" | reg_id == "PRT" | reg_id == "SVK" | reg_id == "SVN" | reg_id == "ESP" | reg_id == "SWE" | reg_id == "CHE" | reg_id == "TUR" | reg_id == "GBR" | reg_id == "USA"
*rename (and relabel) original variables' names to better fit the model 
rename reg_id country_code
label var country_code "OECD member country code" 
rename value i
label var i "Regional innovation rate"
*sort by country_code and year
sort country_code year -i
*we know check for duplicates or disaggregate observations not usefull for our analysis
duplicates drop country_code year, force
*normal works, take the log
generate logi = log(i)
label var logi "Natural logarithm of i"
*verify the better fit of the log transformation through Kernel density plots (example=USA)
kdensity i if country_code == "USA"
kdensity logi if country_code == "USA"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year 
*ultimately manipulate the variables order and sort them
order country_id country_code year i 
sort country_code year
save datai, replace
browse
*display a bar chart to a better visualization of aggregate average value of i per each year within the OECD frame
egen mean_i = mean(i), by(year)
summ mean_i, detail
twoway (bar mean_i year, bargap(20)), ///
       title("OECD average i per year") ///
       xtitle("Year") ytitle("Mean i") ///
       yscale(range(`r(min)' `r(max)'))
         
*1) X1=y--->Gross domestic product (GDP)
*---------------------------------------
*Data Source: OECD (https://data.oecd.org/gdp/gross-domestic-product-gdp.htm)
clear
import delimited GDPy
describe
*save in Stata format 
save dfy, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*some national statistics are displayed in USD_CAP, and some others in MLN_USD depending on data availability. Since we hold that MLN_USD is a more wide-spread and immediate way of measurement and since we are committed to mantain an uniform unit of scale for each variable, we decided to pick this latter and henceforth we will give this assumption as granted when it comes to deal with GDP. 
keep if measure == "MLN_USD"
*drop non-involved variables 
drop indicator measure subject frequency flagcodes
*rename and relabel targetted variables 
rename location country_code
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value y 
label var y "National GDP"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*normal works, take the log
generate logy = log(y)
label var logy "Natural logarithm of y"
*verify the better fit of the log transformation through Kernel density plots (example=USA)
kdensity y if country_code == "USA"
kdensity logy if country_code == "USA"
*sort by coutry_code and year
order country_id country_code year y 
sort country_code year
save datay, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline y, overlay i(country_code) t(year) ///
    title("GDP by country over time") ///
    xtitle("Year") ytitle("GDP") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))

*2) X2=g--->GDP growth
*---------------------
*Data Source: World Bank (https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG?locations=OE)
clear
import delimited GDPg
describe
*save in Stata format 
save dfg, replace
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*drop non-involved variables 
drop indicator measure subject frequency flagcodes
*rename and relabel targetted variables 
rename location country_code
label var country_code "OECD member country code" 
rename time year
rename value g 
label var g "GDP yearly growth rate"
*unlike our previous datasets, this dataframe containes not only yearly data, but also disaggregated quarterly data. With respect to our research purpose, we hold that quarterly data would not show statistically significant variations if compared to yearly ones (Chetty, Raj, and Nathaniel Hendren, 2018), hence, we will keep on working with the latter. 
*avoiding complicated drops function conditional to having a string variable rather than a numeric one, as in the case of quarterly data expressed as YYYY-Qn, we can just easily drop those observations containing a Q letter in their respective year's value. 
drop if strpos(year, "Q") > 0
*destring year to make it a numeric variable 
destring year, generate(year_n)
drop year
rename year_n year
label var year "Year"
*first delete those observations going beyond our target time frame 
keep if year >= 2001 & year <= 2015
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*sort by coutry_code and year 
order country_id country_code year g 
sort country_code year
save datag, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline g, overlay i(country_code) t(year) ///
    title("GDP Growth by country over time") ///
    xtitle("Year") ytitle("GDP growth") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))

*3) X3=s--->Foreign direct investement (FDI)
*-------------------------------------------
*Data Source: OECD (https://data.oecd.org/fdi/fdi-flows.htm)
clear
import delimited FDIs
describe
*save in Stata format 
save dfs, replace
*As for g (GDP growth), this dataframe containes not only yearly data, but also disaggregated quarterly data, for the same reasons mentioned above, we will refuse quarterly data, and keep on working on yearly ones.
*avoiding complicated drops function conditional to having a string variable rather than a numeric one, as in the case of quarterly data expressed as YYYY-Qn, we can just easily drop those observations containing a Q letter in their respective year's value. 
drop if strpos(time, "Q") > 0
*destring year to make it a numeric variable 
destring time, generate(year_n)
drop time
rename year_n year
label var year "Year"
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if year >= 2001 & year <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*drop non-involved variables 
drop indicator measure subject frequency flagcodes
*rename and relabel targetted variables 
rename location country_code
label var country_code "OECD member country code" 
rename value s 
*the dataset still appears not perfectly structured since, even though the time variable (year) should present yearly data for s (FDI), the same year is repeated 4 time, leading us to deduce that this repetition may refer to data with a quarterly frequency. To validate this hypothesis we have cross-checked this data with the ones presented by the World Bank (BX.KLT.DINV.CD.WD), and we actually confirmed our deductions, making a correction to aggregate results a necessary step to be undertaken. This action must be retained mandatory also considering the nature of our dataframe (panel). Stata would not be able to declare this latter as a panel df if it is not able to uniquelyvocably associate each observation to both a country_code and a year. 
collapse (sum) s, by(country_code year)
label var s "Foreign direct investement (FDI)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*normal works, take the log
*unlike previous variables, s (IDF) can potentially assume both positive and negative values, making critical to deal with log since the logarithmic function is undefined for non-positive numbers. To approach this issue we added a constant to "s" before taking the logarithm, small enough to ensure us to be able to take the log without bringing any significant bias to the analysis. When, we say "small enough", we mean a portion of the minimum value taken by s in our data range. 
egen min_s = min(s)
generate log_s = s + abs(min_s) + 1
generate logs = log(log_s)
label var logs "Natural logarithm of s"
drop min_s
drop log_s
*verify the better fit of the log transformation through Kernel density plots (example=USA)
kdensity s if country_code == "USA"
kdensity logs if country_code == "USA"
*plot results through a line graph for each country
tsline s, by(country_code) ///
    title("") ///
    xtitle("Year") ytitle("FDI")
*sort by coutry_code and year
order country_id country_code year s 
sort country_code year
save datas, replace 
browse

*4) X4=t--->Trade Openess (Trade in Good and Services)
*-----------------------------------------------------
*Data Source: OECD (https://data.oecd.org/trade/trade-in-goods-and-services.htm)
clear
import delimited TGSt
describe
*save in Stata format 
save dft, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*since we hold that MLN_USD is a more wide-spread and immediate way of measurement and since we are committed to mantain an uniform unit of scale for each variable, we decided to pick this latter and henceforth we will give this assumption as granted when it comes to deal with GDP. In this sense, we have refused other measurement units such as %GDP.
keep if measure == "MLN_USD"
*data are also classified by their subject, i.e., if the value represents either exports or imports, but it also already displays the net difference between these latter according to the 2007 notation (exp-imp). As measure of trade openess, we hold that the trade balance is a representative and robust index to adress this measure.
*keep only trade balances' results
keep if subject == "NTRADE"
*drop non-involved variables 
drop indicator measure subject frequency flagcodes 
*rename and relabel targetted variables 
rename location country_code
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value t 
label var t "Trade Balance (TB)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*as for s (IDF), t (TB) can potentially assume both positive and negative values, making critical to deal with log since the logarithmic function is undefined for non-positive numbers. To approach this issue we added a constant to "t" before taking the logarithm, small enough to ensure us to be able to take the log without bringing any significant bias to the analysis. When, we say "small enough", we mean a portion of the minimum value taken by t in our data range. 
egen min_t = min(t)
generate log_t = t + abs(min_t) + 1
generate logt = log(log_t)
label var logt "Natural logarithm of t"
drop min_t
drop log_t
*verify the better fit of the log transformation through Kernel density plots (example=USA)
kdensity t if country_code == "USA"
kdensity logt if country_code == "USA"
*sort by coutry_code and year
order country_id country_code year t 
sort country_code year
save datat, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline t, overlay i(country_code) t(year) ///
    title("Trade openess by country over time") ///
    xtitle("Year") ytitle("Trade Balance") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))
		   
*5) X5=e--->Employment rate (LFS)
*--------------------------------
*Data Source: OECD (https://stats.oecd.org/Index.aspx?DataSetCode=LFS_D)
clear
import delimited LFSe
describe
*save in Stata format 
save dfe, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
rename country location
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*data are also disaggregated by gender, but at this tage we are only interested in aggregated results (MW).
*keep only aggregated results
keep if sex == "MW"
*equally we are interested in the whole labourforce (LF), whithout furthering micro-level consideration in different age-range clusters
keep if v6 == "Total"
*same for our target series (E=LFS employment)
keep if series == "E"
*drop non-involved variables 
drop v2 sex v4 age v6 series v8 frequency v10 v12 unit unitcode powercodecode powercode referenceperiodcode referenceperiod flagcodes flags
*rename and relabel targetted variables 
rename location country_code 
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value e 
label var e "Employment rate (LFS)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*normal works, take the log
generate loge = log(e)
label var loge "Natural logarithm of e"
*verify the better fit of the log transformation through Kernel density plots (example=USA)
kdensity e if country_code == "USA"
kdensity loge if country_code == "USA"
*sort by coutry_code and year
order country_id country_code year e 
sort country_code year
save datae, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline e, overlay i(country_code) t(year) ///
    title("Employment rate by country over time") ///
    xtitle("Year") ytitle("Employment rate (LFS)") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))

*6) X6=w--->Gender wage gap indicator (difference between median earnings of men and women relative to median earnings of men)
*-----------------------------------------------------------------------------------------------------------------------------
*Data Source: OECD (https://data.oecd.org/earnwage/gender-wage-gap.htm)
clear
import delimited GWGw
describe
*save in Stata format 
save dfw, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*moreover, we hold that self-employed women represent outliers in the overall frame of female labour force, bringing on the table a considearble risk of bias if included in the model with suitable specifications. Given the limitedness of our data availability, for most pf the countries listed within our dataframe, data regarding the share of self-employed women over the whole female labour force were not available, makimg impossible to run a weighted average to also include these values in our estimation. Hence, we retain more representative and robust to consider the employee component, eventhough we signal that these ruled out observations presents the highest wage gap between male and female individuals.
drop if subject == "SELFEMPLOYED"
*drop non-involved variables 
drop indicator subject measure frequency flagcode
*rename and relabel targetted variables 
rename location country_code 
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value w 
label var w "Gender wage gap indicator (difference between median earnings of men and women relative to median earnings of men)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*sort by coutry_code and year
order country_id country_code year w 
sort country_code year
save dataw, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline w, overlay i(country_code) t(year) ///
    title("Gender wage gap by country over time") ///
    xtitle("Year") ytitle("Gender wage gap") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))

*7) X7=f--->Fertility rate (children/woman ratio)
*------------------------------------------
*Data Source: OECD (https://data.oecd.org/pop/fertility-rates.htm)
clear
import delimited FERf
describe
*save in Stata format 
save dff, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*drop non-involved variables 
drop indicator subject measure frequency flagcode
*rename and relabel targetted variables 
rename location country_code 
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value f 
label var f "Fertility rate (children/woman)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*sort by coutry_code and year
order country_id country_code year f 
sort country_code year
save dataf, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline f, overlay i(country_code) t(year) ///
    title("Fertility rate by country over time") ///
    xtitle("Year") ytitle("children/woman ratio") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))
      
*8) X8=r--->R&D spending (public spending on research and development as percentage of GDP)
*------------------------------------------------------------------------------------------
*Data Source: OECD (https://data.oecd.org/rd/gross-domestic-spending-on-r-d.htm)
clear
import delimited RDSr
describe
*save in Stata format 
save dfr, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*henceforth to evaluate public spending in each sector we will consider percentage of GDP as unit of scale, since our demand scope is to evaluate whether a greater either goverment or private (aggreagate) focus on a specific public sector may impact an individual's chances of being granted of a patent, and not whether this impact is due to higher absolute values where obviously more populated and richer countries would be advantaged. 
keep if measure == "PC_GDP"
*drop non-involved variables 
drop indicator subject measure frequency flagcode
*rename and relabel targetted variables 
rename location country_code 
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value r 
label var r "R&D spending (public spending on research and development as percentage of GDP)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*sort by coutry_code and year
order country_id country_code year r 
sort country_code year
save datar, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline r, overlay i(country_code) t(year) ///
    title("R&D spending by country over time") ///
    xtitle("Year") ytitle("R&D spending/GDP") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))

*9) X9=u--->Education spending (public spending on education as percentage of GDP)
*---------------------------------------------------------------------------------
*Data Source: OECD (https://data.oecd.org/eduresource/public-spending-on-education.htm)
clear
import delimited EDUu
describe
*save in Stata format 
save dfu, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*we take in account the PRY_NTRY index, since according to the current literature, it is the more reliable and widespread pre-processed index to measure public spending in education.
keep if subject == "PRY_NTRY"
*drop non-involved variables 
drop indicator subject measure frequency flagcode
*rename and relabel targetted variables 
rename location country_code 
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value u 
label var u "Education spending (public spending on education as percentage of GDP)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*sort by coutry_code and year
order country_id country_code year u 
sort country_code year
save datau, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline u, overlay i(country_code) t(year) ///
    title("Education spending by country over time") ///
    xtitle("Year") ytitle("Education spending/GDP") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))

*10) X10=h--->Healthcare system spending (public spending on healthcare as percentage of GDP)
*--------------------------------------------------------------------------------------------
*Data Source: OECD (https://data.oecd.org/healthres/health-spending.htm)
clear
import delimited HEAh
describe
*save in Stata format 
save dfh, replace
*now, we do not need all the included variables, but only the only usefull ones for hour regression or functional to our merging process 
*first delete those observations going beyond our target time frame 
keep if time >= 2001 & time <= 2015
*keep only OECD members countries
keep if location == "AUS" | location == "AUT" | location == "BEL" | location == "CAN" | location == "CHL" | location == "CZE" | location == "DNK" | location == "EST" | location == "FIN" | location == "FRA" | location == "DEU" | location == "GRC" | location == "HUN" | location == "ISL" | location == "IRL" | location == "ISR" | location == "ITA" | location == "JPN" | location == "KOR" | location == "LVA" | location == "LTU" | location == "LUX" | location == "MEX" | location == "NLD" | location == "NZL" | location == "NOR" | location == "POL" | location == "PRT" | location == "SVK" | location == "SVN" | location == "ESP" | location == "SWE" | location == "CHE" | location == "TUR" | location == "GBR" | location == "USA"
*we consider the total aggregate value over national GDP
keep if subject == "TOT"
*as for r and u (R&D spending and education spending over GDP) we take %GDP as unit of scale for the same reasons above mentioned
keep if measure == "PC_GDP"
*drop non-involved variables 
drop indicator subject measure frequency flagcode
*rename and relabel targetted variables 
rename location country_code 
label var country_code "OECD member country code" 
rename time year
label var year "Year"
rename value h
label var h "Healthcare system spending (public spending on healthcare as percentage of GDP)"
*declare the data as a panel dataset
egen country_id = group(country_code)
label var country_id "Time-serie country ID number"
xtset country_id year
*sort by coutry_code and year
order country_id country_code year h
sort country_code year
save datah, replace 
browse
*plot results through a line graph for each country
keep if country_code == "AUS" | country_code == "CHL" | country_code == "DEU" | country_code == "FRA" | country_code == "GBR" | country_code == "ITA" | country_code == "JPN" | country_code == "MEX" | country_code == "NOR" | country_code == "USA"
xtline h, overlay i(country_code) t(year) ///
    title("Healthcare spending by country over time") ///
    xtitle("Year") ytitle("Healthcare spending/GDP") /// 
    legend(label(1 "Australia (AUS)") label(2 "Chile (CHL)") label(3 "Germany (DEU)") ///
           label(4 "France (FRA)") label(5 "United Kingdom (GBR)") label(6 "Italy (ITA)") ///
           label(7 "Japan (JPN)") label(8 "Mexico (MEX)") label(9 "Norway (NOR)") ///
           label(10 "United States (USA)"))

/////////////////////////////////////////////////////////////////////////////
///////////////////////////////Final merging/////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

*in this section we progressively merge each dataset to work with a unique dataframe enclosing all our variables and observations in a panel format
use datai, clear
merge 1:1 country_id year using "datay"
drop _merge
save data1, replace
*we repeat the same procedure for all the disaggregate dataframes referred to each variable
use data1, clear
merge 1:1 country_id year using "datag"
drop _merge
save data2, replace

use data2, clear
merge 1:1 country_id year using "datas"
drop _merge
save data3, replace

use data3, clear
merge 1:1 country_id year using "datat"
drop _merge
save data4, replace

use data4, clear
merge 1:1 country_id year using "datae"
drop _merge
save data5, replace

use data5, clear
merge 1:1 country_id year using "dataw"
drop _merge
save data6, replace

use data6, clear
merge 1:1 country_id year using "dataf"
drop _merge
save data7, replace

use data7, clear
merge 1:1 country_id year using "datar"
drop _merge
save data8, replace

use data8, clear
merge 1:1 country_id year using "datau"
drop _merge
save data9, replace

use data9, clear
merge 1:1 country_id year using "datah"
drop _merge
save merged_data, replace
describe
browse
*declare the data as a panel dataset
xtset country_id year

/////////////////////////////////////////////////////////////////////////////
///////////////////Missing values regressive forecasts //////////////////////
/////////////////////////////////////////////////////////////////////////////

*since we have missing values for some variables, we employ a panel random effect estimator for each of these latter to generate a prediction on both possible past and future observations, which, according to Hausman test, provides the better estimation for our unadjusted data. Notwithstanding our large number of involved variables, we are only able to employ those presenting a complete range of observations over time, otheriwise the regression would keep predicting missing results. It is worth to notice that for some countries data are complete also for variables not listed in our estimation model, but in order to avoid over-noise-campturing bias, that would then possibly affect only these exceptional countries, we have opted for a uniformed prediction model, exclusively including those complete variables common to all countries.

use merged_data, clear
xtset country_id year

*1) predicted s
*--------------
xtreg s f logt g logy, re
predict pred_s
*Replace missing values in 's' with predicted values
replace s = pred_s if missing(s)
drop pred_s
describe
*in case of log transformed variables, we also need to retake the log function for the new predicted values. We hold that doing a 2-stage log transformation makes the regressive forecast more robust to possible heteroschedacity or high-skewness possible biases. 
*Redo log for new predicted values
egen max_s = max(s)
generate log_s1 = t + abs(max_s) + 1
generate log_s = log(log_s1)
label var log_s "Natural logarithm of e"
drop max_s
drop log_s1
drop logs

*2) predicted e
*--------------
xtreg e f logt g logy, re
predict pred_e
*Replace missing values in 'e' with predicted values
replace e = pred_e if missing(e)
drop pred_e
describe
*Redo log for new predicted values
egen max_e = max(e)
generate log_e1 = t + abs(max_e) + 1
generate log_e = log(log_e1)
label var log_e "Natural logarithm of e"
drop max_e
drop log_e1
drop loge

*3) predicted w
*--------------
xtreg w f logt g logy, re
predict pred_w
*Replace missing values in 'w' with predicted values
replace w = pred_w if missing(w)
drop pred_w

*4) predicted r
*--------------
xtreg r f logt g logy, re
predict pred_r
*Replace missing values in 'w' with predicted values
replace r = pred_r if missing(r)
drop pred_r

*5) predicted u
*--------------
xtreg u f logt g logy, re
predict pred_u
*Replace missing values in 'w' with predicted values
replace u = pred_u if missing(u)
drop pred_u

*uniformely rename variables
rename logi log_i
rename logy log_y
rename logt log_t
*adjust the order of varibales to better reflect our format
*sort by coutry_code and year
order country_id country_code year i log_i y log_y g s log_s t log_t e log_e w f r u h
sort country_code year
save pred_data, replace
*declare the data as a panel dataset
xtset country_id year
browse

/////////////////////////////////////////////////////////////////////////////
///////////////////////////Stationarity Tests////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

*At this point, stationarity tests are crucial to validate that statistical properties like mean and variance remain constant over time throught our dataframe. Ensuring stationarity is vital for accurate modeling, forecasting, and reliable statistical inference. Non-stationary data may mislead models and compromise forecasting accuracy. Overall, stationarity verification is essential for maintaining the stability and interpretability of time series models. To investigate the heterogenous range of non-stationarity biases we have involved four different tests: Im–Pesaran–Shin (IPS) test, Breitung Panel Unit Root Test, Hadri LM Test and Levin, Lin, and Chu (LLC) Test. It is worth to notice that in our case, traditional approaches, such as Augmented Dickey-Fuller test, are not available given the multitude of panel belonging to the dataframe. 

*1) stationarity analysis for y
*------------------------------
*The Im–Pesaran–Shin (IPS) test is assessing whether the target variable is stationary across individual time series. It provides insights into the common stochastic trend shared by the panel.
xtunitroot ips y
*The Im–Pesaran–Shin (IPS) unit-root test results for the variable "y" indicate a positive test statistic of 8.8557 and a p-value of 1.0000. These values fail to reject the null hypothesis of unit roots, suggesting that "y" is likely non-stationary across the panel.

*The Breitung Panel Unit Root Test examines stationarity in the target variable, considering cross-section dependence. It's robust in the presence of correlated data among entities.
xtunitroot breitung y
*The test yielded a test statistic of 13.5466 and a p-value of 1.0000. These results indicate a failure to reject the null hypothesis, suggesting that y is likely non-stationary across the panel.

*The Hadri LM Test assesses stationarity in the target variable for panel data, particularly considering the impact of cross-sectional dependence on the results.
xtunitroot hadri y

*The Levin, Lin, and Chu (LLC) Test evaluates stationarity in the target variable in panel data while accounting for both individual effects and cross-section dependence.
xtunitroot llc y
*The Hadri LM test yielded a highly significant test statistic of 47.9938 and a p-value of 0.0000. This strong evidence led to the rejection of the null hypothesis that all panels are stationary, indicating that variable "y" is likely non-stationary across the panel

*2) stationarity analysis for g
*------------------------------
*IPS test
xtunitroot ips g
*The test resulted in a test statistic of -6.6116 and a p-value of 0.0000. These findings lead to the rejection of the null hypothesis, providing evidence that the variable "g" is likely non-stationary across the panel.

*Breitung Panel Unit Root Test
xtunitroot breitung g
*The test yielded a test statistic of -9.6198 and a p-value of 0.0000. These results indicate a rejection of the null hypothesis, providing evidence that the variable "g" is likely non-stationary across the panel.

*Hadri LM Test
xtunitroot hadri g
*The test yielded a test statistic of 4.6573 and a p-value of 0.0000. These results indicate a rejection of the null hypothesis, providing evidence that the variable "g" is likely non-stationary across the panel.

*LLC test
xtunitroot llc g
*These results indicate a rejection of the null hypothesis, providing strong evidence that the variable "g" is likely non-stationary across the panel

*3) stationarity analysis for s
*------------------------------
*IPS test
xtunitroot ips s
*The test resulted in a test statistic of -3.5428 and a p-value of 0.0002. These findings lead to the rejection of the null hypothesis, providing evidence that the variable "s" is likely non-stationary across the panel.

*Breitung Panel Unit Root Test
xtunitroot breitung s
*The test yielded a test statistic of -2.5754 and a p-value of 0.0050. These results indicate a rejection of the null hypothesis, providing evidence that the variable "s" is likely non-stationary across the panel.

*Hadri LM Test
xtunitroot hadri s
*The test yielded a test statistic of 19.2455 and a p-value of 0.0000. These results strongly support the rejection of the null hypothesis, indicating that the variable "s" is likely non-stationary across the panel.

*LLC test
xtunitroot llc s
*These results strongly indicate a rejection of the null hypothesis, providing evidence that the variable "s" is likely non-stationary across the panel.  

*4) stationarity analysis for t
*------------------------------
*IPS test
xtunitroot ips t
*The test resulted in a test statistic of -1.1581 and a p-value of 0.9861. The critical values for rejection are -1.830 (1%), -1.740 (5%), and -1.690 (10%). Given that the test statistic is less extreme than the critical values, we fail to reject the null hypothesis. This indicates that variable "t" is likely non-stationary across the panel.

*Breitung Panel Unit Root Test
xtunitroot breitung t
*The test statistic yielded a value of 1.3425 with a corresponding p-value of 0.9103. Given that the test statistic is not extreme and the p-value is high, there is insufficient evidence to reject the null hypothesis. This indicates that variable "t" is likely non-stationary across the panel.

*Hadri LM test
xtunitroot hadri t
*The test statistic yielded a value of 24.9452 with a corresponding p-value of 0.0000. The high test statistic and very low p-value provide strong evidence to reject the null hypothesis. This suggests that variable "t" is likely non-stationary across the panel.

*LLC test
xtunitroot llc t
*The test statistic resulted in a value of -9.1361 with a corresponding p-value of 0.0000. The negative test statistic and very low p-value provide strong evidence to reject the null hypothesis, indicating that variable "t" is likely non-stationary across the panel.

*5) stationarity analysis for e
*------------------------------
*IPS test
xtunitroot ips e
*The test statistics did not provide sufficient evidence to reject the null hypothesis, as the p-value associated with Z-t-tilde-bar was 0.6817, exceeding the common significance levels. Consequently, there is insufficient evidence to conclude that variable "e" is stationary based on the IPS test, implying the potential presence of unit roots.

*Breitung Panel Unit Root Test
xtunitroot breitung e
*The test statistic lambda was -0.0312, and the p-value was 0.4876. The p-value exceeds common significance levels, indicating that there is insufficient evidence to reject the null hypothesis. Consequently, based on the Breitung test, variable "e" may contain unit roots, implying non-stationarity across panels.

*Hadri LM test
xtunitroot hadri e
*The test statistic, lambda, yielded a value of -0.0312, with an associated p-value of 0.4876. Given that the p-value exceeds common significance levels (e.g., 0.05), there is insufficient evidence to reject the null hypothesis. Consequently, based on the Breitung test, variable "e" may exhibit unit roots, indicating non-stationarity across panels.

*LLC test
xtunitroot llc e
*The test results yielded a significant test statistic, with an unadjusted t-value of -9.2830 and an adjusted t-value of -4.2159, both implying strong evidence against the null hypothesis. Therefore, based on the LLC test, variable "e" appears to be stationary across panels, suggesting a lack of unit roots.

*In evaluating the reliability of the unit-root tests for variable "e," the Levin–Lin–Chu (LLC) test stands out as more trustworthy. The LLC test incorporates a lagged regression and addresses heteroskedasticity through the Bartlett kernel, providing a comprehensive assessment of stationarity. The notable and negative adjusted t-value in the LLC test signals a rejection of the null hypothesis, supporting the argument for stationarity.

*6) stationarity analysis for w
*------------------------------
*IPS test
xtunitroot ips w
*The Im–Pesaran–Shin (IPS) unit-root test results for variable "w" indicate a significant test statistic, with a Z-t-tilde-bar value of -6.9155 and a p-value of 0.0000. This suggests evidence against the null hypothesis, implying non-stationarity for the variable.

*Breitung Panel Unit Root Test
xtunitroot breitung w
*The test statistic, lambda, was calculated as -0.4973 with a p-value of 0.3095. This result does not provide sufficient evidence to reject the null hypothesis, indicating that variable "w" may have unit roots, suggesting non-stationarity.

*Hadri LM test
xtunitroot hadri w
*The test yielded a significant statistic, with a z-value of 5.5229 and a p-value of 0.0000. This strong evidence against the null hypothesis suggests that variable "w" is likely stationary across panels, indicating a lack of unit roots.

*LLC test
xtunitroot llc w
*The test yielded a significant unadjusted t-value of -7.3412 and an adjusted t-value of -3.2765, providing strong evidence against the null hypothesis. This implies that variable "w" appears to be stationary across panels, indicating a lack of unit roots.

*Given the results of the Im–Pesaran–Shin (IPS), Breitung, Hadri LM, and Levin–Lin–Chu (LLC) unit-root tests for variable "w," the significant p-value in the Hadri LM test (p-value = 0.0000) and the adjusted t-value in the LLC test (t = -3.2765) suggest strong evidence against the null hypothesis of unit roots, indicating that the variable "w" is likely stationary across panels. Therefore, based on these findings, it is reasonable to conclude that variable "w" is stationary in the panel dataset.

*7) stationarity analysis for f
*------------------------------
*IPS test
xtunitroot ips f
*Based on the results of the Im–Pesaran–Shin (IPS) unit-root test for variable "f," the test statistic (Z-t-tilde-bar) is 2.5333 with a p-value of 0.9944. These values suggest weak evidence against the null hypothesis of unit roots, indicating that variable "f" is likely non-stationary across panels. 

*Breitung Panel Unit Root Test
xtunitroot breitung f
*The Breitung unit-root test for variable "f" yielded a test statistic (lambda) of 2.6238 with a p-value of 0.9957. These results provide insufficient evidence to reject the null hypothesis, suggesting that variable "f" likely contains unit roots and is non-stationary across panels.

*Hadri LM test
xtunitroot hadri f
*The Hadri LM test for variable "f" produced a test statistic of 29.8304 with a p-value of 0.0000. The highly significant p-value suggests strong evidence against the null hypothesis, indicating that variable "f" is likely stationary across panels.

*LLC test
xtunitroot llc f
*The Levin–Lin–Chu unit-root test (LLC) for variable "f" resulted in a test statistic of -4.9840 and an adjusted t-value of -0.2989, with a p-value of 0.3825. The unadjusted and adjusted t-values both fail to reject the null hypothesis of unit roots, suggesting a lack of stationarity. 

*The Hadri LM test accounts for potential violations of homoskedasticity assumptions, making it suitable for this dataset. Additionally, the highly significant test statistic and low p-value reinforce its credibility in rejecting the null hypothesis of unit roots, leading to the conclusion of stationarity for variable "f".

*8) stationarity analysis for r
*------------------------------
*IPS test
xtunitroot ips r
*The test results reveal a test statistic of -1.3947 and a corresponding p-value of 0.9999. The high p-value fails to provide evidence against the null hypothesis, suggesting a lack of stationarity for variable "r."

*Breitung Panel Unit Root Test
xtunitroot breitung r
*The test yielded a statistic of 2.9776 and a corresponding p-value of 0.9985. The high p-value fails to provide evidence against the null hypothesis, suggesting a lack of stationarity for variable "r".

*Hadri LM test
xtunitroot hadri r
*The test resulted in a significant statistic of 29.1043 with a p-value of 0.0000, providing strong evidence against the null hypothesis. This indicates that variable "r" is likely to be non-stationary across panels, suggesting the presence of unit roots. 

*LLC test
xtunitroot llc r
*The Levin–Lin–Chu (LLC) unit-root test for variable "r" in the panel dataset resulted in a non-significant test statistic, with an adjusted t-value of -0.9005 and a p-value of 0.1839. This suggests insufficient evidence to reject the null hypothesis that panels contain unit roots, implying a lack of clear stationarity for variable "r" across panels.

*9) stationarity analysis for u
*------------------------------
*IPS test
xtunitroot ips u
*The test results yielded a significant test statistic, with a Z-t-tilde-bar value of -3.4928 and a p-value of 0.0002, providing strong evidence against the null hypothesis. Therefore, "u" appears to be stationary across panels, suggesting a lack of unit roots.

*Breitung Panel Unit Root Test
xtunitroot breitung u
*The test results produced a highly significant test statistic, with a lambda value of -5.7768 and a p-value of 0.0000, providing strong evidence against the null hypothesis. Therefore, "u" appears to be stationary across panels, indicating a lack of unit roots.

*Hadri LM test
xtunitroot hadri u
*The test results revealed a significant test statistic, with a z-value of 17.4116 and a p-value of 0.0000, providing robust evidence against the null hypothesis. Therefore, "u" is deemed to be stationary across panels, implying the absence of unit roots.

*LLC test
xtunitroot llc u
*The test results yielded a significant test statistic, with an unadjusted t-value of -11.6567 and an adjusted t-value of -4.8094, both implying strong evidence against the null hypothesis. Therefore, "u" appears to be stationary across panels, suggesting a lack of unit roots.


*10) stationarity analysis for h
*-------------------------------
*IPS test
xtunitroot ips h
*The test did not yield a statistically significant result, as the test statistic did not exceed the critical values at conventional significance levels (1%, 5%, 10%). Therefore, we do not reject the null hypothesis, suggesting that variable "h" may exhibit unit roots across the panels.

*Breitung Panel Unit Root Test
xtunitroot breitung h
*The test statistic, represented by lambda, was found to be 3.0187 with a p-value of 0.9987. As the p-value exceeds conventional significance levels, we fail to reject the null hypothesis. Therefore, "h" is likely to have unit roots across the panels, indicating non-stationarity.

*Hadri LM test
xtunitroot hadri h
*The test statistic, represented by "z," was found to be 37.4329 with a p-value of 0.0000, indicating strong evidence against the null hypothesis. Therefore, "h" is likely to be stationary across all panels, suggesting the absence of unit roots.

*LLC test
xtunitroot llc h
*The test results yielded a significant test statistic, with an unadjusted t-value of -7.2781 and an adjusted t-value of -3.5677, both implying strong evidence against the null hypothesis. Therefore, based on the LLC test, variable "h" appears to be stationary across panels, suggesting a lack of unit roots.

*Im–Pesaran–Shin (IPS) and Breitung tests do not provide conclusive evidence in this case to clearly assert whether the variable can be retained stationary or not. Therefore, based on the more reliable Hadri LM and LLC tests, we can infer that variable "h" is likely stationary across panels, as the significancy of their results shows. 

/////////////////////////////////////////////////////////////////////////////
///////////////////////////Stationarity Correction///////////////////////////
/////////////////////////////////////////////////////////////////////////////

*Given the results in terms of stationarity extrapolated from the employed stationarity tests, we want now to implement corrections to those variables that were identified as non-stationary. To do so, we will recur to first differences, following the rationale of eliminating trends and making the series more amenable to statistical analyses.

*recall the unadjusted dataset
use pred_data, clear
xtset country_id year

*1) y adjusted 
*-------------
*generate first differences
gen y_diff = y - L.y
*drop the original variable if you want to keep only the differenced variable
drop y
*rename the differenced variable to the original variable name
rename y_diff y
*LLC test
xtunitroot llc y
*The differenced variable 'y' appears to be stationary across panels, supporting the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg y g log_s log_t log_e w f r, re
predict pred_y
*Replace missing values in with predicted values
replace y = pred_y if missing(y)
drop pred_y
label var y "GDP"

*2) log_y adjusted
*----------------
*generate first differences
gen log_y_diff = log_y - L.log_y
*drop the original variable if you want to keep only the differenced variable
drop log_y
*rename the differenced variable to the original variable name
rename log_y_diff log_y
*LLC test
xtunitroot llc log_y
*The differenced variable 'log_y' appears to be stationary across panels, supporting the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg log_y g log_s log_t log_e w f r u h, re
predict pred_log_y
*Replace missing values in 's' with predicted values
replace log_y = pred_log_y if missing(log_y)
drop pred_log_y
label var log_y "Naural logarithm of y (GDP)"

*3) g adjusted 
*-------------
*generate first differences
gen g_diff = g - L.g
*drop the original variable if you want to keep only the differenced variable
drop g
*rename the differenced variable to the original variable name
rename g_diff g
*LLC test
xtunitroot llc g
*The differenced variable 'g' appears to be stationary across panels, supporting the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg g log_y log_s log_t log_e w f r u h, re
predict pred_g
*Replace missing values in with predicted values
replace g = pred_g if missing(g)
drop pred_g
label var g "GDP yearly growth rate"

*4) s adjusted 
*-------------
*generate first differences
gen s_diff = s - L.s
*drop the original variable if you want to keep only the differenced variable
drop s
*rename the differenced variable to the original variable name
rename s_diff s
*LLC test
xtunitroot llc s
*The differenced variable 's' appears to be stationary across panels, supporting the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg s log_y g log_t log_e w f r u h, re
predict pred_s
*Replace missing values in with predicted values
replace s = pred_s if missing(s)
drop pred_s
label var s "Foreign direct investement (FDI)"

*5) log_s adjusted
*----------------
*generate first differences
gen log_s_diff = log_s - L.log_s
*drop the original variable if you want to keep only the differenced variable
drop log_s
*rename the differenced variable to the original variable name
rename log_s_diff log_s
*LLC test is not applicable in this case, since it requires strongly balanced data. We will then try to deploy IPS test
*xtunitroot ips log_s
*None of the previous, and further, tests actually produces statistically significant results for this variable analysis, leading to uncertain conclusions on the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg log_s g log_y log_t log_e w f r u h, re
predict pred_log_s
*Replace missing values in 's' with predicted values
replace log_s = pred_log_s if missing(log_s)
drop pred_log_s
label var log_s "Naural logarithm of s (FDI)"

*6) t adjusted 
*-------------
*generate first differences
gen t_diff = t - L.t
*drop the original variable if you want to keep only the differenced variable
drop t
*rename the differenced variable to the original variable name
rename t_diff t
*LLC test
xtunitroot llc t
*The differenced variable 't' appears to be stationary across panels, supporting the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg t log_y g log_s log_e w f r u h, re
predict pred_t
*Replace missing values in with predicted values
replace t = pred_t if missing(t)
drop pred_t
label var t "Trade Balance (TB)"

*7) log_t adjusted
*----------------
*generate first differences
gen log_t_diff = log_t - L.log_t
*drop the original variable if you want to keep only the differenced variable
drop log_t
*rename the differenced variable to the original variable name
rename log_t_diff log_t
*LLC test
xtunitroot llc log_t
*The differenced variable 'log_t' appears to be stationary across panels, supporting the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg log_t g log_y log_s log_e w f r u h, re
predict pred_log_t
*Replace missing values in 's' with predicted values
replace log_t = pred_log_t if missing(log_t)
drop pred_log_t
label var log_t "Naural logarithm of t (TB)"

*8) r adjusted 
*-------------
*generate first differences
gen r_diff = r - L.r
*drop the original variable if you want to keep only the differenced variable
drop r
*rename the differenced variable to the original variable name
rename r_diff r
*LLC test
xtunitroot llc r
*The differenced variable 'r' appears to be stationary across panels, supporting the effectiveness of the differencing transformation in achieving stationarity.
*repredict missing values to have complete data from 2001 to 2015
xtreg r log_y g log_s log_t log_e w f u h, re
predict pred_r
*Replace missing values in with predicted values
replace r = pred_r if missing(r)
drop pred_r
label var r "R&D spending (public spending on research and development as percentage of GDP)"

*reorder the final dataframe according to the new changes
*sort by coutry_code and year
order country_id country_code year i log_i y log_y g s log_s t log_t e log_e w f r u h
sort country_code year
save adj_data, replace
*declare the data as a panel dataset
xtset country_id year
browse

/////////////////////////////////////////////////////////////////////////////
////////////////////////////Model Specification//////////////////////////////
/////////////////////////////////////////////////////////////////////////////

use adj_data, clear
xtset country_id year
ssc install outreg2 
ssc install estout

*----------------------------------------------------------------------------
*Specification_1----->Macro-trends
xtreg i log_y g log_s log_t log_e, re
predict pred_i1
outreg2 using "Specification_1", replace ctitle(Baseline)
est store spec1

*Specification_2----->Female empowerment
xtreg i log_y g log_s log_t log_e w f, re
predict pred_i2
outreg2 using "Specification_2", replace ctitle(Baseline)
est store spec2

*Specification_3----->Goverment expenditure targets
xtreg i log_y g log_s log_t log_e w f r u h, re
predict pred_i3
outreg2 using "Specification_3", replace ctitle(Baseline)
est store spec3
*----------------------------------------------------------------------------
*export results
esttab spec1 spec2 spec3 using "raw_res.csv", se label replace wide plain

/////////////////////////////////////////////////////////////////////////////
//////////////////////////Panel data specifications//////////////////////////
/////////////////////////////////////////////////////////////////////////////

use adj_data, clear
xtset country_id year

*1) Introductory setup
*---------------------
*we deploy this test to investigate which panel estimator works better with our data
*FIXED EFFECTS----->without clustered SEs
xtreg i log_y g log_s log_t log_e w f r u h , fe 
estimates store FE
*FIXED EFFECTS----->with clustered SEs
xtreg i log_y g log_s log_t log_e w f r u h , fe cluster(country_id)
estimates store FECL
*RANDOM EFFECTS----->without robust SEs
xtreg i log_y g log_s log_t log_e w f r u h , re 
estimates store RE
*RANDOM EFFECTS----->with robust SEs
xtreg i log_y g log_s log_t log_e w f r u h , re robust 
estimates store RERB
*POOLED OLS
reg i log_y g log_s log_t log_e w f r u h  
estimates store OLS
*Pooled OLS is appropriate when you treat the panel data as if it were a single cross-sectional dataset, ignoring any individual-specific or time-specific effects. This assumes that there is no correlation between the individual effects and the independent variables, which is not our case.

*comparision of different estimated results 
estimates table FE FECL RE RERB OLS

*2) Hausman test
*---------------
hausman FE RE
*The results of the Hausman test indicate that the p-value is very high (0.9992), suggesting that you fail to reject the null hypothesis. The null hypothesis in the Hausman test is that the difference in coefficients between the fixed-effects (FE) and random-effects (RE) models is not systematic. Since the p-value is high, there is no strong evidence to suggest that the differences in coefficients between the two models are systematic. Therefore, we choose the random-effects model as it is consistent with the assumptions of the test.

*hence, generalizing these findings to our previous estimations, we can assert that RE is the best estimator we can deploy within our dataframe for this regression model.  

*3) First-order autocorrelation check
*------------------------------------
sort country_id year
gen lagi=L.i
br country_id year i lagi
correlate i L.i
*The correlation analysis reveals a strong positive autocorrelation between the variable i and its first lag L.i, with a correlation coefficient of approximately 0.9938. This indicates a substantial linear relationship between the variable and its past values.

/////////////////////////////////////////////////////////////////////////////
///////////////////////////Functional form test//////////////////////////////
/////////////////////////////////////////////////////////////////////////////

use adj_data, clear
xtset country_id year

*1) non-normality of error terms checks
*--------------------------------------
/////////SPEC1/////////
xtreg i log_y g log_s log_t log_e, re
predict pred_i1
*Kernel density distribution vs normal
kdensity pred_i1, normal
*Jacques_Bera test
sktest pred_i1 
*Shapiro-Wilk Test 
swilk pred_i1 
*The Jacques-Bera test for skewness and kurtosis, the Skewness and Kurtosis tests, as well as the Shapiro-Wilk test, all indicate a departure from normality. The p-values associated with these tests are all very low (close to or equal to zero), leading to the rejection of the null hypothesis that the data follows a normal distribution.

/////////SPEC2/////////
xtreg i log_y g log_s log_t log_e w f, re
predict pred_i2
*Kernel density distribution vs normal
kdensity pred_i2, normal
*Jacques_Bera test
sktest pred_i2 
*Shapiro-Wilk Test 
swilk pred_i2 
*The Jacques-Bera test for skewness and kurtosis, the Skewness and Kurtosis tests, as well as the Shapiro-Wilk test, all provide evidence against the hypothesis that the data follows a normal distribution. The p-values associated with these tests are very low (close to or equal to zero), leading to the rejection of the null hypothesis. Therefore, based on these normality tests, data do not appear to be normally distributed.

/////////SPEC3/////////
xtreg i log_y g log_s log_t log_e w f r u h, re
predict pred_i3
*Kernel density distribution vs normal
kdensity pred_i3, normal
*Jacques_Bera test
sktest pred_i3 
*Shapiro-Wilk Test 
swilk pred_i3 
*The p-values from the Jacques-Bera test for skewness and kurtosis, the Skewness and Kurtosis tests, and the Shapiro-Wilk test are relatively high. While the p-value from the Jacques-Bera test for skewness and kurtosis is not very elevate, the other two tests provide some support for the hypothesis of normality. Therefore, based on these normality tests, data appears to be relatively close to a normal distribution.

*Lower values of AIC and BIC indicate a better fit. Therefore, Specification 3 (Goverment expenditure targets) appears to have the best fit among the three models also according to R-squared. Moreover, R-squared values increase from Spec1 to Spec3, indicating a better ability to explain the variance in the dependent variable.

*2) Ramsey reset test
*--------------------
xtreg i log_y g log_s log_t log_e w f r u h, re
predict pred_i
gen form2 = pred_i^2
gen form3 = pred_i^3
*fit test including form2 and form3
xtreg i log_y g log_s log_t log_e w f r u h form2 form3, re
*RESET test over the new experimental variables
test form2 form3
*The p-value for the RESET test is very small (p = 0.0000), indicating that we reject the null hypothesis. This suggests that there might be a specification error in the model related to form2 and form3, leading us to rule them out of the model specification- The test confirm us that we are working with the right functional form. 

/////////////////////////////////////////////////////////////////////////////
/////////////////////Heteroscedasticity robustness test//////////////////////
/////////////////////////////////////////////////////////////////////////////

use adj_data, clear
xtset country_id year
ssc install whitetst

*1) Breusch-Pagan test
*---------------------
xtreg i log_y g log_s log_t log_e w f r u h, re
predict uhat, u
*Manually run Breusch-Pagan LM test
robvar uhat, by(country_id)

*The result of the Breusch-Pagan test for heteroscedasticity indicates that the test statistics for all three weighting matrices (W0, W50, and W10) are missing (denoted by .), and the corresponding p-values are also missing. This suggests that the test results are inconclusive or invalid due to some issue with the residuals or the data.

*The test statistic (F-statistic) is 6.14 with a p-value of 0.0000. Since the p-value is less than the conventional significance level of 0.05, you would reject the null hypothesis. This provides evidence against the assumption of constant variance, suggesting the presence of heteroskedasticity in the model.

*2) White test
*-------------
*Manually run White test
gen uhat_sq = uhat^2
robvar uhat_sq, by(country_id)
*The results indicate the presence of heteroscedasticity in the residuals. The test statistics (W0, W50, W10) are not reported, and the associated p-values are also not available (denoted as "."), suggesting potential issues with the estimation or the grouping variable. However, the mean and standard deviation of squared residuals for each country ID reveal substantial variability, supporting the conclusion of heteroscedasticity

*given these results, to avoid highly probable heteroskedasticity biases, we now want Stata to estimate the standard errors using a method that is less sensitive to the presence of heteroskedasticity. To do so, we employ robust standard errors 
xtreg i log_y g log_s log_t log_e w f r u h, re
est store BASE
xtreg i log_y g log_s log_t log_e w f r u h, robust
est store ROBUST
*export results
esttab BASE ROBUST using "Spec_3_Base&Robust.csv", se label replace wide plain

/////////////////////////////////////////////////////////////////////////////
////////////////////////Endogeneity robustness checks////////////////////////
/////////////////////////////////////////////////////////////////////////////

*To ensure the robustness of our analysis, we employ exogeneity tests, which scrutinize whether our chosen explanatory variables are free from correlation with the error term. This test is instrumental in validating the assumptions underlying our model, providing insights into the potential bias introduced by endogeneity.

use adj_data, clear
xtset country_id year
ssc install estout

*1) IV regression
*----------------
*pretend to use model variables as instrumental variables (IV validity)
ivregress 2sls i log_y log_s log_t f r u h ( w=g), first 
est store IV1
ivregress 2sls i log_y log_s log_t f r u  h ( w= g log_e), first 
est store IV2
ivregress 2sls i log_y log_s log_t f r u  h ( w=log_e), first vce(robust)
est store IV3
*display results
est table IV1 IV2 IV3, se p
esttab IV1 IV2 IV3, se

*2) Durbin Wu Hausman test
*-------------------------
estat endogenous
*Both the robust score chi-squared test and the robust regression F-test have low p-values (0.0000), indicating strong evidence against the null hypothesis. Therefore, we reject the null hypothesis, suggesting that the model variables are endogenous. Hence, w and log_e must be included in our model. 

*3) Hansen Sargan test
*---------------------
estat overid
*The robust score chi-squared statistic is 57.7889 with a p-value of 0.0000, and the robust regression F-statistic is 37.0072 with a p-value of 0.0000. These findings provide robust support for the rejection of the hypothesis that the included variables are exogenous, suggesting that they are likely endogenous in your model. Hence, g and log_e must be included.

*4) verify the quality of our instrumental variables 
*---------------------------------------------------
corr w g log_e
reg w g log_e log_y log_s log_t f r u h, robust
test g log_e
*the test indicates that there is no significant evidence to reject the null hypothesis that both g and log_e coefficients are zero. The F-statistic is 1.59, and the p-value is 0.2044. This suggests that these variables do not contribute significantly to explaining the variation in the dependent variable.
