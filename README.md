##  Project: Heart Failures after First AMI in MIDAS       
### Author: Jen Wellings; Davit Sargsyan   
### Created: 04/08/2017   

---

## Daily Logs
### 08/12/2017
* To make the data compatable to the Census Bureau data, move the age reshold from 20 (see bulletpoint #3 from 07/29/2017 log) to 18 y.o.    
* Rerun both, data and analysis scripts (v3 of both scripts)    
* Created an R Notebook file and rendered: *midas15_mi2hf_analysis_v3.Rmd*   

### 07/31/2017
#### Continued working on analysis (script 'midas15_mi2hf_analysis_v3.R')
   
* History of acute CHF vs acute CHF recorded at 1st AMI admission (%)   

||No Current HF|Curent HF|Sum|
|:----------|-----:|----:|-----:|
|No Prior HF|  69.6| 18.3|  87.9|
|Prior HF   |   4.5|  7.7|  12.1|
|Sum        |  74.0| 26.0| 100.0|

### 07/29/2017
#### Updated dataset details (script 'midas15_mi2hf_data_v3.R'):
* There is a total of 18,071,613 records of 4,680,205 patients   
* Keep only the records prior to Janyary 1, 1995, are 
* All records of patients who were admited before the age of 20 are removed 
* Keep inpatients' records only    
* Remove all cancers: ICD-9 140-165, 170-176, and 180-239   
* Kepp only the patients with AMI (ICD-9 codes 410.xx) admissions between 01/01/2000 and 12/31/2015: 1,082,998 records of 183,727 patients    
* Define risk factors:   
a. Acute congsestive heart falure(ICD-9 codes 428.0, 428.20, 428.21, 428.23, 428.30, 428.31, 428.33, 428.40, 428.41 and 428.43)    
b. Chronic congsestive heart falure(ICD-9 codes 428.22, 428.32 and 428.42)    
c. Hypertension (ICD-9 codes 401.xx, 402.xx, 403.xx, 404.xx and 405.xx)
d. Diabetes mellitus (ICD-9 codes 250.xx)   
e. Chronic liver disease and cirrhosis (ICD-9 codes 571.xx)    
f. Chronic kidney disease (ICD-9 codes 585.xx)   
g. Chronic obstructive pulmonary disease (COPD, ICD-9 codes 490.xx to 496.xx)    
h. Disorders of lipoid metabolism (NEW DEFINITION, see note form 07/28/2017 meeting!)(ICD-9 codes 272. to 272.4, 272.8, and 272.9)    
     
* Clock starts at first AMI admission between 01/01/2000 and 12/31/2015
*  Five-year lookback for all risk factors  
* Define readmission and date of readmision: admission for any reason one day after the first AMI dishcarge or later    
* Define readmission and date of readmision for acute CHF: admission for acute CHF on the day of first AMI dishcarge (but not on the same admission) or later with acute CHF as reason for readmission    
* The readmitted patients were discharged alive (if dead, died al least one day after the readmission discharge)
* Dead: died at least one day after the first AMI discharge    
* Remove patients who died at first AMI discharge
* Remove patients with history of AMI (i.e. AMI in DX2-9 prior to the first DX1 AMI)    
* Define cardiovascular death as following:    
** Major cardiovascular diseases: I00-I78   
** Diseases of heart: I00-I09, I11, I13, I20-I51   
** Hypertensive heart disease with or without renal disease: I11,I13      
** Ischemic heart diseases: I20-I25   
** Other diseases of heart: I00-I09,I26-I51   
** Essential (primary) hypertension and hypertensive renal disease: I10,I12   
** Cerebrovascular diseases: I60-I69   
** Atherosclerosis: I70   
** Other diseases of circulatory system: I71-I78   
** Source: http://www.health.state.ok.us/stats/Vital_Statistics/Death/039_causes.shtml   
    
* FINAL DATASET: 157,655 patients' records   

#### Updated analysis (script 'midas15_mi2hf_analysis_v3.R'):
* Added Table1: Demographics    
|                                         |          |
|:----------------------------------------|:---------|
|Mean Age at First AMI Admission +/- S.D. |66.7+/-15 |
|Female(%)                                |39.7      |
|Race(%)                                  |          |
|White                                    |50.1      |
|Black                                    |42.2      |
|Other                                    |7.7       |
|Ethnicity(%)                             |          |
|Hispanic                                 |8.6       |
|Non-hispanic                             |80.6      |
|Other                                    |10.8      |
|Insurance(%)                             |          |
|Commercial                               |45.5      |
|Medicare                                 |46.7      |
|Medicade/Self-Pay/Other                  |7.9       |

* High number of blacks is having AMI: checked against NJ population racial composition
** Percent AMI by Sex and Race (%)    
|    | White| Black| Other|   Sum|
|:---|-----:|-----:|-----:|-----:|
|F   |  20.3|  16.8|   2.6|  39.7|
|M   |  29.8|  25.3|   5.1|  60.3|
|Sum |  50.1|  42.2|   7.7| 100.0|

** Racial Composition of NJ (%) (18 y.o and older; source: https://www.census.gov/cps/data/cpstablecreator.html)   
| white| black| other| total|
|-----:|-----:|-----:|-----:|
|  39.5|   7.5|   4.9|    52|
|  36.8|   6.0|   5.2|    48|
|  76.2|  13.6|  10.2|   100|

** How much more likely to get AMI for a black NJ recident?
** Risk foldchange, adjusted for population    
|Race                      | AMI Risk Foldchage|
|:-------------------------|------------------:|
|Black Female/White Female |                4.3|
|Black Male/White Male     |                5.2|
|Black All/White All       |                4.7|

** Compare to other races    
|Race                      | AMI Risk Foldchage|
|:-------------------------|------------------:|
|Other Female/White Female |                1.0|
|Other Male/White Male     |                1.2|
|Other All/White All       |                1.1|

### 07/28/2017 Weekly Meeting Notes
* The latest version of the manuscript ("HF after MI_JW_072317.docx"") was reviewed    
* NEW DEFINITION: Disorders of lipoid metabolism disorder now exludes ICD-9 codes 272.5, 272.6 and 272.7    
* NEW FILTER: in data from 2008 to 2015, keep inpatients' records only   
* Do not separate data into two period (before and after 2008). Merge lines between years 2007 and 2008    

#### Figures:
* Figure1: HF Admissions After 1st AMI Discharge, 2 panels: with and without prior HF records. Remove the tables from the graphs       
* Figure2: Odds Ratios of HF after 1st AMI Discharge, 4 panels corresponding to 30, 90 and 180 days, and 1 year    
* Figure3: Odds Ratios of HF after 1st AMI Discharge, 4 panels corresponding to 30, 90 and 180 days, and 1 year, no prior HF admissions    

#### Tables:
* Table1: Demographics (NEW!)
* Table2: HF Admissions Rates After 1st AMI Discharge, with and without prior HF records    
* Table3: Odds Ratios of HF Admission after First AMI, including patients with prior HF records    
* Table4: Odds Ratios of HF Admission after First AMI, excuding patients with prior HF records    

### 7/23/2017, Jen's email
Dear Davit, Dr. Kostis, and Jerry,    
Attached is the latest draft of the paper with some reworking of the discussion. I added several more references of prior studies that were done and more importantly, added a few sentences illustrating the novelty of the study: that it is state-wide, all inclusive, and it validates other studies showing a decreasing incidence of HF after AMI.  I may have to change a few sentences based on the latest analysis that we do regarding the 2008 shift. Thank you for all your continued help on this. Please let me know of any advice on revisions that you see fit.    
Take care,    
Jen

### 07/15/2017
PAT_TYPE:   
0 = inpatient   
1 = same day surgery   
2 = ER/other outpatient   
3 = out patient   

### 07/08/2017
* Added Patient Type (inpatient, emergency, other 2 types). The data was created with SAS and R scripts, see project 'midas' on GitHub.
* V2 of data processing and analysis scripts is added
* No notable difference after removing emergency visits between 2008 and 2015. There is still a bump around 2008.

### 07/07/2017
### Email sent to J. Kosis, Jen, Jerry and Jeanne
Below are the minutes from today's meeting regarding the HF-After-AMI study.   

1. Methodology drift: we found the variable that flags the type of admission (pat_type: 0 = in-patient, 1 = ?, 2 = ER, 3 = ?. Jerry, could you please remind me what the levels are, I could not find it in the MIDAS dictionary since my copy is outdated, it is from 2006). I will create a new data set with this flag and merge it with MIDAS-2015 dataset. I will then rerun the entire analysis for inpatients and, possibly, compare their outcomes with the once admitted to ER.   

2. What's novel? Is this a more inclusive study since it has statewide records? Does it confirm downward trend in HF after AMI reported by others? Are the results from previous studies inconclusive or contradictory? Are people admitted to ER at higher risk (very new)?   

3. Move the last highlight (by JK) from "punchline" to "limitations".   

### 06/20/2017
### From Jen's email (06/20/2017):    

* Attached is the revised copy of the HF after MI paper. I took your advice and reworked some of the sections. The things that need improvement or additions from you are:

1. The statistical Analysis section: Just some more detailed explanation of what was done to come up with our results.

2. Table 1: I filled most of it out with the numbers you provided on the email from May 3rd but I'd also like to include p values for significance for each follow up period (30 days, 90 days 180 days, and 1 year). We did this for my presentation but I didn't do all the follow up periods. It'd be nice to see if the decrease over time was significant or not. (it was i think)

3. Table 2: If you don't mind adding this table with the odds ratios and confidence intervals, that'd be helpful. I only included the graph (figure 2) that includes patients without current or prior HF. This table will dictate in my discussion which risk factors ended up being significant - I think only DM and CKD were in the end.

* In subsequent meetings I know various members of CVI have brought up to expand the risk factors and also look at subendocardial MI or STEMI but I would like to leave the analysis as is for now and finish the paper. We can add additional studies later on the future.

* I'd like to make final changes at the end of the week and hopefully get it submitted soon! As soon as you send it to me I will make final changes; I'll also be here all week. I move this weekend to Philly but of course I can work on this remotely. 

### From Davit's email (07/01/2017):

* I made the following changes in the attached version of the HF After AMI paper:   

1. I separated the 2 periods: 200 to 2007 and 2008 to 2014, due to the methodology drift. according to Dr. Kostis, MIDAS started collecting emergency room records in 2008 which is very likely to explain the huge bump around 2008 in HF admission rates. The two graphs of rates over time (figures 1 and 2) now have a gap between 2007 and 2008 to reflect this fact.   

2. I added the tables with the rates to the graphs so no need for a separate tables now.   

3. I did not provide p-values for the rates but I calculated and reported the p-values for the odds ratios that show a significant dicrease of HF admission risk over time. I think, splitting the data into the 2 time period made the estimates much better.   

4. I edited, I think, every section (ICD-9 codes, definitions, results, etc) but I did not track the changes. Could you please go over the text to see if I missed or misunderstood any of it?   

5. I added Table1 with odds ratio estimates, 95% confidence intervals, and p-values. I also stated the results in the section "Associations with Developing Heart Failure".   

6. Figure 3 (odds ratios) was updated to reflect the methodology drift.   

### 06/05/2017
* Added analysis without patients who had acute CHF diagnisis prior or on the 1st AMI admission

### 05/05/2017
* In 'midas15_mi2hf_data.R/PART III' lines 644 to 657, replaced 'prior' with 
'(prior | current)' for histories of risk factors

### 04/08/2017 
* Updated data set to MIDAS15

### 02/05/2017
* Updated subset: using acute CHF admission (DX1 only) following AMI discharge (DX1 only)
* Finilazed the analysis