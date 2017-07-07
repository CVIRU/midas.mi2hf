##  Project: Heart Failures after First AMI in MIDAS       
### Author: Jen Wellings; Davit Sargsyan   
### Created: 04/08/2017   

---

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