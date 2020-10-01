library("data.table")
library("stargazer")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("aod")

exonerations = read_csv("Downloads/1433_short_project/exonerations.csv")

#The following datasets (dna_experiment1,dna_experiment2, and dna_experiment3) are all created
#through the same process, only differeing in the treatment time frame (denoted by the time
#between beg_date and end_date). We first filter out 
#anyone exoneree who was not exonerated within 5 years of the treatment time frame. We then
#fix exonerations for values of None in witness reform and recording interrogations, setting
#all none values to 2025. We then add 3 variables:
#   1)control: indicates if the exoneree is in the control group (state implemented policy at 
#     least 5 years after treatment time frame)
#   2)treatment: exoneree in treatment group. indicates state policy implemented during treatment period
#   3)after target: exonerated after the end of the treatment period
#We then filter to make sure everyone in the sample is in either the control or treatment group




beg_date = 2010
end_date = 2010
modifier = 5

#witness experiment 2010
#look at top comment for explanation of data set creation
witness_experiment1 = exonerations%>%
  filter((exonerated>=end_date)|(exonerated<=beg_date)&((exonerated<=end_date+modifier)&(exonerated>=beg_date-modifier)))%>%
  mutate(date_witnessreform = ifelse(date_witnessreform=="None",2025,date_witnessreform),
         date_recordinginterrog = ifelse(date_recordinginterrog=="None",2025,date_recordinginterrog),)%>%
  mutate(time_served = exonerated-convicted,
         control = date_witnessreform>end_date+modifier,
         treatment = (date_witnessreform<=end_date&date_witnessreform>=beg_date),
         after_target = exonerated>=end_date,
         has_dna = exonerated>=date_postconvictiondna,
         e2 = exonerated^2)%>%
  filter(control==1|treatment==1)

beg_date = 2011
end_date = 2011
modifier = 5

#witness experiment 2011
#look at top comment for explanation of data set creation
witness_experiment2 = exonerations%>%
  filter((exonerated>=end_date)|(exonerated<=beg_date)&((exonerated<=end_date+modifier)&(exonerated>=beg_date-modifier)))%>%
  mutate(date_witnessreform = ifelse(date_witnessreform=="None",2025,date_witnessreform),
         date_recordinginterrog = ifelse(date_recordinginterrog=="None",2025,date_recordinginterrog),)%>%
  mutate(time_served = exonerated-convicted,
         control = date_witnessreform>end_date+modifier,
         treatment = (date_witnessreform<=end_date&date_witnessreform>=beg_date),
         after_target = exonerated>=end_date,
         has_dna = exonerated>=date_postconvictiondna,
         e2 = exonerated^2)%>%
  filter(control==1|treatment==1)


beg_date = 2012
end_date = 2012
modifier = 5


#witness experiment 2012
#look at top comment for explanation of data set creation
witness_experiment3 = exonerations%>%
  filter((exonerated>=end_date)|(exonerated<=beg_date)&((exonerated<=end_date+modifier)&(exonerated>=beg_date-modifier)))%>%
  mutate(date_witnessreform = ifelse(date_witnessreform=="None",2025,date_witnessreform),
         date_recordinginterrog = ifelse(date_recordinginterrog=="None",2025,date_recordinginterrog),)%>%
  mutate(time_served = exonerated-convicted,
         control = date_witnessreform>end_date+modifier,
         treatment = (date_witnessreform<=end_date&date_witnessreform>=beg_date),
         after_target = exonerated>=end_date,
         has_dna = exonerated>=date_postconvictiondna,
         e2 = exonerated^2)%>%
  filter(control==1|treatment==1)

#witness experiment combined
#This aggragate the previous three time frames datasets into a single dataset
witness_experiment_comb = witness_experiment1%>%
  bind_rows(witness_experiment2)%>%
  bind_rows(witness_experiment3)


#The following is the contol and treatment sizes for each experiment
#This is reflected in table 1 of the paper
sum(witness_experiment1$control, na.rm = TRUE)
sum(witness_experiment1$treatment, na.rm = TRUE)
sum(witness_experiment2$control, na.rm = TRUE)
sum(witness_experiment2$treatment, na.rm = TRUE)
sum(witness_experiment3$control, na.rm = TRUE)
sum(witness_experiment3$treatment, na.rm = TRUE)


#The following are all of the linear regressions run for the results section.
#The general format is 'fit_' followed by either ds,fc or mw. ds stands for DNA substantial,
#fc for false confession, and mw for mistaken witness ID. These are the outcomes we are 
#measuring the policies effect on. After this two letter code is a number. This number
#identifies which of the three time periods we're looking at. If instead of a number there is a
#'C', this means that all three time periods have been combined. If there linear regression name
#ends in 'ctrl' that indicated that controls for race and state population were used.
fit_ds1ctrl = lm(dna_substantial~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment1)
fit_fc1ctrl = lm(fc~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment1)
fit_mw1ctrl = lm(mwid~ after_target + treatment + treatment*after_target + race + statepop, data = witness_experiment1)
fit_ds2ctrl = lm(dna_substantial~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment2)
fit_fc2ctrl = lm(fc~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment2)
fit_mw2ctrl = lm(mwid~ after_target + treatment + treatment*after_target + race + statepop, data = witness_experiment2)
fit_ds3ctrl = lm(dna_substantial~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment3)
fit_fc3ctrl = lm(fc~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment3)
fit_mw3ctrl = lm(mwid~ after_target + treatment + treatment*after_target + race + statepop, data = witness_experiment3)
fit_dsCctrl = lm(dna_substantial~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment_comb)
fit_fcCctrl = lm(fc~ after_target + treatment + treatment*after_target + statepop + race, data = witness_experiment_comb)
fit_mwCctrl = lm(mwid~ after_target + treatment + treatment*after_target + race + statepop, data = witness_experiment_comb)


#This is the code for table 8: looking at the effect of witness reform on
#DNA playing a substantial role in the exoneration
stargazer(fit_ds1ctrl,fit_ds2ctrl,fit_ds3ctrl,
          title = "Witness Reform",
          dep.var.labels = c("DNA Substantial"),
          column.labels = c("2010","2011","2012"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)
#This is the code for table 9: looking at the effect of witness reform on
#false confession
stargazer(fit_fc1ctrl,fit_fc2ctrl,fit_fc3ctrl,
          title = "Witness Reform",
          dep.var.labels = c("False Confession"),
          column.labels = c("2010","2011","2012"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)
#This is the code for table 10: looking at the effect of witness reform on
#mistaken witness ID
stargazer(fit_mw1ctrl,fit_mw2ctrl,fit_mw3ctrl,
          title = "Witness Reform",
          dep.var.labels = c("Mistaken Witness ID"),
          column.labels = c("2010","2011","2012"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)

#This is the code for table 3: looking at the effect of witness reform on
#all three dependent variables for the combined data set
stargazer(fit_dsCctrl,fit_fcCctrl,fit_mwCctrl,
          title = "Witness Reform",
          dep.var.labels = c("DNA Substantial","False Confession","Mistaken Witness ID"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)