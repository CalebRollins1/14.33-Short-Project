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


#dna experiment 2001
#look at top comment for explanation of data set creation
beg_date = 2001
end_date = 2001
modifier = 5


dna_experiment1 = exonerations%>%
  filter(((exonerated>=end_date)|(exonerated<=beg_date))&((exonerated<=end_date+modifier)&(exonerated>=beg_date-modifier)))%>%
  mutate(date_witnessreform = ifelse(date_witnessreform=="None",2025,date_witnessreform),
         date_recordinginterrog = ifelse(date_recordinginterrog=="None",2025,date_recordinginterrog),)%>%
  mutate(control = date_postconvictiondna>end_date+modifier,
         treatment = (date_postconvictiondna<=end_date&date_postconvictiondna>=beg_date),
         after_target = exonerated>=end_date)%>%
  filter(control==1|treatment==1)


#dna experiment 2002
#look at top comment for explanation of data set creation
beg_date = 2002
end_date = 2002
modifier = 5


dna_experiment2 = exonerations%>%
  filter(((exonerated>=end_date)|(exonerated<=beg_date))&((exonerated<=end_date+modifier)&(exonerated>=beg_date-modifier)))%>%
  mutate(date_witnessreform = ifelse(date_witnessreform=="None",2025,date_witnessreform),
         date_recordinginterrog = ifelse(date_recordinginterrog=="None",2025,date_recordinginterrog),)%>%
  mutate(control = date_postconvictiondna>end_date+modifier,
         treatment = (date_postconvictiondna<=end_date&date_postconvictiondna>=beg_date),
         after_target = exonerated>=end_date)%>%
  filter(control==1|treatment==1)%>%
  filter(exonerated<=end_date+beg_date-1990)


#dna experiment 2003
#look at top comment for explanation of data set creation
beg_date = 2003
end_date = 2003
modifier = 5


dna_experiment3 = exonerations%>%
  filter(((exonerated>=end_date)|(exonerated<=beg_date))&((exonerated<=end_date+modifier)&(exonerated>=beg_date-modifier)))%>%
  mutate(date_witnessreform = ifelse(date_witnessreform=="None",2025,date_witnessreform),
         date_recordinginterrog = ifelse(date_recordinginterrog=="None",2025,date_recordinginterrog),)%>%
  mutate(control = date_postconvictiondna>end_date+modifier,
         treatment = (date_postconvictiondna<=end_date&date_postconvictiondna>=beg_date),
         after_target = exonerated>=end_date)%>%
  filter(control==1|treatment==1)

#dna experiment combined
#This aggragate the previous three time frames datasets into a single dataset
dna_experiment_comb = dna_experiment1%>%
  bind_rows(dna_experiment2)%>%
  bind_rows(dna_experiment3)

#The following is the contol and treatment sizes for each experiment
#This is reflected in table 1 of the paper
sum(dna_experiment1$control, na.rm = TRUE)
sum(dna_experiment1$treatment, na.rm = TRUE)
sum(dna_experiment2$control, na.rm = TRUE)
sum(dna_experiment2$treatment, na.rm = TRUE)
sum(dna_experiment3$control, na.rm = TRUE)
sum(dna_experiment3$treatment, na.rm = TRUE)

#The following are all of the linear regressions run for the results section.
#The general format is 'fit_' followed by either ds,fc or mw. ds stands for DNA substantial,
#fc for false confession, and mw for mistaken witness ID. These are the outcomes we are 
#measuring the policies effect on. After this two letter code is a number. This number
#identifies which of the three time periods we're looking at. If instead of a number there is a
#'C', this means that all three time periods have been combined. If there linear regression name
#ends in 'ctrl' that indicated that controls for race and state population were used.
fit_ds1ctrl = lm(dna_substantial ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment1)
fit_fc1ctrl = lm(fc ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment1)
fit_mw1ctrl = lm(mwid ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment1)
fit_ds2ctrl = lm(dna_substantial ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment2)
fit_fc2ctrl = lm(fc ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment2)
fit_mw2ctrl = lm(mwid ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment2)
fit_ds3ctrl = lm(dna_substantial ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment3)
fit_fc3ctrl = lm(fc ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment3)
fit_mw3ctrl = lm(mwid ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment3)
fit_dsCctrl = lm(dna_substantial ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment_comb)
fit_fcCctrl = lm(fc ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment_comb)
fit_mwCctrl = lm(mwid ~ after_target + treatment + treatment*after_target + statepop + race, data = dna_experiment_comb)

#This is the code for table 5: looking at the effect of post conviction DNA on
#DNA playing a substantial role in the exoneration
stargazer(fit_ds1ctrl,fit_ds2ctrl,fit_ds3ctrl,
          title = "Post Conviction DNA",
          dep.var.labels = c("DNA Substantial"),
          column.labels = c("2001","2002","2003"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)

#This is the code for table 6: looking at the effect of post conviction DNA on
#false confession
stargazer(fit_fc1ctrl,fit_fc2ctrl,fit_fc3ctrl,
          title = "Post Conviction DNA",
          dep.var.labels = c("False Confession"),
          column.labels = c("2001","2002","2003"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)

#This is the code for table 7: looking at the effect of post conviction DNA on
#mistaken witness ID
stargazer(fit_mw1ctrl,fit_mw2ctrl,fit_mw3ctrl,
          title = "Post Conviction DNA",
          dep.var.labels = c("Mistaken Witness ID"),
          column.labels = c("2001","2002","2003"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)

#This is the code for table 2: looking at the effect of post conviction DNA on
#all three dependent variables for the combined data set
stargazer(fit_dsCctrl,fit_fcCctrl,fit_mwCctrl,
          title = "Post Conviction DNA",
          dep.var.labels = c("DNA Substantial","False Confession","Mistaken Witness ID"),
          covariate.labels = c("After End date","Treatment Group","Black","Hispanic","Native American","Other Race","White","State Population","Treated")
)