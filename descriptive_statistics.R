library("data.table")
library("stargazer")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("aod")


#Descriptive Statistics

#Distribution of exonerees by race
table(exonerations$race)
#Distribution of exonerees by state
table(exonerations$state)

#Comparing sentence lengths for different races
race_avg_sentence = exonerations%>%
  group_by(race)%>%
  summarise(avg = mean(exonerated-convicted))


#Total times each outcome (dna substantial, dna included, false confession, and mistaken 
#witness ID) occured.
sum(exonerations$dna_substantial)
sum(exonerations$dna_included)
sum(exonerations$fc)
sum(exonerations$mwid)

#Total times each outcome (dna substantial, false confession, and mistaken 
#witness ID) occurred in isolation
sum(filter(exonerations,(fc==0)&(mwid==0))$dna_substantial)
sum(filter(exonerations,(dna_substantial==0)&(mwid==0))$fc)
sum(filter(exonerations,(fc==0)&(dna_substantial==0))$mwid)


#Adds a dummy to exonerations dataset indicating if the person was convicted before 
#or after each policy had been implemented in their state
desc_policy_effects = exonerations%>%
  mutate(before_dna = (convicted<date_postconvictiondna)|(date_postconvictiondna=="None"),
         before_int = (convicted<date_witnessreform)|(date_witnessreform=="None"),
         before_wit = (convicted<date_recordinginterrog)|(date_recordinginterrog=="None"))


#The following three data sets summarise the average sentence length for an exoneree
#depending on whether that exoneree's state had implemented one of the three policies


#Sentence length depending on DNA policy
desc_dna_effects = desc_policy_effects%>%
  group_by(before_dna)%>%
  summarise(avg = mean(exonerated-convicted))%>%
  mutate(policy = "DNA",has_policy = 1-before_dna)%>%
  filter(!is.na(has_policy))

#Sentence length depending on Interrogation policy
desc_int_effects = desc_policy_effects%>%
  group_by(before_int)%>%
  summarise(avg = mean(exonerated-convicted))%>%
  mutate(policy = "Interrogation",has_policy = 1-before_int)%>%
  filter(!is.na(has_policy))

#Sentence length depending on witness policy
desc_wit_effects = desc_policy_effects%>%
  group_by(before_wit)%>%
  summarise(avg = mean(exonerated-convicted))%>%
  mutate(policy = "Witness",has_policy = 1-before_wit)%>%
  filter(!is.na(has_policy))
 
#This code generates the figure seen in the background section
desc_dna_effects%>%
  bind_rows(desc_int_effects)%>%
  bind_rows(desc_wit_effects)%>%
  ggplot(aes(x=has_policy,y=avg,color = policy))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks=c(0,1),
                   labels=c("Before", "After"))+
  labs(title = "Sentence Lengths Before and After Policies", x = "Policy Implemented", y = "Average Sentence Length (years)")+
  theme_classic()


