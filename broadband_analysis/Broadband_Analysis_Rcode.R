#STEP 1: We started by integrating all of our data sources (our internal data, the county health rankings data, and the census education data), using FIPS codes as the link. We can't share our internal files, but we've included csvs of the other datasets

#STEP 2: After integrating, we had a lot of meausres we weren't going to use, so we limited our dataframe to variables necessary for analysis. Num_accounts is our dependent variable, it reflects the number of active accounts we have in any given county                                   

temp <- broadband %>% 
  dplyr::select(num_accounts, broad_cat, Population_2010, pop_density, pct_rural, gender_pct_female, pct_highschool, pcp_ratio, num_pcps, Population_2010_std, video_law, State, med_income, age_pct_65over)

#STEP 3: Some of the counties were missing data. We removed them. 

broadband_1 <- na.omit(temp)

#STEP 4: We checked correlations to identify any multicolinearity concerns

corr_check <- broadband_1 %>% dplyr::select(-State) 

cor(corr_check)

#STEP 5: We reviewed descriptives and looked for anything odd or unexpected
stargazer(broadband_1, type = "text", title="Descriptive statistics", digits=2)


#STEP 6: Then, we plotted our data, starting with the distribution of the DV

ggplot(broadband_1, aes(x=num_accounts))+
  geom_histogram(color="black", fill="lightblue", linetype="dashed", binwidth = 5) +  
  xlim(0, 350) + 
  ylim(0, 150)

#We are dealing with count data, and as expected, the distribution looked very poisson-like

#STEP 7: Then, we looked at a boxplot. We noticed that there were some outlying values

ggplot(broadband_1) +
  aes(x = as.factor(broad_cat), y = num_accounts) +
  geom_boxplot() +
  facet_wrap(~as.factor(video_law))
