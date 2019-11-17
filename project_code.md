---
title: "Stats 112 Final Project (New Idea)"
author: 
- "Emma Higgins, Max Danielewicz, Will Hamlin, Edwin Reyes Herrera"
- "STATS 112 Final Project"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: yes
---

# Introduction

* Info on motivation for looking into this dataset
* explain where data came from
* outline research question

## Codebook

Variable                    Meaning
--------------------------  ---------
`state`                     identifying variable
`pop`                       size of population
`lgl_abortion_clinics`      Number of legal abortion clinics available to women in the state
`health_clinics`            women's health clinics that do not provide abortion services
`prop_abortion`             the ratio of people per abortion clinic in the state (bigger numbers mean presumably less access)
`prop_health`               the ratio of people per women's health clinic in the state (bigger numbers mean presumably less access)
`paid_fam_leave`            whether a state has a law requiring an employer to issue paid family leave (women are disproportionately affected by paid family leave or lack thereof because they're the ones having babies!)
`prop_equal_pay`            how many cents women make to the dollar that men make in each state
`equal_pay_rank`            how a state ranks nationally according to equal pay laws
`marital_rape_except`       whether a state has loopholes in sexual assault in the cases of marriage
`law strength`              5 = clear and convincing, 4 = beyond reasonable doubt, 3 = not specified, 2 = conviction, 1 = nothing				
`year_passage`              when the bill (to terminate parental rights in cases of rape that resulted in child's conception) was initially passed
`year_amend`                when/if the bill was amended to expand restrictions from requiring a rape conviction to clear and convincing evidence
`post_2015`                 whether the bill was passed before or after 2015, when the Obama Administration issued grant money to any state that made this issue a legislative priority
`bill_name`                 name of bill passed in the state
`perc_women`                percent of women 
`perc_demo_senate`	        percent of members in the Senate that are of the Democratic party
`perc_demo_house`	          percent of memebrs in the House that are of the Democratic party
`Senate`	                  Party that controlled Senate during the passage of the bill
`House`	                    Party that controlled House during the passage of the bill
`Governor`                  Party of governor of the state during the passage of the bill

# Preliminary Analysis: Clustering


```r
# Process the data: turn the identifying variable into row names
# Eg: assume the identifying variable is labeled 'id'
library(tibble)
my_cluster_data <- my_raw_data %>% 
  column_to_rownames("id")


# Hierarchical clustering
# method can be "complete", "single", "average", "centroid"
hier_model <- hclust(dist(scale(my_cluster_data)), method = ___)


# Visualization: heatmaps (w/ and w/out dendrogram)
heatmap(data.matrix(scale(my_cluster_data)), Colv = NA)
heatmap(data.matrix(scale(my_cluster_data)), Colv = NA, Rowv = NA)


# Visualization: dendrogram (change font size w/ cex)
plot(hier_model, cex = 0.8)


# Assign each sample case to a cluster (you can add to dataset using mutate())
# You specify the number of clusters, k
as.factor(cutree(hier_model, k = ___))
```


```r
# Calculating the mean of each feature for each cluster when there are 2 clusters
my_cluster_data %>%
  mutate(cluster = cluster_2) %>% group_by(cluster) %>%
  summarize_all(list(mean = mean))
```

# Other things to keep in mind as we make visualizations

* Of the 24 states (nearly half) that passed legislation after 2015, 19 (79.2%) of them do not require a rape conviction for termination of parental rights, and 14 (58.33%) require only clear and convincing evidence to terminate parental rights of rapists.

* There are only 16 states that use “clear and convincing evidence” as a burden of proof for terminating parental rights of rapists, and 14 of those states (87.5%) passed legislation AFTER 2015.

* 6 (37.5%) “blue” (majority democratic legislators in the state legislature), 2 (12.5%) “purple” (split between democratic and republican legislators in the state legislature) and 8 (50%) “red” (majority republican legislators in the state legislature) states use the clear and convincing standard. There seems to be no significant variation along party lines with regard to a higher burden of proof.

* 14 states who passed legislation to terminate the parental rights of rapists had democratically controlled legislatures at the time of passage, 5 were split, and 30 were republican-controlled.

* 21 states currently require a rape conviction for TPR. Of those states, 4 (19%) were “blue”, 14 (66.67%) were “red”, and 3 (14.29%) were “purple” at the time of passage.
