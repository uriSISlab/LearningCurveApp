---
title: 'Learning ‘Learning Curves’ with R Shiny'
tags:
  -R
  -human factors
  -management
  -learning curves
  -power curve
  -LEGO&reg; construction
authors:
  -name: Nicholas D. Bernardo
  orcid: 0000-0003-4104-8505
  affiliation: 1
  -name: Gretchen A. Macht, Ph.D.
  orcid: 0000-0002-3222-1145
  affiliation: 1
affiliations: 
  -name: Mechanical, Industrial &amp; Systems, University of Rhode Island
  index: 1
date: 19 November 2019
bibliography: paper.bib
---
#Summary
While training individuals to perform a task, the concept of learning and forgetting differs by worker as each has certain levels of cognitive ability and experience. When assessing a production-based environment, to effectively maximize the output of that process, workers should be selected and assigned based on their skills and abilities (i.e., ''fitting the job to the human'') to a specific task (i.e., worker-task) [@Carnahan:2000; @Nembhard:2001; @Nembhard:2007]. Based on the cross-training literature, the best policy is for workers to learn 2-3 tasks [@Nembhard:2007]. To completely understand the functionality of a worker-task, an analysis of their associated learning curves are required. 

The topic of learning curves is often taught in human factors and management courses. An effective approach to teaching this subject matter is often through interactive demonstration (e.g., building LEGO&reg;). Students are asked to perform the same task multiple times and to record the completion duration per trial. Using this data, students can observe the essence of the learning curve by plotting the values, as well as performing statistical analyses by fitting the data with a power curve. Once a power curve is generated per worker-task relationship, then following the expected behavior of that fitted learning curve can assist in establishing how long it will take someone to be trained on a specific task and the expected times to complete a task (i.e., basic time). These curves can allow for effective planning of worker-task relationships in production and management (e.g., manufacturing, construction). 

R Shiny was utilized to streamline and standardize data entry, data organization, and statistical analyses of task completion durations at varying levels of task complexity. Two versions of this learning curve R Shiny application are presented: a demonstrational version and an assignment version. The demonstrational version is intended for smaller trial sizes (n&equals;3) that can be used to simply demonstrate change in operating a single worker-task with the capabilities to illustrate multiple students at once. The assignment version is intended for a single student with a larger trial size (n&equals;16) but for all LEGO&reg; items within their particular LEGO&reg; color set. Additionally, the assignment version provides a report generation function for assignment submission, further discussed below. 

In the presented R Shiny applications, there are two sections of the GUI: the left-side input panel and the right tabular/graphic data manipulation area. 

The left panel's ‘Introduction’ tab displays a brief set of instructions on how to use the application. On the ‘Analysis’ tab, users can enter their name and select the color LEGO&reg; set that they will be building. The ‘Analysis’ tab’s ‘Report Generation’ function within the assignment version of the app saves an html file to the user’s computer on which all plots, data, and equations are presented. This report allows for easy submission as part of a class assignment or to otherwise save the results of experimentation. 

On the right panel, the ‘Data Entry’ tab's default display is a table available for data entry. Upon populating the entire data table, the remaining tabs of the right-hand panel (i.e., the ‘Individual Plot’ and ‘Combined Plot’ tabs) present plots of both the entered data and the fitted power curves (i.e., learning curves), as well as the formulas fit to each column of data.


#Statement of Need
While learning curves can be calculated by hand, estimations for power curve fitting must be made that are inherently inaccurate. Additionally, when scaled to larger classroom sizes, especially those with students who are newer to coding and/or statistics, having a tool that can quickly present data visualizations and produce a submittable report is likely to increase student participation and engagement in the material. This platform also provides an educator with a consistently formatted and reproducible document from each student (i.e., assignment version) or multiple students at a time (i.e., demonstrational version). 

While not directly engaging students in coding, the source code, as well as an R Markdown activity replicating the generation of a learning curve, is provided to students who are interested in the backend of the application. This may encourage initial student engagement in using coding languages at various ages (i.e., K-12 or higher education) to present information and projects in a more dynamic and creative way.

#Materials
For LEGO&reg; sets, please see the following links: <br>
Blue: CLASSIC - Blue Creativity Box (#10706) with 78 pieces for ages 4-99. <br> https://www.lego.com/en-us/product/blue-creativity-box-10706 <br>
Red: CLASSIC - Red Creativity Box (#10707) with 55 pieces for ages 4-99. <br> https://www.lego.com/en-us/product/red-creativity-box-10707 <br>
Green: CLASSIC - Green Creativity Box (#10708) with 66 pieces for ages 4-99. <br> https://www.lego.com/en-us/product/green-creativity-box-10708 <br>
Orange: CLASSIC - Orange Creativity Box (#10709) with 60 pieces for ages 4-99. <br> https://www.lego.com/en-us/product/orange-creativity-box-10709

#Acknowledgements
Thank you to the students of the University of Rhode Island, ''Introduction to Human Factors & Ergonomics'' course and a local elementary school STEAM night for assisting with testing and validation of the app. Special thank you to Dr. Rachel Schwartz for providing support and advice throughout the development process of these applications.

#References
