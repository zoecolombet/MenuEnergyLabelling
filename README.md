# MenuEnergyLabelling_code

This repository shares the code lists for our analysis of the health impact of the menu calory labelling in England. The results of our analysis is currently submitted in a peer-reviewed paper.

This repository contains the following files:

    DiseaseSummary.csv - this is a list of chronic conditions of interest for our study, narrowed down from the 308 diseases phenotypes by CALIBER. 'Disease' is condition the name in the CALIBER list, 'Disease_mod' is our amended name. The variables 'disease_num' & 'system_num' are arbitrary variables for ease of coding. 'type' refers to whether a single code ever recorded is sufficient, or whether a multivisit criteria (at least 3 visits within 1 year) is required; conditions with an empty type are from test values only. 'testvalues' (yes/no) is for whether test values are used for this condition.
    codelists - .csv files for each disease - codes for test results are in the 'tests' subfolder. the 'mapping' column is how the Aurum code was derived from the CALIBER GOLD lists: a. cleansedreadcode - GOLD Read code matched Aurum 'cleansedreadcode' b. originalreadcode - GOLD Read code matched Aurum 'originalreadcode' c. description - GOLD description matched Aurum 'term' d. termsearch - codes identified by searching Aurum for similar terms to original CALIBER GOLD list
    DiseaseDocumentation - this shows the codelists along with notes on implementation. The .md file is too large to be rendered in github, a .html version can be viewed here: https://annalhead.github.io/CPRD_multimorbidity_codelists/docs/DiseaseDocumentation.html
