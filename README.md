# AdFL
This is a data repository for AdFL paper. 
It contains 40 subject files, 800 synthetic adaptations, labeled tests for each adapatiaons and set of unlableled tests. 
For example:
UrlValidator_1_10.java file is a synthetic adaptation for UrlValidator.java file produced by 10% labeling scheme with corrrosponing
labeled tests. Similarly for 20% labeling scheme. 
UrlValidator.java is the original file. 

labeled_ublabeled folder contains labeled and unlabeled tests.

For example:
Consider UrlValidator.label10 file.
10,1, testHierarchicalUriMutation, testEmpty, testRelativePath.
10: is the labeling scheme. 
1 is the run number.
and 3 labeled tests are testHierarchicalUriMutation, testEmpty, testRelativePath.


We note that currently few _10 and _20 files are missing. They will be soon updated. 

diff files: .java file under diff folder contains statement diff and .diff file contains UUID associated with those diff statements. Helps to compute fault loclaization for removed statements quickly.

annotated files: annotated files contains every statement of the java file annotated. We collect coverage information based on the annotations. We use our own tool for computing fault localizaiton. The reason for such a side track is: It helps us to compare with results with 2 previous papers effortlessly. If we used Gzolator or other tools, it would be difficult for us to compare results with previous papers based on how results for previous papers are maintained. 

Pristine folder contains pristine java projects that we used for running our tests. These java projects gets updated fast so we recommend using same projects to get exact same results. Some of them may be few years old.





