# forestFloor development

 get latest stable version on [CRAN forestFloor](https://cran.r-project.org/web/packages/forestFloor)

[Travis](https://travis-ci.org/sorhawell/forestFloor) says: ![alt tag](https://travis-ci.org/sorhawell/forestFloor.svg?branch=master)

**note to my self:**
1.8.9 (unfrozen github)

- (v) XY axes can be cropped for outliers also in show3d
- (v) forestFloor supports class randomForest.formula and will try other subclasses of randomForest
- (v) fidxed: Z-label in show3d was sometimes written twice
- (v) bug if forestFloor X input is matrix without dimnames, show3d() function label columns by all values concatenated.
could be fixed by casting matrix to data.frame always

- fcol should support color gradients perpendicular to fitted functions
- implement some matrix/data.frame checks to avoid crash if non training feature matrix is forwarded to forestFloor()
- par(mfrow) does not always restore after plot.forestFloor

- coming soon:
forestFloor will support Rborist models

1.8.7-1.8.8(Stable Cran):
- (v) forestFloor will support rfPermute models (easy, as they are 95% a randomForest object)
- (v) fix bug, show3d will produce error of only on feature contribution column is selected
- (V) fix bug, test input in randomForest functions will prpevent forestFloor from runnig
        including a test input in randomForest (xtest , ytest) make keep.forest default FALSE If forest is missing, error meassage prompts user to force keep.forest=T. 
- (v) set Xi=1:2 as default, in show3d.forestFloor_multiClass
