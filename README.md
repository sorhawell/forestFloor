# forestFloor development

 get latest stable version on [CRAN forestFloor](https://cran.r-project.org/web/packages/forestFloor)

[Travis](https://travis-ci.org/sorhawell/forestFloor) says: ![alt tag](https://travis-ci.org/sorhawell/forestFloor.svg?branch=master)

**note to my self:**

- implement some matrix/data.frame checks to avoid crash if non training feature matrix is forwarded to forestFloor()
- par(mfrow) does not always restore after plot.forestFloor


1.9.0 (unfrozens Github)
New Features:
 - user no longer have to use the trimTrees:cinbag to train a classification forest to obtain inbag counts .Restrictions and error messages removed.
 - () Hence documentation needs an update
 

1.8.9 (CRAN)
New features
- XY axes can be cropped for outliers also in show3d
- forestFloor supports class randomForest.formula and will try other subclasses of randomForest
- fcol(byResiduals) fcol support color gradients perpendicular to fitted functions
- ggplot functions has been completely disabled
- link to webpage forestFloor.dk

Bugs:
- Z-label in show3d was sometimes written twice
- If forestFloor X input is matrix without dimnames, show3d() plot labels were messed up.



- coming soon:
forestFloor will support Rborist models

1.8.7-1.8.8(Stable Cran):
- (v) forestFloor will support rfPermute models (easy, as they are 95% a randomForest object)
- (v) fix bug, show3d will produce error of only on feature contribution column is selected
- (V) fix bug, test input in randomForest functions will prpevent forestFloor from runnig
        including a test input in randomForest (xtest , ytest) make keep.forest default FALSE If forest is missing, error meassage prompts user to force keep.forest=T. 
- (v) set Xi=1:2 as default, in show3d.forestFloor_multiClass
