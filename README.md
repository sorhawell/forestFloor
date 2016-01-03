# forestFloor development


 get latest stable version on [CRAN forestFloor](https://cran.r-project.org/web/packages/forestFloor)

![alt tag](https://travis-ci.org/sorhawell/forestFloor.svg?branch=master)
![alt tag](http://cranlogs.r-pkg.org/badges/last-day/forestFloor)
![alt tag](http://cranlogs.r-pkg.org/badges/forestFloor)
![alt tag](http://cranlogs.r-pkg.org/badges/grand-total/forestFloor)



**todo list:**

- implement some matrix/data.frame checks to avoid crash if non training feature matrix is forwarded to forestFloor()
- par(mfrow) does not always restore after plot.forestFloor
- Documentation and examples related to trimTrees::cinbag needs an update
- Implement a better interface with par(), such that e.g. auto mfrow and mar can be overridden by user

1.9.0-1.9.1 (CRAN)
 - **New Features:**
 - user no longer have to use the trimTrees:cinbag to train a classification forest to obtain inbag counts as randomForest some time ago started to support inbag counts. Restrictions and error messages removed.
 - any graphical argument can passed through plot.forestFloor to plot or points. GOF_col arg is changed to GOF_args. If conflicts with internal arguments, user provided arguments have priority.
 - likewise for vec.plot, plot and plot3d through ..., points and surface3d through moreArgs=list()
 - graphical parameters par() can be set temporarily through dot args plot.forestFloor(,...). Hereby, e.g. mar or mfrow can be set manually and will override standard settings of this function.
 - testing of most examples are moved to travis as runtime exceeds CRAN policy
 
**Misc:**
 - Major Xmas induced revision of all docs & examples
 

1.8.9
- **New features:**
- XY axes can be cropped for outliers also in show3d
- forestFloor supports class randomForest.formula and will try other subclasses of randomForest
- fcol(byResiduals) fcol support color gradients perpendicular to fitted functions
- ggplot functions has been completely disabled
- link to webpage forestFloor.dk
- **Bugs:**
- Z-label in show3d was sometimes written twice
- If forestFloor X input is matrix without dimnames, show3d() plot labels were messed up.


1.8.7-1.8.8:
- forestFloor will support rfPermute models (easy, as they are 95% a randomForest object)
- fix bug, show3d will produce error of only on feature contribution column is selected
- fix bug, test input in randomForest functions will prpevent forestFloor from runnig
        including a test input in randomForest (xtest , ytest) make keep.forest default FALSE If forest is missing, error meassage prompts user to force keep.forest=T. 
- set Xi=1:2 as default, in show3d.forestFloor_multiClass
