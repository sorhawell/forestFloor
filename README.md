# forestFloor development


 get latest stable version on [CRAN forestFloor](https://cran.r-project.org/web/packages/forestFloor)

![alt tag](https://travis-ci.org/sorhawell/forestFloor.svg?branch=master)
![alt tag](http://cranlogs.r-pkg.org/badges/last-day/forestFloor)
![alt tag](http://cranlogs.r-pkg.org/badges/forestFloor)
![alt tag](http://cranlogs.r-pkg.org/badges/grand-total/forestFloor)


##### 1.9.4 (cran)
Features:
 * plot.forestFloor_multiClass, now have col= parameter to control colours. Intended to replace the use of the colLists colLists. Interfacing through colLists still possible.
Bug-fix:
 * plot.forestFloor_multiclass did not index by variable importance correctly


##### 1.9.3 (cran)

Features:
 * It is now possible to compute feature contributions of feature test set Xtest. Formula interface not implemented for Xtest yet. For data.frame X and Xtest, names and classes of columns (numeric/factor). Also, levels of factors must match. Any used level in any factor of Xtest must have been used at least once in X during training. If Xtest is provided, any visualization will as standard visualize feature contributions of  Xtest rather than X. plotTest=F will revert this. plotTest="andTrain" (partial matched) will enable visualization of both test and train. In the forestFloor output object, feature contributions for X and Xtest are row binded in the same matrix FCmatrix / FCarray. A booleen vector isTrain describes what rows are train and what are test.

 * Bootstrapping and stratification can also be seen as local increments and do influence the final RF prediction. To precisely assure, that all feature contributions for each observation do sum to the RF OOB-CV prediction, a new param bootstrapFC has been included. When set to TRUE, one extra column is added to FCmatrix(regression) or n.classes columns to the FCarray(classification). Each tree has a 'bootstrap local increment' (bootstrapLI). bootstrapLI = rootNode_rate - base_rate, where rootNode_rate is the bootstrapped sampled (inbag) class label distribution in root node, and base_rate is the overall class label distribution in training set. bootstrapFC is for a given observation the sum of bootstrapLI in those trees, where that observation was out-of-bag. bootstrapFC=TRUE does not change any visualization.

Bug-fix:
 * Until now, forestFloor has assumed for not fully grown trees of randomForest classification, that non-unaimous votes would be passed directly to the total ensemble vote. This is for the randomForest implementation incorrect, as majority vote is used both in terminal nodes and as default on ensemble level. A new setting majorityTerminal=TRUE for the low-level method forestFloor_randomForest_multiclass has been implemented. (Bonus info: This new setting can be reverted passing majorityTerminal=FALSE through forestFloor(...) and could probably be used to tweak randomForest into producing 'probability forest' / 'sklearn'-isch predictions.)
 

1.9.0-1.9.1
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
