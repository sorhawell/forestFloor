#include <Rcpp.h>
using namespace Rcpp;

//internal function to convert int to binary vector
bool inbin(int bestsplit_groups, int obs_group) { 
  int divisionStore=0,modStore=0;
  int i = 0;
  do {
      modStore=bestsplit_groups%2;
      bestsplit_groups=bestsplit_groups/2;
      i++;
  } while(i < obs_group); //counting groups from 1.2....
  return modStore==1;
}

//defining recursive function to iterate nodes
void follow_path(
//local recursive function environment                 
                 bool calculate_node_pred,
                 int i_tree,
                 int this_node,
                 double parent_pred,
                 int parent_bestvar,
                 int passed_OOB_count,
                 int passed_IB_count,
                 IntegerVector passed_innodes,
                 IntegerVector train_innodes,
//global R-objects
                 NumericMatrix X,                  // 1  X
                 NumericVector Y,
                 IntegerMatrix leftDaughter,       // 6  LD
                 IntegerMatrix rightDaughter,      // 7  RD
                 IntegerMatrix nodestatus,         // 8  nodestatus
                 NumericMatrix xbestsplit,         // 10 xsplits
                 NumericMatrix nodepred,           // 11 averagetrainnodes
                 IntegerMatrix bestvar,
                 IntegerVector varLevels,          //12 bestvar
                 NumericMatrix localIncrements) 
  {
  
   
  //computing and adding the latest increment to localIncrements-matrix
  double current_pred = 0;
  if(calculate_node_pred) {
    for(int thisIB_count = 0; thisIB_count<passed_IB_count;thisIB_count++) {
      current_pred += Y(train_innodes(thisIB_count));
    }
    if(passed_IB_count>0) {
      current_pred /= (passed_IB_count);
    } else {
//      printf("error no IB \n");
//      current_pred = parent_pred;
    }// divide sum of IB_preds with IB_count
  } else { 
    current_pred = nodepred(this_node,i_tree); //reuse node_pred from RF-object, only regression
  }
  
  
  double this_increment = current_pred - parent_pred;
  int outcome  = 0;
  int this_obs = 0;
    
  for(int i_obs=0;i_obs<passed_OOB_count;i_obs++) {
      this_obs = passed_innodes[i_obs];
      localIncrements(this_obs,parent_bestvar) += this_increment;  
  }

  //if not a terminal node, split 
  if(passed_OOB_count>0) {
    if(nodestatus(this_node,i_tree)==-3) { // #if this is not a terminal node
      int current_bestvar = bestvar(this_node,i_tree);
      int OOB_count_left  = 0;
      int OOB_count_right = 0;
      int IB_count_left   = 0;
      int IB_count_right  = 0;
      
      IntegerVector OOBs_leftnode (passed_OOB_count);
      IntegerVector OOBs_rightnode(passed_OOB_count);
      IntegerVector IBs_leftnode (passed_IB_count);
      IntegerVector IBs_rightnode(passed_IB_count);
      
      double this_split = xbestsplit(this_node,i_tree); // splitting-point
      int this_int_split = this_split;                  // used if split point is categorical
      int lev = varLevels(current_bestvar);             // get number of levels for this variable, >1 is categorical               
      bool send_this_OOB_left;
      bool send_this_IB_left;

      //splitting OOB
      for(int i_obs = 0;i_obs<passed_OOB_count;i_obs++) {
        //lev==1, means numeric split, lev>1 means categorical split
        if(lev==1) {
        // numeric split, less than or equal goes left, see details section of help(getTree::randomForest)
        send_this_OOB_left = X(passed_innodes[i_obs],current_bestvar) <= this_split; //split by X-value
        } else {
        //binary expansion is explained in details section of help(getTree::randomForest)
        send_this_OOB_left = inbin(this_int_split,X(passed_innodes[i_obs],current_bestvar)+1); //split by binary expansion
        }
        if(send_this_OOB_left) {
          OOBs_leftnode[OOB_count_left] = passed_innodes[i_obs];
          OOB_count_left++;
        } else {
          OOBs_rightnode[OOB_count_right] = passed_innodes[i_obs];
          OOB_count_right++;
        }
      }
      
      //splitting IB
      //printf("passed ib count is %d \n", passed_IB_count);
      if(calculate_node_pred) {
        for(int i_obs = 0;i_obs<passed_IB_count;i_obs++) {
        if(lev==1) {
          // numeric split, less than or equal goes left, see details section of help(getTree::randomForest)
          send_this_IB_left = X(train_innodes[i_obs],current_bestvar) <= this_split; //split by X-value
        } else {
          //binary expansion is explained in details section of help(getTree::randomForest)
          send_this_IB_left = inbin(this_int_split,X(train_innodes[i_obs],current_bestvar)+1); //split by binary expansion
        }
        if(send_this_IB_left) {
            IBs_leftnode[IB_count_left] = train_innodes[i_obs];
            IB_count_left++;
          } else {
            IBs_rightnode[IB_count_right] = train_innodes[i_obs];
            IB_count_right++;
          }
        }
      }
      //printf("got to here, left %d right %d \n",IB_count_left,IB_count_right);
      //for(int i_print=0;i_print<IB_count_left;i_print++) printf("%d %d \n ",i_print,IBs_leftnode(i_print));
      
      
      //initiate left step
      if(OOB_count_left>0) {
      follow_path(
//local recursive function environment                 
        calculate_node_pred,
        i_tree,
        leftDaughter(this_node,i_tree),
        current_pred,
        current_bestvar,
        OOB_count_left,
        IB_count_left,
        OOBs_leftnode,
        IBs_leftnode,
//pointers to global R-objects
        X,                  // 1  X
        Y,
        leftDaughter,       // 6  LD
        rightDaughter,      // 7  RD
        nodestatus,         // 8  nodestatus
        xbestsplit,         // 10 xsplits
        nodepred,           // 11 averagetrainnodes
        bestvar,            //12 bestvar
        varLevels,
        localIncrements);
        OOB_count_left=0;
        //Rprintf("going back from left, now in %d \n",this_node);
        //if(outcome != 0) return outcome;
      }
      
      //... came back from a left step, now going right
      if(OOB_count_right>0) {
        follow_path(
        calculate_node_pred,
        i_tree,
        rightDaughter(this_node,i_tree),
        current_pred,
        current_bestvar,
        OOB_count_right,
        IB_count_right,
        OOBs_rightnode,
        IBs_rightnode,
        X,                  // 1  X
        Y,
        leftDaughter,       // 6  LD
        rightDaughter,      // 7  RD
        nodestatus,         // 8  nodestatus
        xbestsplit,         // 10 xsplits
        nodepred,           // 11 averagetrainnodes
        bestvar,            //12 bestvar
        varLevels,
        localIncrements);
        OOB_count_right=0;
        //Rprintf("going back from righ, now in %d \n",this_node);
        //if(outcome != 0) return outcome;
        }
        
        
      //... came back from a right step, all nodes below have been completed
        nodestatus(this_node,i_tree)==-1;
    }
  }
  //got to here as node was terminal or all nodes below have been checked
  //Now leaving node going up. Next OOB indices is saved locally in upper node as OOBs_leftnode or OOBs_rightnode
  // if upper node is rootnode, the path will be terminated
  //no returns all changes were applied to global NumericVector localIncrements
//return 0;
}


/// defining Rcpp function to communicate with R
//[[Rcpp::export]]
void recTree(int  vars,               //local 3  nvar
            int  obs,                 //local 4  nobs
            int  ntree,               //local  5  ntrees
            bool calculate_node_pred, //should node prediction
            NumericMatrix X,                  // 1  X
            NumericVector Y,
            IntegerMatrix leftDaughter,       // 6  LD
            IntegerMatrix rightDaughter,      // 7  RD
            IntegerMatrix nodestatus,         // 8  nodestatus
            NumericMatrix xbestsplit,         // 10 xsplits
            NumericMatrix nodepred,           // 11 averagetrainnodes
            IntegerMatrix bestvar,            // 12 bestvar
            IntegerMatrix inbag,
            IntegerVector varLevels,
            IntegerVector OOBtimes,
            NumericMatrix localIncrements)    // 15 inbag obsXtrees
{
  
  
  //declare internal function variables
  IntegerVector innodes_root(obs);        //list of OOBs 
  IntegerVector train_innodes_root(obs);  //list of IBs
  int this_bagcount = 0;
  int times_inbag = 0;
  int OOB_count = 0;
  int IB_count = 0;
  int outcome = 0;
  double root_pred = 0;
  
  //iterate each tree and compute and sum to localIncrements
  for(int i_tree=0;i_tree<ntree;i_tree++){
    
    //make ranges of Out Of Bag and inbag observations in root of tree
    OOB_count = 0;  //reset OOB_count for this new tree
    IB_count  = 0 ; 
    root_pred =  0;
    for(int i_obs=0;i_obs<obs;i_obs++) {  // for all observations 
      this_bagcount = inbag(i_obs,i_tree);// how many time was obs used in tree, 0,1,2,3...
      if(this_bagcount==0) {              // if obs was used 0 times, it is OOB
        innodes_root[OOB_count] = i_obs;  // add observation_indice to list
        OOB_count++;
      } else {
        if(calculate_node_pred) {
          for(times_inbag = this_bagcount;times_inbag>0;times_inbag--) {
            train_innodes_root[IB_count] = i_obs;
            root_pred += Y(i_obs);
            IB_count++;
          }
        }  
      }     
    }
    
    if(calculate_node_pred) {
      root_pred /= IB_count;
    } else {
      root_pred= nodepred(0,i_tree);
    }
    
    
    //printf("root pred %f \n",root_pred);

    //initiating varibles for recursive search of tree
  
     follow_path(
//local recursive function environment                 
        calculate_node_pred,
        i_tree,
        0,   //start in root node, 0
        root_pred, //parent_pred set to root_pred,
        0,    //dummy number, any var within X vars will do 
        OOB_count,  //how many obs OOB to start with in root node
        IB_count,
        innodes_root,  //X.rows indices of OOB observations in rootnode
        train_innodes_root,
//pointers to global R-objects
        X,                  // 1  X dataset
        Y,
        leftDaughter,       // 6  LD
        rightDaughter,      // 7  RD
        nodestatus,         // 8  nodestatus
        xbestsplit,         // 10 xsplits
        nodepred,           // 11 averagetrainnodes
        bestvar,            //12 bestvar
        varLevels,
        localIncrements);
       // if(outcome != 0) return outcome;
  } // go to next tree
  
  //divide sum of increments with trees iterated and mulitiply with 0
  int timesUsed=0;
  for(int i_obs=0;i_obs<obs;i_obs++){
    timesUsed = OOBtimes(i_obs);
    for(int i_vars=0;i_vars<vars;i_vars++){
      localIncrements(i_obs,i_vars) /= timesUsed;
    }
  }

}


int arr2vec(int iObs, int jVar, int Nclass, int Nobs) {
  return(iObs*Nclass+jVar*Nclass*Nobs);
}

//defining recursive function to iterate nodes
void follow_path2(
//local recursive function environment                 
                 //bool calculate_node_pred, //deleted
                 int nClasses,               //new
                 int nObs,                   //new
                 int i_tree,
                 int this_node,
                 NumericVector parent_pred,  //changed from double
                 int parent_bestvar,
                 int passed_OOB_count,
                 int passed_IB_count,
                 IntegerVector passed_innodes,
                 IntegerVector train_innodes,
//global R-objects
                 NumericMatrix X,                
                 IntegerVector Y,
                 IntegerMatrix leftDaughter,       
                 IntegerMatrix rightDaughter,     
                 IntegerMatrix nodestatus,         
                 NumericMatrix xbestsplit,         
                 NumericMatrix nodepred,           
                 IntegerMatrix bestvar,
                 IntegerVector varLevels,        
                 NumericVector localIncrements)  //CHANGED from numericMatrix
  {
  
   
  //computing and adding the latest increment to localIncrements-matrix
  NumericVector current_pred(nClasses);   // placeholder for current predictions
  NumericVector this_increment(nClasses); //placeholder for local increment
  int FCblock_pointer = 0;                // points to block in localIncrements (defacto 3dcube)
  
  //count number of inbag examples of each class in node
  //go through all inbag obs
  for(int thisIB_count = 0; thisIB_count<passed_IB_count;thisIB_count++) { 
    //add one for each class counted
    current_pred(Y(train_innodes(thisIB_count))) += 1; 
  }
  
  //divide by total number of inbag examples in node to get class prevalencies
   //calculate local increment
  if(passed_IB_count>0) {
    //Rprintff("IBn%d \n" ,passed_IB_count);
    double passed_IB_count_double = passed_IB_count;
    for(int iClass = 0; iClass < nClasses; iClass++) {
      current_pred(iClass) /= passed_IB_count_double;
      this_increment(iClass) = current_pred(iClass) - parent_pred(iClass);
      //Rprintff("C%d , I %f  ",passed_IB_count,this_increment(iClass));
    }
  }
  //Rprintff("\n");
  for(int i_obs = 0; i_obs<passed_OOB_count;i_obs++) { //go through all OOB obs
    //find memory-block for this feature contribution by sample and var of nClasses length
    ////Rprintff("parenbestvar %d \n",parent_bestvar);
    FCblock_pointer = arr2vec(passed_innodes(i_obs),       //this iobs
                              parent_bestvar,              //this parent split / jVar
                              nClasses,                    //the number of target classes in model
                              nObs);                       //number of obs in model
    //Rprintff("iOBS%d bvar%d nCl%d nO%d  ",
//    passed_innodes(i_obs),       //this iobs
//    parent_bestvar,              //this parent split / jVar
//    nClasses,                    //the number of target classes in model
//    nObs);
    //Rprintff("FCblock_pointer %d\n",FCblock_pointer);
    //sum local increments to global vector
    for(int iClass = 0; iClass < nClasses; iClass++) {
      localIncrements(FCblock_pointer+iClass) +=  this_increment(iClass);
      ////Rprintff("o%d i%f",i_obs,this_increment(iClass));
      
    }
  }
  
  

//  int outcome  = 0;
//  int this_obs = 0;
//    
//  for(int i_obs=0;i_obs<passed_OOB_count;i_obs++) {
//      this_obs = passed_innodes[i_obs];
//      localIncrements(this_obs,parent_bestvar) += this_increment;  
//  }

  //if not a terminal node, split 
  if(passed_OOB_count>0) {
    if(nodestatus(this_node,i_tree)==-3) { // #if this is not a terminal node
      int current_bestvar = bestvar(this_node,i_tree);
      int OOB_count_left  = 0;
      int OOB_count_right = 0;
      int IB_count_left   = 0;
      int IB_count_right  = 0;
      
      IntegerVector OOBs_leftnode (passed_OOB_count);
      IntegerVector OOBs_rightnode(passed_OOB_count);
      IntegerVector IBs_leftnode (passed_IB_count);
      IntegerVector IBs_rightnode(passed_IB_count);
      
      double this_split = xbestsplit(this_node,i_tree); // splitting-point
      int this_int_split = this_split;                  // used if split point is categorical
      int lev = varLevels(current_bestvar);             // get number of levels for this variable, >1 is categorical               
      bool send_this_OOB_left;
      bool send_this_IB_left;

      //splitting OOB
      for(int i_obs = 0;i_obs<passed_OOB_count;i_obs++) {
        //lev==1, means numeric split, lev>1 means categorical split
        if(lev==1) {
        // numeric split, less than or equal goes left, see details section of help(getTree::randomForest)
        send_this_OOB_left = X(passed_innodes[i_obs],current_bestvar) <= this_split; //split by X-value
        } else {
        //binary expansion is explained in details section of help(getTree::randomForest)
        send_this_OOB_left = inbin(this_int_split,X(passed_innodes[i_obs],current_bestvar)+1); //split by binary expansion
        }
        if(send_this_OOB_left) {
          OOBs_leftnode[OOB_count_left] = passed_innodes[i_obs];
          OOB_count_left++;
        } else {
          OOBs_rightnode[OOB_count_right] = passed_innodes[i_obs];
          OOB_count_right++;
        }
      }
      
      //splitting IB
      //printf("passed ib count is %d \n", passed_IB_count);
     
        for(int i_obs = 0;i_obs<passed_IB_count;i_obs++) {
        if(lev==1) {
          // numeric split, less than or equal goes left, see details section of help(getTree::randomForest)
          send_this_IB_left = X(train_innodes[i_obs],current_bestvar) <= this_split; //split by X-value
        } else {
          //binary expansion is explained in details section of help(getTree::randomForest)
          send_this_IB_left = inbin(this_int_split,X(train_innodes[i_obs],current_bestvar)+1); //split by binary expansion
        }
        if(send_this_IB_left) {
            IBs_leftnode[IB_count_left] = train_innodes[i_obs];
            IB_count_left++;
          } else {
            IBs_rightnode[IB_count_right] = train_innodes[i_obs];
            IB_count_right++;
          }
        }
      
      //printf("got to here, left %d right %d \n",IB_count_left,IB_count_right);
      //for(int i_print=0;i_print<IB_count_left;i_print++) printf("%d %d \n ",i_print,IBs_leftnode(i_print));
      
      
      //initiate left step
      if(OOB_count_left>0) {
      follow_path2(
//local recursive function environment                 
       // calculate_node_pred, //deleted
        nClasses,               //new
        nObs,                   //new
        i_tree,
        leftDaughter(this_node,i_tree),
        current_pred,
        current_bestvar,
        OOB_count_left,
        IB_count_left,
        OOBs_leftnode,
        IBs_leftnode,
//pointers to global R-objects
        X,                  // 1  X
        Y,
        leftDaughter,       // 6  LD
        rightDaughter,      // 7  RD
        nodestatus,         // 8  nodestatus
        xbestsplit,         // 10 xsplits
        nodepred,           // 11 averagetrainnodes
        bestvar,            //12 bestvar
        varLevels,
        localIncrements);
        OOB_count_left=0;
        ////Rprintff("going back from left, now in %d \n",this_node);
        //if(outcome != 0) return outcome;
      }
      
      //... came back from a left step, now going right
      if(OOB_count_right>0) {
        follow_path2(
        //calculate_node_pred,  //deleted
        nClasses,               //new
        nObs,                   //new
        i_tree,
        rightDaughter(this_node,i_tree),
        current_pred,
        current_bestvar,
        OOB_count_right,
        IB_count_right,
        OOBs_rightnode,
        IBs_rightnode,
        X,                  // 1  X
        Y,
        leftDaughter,       // 6  LD
        rightDaughter,      // 7  RD
        nodestatus,         // 8  nodestatus
        xbestsplit,         // 10 xsplits
        nodepred,           // 11 averagetrainnodes
        bestvar,            //12 bestvar
        varLevels,
        localIncrements);
        OOB_count_right=0;
        ////Rprintff("going back from righ, now in %d \n",this_node);
        //if(outcome != 0) return outcome;
        }
        
        
      //... came back from a right step, all nodes below have been completed
        nodestatus(this_node,i_tree)==-1;
    }
  }
  //got to here as node was terminal or all nodes below have been checked
  //Now leaving node going up. Next OOB indices is saved locally in upper node as OOBs_leftnode or OOBs_rightnode
  // if upper node is rootnode, the path will be terminated
  //no returns all changes were applied to global NumericVector localIncrements
//return 0;
}


/// defining Rcpp function to communicate with R
//[[Rcpp::export]]
int multiTree(int  vars,               //local 3  nvar
            int  obs,                 //local 4  nobs
            int  ntree,               //local  5  ntrees
            int  nClasses, //changed from calculate_node_pred
            NumericMatrix X,                  // 1  X
            IntegerVector Y,
            IntegerMatrix leftDaughter,       // 6  LD
            IntegerMatrix rightDaughter,      // 7  RD
            IntegerMatrix nodestatus,         // 8  nodestatus
            NumericMatrix xbestsplit,         // 10 xsplits
            NumericMatrix nodepred,           // 11 averagetrainnodes
            IntegerMatrix bestvar,            // 12 bestvar
            IntegerMatrix inbag,
            IntegerVector varLevels,
            IntegerVector OOBtimes,
            NumericVector localIncrements)    // 15 inbag obsXtrees
{
  
  //Rprintf("obs is %d \n",obs);
  //Rprintf("nclasses is %d \n",nClasses);
  
  //declare internal function variables
  IntegerVector innodes_root(obs);        //list of OOBs 
  IntegerVector train_innodes_root(obs);  //list of IBs
  int this_bagcount = 0;
  int times_inbag = 0;
  int OOB_count = 0;
  int IB_count = 0;
  int outcome = 0;
  IntegerVector root_classCount(nClasses);
  NumericVector root_pred(nClasses);
  
  //iterate each tree and compute and sum to localIncrements
  for(int i_tree=0;i_tree<ntree;i_tree++){
    //make ranges of Out Of Bag and inbag observations in root of tree
    OOB_count = 0;  //reset OOB_count for this new tree
    IB_count  = 0 ; 
    root_classCount =  root_classCount * 0; //reset counter
    
    for(int i_obs=0;i_obs<obs;i_obs++) {  // for all observations 
      //Rprintf("%d-%d \n ",i_obs,obs);
      
      this_bagcount = inbag(i_obs,i_tree);// how many time was obs used in tree, 0,1,2,3...
      if(this_bagcount==0) {              // if obs was used 0 times, it is OOB
        //Rprintf(" 572");
        innodes_root[OOB_count] = i_obs;  // add observation_indice to list
        OOB_count++;
      } else {
      //Rprintf(" 576");  
        for(times_inbag = this_bagcount;times_inbag>0;times_inbag--) {
          ////Rprintff("278");
          train_innodes_root[IB_count] = i_obs;
          ////Rprintff("280");
          int thisNumber = Y(i_obs);
          ////Rprintff("this unmber = %d",Y(thisNumber));
          root_classCount(thisNumber) += 1;
          IB_count++;
        }  
      }     
    }
    //Rprintf("finish inbag OOB \n");
    for(int u =0; u<nClasses;u++) {
    double IB_countDouble = IB_count;
    root_pred(u) = root_classCount(u) / IB_countDouble;
    ////Rprintff(" c%d pred%f ",  root_classCount(u),root_pred(u));
    }
    //printf("root pred %f \n",root_pred);

    //initiating varibles for recursive search of tree
    //if(true)   return(1);
    
     follow_path2(
//local recursive function environment                 
        // calculate_node_pred, //deleted
        nClasses,               //new
        obs,                    //new nObs = obs
        i_tree,
        0,   //start in root node, 0
        root_pred, //parent_pred set to root_pred,
        0,    //dummy number, any var within X vars will do 
        OOB_count,  //how many obs OOB to start with in root node
        IB_count,
        innodes_root,  //X.rows indices of OOB observations in rootnode
        train_innodes_root,
//pointers to global R-objects
        X,                  // 1  X dataset
        Y,
        leftDaughter,       // 6  LD
        rightDaughter,      // 7  RD
        nodestatus,         // 8  nodestatus
        xbestsplit,         // 10 xsplits
        nodepred,           // 11 averagetrainnodes
        bestvar,            //12 bestvar
        varLevels,
        localIncrements);
       // if(outcome != 0) return outcome;
  } // go to next tree
  
  //divide sum of increments with trees iterated and mulitiply with 0
  int timesUsed=0;
  int FCblock_pointer=0;
  for(int i_obs=0;i_obs<obs;i_obs++){
    timesUsed = OOBtimes(i_obs);
   // //Rprintff("timesUsed %d \n",timesUsed);
    if(timesUsed!=0) {
      for(int i_vars=0;i_vars<vars;i_vars++){
        FCblock_pointer = arr2vec(i_obs,       //this iobs
                                  i_vars,              //this parent split / jVar
                                  nClasses,                    //the number of target classes in model
                                  obs);
        for(int i_class=0;i_class<nClasses;i_class++){
         // //Rprintff("o%d, v%d, c%d, BP%d \n",i_obs,i_vars,i_class,FCblock_pointer);
          localIncrements(FCblock_pointer+i_class) /= timesUsed;
        }
      }
    }
  }

}
