Attempting to pull iterations out of RSiena
================
Sam Tyner
1/23/2017

First I set up some simple examples from the `RSiena` website.

``` r
library(RSiena)
# ---- A. ----------------------------------------------------------------------
# First we have to create objects for the dependent variables.

# sienaDependent creates a sienaDependent object, here a network,
# from a matrix or array or list of sparse matrix of triples.
# The name of this network object (here: friendship) will be used
# in the output file.
 setwd("data/s50_data")
  friend.data.w1 <- as.matrix(read.table("s50-network1.dat"))
  friend.data.w2 <- as.matrix(read.table("s50-network2.dat"))
  friend.data.w3 <- as.matrix(read.table("s50-network3.dat"))
  drink <- as.matrix(read.table("s50-alcohol.dat"))
  smoke <- as.matrix(read.table("s50-smoke.dat"))

 friendship <- sienaDependent(
                     array( c( friend.data.w1, friend.data.w2, friend.data.w3 ),
                     dim = c( 50, 50, 3 ) ) )

# The integers in the dim() here refer to the number of nodes (senders,
# receivers) and the number of waves.
# This object is an array of dimension 50 x 50 x 3, representing
# three adjacency matrices, with a number of attributes.
# Note that this is an object of class

        class(friendship)
```

    ## [1] "sienaDependent"

``` r
# with specific attributes and methods associated with it.
# You can get the detailed information by requesting

        dim( friendship )
```

    ## [1] 50 50  3

``` r
        attributes( friendship )
```

    ## $dim
    ## [1] 50 50  3
    ## 
    ## $class
    ## [1] "sienaDependent"
    ## 
    ## $type
    ## [1] "oneMode"
    ## 
    ## $sparse
    ## [1] FALSE
    ## 
    ## $nodeSet
    ## [1] "Actors"
    ## 
    ## $netdims
    ## [1] 50 50  3
    ## 
    ## $allowOnly
    ## [1] TRUE

``` r
# If you only are interested in the value of one particular attribute,
# you can request this by, e.g.,

        attributes( friendship )$type
```

    ## [1] "oneMode"

``` r
# A very concise description of the friendship data is obtained by typing

        friendship
```

    ## Type         oneMode             
    ## Observations 3                   
    ## Nodeset      Actors (50 elements)

``` r
# The function sienaDependent can also be used to create a behavior variable object
# with the extra argument type = "behavior".
# (Non-mentioned attributes get the default value, and in this case
# oneMode is the default; see below.)
# (Note: only use the variable in ONE role in a given model:
#  behavior variable or changing covariate!)

# ---- B. ----------------------------------------------------------------------
# Second we construct objects for the explanatory (independent) variables.
# From the help request
#       ?sienaDataCreate
# we see that these can be of five kinds:
    # coCovar            Constant actor covariates
    # varCovar           Time-varying actor covariates
    # coDyadCovar        Constant dyadic covariates
    # varDyadCovar       Time-varying dyadic covariates
    # compositionChange  Composition change indicators

# You can get help about this by the following requests:
#       ?coCovar
#       ?varCovar
#       ?coDyadCovar
#       ?varDyadCovar
#       ?sienaCompositionChange

# The variables available for this data set all are changing actor covariates.
# For illustrative purposes, we use smoking as observed at the first wave
# as a constant covariate:

        smoke1 <- coCovar( smoke[ , 1 ] )

# We use the drinking data as a changing covariate.
# the name comes from 'varying covariate'.

        alcohol <- varCovar( drink )

# You need at least three waves in the data set to define a varying covariate
# by the function varCovar as the previous wave is used
# as a predictor of the next wave.

# The command

        attributes( alcohol )
```

    ## $dim
    ## [1] 50  3
    ## 
    ## $dimnames
    ## $dimnames[[1]]
    ## NULL
    ## 
    ## $dimnames[[2]]
    ## [1] "V1" "V2" "V3"
    ## 
    ## 
    ## $class
    ## [1] "varCovar"
    ## 
    ## $centered
    ## [1] TRUE
    ## 
    ## $nodeSet
    ## [1] "Actors"

``` r
# will tell you the information that RSiena now has added to the drink data.

# ---- C. ----------------------------------------------------------------------
# We now combine the dependent and independent variables.
# The function sienaDataCreate creates a Siena data object from input networks,
# covariates and composition change objects;
# the objects that earlier were created by sienaDependent will have the role
# of dependent variables, and similarly the other roles are predetermined
# by creation by the functions coCovar, varCovar,
# coDyadCovar, varDyadCovar, and sienaCompositionChange.

        mydata <- sienaDataCreate( friendship, smoke1, alcohol)
# You may check the result by requesting

        mydata
```

    ## Dependent variables:  friendship 
    ## Number of observations: 3 
    ## 
    ## Nodeset                  Actors 
    ## Number of nodes              50 
    ## 
    ## Dependent variable friendship      
    ## Type               oneMode         
    ## Observations       3               
    ## Nodeset            Actors          
    ## Densities          0.046 0.047 0.05
    ## 
    ## Constant covariates:  smoke1 
    ## Changing covariates:  alcohol

``` r
# ---- D. ----------------------------------------------------------------------
########################################################################## ---- DEFINING EFFECTS 
# The data set as combined in mydata implies a certain set of effects
# that can be included in the specification of the model.
# The function getEffects creates a dataframe of effects with a number of extra
# properties for use in RSiena:

        myeffnull <- getEffects( mydata )

# Let us now consider the myeff object, which is used to specify the model.
# add the transitive triples and 3-cycles effects

        myeff1addl <- includeEffects( myeffnull, transTrip )
```

    ##   effectName          include fix   test  initialValue parm
    ## 1 transitive triplets TRUE    FALSE FALSE          0   0

``` r
        myeff2addl <- includeEffects( myeffnull, transTrip, cycle3 )
```

    ##   effectName          include fix   test  initialValue parm
    ## 1 transitive triplets TRUE    FALSE FALSE          0   0   
    ## 2 3-cycles            TRUE    FALSE FALSE          0   0

``` r
# fit the models to the data. 
  myalgorithm <- sienaAlgorithmCreate(projname = 's50_3')
  
    ansnull <- siena07( myalgorithm, data = mydata, effects = myeffnull,
                        returnChains = TRUE, returnDataFrame = TRUE,
                        returnDeps = TRUE, silent = TRUE, verbose = FALSE)
  ans1addl <- siena07( myalgorithm, data = mydata, effects = myeff1addl,
                       returnChains = TRUE, returnDataFrame = TRUE,
                       returnDeps = TRUE,silent = TRUE, verbose = FALSE)
  ans2addl <- siena07( myalgorithm, data = mydata, effects = myeff2addl,
                       returnChains = TRUE, returnDataFrame = TRUE,
                       returnDeps = TRUE,silent = TRUE, verbose = FALSE)

# Function siena07 produces a so-called sienaFit object, here called ans;
# and it fills in a few things in the sienaEffects object myeff,
```

Print out the results from these models for future reference:

``` r
ansnull
```

    ## Estimates, standard errors and convergence t-ratios
    ## 
    ##                                    Estimate   Standard   Convergence 
    ##                                                 Error      t-ratio   
    ## 
    ## Rate parameters: 
    ##   0.1      Rate parameter period 1  5.7963  ( 0.8743   )             
    ##   0.2      Rate parameter period 2  4.4936  ( 0.6812   )             
    ## 
    ## Other parameters: 
    ##   1.  eval outdegree (density)     -2.3787  ( 0.1093   )   -0.0307   
    ##   2.  eval reciprocity              2.8507  ( 0.1879   )   -0.0679   
    ## 
    ## Overall maximum convergence ratio:    0.0875 
    ## 
    ## 
    ## Total of 2212 iteration steps.

``` r
ans1addl
```

    ## Estimates, standard errors and convergence t-ratios
    ## 
    ##                                    Estimate   Standard   Convergence 
    ##                                                 Error      t-ratio   
    ## 
    ## Rate parameters: 
    ##   0.1      Rate parameter period 1  6.4681  ( 1.1413   )             
    ##   0.2      Rate parameter period 2  5.2626  ( 0.9294   )             
    ## 
    ## Other parameters: 
    ##   1.  eval outdegree (density)     -2.6837  ( 0.1160   )   -0.0985   
    ##   2.  eval reciprocity              2.4523  ( 0.1973   )   -0.1136   
    ##   3.  eval transitive triplets      0.6232  ( 0.0726   )   -0.0619   
    ## 
    ## Overall maximum convergence ratio:    0.1262 
    ## 
    ## 
    ## Total of 1725 iteration steps.

``` r
ans2addl
```

    ## Estimates, standard errors and convergence t-ratios
    ## 
    ##                                    Estimate   Standard   Convergence 
    ##                                                 Error      t-ratio   
    ## 
    ## Rate parameters: 
    ##   0.1      Rate parameter period 1  6.3663  ( 1.0300   )             
    ##   0.2      Rate parameter period 2  5.2580  ( 0.8938   )             
    ## 
    ## Other parameters: 
    ##   1.  eval outdegree (density)     -2.7005  ( 0.1227   )   0.0812    
    ##   2.  eval reciprocity              2.4896  ( 0.2087   )   0.0951    
    ##   3.  eval transitive triplets      0.6748  ( 0.1435   )   0.1299    
    ##   4.  eval 3-cycles                -0.0952  ( 0.2853   )   0.1361    
    ## 
    ## Overall maximum convergence ratio:    0.1485 
    ## 
    ## 
    ## Total of 1747 iteration steps.

Okay, now let's look at the chains. These (supposedly) contain the microsteps from the simulation process.

``` r
class(ansnull$chain)
```

    ## [1] "list"

``` r
length(ansnull$chain)
```

    ## [1] 1000

``` r
# first question: why is the length of ansnull$chain 1000? 
class(ansnull$chain[[1]])
```

    ## [1] "list"

``` r
length(ansnull$chain[[1]])
```

    ## [1] 1

``` r
length(ansnull$chain[[1]][[1]])
```

    ## [1] 2

``` r
length(ansnull$chain[[1]][[1]][[1]])
```

    ## [1] 294

``` r
length(ansnull$chain[[1]][[1]][[2]])
```

    ## [1] 213

``` r
# second question: why do these have different lengths? 
# hypothesis: these are the steps of the CTMC (microsteps), so each sim will have a different number of steps. 
class(ansnull$chain[[1]][[1]][[1]])
```

    ## [1] "list"

``` r
length(ansnull$chain[[1]][[1]][[1]][[1]])
```

    ## [1] 13

``` r
# ok. this is the lowest level. is it the same length for the others?
length(ans1addl$chain[[1]][[1]][[1]][[1]])
```

    ## [1] 13

``` r
length(ans2addl$chain[[1]][[1]][[1]][[1]])
```

    ## [1] 13

``` r
# yes. Crap. so it probably doesn't contain any parameter information...
```

For the ans object, the length of `ans$chain[[1]][[1]][[m]][[n]]` is always 13 for any value of `m` and `n`. What are in these fields? Class and value are printed below.

``` r
for (i in 1:13){
  print(paste(class(ansnull$chain[[1]][[1]][[1]][[1]][[i]]),
              ansnull$chain[[1]][[1]][[1]][[1]][[i]]))
}
```

    ## [1] "character Network"
    ## [1] "integer 0"
    ## [1] "character friendship"
    ## [1] "integer 27"
    ## [1] "integer 27"
    ## [1] "integer 0"
    ## [1] "numeric 0.02"
    ## [1] "numeric -3.91202300542815"
    ## [1] "numeric -1.80370611545247"
    ## [1] "NULL "
    ## [1] "NULL "
    ## [1] "logical FALSE"
    ## [1] "logical TRUE"

My current guesses for the fields:

1.  The class of the dependent variable (?)
2.  No idea
3.  The name of the dependent variable
4.  "from" node id of microstep
5.  "to" node id of microstep
6.  No idea
7.  *a*<sub>*n*</sub> value at parameter update (??)

for 8 - 13 I also have no idea.

When I was printing out the chains in the console, I noticed some attributes printing off. Let's see those in action.

``` r
attributes(ansnull$chain[[1]][[1]][[1]])
```

    ## $mu
    ## [1] 5.88
    ## 
    ## $sigma2
    ## [1] 0.1176
    ## 
    ## $finalReciprocalRate
    ## [1] 0.02
    ## 
    ## $initialStateDifferences
    ## list()
    ## 
    ## $endStateDifferences
    ## list()

Okay, we've got a mean (`mu`), a variance (`sigma2`), a final rate of some kind (`finalReciprocalRate`) and two empy fields, `initialStateDifferences` and `endStateDifferences`. Recall length of `ansnull$chain[[1]][[1]][[1]]` is 294 so these attributes exist for all microsteps in the process. That's informative-ish. Here's another set of attributes.

``` r
attributes(ansnull$chain[[1]][[1]][[2]])
```

    ## $mu
    ## [1] 4.26
    ## 
    ## $sigma2
    ## [1] 0.0852
    ## 
    ## $finalReciprocalRate
    ## [1] 0.02
    ## 
    ## $initialStateDifferences
    ## list()
    ## 
    ## $endStateDifferences
    ## list()

Only the first two attributes changed. Update guesses about what the 13 entries are:

1.  The class of the dependent variable (?)
2.  No idea
3.  The name of the dependent variable
4.  "from" node id of microstep
5.  "to" node id of microstep
6.  No idea
7.  ~~*a*<sub>*n*</sub> value at parameter update (??)~~ `finalReciprocalRate` attribute, whatever that is.
8.  No idea
9.  No idea
10. The `initialStateDifferences` attribute value (?)
11. The `endStateDifferences` attribute value (?)
12. No idea
13. No idea

I'll come back to the attributes later. Next, get all of the microstep data together.

``` r
get_chain_info <- function(ans){
  
  L <- length(ans$chain)
  M <- length(ans$chain[[1]][[1]])
  bigres <- NULL
  res <- data.frame()
  for (l in 1:L){
    res <- data.frame()
    for (m in 1:M){
      sub <- data.frame(plyr::ldply(
          lapply(ansnull$chain[[l]][[1]][[m]], unlist),
        rbind), stringsAsFactors = FALSE)
      sub$rep <- l
      sub$period <- m
      res <- rbind(res, sub)
    }
    bigres[[l]] <- res  
  }
  ret <- plyr::rbind.fill(bigres)
  return(ret)
}

ansnullchains <- get_chain_info(ansnull)
```
