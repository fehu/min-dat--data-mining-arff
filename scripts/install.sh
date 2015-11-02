#!/bin/bash

# The script will download and perform an installation 
#  of the following projects in the current directory:
# 
#  DataAssociation, DecisionTrees, NaiveBayes, WekaData and DataMiningArff.
#
# --sandbox flag will perform installation in a cabal sandbox.
#       Note: to reuse the script globaly from the same directory, 
#             you would need to run 'cabal sandbox delete'.
# 
# 
# Requires:
#       * git           https://git-scm.com/
#       * Haskell:      https://www.haskell.org/platform/
#               - GHC   https://www.haskell.org/ghc/
#               - cabal https://www.haskell.org/cabal/
#

SAND=( "$1" -eq "--sandbox" )

Github="https://github.com/fehu/min-dat--" # .git

N=5

PROJECTS=("WekaData" "DataAssociation" "DecisionTrees" "NaiveBayes" "DataMiningArff")

GIT_LINKS=("weka-data" "a-priori" "decision-trees" "naive-bayes" "data-mining-arff")

ROOT_DIR=`pwd`


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # #                           Functions                           # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Install the given project.
inst() {
  cabal install $1 --enable-tests --enable-documentation
  }

# Clone or pull a project with "git"; cd into the directory.
fetch() {
  DIR=$1
  GIT=$2
  
  if [ -d $DIR ];
  then
      cd $DIR
      git pull origin master
  else 
      git clone $GIT $DIR
      cd $DIR
  fi
  }



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # #                      Clone or pull with Git                   # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
for i in $(seq 1 $N)
do 
  cd $ROOT_DIR
  fetch ${PROJECTS[i-1]} "$Github${GIT_LINKS[i-1]}.git"
done


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # #                    Install projects in sandbox                # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cd $ROOT_DIR

# init sandbox if --sandbox arg provided
if [ $SAND ]
then cabal sandbox init
fi

# There are two versions of 'WekaData' needed
inst "WekaData/"
cd WekaData
git checkout -q 0.1.2.5
cd $ROOT_DIR
inst "WekaData/"
cd WekaData
git checkout -q master

cd $ROOT_DIR

for i in $(seq 2 $N) 
do
  inst "${PROJECTS[i-1]}/"
done


