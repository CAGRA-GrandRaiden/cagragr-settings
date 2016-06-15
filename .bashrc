#ROOT
export ROOTSYS=/usr/local/hep/root/v5.32.00
export ROOTSYS=/usr/local/root/v5.34.32
export PATH=${ROOTSYS}/bin:${PATH}
export LD_LIBRARY_PATH=${ROOTSYS}/lib/root:${LD_LIBRARY_PATH}
export DYLD_LIBRARY_PATH=${ROOTSYS}/lib:${DYLD_LIBRARY_PATH}

#anaroot
source $HOME/ana/tetsuya/anaroot/setup.sh
export TARTSYS=$HOME/ana/tetsuya/anaroot
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$TARTSYS/lib

alias cdexp1='cd /np1a/v05/cagragr/raw/2015MarchEN/CAGRA1/user/EXP1'

source ~/apps/dot_cagragrc
