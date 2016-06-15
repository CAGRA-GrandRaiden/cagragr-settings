# ROOT #
# TOO OLD!!!!!!
#set path = ($path /user/local/cern/2002/bin)
#setenv ROOTSYS /usr/local/root64/v5.27.04
#setenv PATH ${PATH}:${ROOTSYS}/bin
#setenv LD_LIBRARY_PATH ${ROOTSYS}/lib
#setenv ROOTSYS /usr/local/root64/v5.32.01
#setenv ROOTSYS /usr/local/root/v5.27.04

setenv ROOTSYS /usr/local/root/v5.34.32.Linux-slc6-x86_64-gcc4.4/
#setenv PATH ${ROOTSYS}/bin:/home/1/cagraen/radware/rw01/bin:${PATH}
setenv PATH ${PATH}:${ROOTSYS}/bin
setenv LD_LIBRARY_PATH ${ROOTSYS}/lib
alias root "root -l"

#CERN Lib
setenv CERN '/usr/local/cern'
setenv CERN_LEVEL '2006b'
#setenv CERN_LEVEL '2005'

# anaroot
setenv TARTSYS /home1/cagragr/ana/tetsuya/anaroot
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${TARTSYS}/lib

# tamii daq 
set path = (~/udaq/bin  ~/daq/bin ~/ana/2015iwamoto/analyzer/bin ~/ana/2015iwamoto/analyzer/hm ~/bin $path $CERN/$CERN_LEVEL/bin)

# alias
alias rm "rm -iv"
alias mv "mv -iv"
alias cp "cp -iv"
alias ls "ls -hF --color=auto"
alias la "ls -a"
alias ll "ls -lh"
alias lh "ls -ltrh"
alias pur "rm -rf *~;ls"
alias sl "echo SL is a train."
alias ssh "ssh -X"

alias emacsul /home/8/sullivan/apps/emacs-24.5/bin/bin/emacs -q -l ~/.emacs_sullivan

#Begin C. Sullivan CAGRA+GR analyzer settings
setenv PATH ${PATH}:/home1/cagragr/ana/sullivan/lib
setenv PATH ${PATH}:/home1/cagragr/ana/sullivan/bin
setenv PATH ${PATH}:/home1/cagragr/ana/sullivan/shell
alias root='root -l'
#End

alias gitstatus "git status -uno"
alias grutsh "bash --rcfile $HOME/apps/dot_cagragrc"


if($?prompt) then
grutsh
endif
