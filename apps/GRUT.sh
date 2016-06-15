source /usr/local/bin/gcc493R.sh
export PATH=$HOME/.local/bin:$PATH
export LD_LIBRARY_PATH=$HOME/.local/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$HOME/.local/lib/root:$LD_LIBRARY_PATH
export ROOTSYS=$HOME/.local
export PYTHONPATH=$PYTHONPATH:$HOME/.local/lib/root

cd codes

# notes, when compiling ROOT locally, you must do so with the following configure flags:
# ./configure --with-python-incdir=/home/cagragr/.local/include/python2.7 --with-python-libdir=/home/cagragr/.local/lib/ --prefix=$HOME/.local --etcdir=$HOME/.local/etc --enable-rpath --enable-soversion --enable-explicitlink
