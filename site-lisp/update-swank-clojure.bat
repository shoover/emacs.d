rem Update swank-clojure from git repo
cd swank-clojure && rm -rf * && xcopy ..\..\..\swank-clojure . /S && rm -rf .git && hg addre . && cd ..
