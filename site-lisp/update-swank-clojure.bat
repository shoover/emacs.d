rem Update swank-clojure from git repo on shoover-mobile.
cd swank-clojure && rm -rf * && xcopy c:\users\shawn\swank-clojure . /S && rm -rf .git && hg addre . && cd ..
