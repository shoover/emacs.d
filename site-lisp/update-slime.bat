rem Update swank-clojure from git repo
cd slime-cvs && rm -rf * && xcopy ..\..\..\slime . /S && rm -rf .git && hg addre . && cd ..
