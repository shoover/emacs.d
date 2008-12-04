rem Update clojure-mode from git repo
cd clojure && rm -rf * && xcopy ..\..\..\clojure-mode . /S && rm -rf .git && hg addre . && cd ..
