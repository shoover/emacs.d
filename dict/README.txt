Dictionary for hunspell. The dictionary comes from the latest
http://extensions.libreoffice.org/extension-center release. See
https://stackoverflow.com/a/9436234.

```
# Check dictionaries, probably none to find
hunspell -D
cd %HOME%/Downloads
wget -o dict-en.oxt https://extensions.libreoffice.org/extensions/english-dictionaries/2019-05.01b/@@download/file/dict-en-20190501b.oxt
unzip dict-en.oxt -d dict-en
copy dict-en/en_US.* emacs/dict/
```
