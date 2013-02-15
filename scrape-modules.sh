#!/bin/sh

stdlib=`ocamlfind query unix`
stdlib_packs=""

# Package names without '.'
for l in `ocamlfind list | sed -e 's/ .*//' | grep -v '\.'`
do
  dir=`ocamlfind query $l`
  if [ "$stdlib" = "$dir" ]; then 
    stdlib_packs="$stdlib_packs $l"
  else # skip if dir is as same as stdlib
    echo "- $l"
    echo "PATH:: $dir"
    ls $dir/*.cmi 2> /dev/null | sed -e 's/.*\///g' -e 's/\.cmi//' -e 's/^./\u&/'
  fi 
done

echo "- stdlib"
echo "# Thread realted modules are found in the next PATH spec. The others are found in the default one."
echo "PATH:: $stdlib/threads"
ls $stdlib/*.cmi | sed -e 's/.*\///g' -e 's/\.cmi//' -e 's/^./\u&/'
for p in $stdlib_packs
do
  if [ -d "$stdlib/$p" ]; then
    ls $stdlib/$p/*.cmi | sed -e 's/.*\///g' -e 's/\.cmi//' -e 's/^./\u&/'
  fi
done
