#!/bin/bash
if [ -e /usr/bin/python2 ] ; then
    PYTHON=/usr/bin/python2
else
    if [ -e /usr/bin/python ] ; then
        PYTHON=/usr/bin/python
    else
        echo Where is your Python interpreter...
        exit 1
    fi
fi
declare -A langs
langs[CPlusPlus]=Unblock-solve
langs[CPlusPlus11]=Unblock-solve-c++11
langs[OCaml]=Unblock
for lang in "${!langs[@]}" ; do
    binary=${langs[$lang]}
    echo "Benchmarking $lang ..."
    for i in IMG_03* ; do
            convert $i data.rgb
            for i in {1..10} ; do
                    { time sh -c "yes | ./$binary >/dev/null" ; } 2>&1 | grep real | cut -c8-12
            done | $PYTHON ./stats.py | grep Overall
    done
done
