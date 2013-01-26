for i in IMG_03* ; do convert $i data.rgb  ; for i in {1..10} ; do { time sh -c "yes | ./Unblock-solve >/dev/null" ; } 2>&1 | grep real | cut -c8-12 ;  done | stats.py | grep Overal ; done
