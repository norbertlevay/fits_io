
for var in "$@"
do
    # echo "$var"
    addr2line -e ./fits -a $var
done

