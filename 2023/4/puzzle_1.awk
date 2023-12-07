#!/bin/awk -f

BEGIN{
    FS=":"
}

{
    split($2, io, "|");
    split(io[1], win, " ");
    num = 0;
    for(w in win) {
        if(index(io[2]" ", " "win[w]" ")) {
            num=num+1;
        }
    }
    if(num) {
        sum=sum+2^(num-1);
    }
}

END {
    print(sum);
}
