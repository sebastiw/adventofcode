#!/bin/awk -f

BEGIN{
    FS=":"
}

{
    card[NR]++
    split($2, io, "|");
    split(io[1], win, " ");
    num = 0;
    for(w in win) {
        if(index(io[2]" ", " "win[w]" ")) {
            num++;
        }
    }
    for(n = 1; n <= card[NR]; n++) {
        for(i = 1; i <= num; i++) {
            card[NR+i]++;
        }
    }
}

END {
    for(i in card) {
        if(i <= NR) {
            sum=sum+card[i];
        }
    }
    print(sum);
}
