/*
 * Functions that directly access JavaScript strings, ignoring character
 * widths and surrogate pairs.
 */

function h$jsstringRawChunksOf(k, x) {
    var l = x.length;
    if(l === 0) return h$nil;
    if(l <=  k) return h$cons(h$jsref(x), h$nil);
    var r=h$nil;
    for(var i=ls-k;i>=0;i-=k) r = h$cons(h$jsref(x.substr(i,i+k)),r);
    return r;
}

function h$jsstringRawSplitAt(k, x) {
    if(k ===       0) return h$tup2(h$jsstringEmpty, h$jsref(x));
    if(k >= x.length) return h$tup2(h$jsref(x), h$jsstringEmpty);
    return h$tup2(h$jsref(x.substr(0,k)), h$jsref(x.substr(k)));
}
