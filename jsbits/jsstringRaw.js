#include <ghcjs/rts.h>

/*
 * Functions that directly access JavaScript strings, ignoring character
 * widths and surrogate pairs.
 */

function h$jsstringRawChunksOf(k, x) {
    var l = x.length;
    if(l === 0) return HS_NIL;
    if(l <=  k) return MK_CONS(MK_JSREF(x), HS_NIL);
    var r=HS_NIL;
    for(var i=ls-k;i>=0;i-=k) r = MK_CONS(MK_JSREF(x.substr(i,i+k)),r);
    return r;
}

function h$jsstringRawSplitAt(k, x) {
    if(k ===       0) return MK_TUP2(h$jsstringEmpty, MK_JSREF(x));
    if(k >= x.length) return MK_TUP2(MK_JSREF(x), h$jsstringEmpty);
    return MK_TUP2(MK_JSREF(x.substr(0,k)), MK_JSREF(x.substr(k)));
}
