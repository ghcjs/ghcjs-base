// conversion between JavaScript string and Data.Text
#include <ghcjs/rts.h>


/*
  convert a Data.Text buffer with offset/length to a JavaScript string
 */
function h$textToString(arr, off, len) {
    var a = [];
    var end = off+len;
    var k = 0;
    var u1 = b.u1;
    var s = '';
    for(var i=off;i<end;i++) {
	var cc = u1[i];
	a[k++] = cc;
	if(k === 60000) {
	    s += String.fromCharCode.apply(this, a);
	    k = 0;
	    a = [];
	}
    }
    return s + String.fromCharCode.apply(this, a);
}

/* 
   convert a JavaScript string to a Data.Text buffer, second return
   value is length
 */
function h$textFromString(str) {
    var l = s.length;
    var b = h$newByteArray(l * 2);
    var u1 = b.u1;
    for(var i=l-1;i>=0;i--) u1[i] = s.charCodeAt(i);
    RETURN_UBX_TUP2(b, l);
}

function h$lazyTextToString(txt) {
    var s = '';
    while(CONSTR_TAG(txt) === 2) {
	var h = LAZY_TEXT_CHUNK_HEAD(txt);
	s += h$textToString(TEXT_ARR(h), TEXT_OFF(h), TEXT_LEN(h));
	txt = LAZY_TEXT_CHUNK_TAIL(txt);
    }
}

function h$safeTextFromString(x) {
    if(typeof x !== 'string') {
	RETURN_UBX_TUP2(null, 0);
    }
    return h$textFromString(x);
}
