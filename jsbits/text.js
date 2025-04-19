//#OPTIONS: CPP
// conversion between JavaScript string and Data.Text


/*
  convert a Data.Text buffer with offset/length to a JavaScript string
 */
function h$textToString(arr, off, len) {
    return new TextDecoder().decode(arr.u8.subarray(off, off+len));
}

/*
   convert a JavaScript string to a Data.Text buffer, second return
   value is length
 */
function h$textFromString(s) {
    var encoder = new TextEncoder("utf-8");
    var u8 = encoder.encode(s);
    var b = h$wrapBuffer(u8.buffer, true, u8.byteOffset, u8.byteLength);
    RETURN_UBX_TUP2(b, u8.byteLength);
}

function h$lazyTextToString(txt) {
    var s = '';
    while(LAZY_TEXT_IS_CHUNK(txt)) {
        var head = LAZY_TEXT_CHUNK_HEAD(txt);
        s  += h$textToString(DATA_TEXT_ARRAY(head), DATA_TEXT_OFFSET(head), DATA_TEXT_LENGTH(head));
        txt = LAZY_TEXT_CHUNK_TAIL(txt);
    }
    return s;
}

function h$safeTextFromString(x) {
    if(typeof x !== 'string') {
	RETURN_UBX_TUP2(null, 0);
    }
    return h$textFromString(x);
}
