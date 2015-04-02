function h$exportValue(fp1a,fp1b,fp2a,fp2b,o) {
    var e = { fp1a: fp1a
            , fp1b: fp1b
            , fp1c: fp1c
            , fp1d: fp1d
            , root: o
	    , _key: -1
            };
    return e;
}

function h$derefExport(fp1a,fp1b,fp2a,fp2b,e) {
    if(!e || typeof e !== 'object') return null;
    if(!e.root) return null;
    if(fp1a !== e.fp1a || fp1b !== e.fp1b ||
       fp2a !== e.fp2a || fp2b !== e.fp2b) return null;
    return e.root;
}

function h$releaseExport(e) {
    h$release(e);
    e.root = null;
}
