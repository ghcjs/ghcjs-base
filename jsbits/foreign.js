//#OPTIONS: CPP
function h$foreignListProps(o) {
    var r = HS_NIL;
    if(typeof o === 'undefined' || o === null) return null;
    throw "h$foreignListProps";
/*    for(var p in o) {

    } */
}

function h$splitmix_init() {
    return Math.floor(Math.random()*0x100000000);
}