#include <ghcjs/rts.h>

/* 
   convert a JavaScript string to a Data.Text buffer, set second field in
   Addr# to length
 */
function h$textFromString(s) {
  var l = s.length;
  var b = h$newByteArray(l * 2);
  var dv = b.dv;
  for(var i=l-1;i>=0;i--) {
    dv.setUint16(i<<1, s.charCodeAt(i), true);
  }
  RETURN_UBX_TUP2(b, l);
}
