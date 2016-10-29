#include <ghcjs/rts.h>

function h$makeCallbackMulti(f, extraArgs, fun) {
  var c;
  c = function() {
    var args = extraArgs.slice(0);
    var action = MK_AP1(fun, MK_JSVAL(arguments));
    args.unshift(action);
    return f.apply(this, args);
  };
  c.root = fun;
  // Beware that h$extraRootsN and h$extraRoots come from
  // https://github.com/ghcjs/shims/blob/master/src/mem.js
  c._key = ++h$extraRootsN;
  h$extraRoots.add(c);
  return c;
}
