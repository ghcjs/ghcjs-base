//#OPTIONS: CPP

function h$fromAddr(ptr, off) {
  RETURN_UBX_TUP2(ptr, off);
}

function h$toAddr(ptr) {
  RETURN_UBX_TUP2(ptr,0);
}
