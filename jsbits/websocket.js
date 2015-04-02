#include <ghcjs/rts.h>

function h$createWebSocket(url, protocols) {
    return new WebSocket(url, protocols);
}

/*
   this must be called before the websocket has connected,
   typically synchronously after creating the socket
 */
function h$openWebSocket(ws, mcb, ccb, c) {
    if(ws.readyState !== 0) {
	throw new Error("h$openWebSocket: unexpected readyState, socket must be CONNECTING");
    }
    ws.lastError = null;
    // ws.hsListeners = { close: ccb, message: mcb, close: ccb };
    ws.onopen = function() {
	if(mcb) {
	    ws.onmessage = mcb;
	    h$retain(mcb);
	}
	if(ccb || mcb) {
	    ws.onclose = function(ce) {
		if(ws.onmessage) {
		    h$release(ws.onmessage);
		    ws.onmessage = null;
		}
		h$release(ccb);
		ccb(ce);
	    }
	}
	ws.onerror = function(err) {
	    ws.lastError = err;
	    if(ws.onmessage) {
		h$release(ws.onmessage);
		ws.onmessage = null;
	    }
	    ws.close();
	}
	c(0, ws);
    }
    ws.onerror = function(err) {
	ws.close();
	c(1, err);
    }
}

function h$closeWebSocket(status, reason, ws) {
    ws.onerror = null;
    if(ws.onmessage) {
	h$release(ws.onmessage);
	ws.onmessage = null;
    }
    ws.close();
}
