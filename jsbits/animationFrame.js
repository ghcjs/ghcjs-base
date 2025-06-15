//#OPTIONS: CPP
function h$animationFrameCancel(h) {
    if(h.handle) cancelAnimationFrame(h.handle);
    if(h.callback) {
        h$release(h.callback)
        h.callback = null;
    }
}

function h$animationFrameRequest(h) {
    h.handle = requestAnimationFrame(function(ts) {
        var cb = h.callback;
        if(cb) {
	        h$release(cb);
	        h.callback = null;
	        cb(ts);
        }
    });
}
