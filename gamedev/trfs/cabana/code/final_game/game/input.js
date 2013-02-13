node.onCreateEntity = function() { node.observe(node,"load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Input Manager");
trfs.input = {};
trfs.events = [];

trfs.input.touchStart = function(evt) {
    evt.preventDefault();
    if (evt.touches.length>1) 
        return;
    
    trfs.input.scrX0 = evt.touches[0].screenX;
    trfs.input.scrY0 = evt.touches[0].screenY;
    trfs.input.scrX1 = evt.touches[0].screenX;
    trfs.input.scrY1 = evt.touches[0].screenY;
    trfs.input.touching = true;
}

trfs.input.touchMove = function(evt) {
    evt.preventDefault();
    if (evt.touches.length>1) 
        return;
        
    trfs.input.scrX1 = evt.touches[0].screenX;
    trfs.input.scrY1 = evt.touches[0].screenY;
}

trfs.input.touchEnd = function(evt) {
    evt.preventDefault();
    if (evt.touches.length==0 && trfs.input.touching) 
        trfs.input.fireAction();
}

trfs.input.mouseDown = function(evt) {
    evt.preventDefault();

    trfs.input.scrX0 = evt.screenX;
    trfs.input.scrY0 = evt.screenY;
    trfs.input.scrX1 = evt.screenX;
    trfs.input.scrY1 = evt.screenY;
    trfs.input.touching = true;

    setTimeout(trfs.input.timeout, trfs.input.touchDur);
}

trfs.input.mouseUp = function(evt) {
    if (trfs.input.touching) {
        evt.preventDefault();
        trfs.input.scrX1 = evt.screenX;
        trfs.input.scrY1 = evt.screenY;
        trfs.input.fireAction();
    }
}

trfs.input.mouseMove = function(evt) {
    if (trfs.input.touching) {
        evt.preventDefault();
        trfs.input.scrX1 = evt.screenX;
        trfs.input.scrY1 = evt.screenY;
    }
}

trfs.input.timeout = function() {
    if (trfs.input.touching) {
        trfs.input.fireAction();
    }
}

trfs.input.fireAction = function() {
    var dx = trfs.input.scrX1 - trfs.input.scrX0;
    var dy = trfs.input.scrY1 - trfs.input.scrY0;

    var dist = Math.sqrt((dx*dx) + (dy*dy));
    
    var normX = dx/dist || 0;
    var normY = dy/dist || 0;
    
    var fling = 0;
    if (dist > this.flingDist) {
        fling |= K.FLING;
        fling |= Math.abs(normX) < this.flingAng ? K.FLING_DOWN : K.FLING_SIDE;
    }    
    trfs.events.push({type:K.EVT_ACTION, x:normX, y:normY, fling: fling});

    trfs.input.touching = false;
}

trfs.input.setup = function() {
    trfs.input.flingDist = settings.INP_FLING_DIST;
    trfs.input.flingAng = Math.cos(Math.PI / 180 * settings.INP_FLING_ANG)
    trfs.input.touchDur = 1000 * settings.INP_TOUCH_DUR;

    var overlay = document.getElementById(node.properties.overlay);

    var app = document.getElementById("application");
    if (browser.iphone) {
        overlay.addEventListener("touchstart", trfs.input.touchStart, false);
        overlay.addEventListener("touchmove", trfs.input.touchMove);
        overlay.addEventListener("touchend", trfs.input.touchEnd);
    }
    else {
        overlay.addEventListener("mousedown", trfs.input.mouseDown, true);
        overlay.addEventListener("mousemove", trfs.input.mouseMove, true);
        overlay.addEventListener("mouseup", trfs.input.mouseUp, true);
    }
}

}