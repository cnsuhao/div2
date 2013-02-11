node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Game Loop");
trfs.play = function() {
    if (trfs.state == K.GM_READY) {
        this.paused = true;
        this.pause();
    }
}

trfs.pause = function() {
    if (this.paused === true) 
{
        trfs._loop = setInterval(trfs.step, trfs.framerate);
        this.paused = false;
    }
    else if (this.paused === false) {
        clearInterval(trfs._loop);
        trfs._loop = trfs._lastTime = undefined;
        this.paused = true;
    }
}

trfs.step = function () {
    // state update
    
    // handle first step of actually running
    var now = (new Date()).getTime();
    if (trfs._lastTime === undefined) {
        trfs._lastTime = now;
        trfs.events.flush();
        return;
    }
    
    // handle input
    events = trfs.events.flush();
    for (var i=0; i<events.length; i++) {
   	    evt = events[i];
        switch(evt.type) {
        case K.EVT_ACTION:
   	        // if locked by something, requeue
  		        if (trfs.player.state == K.PLY_STUN || 
                trfs.player.state == K.PLY_TURN ||
                (trfs.player.state == K.PLY_WALL && trfs.player.wallTime > 0)) {
                trfs.events.push(evt);
            } else {
                trfs.player.action(evt);
            }
        }
    }
    
    var dt = (now - trfs._lastTime) / 1000;
    dt = Math.min(dt, trfs.minFramerate);
    dt *= settings.DBG_SLOW;
    trfs.update(dt);
    trfs.draw();
    
    trfs._lastTime = now;
}

trfs.update = function(dt) {
//    trfs.bg.update(dt);
    trfs.player.update(dt);
//    trfs.sun.update(dt);
    trfs.cam.update(dt);
}

trfs.draw = function() {
//    trfs.bg.render();
    trfs.player.render();
    trfs.cam.render();
//    trfs.level.render();  // really should be level rendering but for now
}

}