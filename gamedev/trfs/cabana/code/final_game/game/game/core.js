node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Game Core");
trfs.state = K.GM_LOADING;

trfs.reset = function (seed) {
    trfs.tileSize = settings.TILE_SIZE;
    trfs.framerate = parseInt(1000 / settings.FRAMERATE)
    trfs.minFramerate = parseInt(1000 / settings.MIN_FRAMERATE);
    
    trfs.seed = seed || (new Date()).getTime()+'';

    trfs.player.init();
    trfs.level.init();
    trfs.cam.init();
//    trfs.sun.init();

    trfs.events = [];
    trfs.ready();
}

trfs.ready = function() {
//    trfs._signal.signal();
    trfs.state = K.GM_READY;
}

}