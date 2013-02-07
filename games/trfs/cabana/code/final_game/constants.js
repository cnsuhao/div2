node.onCreateEntity = function() { node.observe(node,"load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
var K = {};

K.LEFT = -1;
K.RIGHT = 1;

// keys
K.SPACE = 32;

K.APPID = "C5CCC93419B44B7283EB6B388DE7F704";

// collision flags
var coll = [ "north", "east", "south", "west" ];
for (var i=0; i<coll.length; i++) {
    var k = coll[i].toUpperCase();
    K[k] = 1 << i;
}

//game states
K.GM_STATES = [ "loading", "menu", "ready", "playing", "paused", "gameover" ];
for (var i=0; i<K.GM_STATES.length; i++) {
    var k = "GM_" + K.GM_STATES[i].toUpperCase();
    K[k] = i;
}


// player states
K.PLY_STATES = [ "idle", "run", "roll", "turn", "stun", "jump", "wall", "dash", "dead" ];
for (var i=0; i<K.PLY_STATES.length; i++) {
    var k = "PLY_" + K.PLY_STATES[i].toUpperCase();
    K[k] = i;
}

K.EVT_TYPES = [ "action" ];
for (var i=0; i<K.EVT_TYPES.length; i++) {
    var k = "EVT_" + K.EVT_TYPES[i].toUpperCase();
    K[k] = i;
}

// Other constants
K.SCR_WIDTH = 320;
K.SCR_HEIGHT = 480;

K.FLING = 1 << 0;
K.FLING_SIDE = 1 << 1;
K.FLING_DOWN = 1 << 2;

window.K = K;
}