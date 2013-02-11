node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
var settings = {}

/***********************\
          GENERAL
\***********************/
settings.FRAMERATE = 25;
settings.MIN_FRAMERATE = 15;
settings.TILE_SIZE = 36;
settings.START_MESSAGE = "Touch to Run";

/***********************\
          DEBUG
\***********************/
settings.DBG_SLOW = 3/4;
settings.MAX_COLL_CHECKS = 5;

/***********************\
          CAMERA
\***********************/
settings.CAM_OFF_X = 1.75;
settings.CAM_OFF_Y = 2;
settings.CAM_PAN_DUR = 0.3;


/***********************\
         INPUT
\***********************/
settings.INP_FLING_ANG = 60;
if (browser.iphone) {
    settings.INP_FLING_DIST = 15;
    settings.INP_TOUCH_DUR = 0.05;
} else {
    settings.INP_FLING_DIST = 10;
    settings.INP_TOUCH_DUR = 0.2;
}

/***********************\
         PLAYER
\***********************/
settings.PLY_RUN_SPEED = 10;    // tiles per second
settings.PLY_RUN_ACC_DIST = 1;  // tiles 

settings.PLY_ROLL_DIST = 4;

settings.PLY_STUN_DIST = 0.8;
settings.PLY_STUN_DUR = 0.1;

settings.PLY_JUMP_ANG = 60;      // tiles per second
settings.PLY_JUMP_DIST = 8;
settings.PLY_CEIL_COLL = 4;

settings.PLY_TURN_DUR = 0.05;

settings.PLY_WALL_DISPLACE = 6;
settings.PLY_WALL_HEIGHT = 36;
settings.PLY_WALL_SNAP = 0.04;	// makes continuouse jumps feel more solid

settings.PLY_WALL_FALL_DIST = 0.075; // how far troll pushs off of wall to fall

// for hopping off edges
settings.PLY_EDGE_DIST_TO = 0.1;
settings.PLY_EDGE_DIST_FROM = 0.3;

// wall jump settings
//settings.PLY_WALL_JUMP_ANG = 45;		// define at most
//settings.PLY_WALL_JUMP_DIST = 5;		// one of these
//settings.PLY_WALL_JUMP_HEIGHT = 0.5;	// settings

// how for to slide in how gong
settings.PLY_WALL_SLIDE_ACC_DUR = 2;
settings.PLY_WALL_SLIDE_SPEED = 8;

settings.PLY_STUMBLE_DIST = .5;

// dash settings
settings.PLY_DASH_DUR = 0.15;	// how long to dash for
settings.PLY_DASH_DIST = 5;		// how far the dash goes
settings.PLY_DASH_RATIO = 0.5;	// accel vs decel

// Sun settings
settings.SUN_BEAM_WIDTH = 1.5;
settings.SUN_BEAM_RATIO = 0.8;
settings.SUN_DELAY = 5;
settings.SUN_SPEED = 1/4;
settings.SUN_ACCEL = 0;

window.settings = settings;
}