var settings = {};
(function (settings) {

settings.DEBUG = true;
settings.LOG_LEVEL = K.LOG_DEBUG;

settings.FRAMERATE = 30;

settings.ORIENTATION = K.SCR_PORTRAIT;

settings.TILE_SIZE = 36;	// in pixels (considered max if camera zooms)

// level dimensions (in tiles)
settings.BORDER_SIZE = 2;
settings.LEVEL_WIDTH = 18;
settings.ROW_PADDING = 8;	// number of regions to have queued on either side

settings.GAP_WIDTH = 3;
settings.GAP_HEIGHT = 2;

settings.CAM_PAN_DUR = .3;

settings.PLY_START_ROW = 0;
settings.PLY_RUN_SPEED = 8;  // tiles per second
settings.PLY_RUN_ACCEL = 10;  // tiles per second^2
settings.PLY_JUMP_SPEED = 8; // tiles per second
settings.PLY_WALL_FRIC = 0.50;	 // percent of gravitational force
settings.PLY_WALL_SPEED = 4;	// max speed of wall slide
settings.PLY_WALL_EDGE = 0.25;	// percent of a tile needed to wall slide
settings.PLY_WALL_LATCH = 6; // number of pixels required to latch onto wall
settings.PLY_DASH_MULT = 3;
settings.PLY_DASH_DUR = 0.40;
settings.PLY_CEIL_COLL = 2.5;
settings.PLY_WALL_COLL = 1.5;

settings.GRAVITY = 14;

settings.SLOW_FACTOR = 4;

})(settings);