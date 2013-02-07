var K = {};
(function (K){
/* ENUMS */
	
// general index enums
K.X = 0;
K.Y = 1;

K.LEFT = -1;
K.RIGHT = 1;

K.SPACE = 32;

// log levels
K.LOG_DEBUG = 0;
K.LOG_INFO = 1;
K.LOG_WARN = 2;
K.LOG_ERROR = 3;
K.LOG_FATAL = 4;

// screen types
K.SCR_LANDSCAPE = 0;
K.SCR_PORTRAIT = 1;
	
//game states
K.GM_LOADING = 0;
K.GM_MAINMENU = 1;
K.GM_COUNTDOWN = 2;
K.GM_PLAYING = 3;
K.GM_GAMEOVER = 4;

// player states
K.PLY_STATES = [ "idle", "run", "jump", "wall", "dash", "turn"];
for (var i=0; i<K.PLY_STATES.length; i++) {
	var k = "PLY_" + K.PLY_STATES[i].toUpperCase();
	K[k] = i;
}

// collision types
K.COL_CEIL = 1;
K.COL_WALL = 2;
K.COL_FLOOR = 4;

// Other constants
K.SCR_WIDTH = 320;
K.SCR_HEIGHT = 480;

})(K);
window.K = K;