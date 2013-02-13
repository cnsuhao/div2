/*
 * constants.js
 * 
 * Description
 */
(function() {

// our constants dictionary
var K = {};

K.IDLE = 0;
K.RUN = 1;
K.WALL = 2;
K.JUMP = 3;

K.GRAVITY = 10;
K.MAX_RUN_SPEED = 20;
K.WALL_FRICTION = 0.6;
K.MAX_WALL_SPEED = 0.6;
K.BOUND_SPEED = 20;

// general info dictionary, only really used for graphics test
K.INFO = {
	STATES: {
			_desc: "Player States",
		
		idle: {
			_desc: "Idle"
		},
		run: {
			_desc: "Running"
		},
		wall: {
			_desc: "Wall-Sliding"
		},
		jump: {
			_desc: "Jumping"
		}
	},
	
	FACING: {
		_desc: "Player Orientation",
		
		left: {
			_desc: "Left"
		},
		right: {
			_desc: "Right" 
		}
	}
}

window.K = K;
})()