/**
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

window.K = K;
})();