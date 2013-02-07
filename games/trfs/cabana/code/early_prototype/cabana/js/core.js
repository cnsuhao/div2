/**
 * core.js
 * 
 * Description
 */
(function () {

var game = {};

// Called when the application is first initialized, does not
//   need to be reinitialized between rounds
game.appInit = function appInit() {
	// build the inards of our game
	game.log("build html");
	game.build();
	game.camera.build();
	
	// add all nodes to our root
	game.log("place html");
	game.appNode.innerHTML = "";
	
	game.node.appendChild(game.camera.node);
	game.appNode.appendChild(game.node);
}

game.build = function gameBuild() {
	var node = document.createElement("div");
	node.id = "game";
	
	game.node = node;
}

//this gets called once every time the game is started
game.run = function gameRun() {
	game.init();
	game.load();
}

// initialize the game structure, acts as a reset
game.init = function gameInit() {
	
};

// load the game before start
// this includes any and all art assets etc
game.load = function gameLoad() {
	
}

/******************
 * Logging
 ******************/
if(log==undefined) {
	var log;
	if (dobl != undefined)
		log = function() { dobl.core.log.apply(dobl.core, arguments); }
	else if (console != undefined)
		log = function() { console.log.apply(console, arguments); }
	else
		log = function(m) {};
	game.log = log;
}

window.game = game;
})();