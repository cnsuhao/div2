var game;

function setup() {
	// create game
	game = new Game();
	
	// setup logging
	if (settings.DEBUG) {
		game.log = function() { console.log.apply(console, arguments); };
	}
	
	// build our root nodes
	var app = document.createElement("div");
	app.id = "application";
	if (settings.ORIENTATION == K.SCR_LANDSCAPE)
		app.className = "landscape";
	else
		app.className = "portrait";
	
	var node = document.createElement("div");
	node.id = "gameRoot";
	game.root = node;
	
	app.appendChild(node);
	document.body.appendChild(app);
	
	// initialize the game
	game.init();
	
	var cameraPan = false;
	var prevEvt;
	document.body.addEventListener("mousedown", function(evt) {
		if (!cameraPan)
			game.player.action();
		else {
			prevEvt = evt;
			game.camera.lock();
		}
	});
	document.body.addEventListener("mouseup", function(evt) {
		if (cameraPan) {
			game.camera.unlock();
		}
	});
	
	document.body.addEventListener("mousemove", function(evt) {
		if (cameraPan && prevEvt) {
			var x = evt.pageX - prevEvt.pageX;
			var y = evt.pageY - prevEvt.pageY;
			game.camera.offset(x,y);
			prevEvt = evt;
		}
	});
	
	document.body.addEventListener("keydown", function(evt) {
		if (evt.keyCode == K.SPACE) {
			cameraPan = true;
		}
	}, false);
	document.body.addEventListener("keyup", function(evt) {
		if (evt.keyCode == K.SPACE) {
			cameraPan = false;
			game.camera.unlock();
			prevEvt = undefined;
		}
	}, false);
	
	// start game
	game.run();
}

document.addEventListener("DOMContentLoaded", function() {
	// import all the files
	setup();
});

