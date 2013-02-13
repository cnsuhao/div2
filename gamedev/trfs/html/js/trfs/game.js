function Game() {
	this.rate = parseInt(1000/settings.FRAMERATE);
	//this.state = K.GM_LOADING;
	
	var step = this.step.bind(this);
	this.tick = function() {
		setTimeout(step, game.dt);
	};
}

Game.prototype.init = function() {
	if (!this.built) {
		this.build();
		this.built = true;
	}
	this.reset();
};

Game.prototype.build = function() {	// make sure we have some kind of root node
	this.root = this.root || document.body;
	
	this.world = document.createElement("div");
	this.world.id = "world";
	
	// create the level
	this.level = new Level();
	this.level.build();
	
	// Create the player node
	this.player = new Player();
	this.player.build();
	
	this.camera = new Camera();
	this.camera.target = this.world;
	this.camera.build();
	
	this.world.appendChild(this.level.node);
	this.world.appendChild(this.player.node);
	
	this.root.appendChild(this.camera.node);
	
	if (settings.ORIENTATION == K.SCR_PORTRAIT) {
		this.scrWidth = K.SCR_WIDTH;
		this.scrHeight = K.SCR_HEIGHT;
	} else {
		this.scrWidth = K.SCR_HEIGHT;
		this.scrHeight = K.SCR_WIDTH;
	}
};

Game.prototype.reset = function() {
	this.slowFactor = settings.SLOW_FACTOR;
	this.slow = false;
	
	this.tileSize = settings.TILE_SIZE;
	this.gravity = settings.GRAVITY * settings.TILE_SIZE;
	
	this.level.init();
	this.player.init();
	this.camera.init();
};

Game.prototype.run = function() {
	this.step();
};

Game.prototype.step = function() {	
	var dt = this.rate / 1000;
	
	// update section
	this.player.update(dt);
	
	// game height is at this.y
	this.y = this.player.y;
	
	// redraw section
	this.level.draw();
	
	// player is moved last since it triggers the next update
	this.player.draw();
	this.camera.draw(dt);
	
	this.dt = this.rate;
	this.tick();
};

Game.prototype.pause = function() {};