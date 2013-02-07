function Player() {
	this.id = "player";
	this.type = "sprite";
}

Player.prototype.init = function() {
	// configure all based on settings
	this.maxSpeed = settings.PLY_RUN_SPEED * settings.TILE_SIZE;
	this.aRun = settings.PLY_RUN_ACCEL * settings.TILE_SIZE;
	this.jumpSpeed = settings.PLY_JUMP_SPEED * settings.TILE_SIZE;
	this.wallFric = settings.PLY_WALL_FRIC; 
	this.wallSpeed = settings.PLY_WALL_SPEED * settings.TILE_SIZE;
	this.wallEdge = settings.PLY_WALL_EDGE * settings.TILE_SIZE;
	this.wallLatch = settings.PLY_WALL_LATCH;
	this.dashMult = settings.PLY_DASH_MULT;
	this.dashDur = settings.PLY_DASH_DUR;
	this.ceilColl = settings.PLY_CEIL_COLL;
	this.wallColl = settings.PLY_WALL_COLL;
	
	// grab enough of the level to calc starting positions
	game.level.getRow(8);
	
	// start in the middle of the bottom gap
	var y = settings.PLY_START_ROW;
	this.x = (game.level.gaps[y] + settings.GAP_WIDTH/2) * settings.TILE_SIZE;
	this.y = y * settings.TILE_SIZE;
	
	// start from idle
	this.vx = 0;
	this.vy = 0;
	
	// set direction facing
	this.facingRight = game.level.gaps[0] < game.level.gaps[1];
	
	// make sure to draw the first time
	this.land();
	this.dirty = true;
}

Player.prototype.update = function(dt) {
	this.move(dt);
	this.handleCollision(dt);
	
	this.x = this.dx;
	this.y = this.dy;
	
	this.vx = this.dvx;
	this.vy = this.dvy;
	
	return;
	
	if (this.state == K.PLY_JUMP) {
		if (dy < 0) {
			dy = 0;
			this.land();
		};
		// handle collision with row above
		var h;
		if (this.vy > 0) 
			h = parseInt(dy / settings.TILE_SIZE) + 1;
		// handle collision with floor
		else if (this.vy < 0)
			h = parseInt(dy / settings.TILE_SIZE);
		var nextRowCollision = false;
		
		if (h != undefined) {
			var bounds = game.level.bounds[h];
			nextRowCollision = dx < bounds[0] || dx > bounds[1];
		}
		
		if (nextRowCollision) {
			dy = (h-(this.vy>0)) * settings.TILE_SIZE;
			this.vy /= this.cielCollFac;
		}
	}
	
	// get row bounds
	var bounds = game.level.getRowBounds(dy);
		
	// collide with row (always necessary)
	var rowBoundCollision = false;
	if (dx < bounds[0]) {
		dx = bounds[0];
		rowBoundCollision = true;
	}
	else if (dx > bounds[1]) {
		dx = bounds[1];
		rowBoundCollision = true;
	}
	
	if (rowBoundCollision) {
		this.facingRight = !this.facingRight;
		this.vx = -this.vx;
		this.dirty = true;
	}
	
	// update velocity/states
	
	// kill the dash
	if (this.state == K.PLY_DASH) {
		this.dashTime -= dt;
		if (this.dashTime <= 0) {
			this.state = K.PLY_JUMP;
			this.dirty = true;
			this.vx /= this.dashMult;
		}
	}
	
	// gravity
	if (this.state == K.PLY_JUMP) {
		this.vy -= game.gravity * dt;
	}
}