// basic newtonian physics
Player.prototype.move = function(dt) {
	this.dx = this.x + this.vx * dt;
	this.dy = this.y + this.vy * dt;
	
	this.dvx = this.vx + this.ax * dt;
	this.dvy = this.vy + this.ay * dt;
	
	// clamp to top speed if running or sliding
	if (this.state == K.PLY_RUN && Math.abs(this.dvx) > this.maxSpeed) {
		this.dvx = (2 * this.facingRight - 1) * this.maxSpeed;
	}
	
	
	// fall once max speed is reached
	if (this.state == K.PLY_WALL && this.dvy < -this.wallSpeed) {
		this.jump(true);
	}
}

Player.prototype.handleCollision = function() {	
	this.coll = [];
	
	this.collideObjs();
	this.collidePlatforms();
	this.collideLevel();
	// handle level collision and wall collision
}

// no game objects yet, does nothing most likely a simple radius/AABB calc
Player.prototype.collideObjs = function() { }

// 
Player.prototype.collidePlatforms = function() {
	
}

Player.prototype.collideLevel = function(dt) {
	// level bounds
	if (this.dy < 0) {
		this.dy = 0;
		this.land();
	}
	
	// test to see if there is any collision between player and row
	this.floorCollision();
	
	var row = game.level.getRowBounds(this.dy);
	this.wallCollision(row);
	
	// fall off the fall if no longer supported
	if (this.state == K.PLY_WALL) {
		row = game.level.getRowBounds(this.dy - this.wallEdge);
		if (row[0] < this.dx && row[1] > this.dx) 
			this.jump(true);
	}
	
	// start falling if there is no floor underneath
	if (this.state == K.PLY_RUN && this.dy > game.tileSize) {
		var nextRow = game.level.getRowBounds(this.y - game.tileSize);
		if (this.dx > nextRow[0] && this.dx < nextRow[1]) {
			this.jump(true);
		}
	}
}

// trouble is this is the exact same logic for wall collision
Player.prototype.floorCollision = function() {
	var row = game.level.getRowBounds(this.y);
	var dRow = game.level.getRowBounds(this.dy);
	
	// if rows are equal, no need to test for floor
	if (row[0] == dRow[0] && row[1] == dRow[1])
		return;
		
	// check to see if we need to land
	if ((dRow[0] > row[0] && this.dx < dRow[0]) ||
		(dRow[1] < row[1] && this.dx > dRow[1])) {
		 
		if (this.vy < 0) {
			this.dy = game.tileSize * parseInt(this.dy / game.tileSize + 1);
			this.land();
		}
		else {
			this.dvy /= this.ceilColl;
			this.dy = game.tileSize * parseInt(this.dy / game.tileSize);
		}
	}
}

Player.prototype.wallCollision = function(bounds) {
	// test for collision
	var coll = -1;
	if (this.dx < bounds[0])
		coll = 0;
	if (this.dx > bounds[1])
		coll = 1;
	
	// return if there are no collisions
	if (coll == -1) return
	
	this.dx = bounds[coll];
	
	// otherwise handle the wall collision accordingly
	switch (this.state) {
	case K.PLY_JUMP:
		// wall slide if jump direction matches coll direction
		if (!this.facingRight ^ coll) {
			this.turn();
			this.wallSlide();
		}
		break;
	
	case K.PLY_RUN:
		// thanks to 0 being false, an xor will handle whether or not this is
		// the initial collision or just a clamp
		if (!this.facingRight ^ coll) {
			this.ax = -this.ax;
			this.turn();
		}
		
		// handle wall inelastic collision
		var d = 2 * this.facingRight - 1;
		if (d*this.dvx < 0)
			this.dvx /= this.wallColl;
		
		break;
		
	case K.PLY_DASH:
		this.ax = -this.ax;
		break;
	}
}