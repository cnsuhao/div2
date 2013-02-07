Player.prototype.action = function() {
	if (this.state == K.PLY_RUN) {
		this.jump();
	}
	else if (this.state == K.PLY_JUMP) {
		var row = game.level.getRowBounds(this.y);
		// check to see if we are within the walls range
		if (this.x <= row[0] + this.wallLatch) {
			this.facingRight = true;
			this.x = row[0];
			this.wallSlide();
		}
		else if(this.x >= row[1] - this.wallLatch) {
			this.facingRight = false;
			this.x = row[1];
			this.wallSlide();
		}
	}
	
	else if (this.state == K.PLY_WALL) {
		this.wallJump();
	}
	else if (this.state == K.PLY_DASH) {
		var row = game.level.getRowBounds(this.y);
	}
}

Player.prototype.jump = function(falling) {
	if (!falling)
		this.vy = this.jumpSpeed;
	
	this.ax = 0;
	this.ay = -game.gravity;
	
	this.state = K.PLY_JUMP;
	this.dirty = true;
}

Player.prototype.dash = function() {
	this.vy = 0;
	this.vx *= this.dashMult;

	this.state = K.PLY_DASH;
	this.canDash = false;

	this.dirty = true;
	return;
}

Player.prototype.wallSlide = function() {
	this.canDash = true;
	
	this.dvx = 0;
	this.dvy = 0;
	this.vy = 0;
	this.vx = 0;
	this.ax = 0;
	this.ay = -game.gravity * this.wallFric;
	
	this.state = K.PLY_WALL;
	this.dirty = true;
}

Player.prototype.wallJump = function() {
	this.vx = this.maxSpeed * (2*this.facingRight - 1);
	this.jump();
}

Player.prototype.turn = function() {
	this.facingRight = !this.facingRight;
	this.dirty = true;
}

Player.prototype.slow = function() {
	var style = getComputedStyle(this.sprite);
	var dur = parseFloat(style.webkitAnimationDuration);
	dur *= game.slowFactor;
	this.sprite.style.webkitAnimationDuration = dur+"s";
}

Player.prototype.land = function() {
	this.state = K.PLY_RUN;
	this.canDash = true;
	
	this.ax = this.aRun;
	this.ay = 0;
	this.dvy = 0;
	this.vy = 0;
	
	if (!this.facingRight)
		this.ax = -this.ax;
	
	this.dirty = true;
}