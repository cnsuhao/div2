function easeOut(t, a, b, d) {
	return -b * (t/=d)*(t-2) + a;
}

function linear(t, a, b, d) {
	return b * (t/d) + a;
}

function Camera() {	
	this.id = "camera";
}

Camera.prototype.init = function() {
	this.centerW = game.scrWidth/2 - settings.TILE_SIZE/2;
	this.centerH = game.scrHeight/2 + settings.TILE_SIZE/2;
	
	this.panDur = settings.CAM_PAN_DUR;
}

Camera.prototype.build = function() {
	this.node = this.target;
}

Camera.prototype.draw = function(dt) {
	// center if camera isn't locked
	if (!this.locked) {
		this.center();
	}
	
	// pan if a value is set
	if (this.pan > 0) {
		var px = linear(this.panDur-this.pan, this.panX, -this.panX, this.panDur);
		var py = linear(this.panDur-this.pan, this.panY, -this.panY, this.panDur);
		this.offset(px,py);
		this.pan -= dt;
	}
	
	var mat = new WebKitCSSMatrix();
	mat = mat.translate(this.x, this.y);
	this.target.style.webkitTransform = mat;
}

Camera.prototype.lock = function() {
	this.locked = true;
}

Camera.prototype.unlock = function() {
	if (!this.locked) return;
	
	var x = this.x;
	var y = this.y;
	
	this.center();
	
	this.panX = x - this.x;
	this.panY = y - this.y;
	
	this.pan = this.panDur;
	this.locked = false;
}

Camera.prototype.center = function() {
	//update world position
	var x = this.centerW - game.player.x;
	var y = this.centerH + game.player.y;
	
	this.x = x;
	this.y = y;
}

Camera.prototype.clamp = function() {
	
}

Camera.prototype.offset = function(dx, dy) {
	this.x += dx;
	this.y += dy;
}