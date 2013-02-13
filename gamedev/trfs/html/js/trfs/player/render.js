Player.prototype.draw = function(dt) {
	// redraw if dirty
	if (this.dirty) {
		this.node.setAttribute("state", K.PLY_STATES[this.state]);
		this.node.setAttribute("facing", this.facingRight ? "right" : "left");
		this.node.innerHTML = this.node.innerHTML;
		this.dirty = false;
	}
	
	// update to new position
	var mat = new WebKitCSSMatrix();
	mat = mat.translate(this.x, -this.y);
	this.node.style.webkitTransform = mat;
}
