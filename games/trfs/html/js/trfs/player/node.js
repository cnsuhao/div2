Player.prototype.build = function() {
	var node = document.createElement("div");
	node.id = this.id;
	this.node = node;
	
	var sprite = document.createElement("div");
	sprite.className = "sprite";
	this.node.appendChild(sprite);
	this.sprite = sprite;
}