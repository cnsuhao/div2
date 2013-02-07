/*
 * sprites.js
 * 
 * Description
 */

/**
 * Sprite
 */
function Sprite(id) {
	GameElement.call(this);
	
	this.id = id;
	this.class.push("sprite");
}
Sprite.prototype = new GameElement();

/**
 * SpriteLayer
 */
function SpriteLayer(id) {
	Layer.call(this,id);
	this.class.push("sprites");
}
SpriteLayer.prototype = new Layer();

// create sprite layer
onInit("Setup Sprite Layer", function() {
	game.add(new SpriteLayer("sprites"));
});