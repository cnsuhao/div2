/*
 * layers.js
 * 
 * The most basic of all layer definitions
 */

/**
 * Creates a new layer
 * @returns {GameElement}
 */
function Layer(id) {
	GameElement.call(this);
	
	this.id = id;
	this.class.push("layer");
}
Layer.prototype = new GameElement();

// upon init, add to game
Layer.prototype.init = function() {
	this.node.id = this.id + "Layer";
};