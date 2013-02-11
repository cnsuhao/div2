/*
 * core.js
 * 
 * Description
 */

/**
 * Creates a basic GameElement
 * @returns {GameElement}
 */
function GameElement(tag) { 
	this.tag = tag ? tag : "div";
	this.children = [];
	this.class = [];
}

GameElement.prototype.create = function () {
	this.createElement();
	this.init();
	this.ready = true;
	
	// recurse
	this.children
		.filter(function() { return this instanceof GameElement; })
		.each(function() { this.create(); });
}

GameElement.prototype.createElement = function () {
	this.node = document.createElement(this.tag);
	
	// add properties if they exist
	if (this.id != undefined)
		this.node.setAttribute("id", this.id);
	
	this.node.setAttribute("class", this.class.join(" "));
	
	if (this.parent instanceof GameElement)
		this.parent.node.appendChild(this.node);
		
}

GameElement.prototype.init = function() {};

// allow for child elements
GameElement.prototype.add = function(obj) {
	this.children.push(obj);
	obj.parent = this;
}

/**
 * Creates a new game object
 * @returns {Game}
 */
function Game() {
	GameElement.call(this);
	this.id = "application";
	this.running = false;
	
	this.layers = {};
}

Game.prototype = new GameElement();
Game.prototype.init = function() {}

Game.prototype.add = function(obj) {
	GameElement.prototype.add.call(this, obj);
	if (obj instanceof Layer)
		this.layers[obj.id] = obj;
}

onInit("Setup Game", function () {
	window.game = new Game();
});

onCreate("Create Game Nodes", function() {
	window.game.create();
	
	document.body.appendChild(game.node);
});