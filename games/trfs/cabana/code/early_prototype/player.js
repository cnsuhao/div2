/*
 * player.js
 * 
 * Description
 */

function Player() {
	Sprite.call(this);
	this.id = "player";
}
Player.prototype = new Sprite();

onInit("Setup Player", function(){
	var ply = new Player();
	game.layers.sprites.add(ply);
	game.player = ply;
});
/**
game.player = {
	facingLeft: false,
		
	init: function playerInit() {
		
	},
	
	update: function playerUpdate(dt) {
		
	},
	
	turn: function() { 
		game.player.facingLeft = !game.player.facingLeft;
	},
	
	updateState: function() { game.player.node.setAttribute("state", K.STATES[game.player.state]); },
	updateFacing: function () {}
		if (game.player.facingLeft) 
			game.player.node.setAttribute("facing", "left");
		else
			game.player.node.setAttribute("facing", "right");
	},
	
	draw: function playerDraw() {
		game.player.node.setAttribute("");
		game.player.node.innerHTML = "";
	}
}
**/