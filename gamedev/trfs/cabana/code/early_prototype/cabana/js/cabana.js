/**
 * cabana.js
 * 
 * Description
 */
(function(game) {

// way to find the game screen
var LOAD_STRING = "If you can read this the game didn't load properly...";
var DOBL_ENT = "dobl-entity-";

game.cabana = {};

// Initialize access to cabana 
game.cabana.init = function cabanaInit() {
	// hacky hacky way to find the game screen!
	var divs = document.getElementsByTagName("div");
	var node = undefined;
	
	// loop through all divs until we find the replacement text
	for(var i=0; i<divs.length; i++) {
		var typ = divs[i].firstChild ? divs[i].firstChild.nodeType : undefined; 
		if(typ == document.TEXT_NODE) {
			node = divs[i];
			break;
		}
	}
	
	// climb out until id starts with DOBL_ENT
	while (!node.id.startsWith(DOBL_ENT)) 
		node = node.parentNode;
	
	// find the screen (it's visibility will be hidden)
	while (node.style.visibility != "hidden")
		node = node.parentNode;
	
	game.appNode = node;
}

// fetch any and all art assets
game.cabana.getAssets = function cabanaGetAssets() {
	
}

	
})(window.game);