/*
 * stylesheets.js
 * 
 * Description
 */

var css = {};

CSSStyleSheet.create = function (name, media) {
	media = media || 'screen';
	
	var node = document.createElement('style');
	node.type = 'text/css';
	node.rel = 'stylesheet';
	node.media = media;
	document.getElementsByTagName("head")[0].appendChild(node);
	
	var sheet = document.styleSheets[document.styleSheets.length-1];
	sheet.name = name;
	css[name] = sheet;
	return sheet;
}