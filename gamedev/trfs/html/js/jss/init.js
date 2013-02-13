var jss = {};

function JSS(name) {
	if (name in jss)
		return jss[name];
	
	var node = document.createElement("style");
	node.id = name+"-jss";
	
	var head = document.getElementsByTagName("head")[0];
	head.appendChild(node);
	
	this.sheet = document.styleSheets[ document.styleSheets.length - 1 ];
	
	jss[name] = this;
}

JSS.prototype.addRule = function(selector, rules) {
	for (var k in rules) {
		this.sheet.addRule(selector, k+": "+rules[k]);
	}
}