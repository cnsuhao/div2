/** @fileoverview
 * 
 * Object to load all the required javascript libraries
 */

var importers = {};

function Importer(name) {
	this.name = name;
	this.scripts = [];
	this.domain = "";
	this.called = false;
	this.callbacks = [];
}

Importer.fetch = function(name) {
	if (name in importers)
		return importers[name];
	return importers[name] = new Importer(name);
}

Importer.append = function(imp, src) {
	var node = Importer.node.cloneNode(true);
	node.src = imp.domain+src;
	node.name = src.replace(/\//g,'-');
	node.onload = function() { 
		logger.debug(imp.name, "loaded '"+src+"'");
		imp.onload(); 
	};
	Importer.head.appendChild(node);
}

Importer.node = document.createElement("script");
Importer.node.type = "text/javascript";	
Importer.head = document.getElementsByTagName("head")[0];

Importer.prototype.add = function(src) {
	if (this.scripts.indexOf(src) == -1) 
		this.scripts.push(src);
	return this;
}

Importer.prototype.require = function(name) {
	this.scripts.push(Importer.fetch(name));
	return this;
}

Importer.prototype.import = function (callback, scope) {
	logger.info(this.name,"importing...");
	
	// setup callback
	scope = scope || this;
	this.callbacks.splice(0, 0, [scope, callback]);
	
	// skip if it's ready or called
	if (this.ready) {
		this.onready();
		return;
	}
	else if (this.called)
		return
	
	// call load
	this.load();
	this.called = true;
}

Importer.prototype.load = function (callback, scope) {
	this.index = this.index || 0;
	
	if (this.index == this.scripts.length) { 
		this.onready();
		return;
	}
	
	var src = this.scripts[this.index];
	
	if (src instanceof Importer) 
		src.import(this.onload, this);
	else {
		Importer.append(this, src);
	}
}

Importer.prototype.onready = function() {
	logger.info(this.name,"imported");
	while (this.callbacks.length > 0) {
		itm = this.callbacks.pop();
		itm[1].call(itm[0]);
	}
}

Importer.prototype.onload = function() {
	this.index += 1;
	this.load();
}

var imports = {};