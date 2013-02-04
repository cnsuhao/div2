function toArray(arr) {
	if(arr.length===undefined) throw Error("could not find object's length");
	
	result = new Array();
	for ( var i=0; i<arr.length; i++) {
		result.push( arr[i] );
	}
	return result;
}

// only issue with this method is that it must take all parameters
Function.prototype.partial = function partial() {
	var fn = this;
	var args = new Array();
	function exec() {
		args = args.concat( toArray(arguments) );
		return fn.apply(scope, args);
	};
	
	return function() {
		arguments.callee.exec = exec;
		args = args.concat( toArray(arguments) );
		if(args.length>=fn.length) 
			exec();
		else
			return arguments.callee;
	}.apply(fn,arguments);
}

String.prototype.fill  = function fill(n,c) {
	c = c || ' ';
	if(this.length<n) {
		s = "";
		var diff = n-this.length;
		for(var i=0; i<diff; i++) {
			s += c;
		}
		return s+this;
	}
}

String.prototype.strip = function strip(multiline) {
	var headPattern = "^[\\s\\n\\t\\r\\f]*";
	var tailPattern = "[\\s\\n\\t\\r\\f]*$";
	
	var attr = "g";
	if(multiline) attr+="m";
	
	head = new RegExp(headPattern,attr);
	tail = new RegExp(tailPattern,attr);

	return this.replace(head,"").replace(tail,"");
}

function isArray(arr) {
	return !arr.propertyIsEnumerable('length') && typeof(arr) === 'object' && typeof(arr.length) === 'number';
}

Array.prototype.max = function max( ){
    return Math.max.apply( Math, this );
};

Array.prototype.min = function min( ){
    return Math.min.apply( Math, this );
};

Array.prototype.copy = function copy() {
	return this.concat([]);
}

Array.prototype.remove = function(from, to) {
  var rest = this.slice((to || from) + 1 || this.length);
  this.length = from < 0 ? this.length + from : from;
  return this.push.apply(this, rest);
};
/*
Array.prototype.weave = function(arr) {
	var weaved = new Array();
	
	var i=0;
	while(i<this.length || i<arr.length) {
		if(i<this.length)
			weaved.push(this[i]);
		if(i<arr.length)
			weaved.push(arr[i]);
		i++;
	}
	return weaved;
}
*/
Function.prototype.signature = function signature() {
	var matches = this.toString().match(/^function (.*?)(\x28.*?\x29).*/);
	var name = matches[1];
	var args = matches[2];
	if (name === "")
		name="function";
	return name+args
}