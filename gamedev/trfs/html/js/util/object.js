/** @fileoverview
 * 
 * A bunch of methods which extend the {@link Object} prototype.
 * 
 * @see Object
 */

/**
 * Applies a function to each element. 
 * 
 * Taken from jQuery 1.4.3
 * 
 * @param {Function} callback function to apply
 * @param {args} optional arguments to pass to callback
 * @type Object
 */
/*
Object.prototype.each = function(callback, args) {
	var object = this;
	var name, i = 0,
		length = object.length,
		isObj = length === undefined || (typeof object === "function");
	
	if (args) {
		if (isObj) {
			for (name in object) {
				if (object.hasOwnProperty(name)) {
					if callback.apply(object[name], args) === false) 
						break;
				}
			}
		} else {
			for ( ; i < length; ) {
				if (callback.apply(object[i++], args) === false)
					break;
			}
		}
	// A special, fast, case for the most common use of each
	} else {
		if (isObj) {
			for (name in object) {
				if (object.hasOwnProperty(name)) {
					if (callback.call(object[name], name, object[name]) === false )
						break;
				}
			}
		} else {
			for ( var value = object[0];
				i < length && callback.call( value, i, value ) !== false; value = object[++i] ) {}
		}
	}
	
	return object;
}
*/
/* to replace
Object.prototype.filter = function filter(fn, preserve) {
	if (this.length != undefined) 
		return Array.prototype.filter.call(this,fn);
	
	var obj = preserve ? this : {};
	var keys = [];
	for (var k in this) {
		if (this.hasOwnProperty(k)) {
			var scope = this[k];
			if (! scope instanceof Object) 
				scope = this;
			
			var result = fn.call(scope, k, this[k]);
			// if result and !preserve, update obj
			if (result && !preserve) 
				obj[k] = this[k];
			// if !result and preserve, delete key
			else if (!result && preserve)
				keys.push(k)
		}
	}
	
	if (preserve) {
		for (var i=0; i< keys.length; i++) 
			delete obj[keys[i]];
	}
	
	return obj;
}
*/