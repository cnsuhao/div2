/** @fileoverview
 * 
 * A bunch of methods which extend the {@link Function} prototype.
 * 
 * @see Function
 */
Function.prototype.bind = function (object) {
	var method = this;
	return function() {
		return method.apply(object, arguments);
	}
}

Function.prototype.run = function () {
	return this.apply(this, arguments);
}