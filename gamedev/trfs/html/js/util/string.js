/** @fileoverview
 * 
 * A bunch of methods which extend the {@link String} prototype.
 * 
 * @see String
 */
	
/**
 * Test is a string starts with another 
 * 
 * @param {String} s string to test with
 * @type boolean
 */
String.prototype.startsWith = function (s) {
	return (this.indexOf(s) === 0);
}
