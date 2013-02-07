/*
 * chain.js
 * 
 * Miscellaneous helper functions.  Mostly to replace anything that 
 * Cabana would provide.  Read as event listeners and what not.
 */

(function() {
var initialize = new Chain(true);
var create = new Chain(true);
var run	= new Chain(true);

initialize.callback = create;
create.callback = run;

window.onInit = initialize.add;
window.onCreate = create.add;
window.onStart = run.add;

document.addEventListener("DOMContentLoaded", function() {
	initialize.run();
}, false);

})();