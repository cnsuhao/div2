
/**
 * Creates a chain of functions
 * @returns {Chain}
 */
function Chain(debug, clear) {
	this.debug = debug!=undefined ? debug : false;
	this.clear = clear!=undefined ? clear : false;
	
	var funcs = [];
	var mesgs = [];
	var scopes = [];
	
	this.add = function add() {
		var func, scope, mesg;
		if (typeof arguments[0] == "string") {
			func = arguments[1];
			scope = arguments[2];
			mesg = arguments[0];
		}
		else {
			func = arguments[0];
			scope = arguments[1];
			mesg = arguments[2];
		}
		
		// correct broken
		scope = scope!=undefined ? scope : this;
		mesg = (typeof mesg == "string") ? mesg : null; 
		
		funcs.push(func);
		mesgs.push(mesg);
		scopes.push(scope);
		
		return this;
	};
	
	this.run = function run(obj) {
		obj = obj instanceof Object ? obj : this;
		
		for(var i=0; i<funcs.length; i++) {
			var scope = scopes[i]==null ? scopes[i] : obj;
			var result = funcs[i].apply(scope, arguments);
			
			if (result==undefined)
				result = "done";
			
			if(this.debug && mesgs[i] != null) {
				var mesg = mesgs[i] +": " + result;
				log(mesg);
			}
		}
		// clear if necessary
		if (this.clear) {
			this.funcs = [];
			this.mesgs = [];
			this.scopes = [];
		}
		
		// run callback if we have one
		if (this.callback)
			this.callback.run(obj);
	}
}



