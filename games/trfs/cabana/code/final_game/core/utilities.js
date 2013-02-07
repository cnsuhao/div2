node.onCreateEntity = function() {
node.observe(node,"load"); 
}
node.onLoadSignaled = function() { setup();
node.properties.loaded.signal(); 
}
function setup() {

/****************\
 *   FUNCTION   *
\****************/
Function.prototype.bind = function (object) {
    	var method = this;
    	return function() {
        		return method.apply(object, arguments);
    	}
}

/****************\
 *    STRING    *
\****************/
String.prototype.startsWith = function (s) {
    	return (this.indexOf(s) === 0);
}

String.prototype.escapeHTML = function () {                                        
    return(                                                                 
        this.replace(/&/g,'&amp;').                                         
            replace(/>/g,'&gt;').                                           
            replace(/</g,'&lt;').                                           
            replace(/"/g,'&quot;')                                         
    );                                                                      
};

/****************\
 *    HELPERS   *
\****************/
// thanks to gimicky cssRule handling, all attribute selectors are just classes
window.attr = function attr(k,v) {
    var a = '.' + k;
    if (v !== undefined)
        a+=v;
    return a;
}

window.pc = function pc(n) { return (n*100) + "%"; }   
window.px = function px(n) { return parseInt(Math.round(n))+"px"; }
window.E = function E(n) { return Math.pow(10,n); }

Number.prototype.digits = function digits(d) { return parseFloat( this.toFixed(d) ); }

window.url = function url(s) { return "url(\"" + s + "\")"; }

// grab user agent info
var agent = navigator.userAgent.toLowerCase();
var browser = {};

if (agent.indexOf('iphone') != -1)
    browser.iphone = true;
    
window.browser = browser;

/****************\
 *    MATH      *
\****************/
Math.linear = function linear(p,l,h) { return Math.round(p*(h-l)+l); }

/****************\
 *    ARRAY     *
\****************/ 
Array.prototype.last = function() { return this[this.length-1]; }

Array.prototype.flush = function() {
    return this.splice(0,this.length);
}

Array.prototype.flatten = function() {
    var result = []; 
    for (var i=0; i<this.length; i++) {
    var v = this[i];
        if (v && v.length !== undefined && typeof(v) != "string") {
            result = result.concat(v.flatten());
        } else {
            result.push(v);
        }
    }
    return result;
}

Array.prototype.set = function() {
    var arr = [];
    for (var i=0; i<this.length; i++) {
        if (arr.indexOf(this[i]) == -1)
            arr.push(this[i]);
    }
    return arr;
}

}