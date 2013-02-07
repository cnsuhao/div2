node.onCreateEntity = function () {
node.observe(node, "load"); 
}
node.onLoadSignaled = function () {
setup();
node.properties.loaded.signal(); }
function setup() {

/* basic style sheet */
var MAX_DIGITS = 5;

function buildRules (rules) {
    var rule = "{ ";
    for (var k in rules) {
        var v = rules[k];
        k = k.replace(/([A-Z])/g, "-$1").toLowerCase();
        k = k.replace(/^wbkt/, "-webkit");        
        rule += k + ": " + v + "; ";
    }
    rule += "}";

    return rule;
}

var jss = {};
function JSS(name) {
    this.isJSS = true;
    this.name = name;
    
    var elem = document.createElement("style");
    elem.id = name+"JSS";
    
    var head = document.getElementsByTagName("head")[0];
    head.appendChild(elem);
    
    this.sheet = document.styleSheets[ document.styleSheets.length - 1 ];
    
    jss[name] = this;
}

JSS.prototype.add = function(selector, rules) {
    if (typeof rules == "object") {
        rules = buildRules(rules);
    }

    // handle multiple selectors
    if (typeof selector != "string") {
        selector = selector.join(",");
    } else if (arguments.length > 2) {
        for (var i=1; i<arguments.length - 1; i++) {
            selector += "," + arguments[i];
        }
        rules = arguments[arguments.length - 1];
    }
   
    var rule = selector + " " + rules;
    this.sheet.insertRule(rule, this.sheet.cssRules.length);
    return this;
}

JSS.prototype.anim = function(name, steps) { 
    var frames = {};
    var keys = [];
    var locked = {};
    
    for (var i=0; i<steps.length; i++) {
        var step = steps[i];
        var key = Math.abs(step[0]).digits(MAX_DIGITS);
        
        // setup frame and key
        if (frames[key] === undefined) {
            frames[key] = {};
            locked[key] = {};
            keys.push(key);
        }
        
        var l = step[0] < 0;
        for (var j=1; j<step.length; j++) {
            for (var k in step[j]) {
                frames[key][k] = step[j][k];
                if (l) 
                    locked[key][k] = true;
            }
        }
    }
    
    keys.sort();

    // handle any locked rules
    var c = keys.length;
    for (var i=1; i<c; i++) {
        var key = keys[i];
        var lockedRules = {};
        var found = false;
        
        var nk = (key - E(-MAX_DIGITS)).digits(MAX_DIGITS);
        frames[nk] = {};
        locked[nk] = {};
        keys.push(nk);

        for (var k in frames[key]) {
            if (locked[key][k])
                frames[nk][k] = frames[keys[i-1]][k];
            else
                frames[nk][k] = frames[key][k];
        }
    }
    
    keys.sort();

    // loop through and build the rules
    var rule = "{ ";
    for (var i = 0; i<keys.length; i++) {
        var key = keys[i];
        var step = (key*100).digits(MAX_DIGITS-2) + "% " + buildRules(frames[key]);
        rule += step + " ";

    }
    rule += "}";
    this.add("@-webkit-keyframes " + name, rule);

    return this;
}

window.jss = jss;
window.JSS = JSS;

}