var DELAY = 1000;
function setup() {
    if(window.assets === undefined) {
        window.assets = {
            img: {},
            snd: {},
            ready: { img: {}, snd: {} },
            require: function() {
                var func = arguments.callee.caller;
                for (var i=0; i<arguments.length; i++) {
                    var asset = eval('window.assets.ready.' + arguments[i]);
                    setTimeout(func, DELAY);
                    throw("'" + arguments[i] + "' is missing");
                }
        
    }
        }
    }
}

function loadAssetInfo() {
    var name = node.properties.name;
    var typ = node.properties.type;
    
    // build asset tree list
    var url;
    switch (typ) {
    case "img":
        url = document.getElementById(node.properties.asset).firstChild.src;
        break;
    }
    
    assets[typ][name] = url;

    // load the asset in background
    if (url !== undefined) {
        var req = new XMLHttpRequest();
        req.open("GET", url, true);
        req.onreadystatechange = assetResponse;
        req.type = typ;
        req.name = name;
        req.send(null);
    }
}

function assetResponse(evt) {
    var req = evt.target;
    if (req.readyState == 4) {  
        if(req.status < 400) {
            assets.ready[req.type][req.name] = true;
        } else {
            assets.ready[req.typ][req.name] = false;
        }
    }
}

node.onCreateEntity = function() { setup(); node.observe(node, "load");  }
node.onLoadSignaled = function() { loadAssetInfo(); node.properties.loaded.signal(); }