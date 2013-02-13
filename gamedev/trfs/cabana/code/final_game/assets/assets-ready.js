function checkAssets() {
    if (window.assets === undefined) 
        return setTimeout(checkAssets, 100);
    
    var missing = false;
    for (var typ in assets.ready) {
        for (var name in assets.ready[typ]) {
            missing = missing || assets.ready[typ][name] === undefined;
        }
    }
    
    if (missing) {
        setTimeout(checkAssets, 100);     
    }
    else {
        node.properties.ready.signal();
    }
}

node.onCreateEntity = function() { node.observe(node, "start"); }
node.onStartSignaled = function() {
    checkAssets();
}