node.onCreateEntity = function() { node.observe(node, "fire"); }
node.onFireSignaled = function() { 
    var elem = document.getElementById(node.properties.loadingScreen);
    elem.innerHTML = "";
    node.properties.fired.signal();
}