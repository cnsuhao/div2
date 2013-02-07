node.onCreateEntity = function() {
node.observe(node, "load");
 }

node.onLoadSignaled = function() {
setup(); node.properties.loaded.signal();
 }
function setup() {
console.log("Game Build");
trfs.build = function() { 
    // skip if already built
    if (trfs.built) return;

    // get root element
    trfs.screen = document.getElementById(node.properties.root);

    // hide existing elements
    for (var i=0; i<trfs.screen.children.length; i++) {
        trfs.screen.children[i].style.display = "none";
    }
    
    // add root
    trfs.root = document.createElement("div");
    trfs.root.id = "trfs";
    trfs.screen.appendChild(trfs.root);

    // create world
    trfs.world = document.createElement("div");
    trfs.world.id = "world";
    trfs.root.appendChild(trfs.world);

    // build everything
    trfs.player.build();
    trfs.level.build();
    trfs.cam.build();
    // add all nodes


//    trfs.root.appendChild(trfs.cam.node);
    
//    screen.appendChild(trfs.overlay.node);
//    screen.appendChild(trfs.bg.node);
    
    // setup input manager
    trfs.state = K.GM_LOADING;
    
    trfs.input.setup()
    trfs.built = true;
}

}