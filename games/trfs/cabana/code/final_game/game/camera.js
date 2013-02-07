node.onCreateEntity = function onCreateEntity() {
node.observe(node, "load"); 
}
node.onLoadSignaled = function onLoadSignaled() {
setup();
node.properties.loaded.signal(); 
}
function setup() {
console.log("Camera Core");

trfs.cam = {};

trfs.cam.build = function (t) {
    
    trfs.cam.target = trfs.world;
}

trfs.cam.init = function() {

}

trfs.cam.update = function(dt) {
    this.x = trfs.player.x;
    this.y = trfs.player.y;
    
    this.dx = 0;
    this.dy = 0;
    
    this.targetX = 0;
    this.targetY = 0;
}

trfs.cam.render = function() {
    // move target to player's offset
    var mat = new WebKitCSSMatrix();
    this.target.style.webkitTransform = mat.translate(-Math.round(this.x), Math.round(this.y));
}

}