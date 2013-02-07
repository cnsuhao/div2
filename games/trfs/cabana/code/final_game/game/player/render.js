node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Player Render");
trfs.player.render = function() {
    // redraw if necessary
    if (this.dirty) {
        this.node.className = "state" + trfs.player.state + " facing" + trfs.player.facing + " inSun" + (trfs.player.inSun * 1);
        this.node.innerHTML = trfs.player.node.innerHTML;
        this.dirty = false;
    }
    
    // move to new position
    var mat = new WebKitCSSMatrix();
    mat = mat.translate(Math.round(this.x - this.w/2), Math.round(-this.y));
    this.node.style.webkitTransform = mat;
}

}