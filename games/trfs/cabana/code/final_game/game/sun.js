node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {

trfs.sun = {};

trfs.sun.init = function() {
    
    console.log("test");
    this.beamWidth = settings.SUN_BEAM_WIDTH * settings.TILE_SIZE;
    this.beamTop = this.beamWidth * settings.SUN_BEAM_RATIO;
    this.beamBottom = this.beamWidth - this.beamTop;

    // calculate the initial velocity
    this.vy = settings.SUN_SPEED * settings.TILE_SIZE;
    this.ay = settings.SUN_ACCEL * settings.TILE_SIZE;

    // compensate for the delay
    this.delay = settings.SUN_DELAY;
    this.vy -= this.ay * this.delay;
    this.y = -(this.vy * this.delay + 0.5 * this.ay * this.delay * this.delay);
}

trfs.sun.update = function(dt) {
    this.y += this.vy * dt;
    this.vy += this.ay * dt;
}

}