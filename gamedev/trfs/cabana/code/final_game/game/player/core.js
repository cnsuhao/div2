node.onCreateEntity = function() {
node.observe(node, "load"); 
}
node.onLoadSignaled = function() {
setup();
node.properties.loaded.signal(); 
}
function setup() {
console.log("Player Core");
trfs.player = {};

trfs.player.build = function () {
    var elem = document.createElement("div");
    elem.id = "player";
    
    var spr = document.createElement("div");
    spr.className = "sprite";
    elem.appendChild(spr);
    this.node = elem;
    
    trfs.world.appendChild(this.node);
}

trfs.player.init = function() {
    var tile = settings.TILE_SIZE;
    
    this.maxSpeed = settings.PLY_RUN_SPEED * tile;
    this.runAcc = ((settings.PLY_RUN_SPEED * settings.PLY_RUN_SPEED) / (2 * settings.PLY_RUN_ACC_DIST)) * tile;
    this.stunAcc = (-(settings.PLY_RUN_SPEED * settings.PLY_RUN_SPEED) / (2 * settings.PLY_STUN_DIST)) * tile;
	
    this.rollDist = settings.PLY_ROLL_DIST * tile;

    this.jumpSpeed = this.maxSpeed * Math.tan(settings.PLY_JUMP_ANG / 180 * Math.PI);
    this.ceilColl = settings.PLY_CEIL_COLL;
    this.gravity = ((2 * (settings.PLY_RUN_SPEED * settings.PLY_RUN_SPEED) * Math.tan(settings.PLY_JUMP_ANG / 180 * Math.PI)) / settings.PLY_JUMP_DIST) * tile;
    
    if (settings.PLY_WALL_JUMP_ANG) {
        this.wallJumpSpeed = this.maxSpeed * Math.tan(settings.PLY_WALL_JUMP_ANG / 180 * Math.PI);
    } else if (settings.PLY_WALL_JUMP_DIST) {
        this.wallJumpSpeed = (this.gravity * trfs.tileSize * settings.PLY_WALL_JUMP_DIST)/ (2 * this.maxSpeed);
    } else if (settings.PLY_WALL_JUMP_HEIGHT) {
        this.wallJumpSpeed = Math.sqrt(2 * this.gravity * settings.PLY_WALL_JUMP_HEIGHT * trfs.tileSize);
    } else {
        this.wallJumpSpeed = this.jumpSpeed;
    } 
    
    this.wallSlideSpeed = trfs.tileSize * settings.PLY_WALL_SLIDE_SPEED;
    this.wallSlideAcc = this.wallSlideSpeed / settings.PLY_WALL_SLIDE_ACC_DUR;
    
    this.wallFallSpeedX = 0.5 * Math.sqrt(trfs.tileSize * settings.PLY_WALL_FALL_DIST * this.gravity / 2);
    this.wallFallSpeedY = Math.sqrt(2 * trfs.tileSize * settings.PLY_WALL_FALL_DIST * this.gravity);
    
    this.wallSnapDur = settings.PLY_WALL_SNAP;
    
    this.edgeDistTo = settings.PLY_EDGE_DIST_TO * trfs.tileSize;
    this.edgeDistFrom = settings.PLY_EDGE_DIST_FROM * trfs.tileSize;
    this.edgeJumpSpeed = this.gravity / this.maxSpeed * (this.edgeDistTo + this.edgeDistFrom); 
    
    this.stumbleDist = settings.PLY_STUMBLE_DIST * trfs.tileSize;
    
    this.dashAcc = 2 * ((trfs.tileSize * settings.PLY_DASH_DIST) / settings.PLY_DASH_DUR - this.maxSpeed) / (settings.PLY_DASH_RATIO * settings.PLY_DASH_DUR);  
    this.dashDur = settings.PLY_DASH_DUR;
    
    this.state = K.PLY_RUN;
    this.facing = K.RIGHT;
    this.inSun = false;
    this.dirty = true;
    
    this.x = 144;
    this.y = 0;
    this.vx = 0;
    this.vy = 0;
    this.ax = 0;
   	this.ay = 0;

    // tempory
    this.inSun = 1;
   	
   	this.run();
}

}