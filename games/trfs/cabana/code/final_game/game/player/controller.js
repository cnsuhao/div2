node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {

console.log("Player Controller");

trfs.player.action = function(evt) {
    switch(this.state) {
    case K.PLY_RUN:
        if ((evt.fling&K.FLING_SIDE) && evt.x * this.facing > 0) {
            this.roll();
        } 
        else if ((evt.fling&K.FLING_SIDE) && evt.x * this.facing < 0) {
            this.turn();
        }
        else if (evt.fling&K.FLING_DOWN) {
            this.roll();
   
    }
        else {
   	        this.jump();
        }
		
        break;
        
    case K.PLY_WALL:
        if (evt.fling&K.FLING_DOWN) {
            this.wallDrop();
        }
        else {
            this.wallJump();	
        }
        break;
    
    case K.PLY_ROLL:
        this.run();
        break;
            
    case K.PLY_JUMP:
        this.dash();
        break;
    }

}

trfs.player.run = function() {
    this.state = K.PLY_RUN;
    this.canDash = true;
	
    this.w = trfs.tileSize;
    this.h = 60;
    this.ax = this.runAcc;
    this.ay = 0;
    this.vy = 0;
	
    if (this.facing < 0)
   	this.ax = -this.ax;
	
    this.dirty = true;
}

trfs.player.roll = function() {
    this.state = K.PLY_ROLL;
    this.rollX = this.rollDist;
	
    this.w = trfs.tileSize;
    this.h = trfs.tileSize;
    this.ax = 0;
    this.ay = 0;
    this.vy = 0;
    this.vx = this.maxSpeed * this.facing;
    this.dirty = true;
}

trfs.player.turn = function() {
    this.state = K.PLY_TURN;
    this.ax = 0;
    this.ay = 0;
    this.vy = 0;
    this.vx = 0;
    this.facing = -this.facing;
    this.turnTime = settings.PLY_TURN_DUR;
    this.dirty = true;
}

trfs.player.stun = function() {
    this.state = K.PLY_STUN;
    this.w = trfs.tileSize;
    this.h = 50;
    this.facing = -this.facing;
	
    this.ax = this.facing * this.stunAcc;
    this.ay = 0;
    this.vy = 0;
    this.stunTime = settings.PLY_STUN_DUR;
    this.dirty = true;
}

trfs.player.jump = function(vx, vy) {
    this.state = K.PLY_JUMP;
    this.w = trfs.tileSize;
    this.h = trfs.tileSize;
	
    this.vy = vy === undefined ? this.jumpSpeed : vy;
    this.vx = vx === undefined ? this.facing * this.maxSpeed : vx;
    this.ax = 0;
    this.ay = -this.gravity;

    this.dvx = this.vx;
    this.dvy = this.vy;
	
    this.dirty = true;
}

trfs.player.wallSlide = function() {
    this.state = K.PLY_WALL;
    this.canDash = true;
    this.facing = -this.facing;
    this.wallTime = this.wallSnapDur;
	
    this.w = trfs.tileSize;
    this.h = 50;
	
    this.dvx = 0;
    this.dvy = 0;
    this.vy = 0;
    this.vx = 0;
    this.ax = 0;
    this.ay = 0;
	
    this.dirty = true;
}

trfs.player.wallDrop = function() {
    vx = this.wallFallSpeedX * this.facing;
    vy = this.wallFallSpeedY;
    this.jump(vx,vy);
}

trfs.player.wallJump = function() {
    vx = this.maxSpeed * this.facing;
    vy = this.wallJumpSpeed;
    this.jump(vx,vy);
}

trfs.player.edgeJump = function() {
    vx = this.maxSpeed * this.facing;
    vy = this.edgeJumpSpeed;
    this.jump(vx,vy);
}


trfs.player.dash = function() {
    if (!this.canDash) return;
    this.w = trfs.tileSize;
    this.h = 60;
    this.vy = 0;
    this.vx = this.facing * this.maxSpeed;
    this.ay = 0;
    this.ax = this.dashAcc * this.facing;
    this.state = K.PLY_DASH;
    this.canDash = false;

    this.dirty = true;
    this.dashTime = this.dashDur;
}

}