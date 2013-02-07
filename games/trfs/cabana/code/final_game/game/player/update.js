node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Player Update");
trfs.player.update = function (dt) {   
    this.updateState(dt);
    this.move(dt);
    this.handleCollisions(dt);
	
    this.x = this.dx;
    this.y = this.dy;
	
    this.vx = this.dvx;
    this.vy = this.dvy;
}

trfs.player.move = function (dt) {
    this.dvx = this.vx + this.ax * dt;
    this.dvy = this.vy + this.ay * dt;
	
    this.dx = this.x + this.vx * dt + 0.5 * this.ax * dt * dt;
    this.dy = this.y + this.vy * dt + 0.5 * this.ay * dt * dt;
}

trfs.player.updateState = function(dt) {
    switch(this.state) {
    case K.PLY_RUN:
        if (Math.abs(this.vx) > this.maxSpeed) {
            this.ax = 0;
            this.vx = this.facing * this.maxSpeed;
   
    }
   
    var edge = false;
        var x = this.x + this.facing * this.edgeDistTo;
        var y = this.y - 5;
        if (y < 5) break;
        var rects = trfs.level.getRects(y,0);
        for (var i=0; i<rects.length; i++) {
            edge = edge || rects[i].contains(x,y);
        }
        if (!edge) { 
            this.edgeJump();
        }

        break;
        
    	case K.PLY_STUN:
    	   this.stunTime -= dt;
        if (this.facing * this.vx < 0) {
            this.ax = 0;
            this.vx = 0;
        }
        if (this.stunTime < 0)
            this.run();
        break;


    case K.PLY_TURN:
        this.turnTime -= dt;
        if (this.turnTime < 0)
            this.run();
        break;
    
    case K.PLY_ROLL:
        this.rollX -= this.maxSpeed * dt;
        if (this.rollX < 0)
            this.run();
        var edge = false;
        var x = this.x + this.facing * this.edgeDistTo;
        var y = this.y - 5;
        if (y < 5) break;
        var rects = trfs.level.getRects(y,0);
        for (var i=0; i<rects.length; i++) {
            edge = edge || rects[i].contains(x,y);
        }
        if (!edge) { 
            this.edgeJump();
        }
        break;
    case K.PLY_DASH:
        this.dashTime -= dt;
        if (this.dashTime < 0) {
            this.ax = 0;
            this.ay = -this.gravity;
            this.jump(this.facing * this.maxSpeed, 0);
        } else if (this.dashTime < this.dashDur/2) {
            this.ax = -this.ax;
        }
        break;
    
    case K.PLY_WALL:
        this.wallTime -= dt;
        if (this.wallTime < 0)
            this.ay = -this.wallSlideAcc;
        if (this.vy < -this.wallSlideSpeed) {
            this.ay = 0;
            this.vy = -this.wallSlideSpeed;
        }
        // probe to fall
        
        var edge = false;
        var x = this.x + -this.facing * this.w;
        var y = this.y + trfs.tileSize/3 - 3;
        var rects = trfs.level.getRects(y,0);
        for (var i=0; i<rects.length; i++) {
            edge = edge || rects[i].contains(x,y);
        }
        if (!edge) { 
            this.wallDrop();
        }
        break;
     }
}

}