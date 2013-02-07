node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Player Physics");

trfs.player.handleCollisions = function(dt) {
    var coll;
    for (var i=0; i<settings.MAX_COLL_CHECKS; i++) {
        var colls = this.collideLevel(dt);
        if (colls === null) break;
        this.resolveCollision(colls);
    }
}

trfs.player.collideLevel = function(dt) {  
    if (this.dy < 0)
        return {floor:true}

   	plyRect = new Rect(this.dx - this.w/2, this.dy, this.w, this.h);
   	rects = trfs.level.getRects(this.dy, this.h);
  
   	var colls = null;
    for (var i=0; i<rects.length; i++) {
        var rect = rects[i];
        if (rect.intersects(plyRect)) {
            colls = colls || {};
            // only has a floor if there is something directly bellow previous x
            colls.floor = colls.floor || (this.dy < this.y && (rect.contains(this.x-this.w/2, this.dy) || rect.contains(this.x+this.w/2,this.dy)));
            colls.ceil = colls.ceil || (this.dy > this.y && (rect.contains(this.x-this.w/2, this.dy+this.h) || rect.contains(this.x+this.w/2,this.dy+this.h)));
            colls.wall = colls.wall || (this.dx != this.x && (rect.contains(this.dx+this.facing*this.w/2, this.y) || rect.contains(this.dx+this.facing*this.w/2,this.y+this.w/2)));
            colls.slide = colls.slide || (colls.wall && !colls.ceil);
//            colls |= (rect.y < this.dy + this.h) && (rect.y >= this.y + this.h) ? K.NORTH : 0;
//            colls |= (rect.y + rect.h > this.dy) && (rect.y + rect.h <= this.y) ? K.SOUTH : 0;
//            colls |= (rect.cx - rect.ex < this.dx + this.w/2) && (rect.cx - rect.ex >= this.x + this.w/2) ? K.WEST : 0;
//            colls |= (rect.cx + rect.ex > this.dx - this.w/2) && (rect.cx + rect.ex <= this.x + this.w/2) ? K.EAST : 0;
        }
    }
    return colls;
}

trfs.player.resolveCollision = function(colls) {
    if (colls.floor) {
        this.dy = this.y - this.y % trfs.tileSize;
        this.y = this.dy;
        this.dvy = 0;
        if (this.state != K.PLY_RUN)
            this.run();
    }
    else if (colls.ceil) {
        this.dvy /= this.ceilColl;
        this.dy = this.dy - this.dy % trfs.tileSize;
        this.y = this.dy;
    }
    else if (colls.wall) {
        var x = Math.floor(this.dx / trfs.tileSize) + 0.5;
        this.dx = trfs.tileSize * x;
        
        if (this.state == K.PLY_RUN) {
            this.dvx = -this.facing * this.maxSpeed;
            this.stun();
            return;
        }
        else if (this.state == K.PLY_ROLL || this.state == K.DASH) {
            this.facing = -this.facing;
            this.dvx = -this.dvx;
            this.dirty = true;
        }
        else if (colls.slide) {
            this.wallSlide();
        }
        else {
                this.dvx /= this.ceilColl;
        }
    }

    return;
    
        
//            
        /*
            var edge = false;
            var rects = trfs.level.getRects(this.y+this.h,0);
            for (var i=0; i<rects.length; i++) {
                edge = edge || rects[i].contains(x,y);
            }
            if (!edge) { 
                
            } else {
                this.dvx=0;
            }
        */
            //this.jump(this.facing * this.wallFallSpeedX, this.dy);
//            this.dvy = (coll & K.NORTH) ? 0 : this.wallFallSpeedY;

            // check if there is only a bit left
//            var x = this.dx + this.facing * (this.w);
//            var y = this.dy + this.stumbleDist;
//            var rects = trfs.level.getRects(y, 0);
//            var blocked = false;
//            for (var i=0; i<rects.length; i++) {
//                blocked = blocked || rects[i].contains(x,y);
//            }
            
//            //if (!blocked) {
                
            //} else {
//                this.wallSlide();
            //}
            //this.dy -= (this.dvy > 0) ? (this.h - h) : 0;    
    
    
 
    /*
    // handle special undetectable cases
    if (coll == (K.Xcoll|K.Ycoll|K.coll) && this.state == K.PLY_WALL) {
        var y = this.dy - this.dy % trfs.tileSize;
        y += 2 * trfs.tileSize;
        y -= this.h;
        this.dy = y;
    }*/
}

}