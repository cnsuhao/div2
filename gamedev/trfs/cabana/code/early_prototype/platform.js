function Platform(x,y,w,h) {
	this.x = x;
	this.y = y;
	this.w = w;
	this.h = h;
}


game.platforms = {	       
    all: [ new Platform(0,0,20,1),
           new Platform(4,3,4,2),
           new Platform(12,3,4,2),
           new Platform(9,7,2,4), ],
}