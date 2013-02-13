var game = {};
(function() {

var K = {
	FPS: 30,
	TRAMPOLINE: {
		MAX_WIDTH: 120,
		THICKNESS: 12
	},
	player: {
	
	},
	PHYS: {
		G: 20
	}
}

var input = {
	setup: function() {
		// setup basic mouse listeners
		window.addEventListener("mousedown", function(evt) {
			evt.preventDefault()
			input.touchStart = evt;
		}, false);
		window.addEventListener("mouseup", function(evt) {
			evt.preventDefault();
			input.touchFinish = [ input.touchStart, input.touchPos ];
			var x0 = input.touchStart.pageX;
			var y0 = input.touchStart.pageY;
			var x1 = input.touchPos.pageX;
			var y1 = input.touchPos.pageY;
			
			if (x0 > x1) {
				var x  = x0, y  = y0;
				    x0 = x1; y0 = y1;
				    x1 = x;  y1 = y;
			}
			
			if (x0!=x1)
				state.trampoline = { x0:x0, y0:y0, x1:x1, y1:y1, width: 40};
				
			input.touchStart = undefined;
		}, false);
		window.addEventListener("mousemove", function(evt) { input.touchPos = evt; }, false);
	}
};

var state = {
	trampoline: {
		move: function (dt) {
		
		},
		update: function(dt) {}
	}
};

var layers = {
	"Sprites": {
		setup: function(g) {
			g.fillStyle = "red";
			g.lineWidth = 2;
		},
		draw: function(g) {
			g.clearRect(0,0,320,480);
		
			var trampoline = state.trampoline;
			
			if (trampoline) {
				g.save();
				var x0 = trampoline.x0;
				var y0 = trampoline.y0;
				var x1 = trampoline.x1;
				var y1 = trampoline.y1;
				
				var dx = x1-x0;
				var dy = y1-y0;
				
				var w = Math.sqrt(dx*dx + dy*dy);
				if (w > K.TRAMPOLINE.MAX_WIDTH) w = K.TRAMPOLINE.MAX_WIDTH;
				
				var cx = dx/2+x0;
				var cy = dy/2+y0;
				
				theta = Math.atan(dy/dx);
				
				g.translate(cx, cy);
				g.rotate(theta);
				g.scale(w/2+K.TRAMPOLINE.THICKNESS, K.TRAMPOLINE.THICKNESS);
				
				g.fillStyle="black";
				g.beginPath();
				g.arc(0,0,1.2,0,Math.PI*2);
				g.fill()
				
				g.fillStyle="red";
				g.beginPath();
				g.arc(0,0,1,0,Math.PI*2);
				g.fill();
				
				g.restore();
			}
		}
	},
	"UIOverlay": {
		setup: function(g) { 
			g.lineWidth = K.TRAMPOLINE.THICKNESS*2;
			g.lineCap = "square";
			g.strokeStyle = "rgba(0, 0, 0, 0.7)";
		},
		draw: function(g) { 
			// clear previous
			g.clearRect(0,0,320,480);
			if (input.touchStart) {
				var x0 = input.touchStart.pageX;
				var y0 = input.touchStart.pageY;
				var x1 = input.touchPos.pageX;
				var y1 = input.touchPos.pageY;
				
				g.beginPath();
				g.moveTo(x0,y0);
				g.lineTo(x1,y1);
				g.stroke();
			}
		}
	}
};

function update(dt) {

}

function redraw() {
	for(var k in layers) {
		var g = layers[k].ctx;
		layers[k].draw(g);
	}
}

// our main loop step
function tick() {
	var dt = 0;
	
	update(dt);
	redraw();
}

// start and stop functions
function resume() { 
	state.loop = setInterval(tick, parseInt(1000*(1/K.FPS))); 
}
function pause() { clearInterval(state.loop); }

// public functions
game.resume = resume;
game.pause = pause;

window.addEventListener("load", function() {
	// find all the canvas nodes we need
	layers.UIOverlay.node = document.getElementById("ui-overlay");
	layers.Sprites.node = document.getElementById("sprites");
	
	// setup the layers
	for (var k in layers) {
		var canvas = layers[k].node;
		var g = layers[k].ctx = canvas.getContext('2d');
		layers[k].setup(g);
	}

	input.setup();
	resume();
}, false);

})();