jQuery.fx.off = false;

function range(a,b) {
	lo = (b&&a) || 0;
	hi = b || a;
	
	var tmp = new Array();
	for (var i=lo; i<hi; i++) {
		tmp.push(i);
	}
	return tmp;
}

$(function(){
	$.extend($.fn.disableTextSelect = function() {
		return this.each(function(){
			if($.browser.mozilla){//Firefox
				$(this).css('MozUserSelect','none');
			}else if($.browser.msie){//IE
				$(this).bind('selectstart',function(){return false;});
			}else{//Opera, etc.
				$(this).mousedown(function(){return false;});
			}
		});
	});
});

function Logger(node) {
	if(typeof(node) === 'string') node = $(node)[0]; 
	var count = 1;
	var callbacks = new Array();
	
	this.addCallback = function addCallback(fn) {
		if( typeof(fn) === 'function')
			callbacks.push(fn);
	};
	
	this.log = function log(msg,lvl) {
		lvl = lvl || 2;
		if(settings.debug==undefined || lvl<settings.debug)
			return
		if(msg===undefined)
			return;
		
		msg = msg.toString();
		
		var div = document.createElement("div");
		var span = document.createElement("span");
		
		$(span).clone().appendTo(div)
			.addClass("count")
			.text( (count+"").fill(4,'0')+" ("+lvl+")");
		$(span).clone(true).appendTo(div)
			.addClass("msg")
			.text(msg);
		
		var err = new Error();
		var stack = err.stack;
		if($.browser.mozilla && stack) {
			var stack = err.stack.split('\n')[2].split('@');
			var func  = stack[0];
			var lineno = stack[1].replace(/^.*?(\d+$)/,"$1");
		
		
			$(span).clone(true).appendTo(div)
				.addClass("func")
				.text(func);
			$(span).clone(true).appendTo(div)
				.addClass("lineno")
				.html(lineno.fill(4,'&nbsp;'));
		}	
		$(div).addClass("message").prependTo(node);
		
		for(var i=0; i<callbacks.length; i++) {
			callbacks[i].apply(div,[div]);
		}
		count++;
	}
}


dancingBits = {
	/**********************
	 *  SETTINGS
	 **********************/
	MAX_BITS: 50,
	
	/**********************
	 *  CROSSHAIR
	 **********************/
	crosshair: {
		// crosshair settings
		border: "solid black",
		borderWidth: 2,
		size: 40,
		innerSize: 10,
		bounds: 0,
		
		node: undefined,
		init: function init() {
			debug.log("Initializing 'crosshair'...",1);
			this.buildNode();
			this.bounds = this.innerSize/2+this.borderWidth*1.5;
			debug.log("Bounds set to "+this.bounds,2);
		},
		buildNode: function buildNode() {
			self = this;
			// create elements
			var cross = document.createElement("div");
			var box   = document.createElement("span"),
			    inner = document.createElement("span");
				
			// setup styles
			$(cross).css("position", "relative");
			
			$(box).css({
				border: "0px "+this.border,
				display: "block",
				position:"absolute",
				height: self.size/2-self.borderWidth/2,
				width:  self.size/2-self.borderWidth/2 });
			
			$(inner).css({
				border: "0px "+self.border,
				position:"absolute",
				height: self.innerSize/2,
				width:  self.innerSize/2,
				display: "block" });
			
			// build crosshair DOM
			$(box).append(inner)		  // add inner
				  .appendTo(cross)		  // add four copies to the span
				  .clone().appendTo(cross)
				  .clone().appendTo(cross)
				  .clone().appendTo(cross);
				  
			// position and style each 1/4 of crosshair
			
			$(cross).children().each( function(i,box) {
				var x = i%2, y = Math.floor(i/2);
				var offset = -self.size/2;
				var left = x ? 0 : offset;
				var top  = y ? 0 : offset;
				$(box).css({top:top, left:left});
				if(y) $(box).css("borderTopWidth",	 self.borderWidth/2);
				else  $(box).css("borderBottomWidth",self.borderWidth/2);
				if(x) $(box).css("borderLeftWidth",  self.borderWidth/2);
				else  $(box).css("borderRightWidth", self.borderWidth/2);
			}).children().each(function(i,inner) {
				var x = i%2, y = Math.floor(i/2);
				var offset = (self.size-self.innerSize)/2-(1.5*self.borderWidth);
				var left = x ? 0 : offset;
				var top  = y ? 0 : offset;
				$(inner).css({top:top, left:left});
				if(y) $(inner).css("borderBottomWidth",self.borderWidth);
				else  $(inner).css("borderTopWidth",   self.borderWidth);
				if(x) $(inner).css("borderRightWidth", self.borderWidth);
				else  $(inner).css("borderLeftWidth",  self.borderWidth);
			});
			
			this.node = cross;
		},
		placeCrosshair: function placeCrosshair(x,y) {
			$(this.node)
				.clone(true)
				.css({ top:y, left:x })
				.appendTo("#crosshairs");
			debug.log("Placed Crosshair: "+x+","+y+" Have "+$("#crosshairs").children().length,3);
		},
		getInBounds: function inBounds(x,y,range) {
			debug.log("Checking bounds",2);
			range = range || this.bounds;			
			return $("#crosshairs").children()
				.map( function(i,cross) {
					var posX = parseInt($(cross).css("left")),
					    posY = parseInt($(cross).css("top"));
				
					var dist = Math.sqrt((x-posX)*(x-posX) + 
									     (y-posY)*(y-posY));
					if(dist<=range) {
						cross.dist = dist;
						return cross;
					}
				});
		},
		
		getCoords: function getCoords() {
			return $("#crosshairs").children().map( function(i,cross) {
				return {
					top:  parseInt($(this).css("top")),
					left: parseInt($(this).css("left"))
				};
			});
		},
		
		clear: function clear() {
			$("#crosshairs").children().remove();
		}
	},
	
	
	/**********************
	 *  BITS
	 **********************/
	bits: {
		size: 50,
		color: "red",
		node: undefined,
		splitCost: 6000,
		animTime: 1750,
		fadeInOut: true,
		opacity: .6,
		
		init: function init() {
			debug.log("Initializing 'bits'...",1);
			this.buildNode();
			
			this.start = {top:0, left:0}
		},
		buildNode: function() {
			var self = this;
			
			debug.log("Building bit template...",1);
			var node = document.createElement("div"),
			    span = document.createElement("span");
			
			// setup styles
			$(node).css({
				position:"absolute",
				opacity:this.opacity});
			$(span).css({
				backgroundColor: self.color,
				display: "block",
				position:"absolute",
				height: self.size,
				width:  self.size,
				top:   -self.size/2,
				left:  -self.size/2 });
			
			$(node).append(span);
			self.node = node;
		},
		addBit: function addBit(coord,noanim) {
			self = this;
			debug.log("Creating bit");
			var bit= $(this.node)
					.clone(true)
					.css(coord)
					
			if (this.fadeInOut && !noanim) 
				$(bit).css("opacity",0)
					  .appendTo("#bits")
					  .animate({opacity:self.opacity}, self.animTime/8, "linear");
			else
				$(bit).appendTo("#bits")
			return bit;
		},

		// requires one bit to be spawned previously
		update: function update(coords,time) {
			var self = this;
			time = time || this.animTime;
			
			var bits = $("#bits div");
			// completely inactive, only ever happens if clear
			if(bits.length==0) {
				debug.log("Creating bits: "+coords.length,3);
				for(var i=0; i<coords.length; i++) {
					var bit = this.addBit(self.start,true);
					$(bit).css("opacity",0);
					coords[i].opacity = this.opacity;
					$(bit).animate(coords[i],self.animTime,"swing");
				}
				return;
			}
			
			// if there are no coords, animate bits to the start mark
			if(coords.length==0) {
				debug.log("Removing all bits",3);
				var coord = {
					top:self.start.top,
					left:self.start.left,
					opacity:0
				};
				$(bits).each( function(i) {
					$(this)
						.animate(coord,time,"swing", function() {
							$(this).remove();
						});
				});
				return;
			}
			
			// ok, we handled the easy cases, time to calculate the path
			// first, we'll need the computed distances of the bits and mins
			var bitDist = new Array();
			var bitMin = new Array();
			var coordMin = new Array();
			for( var i=0; i<bits.length; i++) {
				var arr = new Array();
				for (var j=0; j<coords.length; j++) {
					var d = self.distance(bits[i], coords[j]);
					if(coordMin[j]==undefined)
						coordMin.push([i,d]);
					if(d<coordMin[j][1]) 
						coordMin[j] = [i,d]
						
					if(bitMin[i]==undefined)
						bitMin.push([j,d]);
					if(d<bitMin[i][1]) 
						bitMin[i] = [j,d]
					arr.push(d);
				}
				bitDist.push(arr); 
			}
			
			// now that we have the minimums, let's declare a queue
			var queue = [ {
				steps:[],
				bits:range(bits.length),
				cost: 0,
				score: 0
			} ];  //initial queue
			
			// the A* score function
			function score(path) {
				var score = path.cost;
				// underestimate remainder for coords
				for(var j=path.steps.length; j<coords.length; j++) {
					score += bitDist[coordMin[j][0]][j];
					// a weighted factor to speed up larger counts
					score += Math.pow(coords.length,bits.length); // to give priority to closer steps
				}
				
				path.score = score;
			};
			
			var result;
			c = 0;
			while(queue.length!=0) {
				c++;
				var path = queue.pop();
				
				// are we at the goal?
				if(path.steps.length==coords.length) {
					result = path;
					break;
				}
				
				// build for the next coord and make a new path for each
				var coord = path.steps.length
				for(var i=0; i<path.bits.length; i++) {
					var next = {};
					next.steps = path.steps.copy();
					next.steps.push( [coord, path.bits[i]] );
					next.bits = path.bits.copy();
					next.bits.remove(i);
					next.cost = path.cost + bitDist[path.bits[i]][coord];
					score(next);
					
					queue.push(next);
				}
				
				// add one for the split
				var next = {};
				var split = coordMin[coord][0];
				next.steps = path.steps.copy();
				next.steps.push( [coord, split,1] );
				next.bits = path.bits.copy();
				next.cost = path.cost + bitDist[split][coord]+(this.splitCost)/coords.length;
				score(next);
				queue.push(next);
				queue.sort(function(a,b) { return a.score<b.score });
			}
			
			debug.log("Path found after checking: "+c,3);
			if(result==undefined) {
				err = new Error("Could not calculate A* path...something went wrong");
				err.name = "AStarError";
				throw err;
			}
			
			// now that we have a plan, rebuild result into a more useable list
			var movements = new Array();
			var mergeCount = result.bits.length;
			var splitCount = 0;
			for(var i=0;i<result.steps.length; i++) {
				var mv = {}
				var step = result.steps[i];
				
				// add a bit if we need to
				if(step[2]!=undefined) {
					splitCount++;
					var bit = bits[step[1]];
					mv.node=this.addBit($(bit).position());
					$(mv.node).css(  );
				}
				else
					mv.node = bits[step[1]];
				mv.coord = coords[step[0]];
				movements.push(mv);
			}
			
			// handle remaining bits with merging
			for (var i=0; i<result.bits.length; i++) {
				var mv={};
				mv.node  = bits[ result.bits[i] ];
				mv.coord = coords[bitMin[ result.bits[i] ][0]];
				mv.deactivate=true;
				movements.push(mv);
			}

			if(splitCount)
				debug.log("Splitting "+splitCount+" times",3);
			if(mergeCount)
				debug.log("Merging "+mergeCount+" times",3);

			// now that we've structured everything we need, go ahead and 
			// animate it all
			for(var i=0;i<movements.length; i++) {
				var mv = movements[i];
				var callback = mv.deactivate ? this.deactivate : undefined;
				$(mv.node).animate(mv.coord,time,"swing",callback);
			}
		},
		
		distance: function(node,coord) {
			var pos = $(node).position();
			var t = (coord.top-pos.top);
			var l = (coord.left-pos.left);
			return Math.sqrt(t*t+l*l);
		},
		deactivate: function(node) {
			node = node || this; // compensate for anim
			if(dancingBits.bits.fadeInOut)
				$(node).animate({opacity:0},dancingBits.bits.animTime/2,"linear",function() { $(this).remove(); });
			else
				$(this).remove();
		}
	},
	
	/**********************
	 *  INPUT
	 **********************/
	input: {
		handleCrosshair: function(evt) {
			var crosshairs = dancingBits.crosshair.getInBounds(evt.clientX, evt.clientY);
			
			// sort by distance
			crosshairs.sort( function(a,b) {
				a.dist >= b.dist;
			});
			
			var cross = crosshairs[0];// get closest
			
			// if we have a crosshair in the bounds (of clicking) remove it
			if (cross!=undefined)  {
				var pos = $(cross).position();
				$(cross).remove();
				debug.log("Removed crosshair at: "+pos.left+","+pos.top+ " "+$("#crosshairs").children().length+" remaining",3);
			}
			else if ($('#crosshairs').children().length < dancingBits.MAX_BITS) 
				dancingBits.crosshair.placeCrosshair(evt.clientX, evt.clientY);
			else 
				debug.log("Cannot create anymore crosshairs.  Have "+$("#crosshairs").children().length,3);
		},
		
		keyDown: function (evt) {
			if(evt.keyCode==32 || evt.keyCode==13)
				return;   // do nothing for now
		},
		keyUp: function (evt) {
			if(evt.keyCode==32 || evt.keyCode==13) {
				var coords = dancingBits.crosshair.getCoords();
				try {
					dancingBits.crosshair.clear();
					debug.log("Fetching crosshairs.  Found "+coords.length,3);
					debug.log("Updating bits!",3);
					dancingBits.bits.update(coords);
				} catch (e) {
					debug.log(e.name+":  "+e.message+"\n"+e.fileName+":"+e.lineNumber,4)
				}
			}
		},
		
		init: function init() {	
			debug.log("Initializing 'input'...",1);
			
			$("#pane").click( this.handleCrosshair );
			$(document).keydown( this.keyDown );
			$(document).keyup( this.keyUp );
		}
	},
	
	// initialize all of dancing bits
	init: function init() {
		debug.log("Initializing 'dancingBits'...",1);
		this.bits.init();
		this.crosshair.init();
		this.input.init();
	}
}
