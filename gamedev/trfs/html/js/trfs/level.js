function Level() {
	this.id = "level";
	this.tag = "div";
	
	this.rowClass = "row";
	this.rowTag = "div";
}

Level.prototype.build = function() {	
	var node = document.createElement(this.tag);
	node.id = this.id;
	this.node = node;
	
	this.rows = [];
	var n = settings.ROW_PADDING * 2 + 1; // twice pad + player's center
	for (var i=0; i<n; i++) {
		var row = document.createElement(this.rowTag);
		row.className = this.rowClass;
		row.setAttribute("rowid", i);
		this.rows.push(row);
		this.node.appendChild(row);
	}
	
	// add a row for the floor
	var floorRow = document.createElement("div");
	floorRow.className = this.rowClass;
	floorRow.style.top = "0px";
	var floor = document.createElement("div");
	floor.style.float = "left";
	floor.style.backgroundColor = "#222";
	floor.style.width = "100%";
	floor.style.height = "100%";
	floorRow.appendChild(floor);
	this.node.appendChild(floorRow);
	
	// setup style
	var id = "#"+this.id;
	var borderWidth = settings.TILE_SIZE * settings.BORDER_SIZE;
	this.style = new JSS(this.id);
	this.style.addRule(id + " .row", {
		"width": (settings.TILE_SIZE * settings.LEVEL_WIDTH)+"px",
		"height": settings.TILE_SIZE + "px",
		"position": "absolute",
		"top": -settings.TILE_SIZE + "px",
		"left": -borderWidth + "px",
		"border-left": borderWidth + "px solid black", 
		"border-right":borderWidth + "px solid black",
	});
}

Level.prototype.init = function() {
	this.gaps = [];
	this.map = [];
	this.bounds = [];
	this.prng = new Math.Random((new Date()).getTime()+'');
	
	var g = parseInt(this.prng.rand() * (settings.LEVEL_WIDTH - settings.GAP_WIDTH));
	this.gaps.push(g);
	
	for (var i=0; i<settings.GAP_HEIGHT; i++) {
		this.map.push([0,0]);
		this.bounds.push([0, (settings.LEVEL_WIDTH - 1) * settings.TILE_SIZE]);
	}
	
	// clear rows
	for (var i=0; i<this.rows.length; i++) 
		this.rows[i].y = -1;
	this.row = undefined;
}

Level.prototype.draw = function() {
	var row = parseInt(game.y / settings.TILE_SIZE);
	
	row = Math.max(row, settings.ROW_PADDING);
	
	if (row !== this.row) {
		var w = 2 * settings.ROW_PADDING + 1;
		for (var i=0; i<w; i++) {
			// crazy math to cycle which row is where
			var b = parseInt((w-i+row-1) / w);
			var o = w * b + i - settings.ROW_PADDING;
			
			// update rows which are wrong
			if (this.rows[i].y != o) {
				this.rows[i].y = o;
				this.updateRow(this.rows[i]);
			}
		}
	}
	this.row = row;
}

Level.prototype.getRow = function(n) {
	// map generation
	while (n > this.map.length-1) {
		// generate 1 more gap
		var i = this.gaps.length;
		var gap = parseInt(this.prng.rand() * (settings.LEVEL_WIDTH - settings.GAP_WIDTH));
		this.gaps.push(gap);
		
		// place the gap
		var l = gap;
		var r = settings.LEVEL_WIDTH - gap - settings.GAP_WIDTH;
		
		// clear from previous gap
		var prevGap = this.gaps[i-1];
		if (prevGap>gap)
			r = settings.LEVEL_WIDTH - prevGap - settings.GAP_WIDTH;
		else
			l = prevGap;
		
		var ml = l;
		var mr = r;
		var bl = l * settings.TILE_SIZE;
		var br = (settings.LEVEL_WIDTH - (r+1)) * settings.TILE_SIZE;
		
		// add n more rows
		for (var i=0; i<settings.GAP_HEIGHT; i++) {
			this.map.push([l,r]);
			this.bounds.push([bl,br]);
		}
	}
	
	return this.map[n];
}

Level.prototype.getRowBounds = function(y) {
	var h0 = Math.floor(y / settings.TILE_SIZE);
	var h1 = Math.ceil(y / settings.TILE_SIZE);
	
	h0 = Math.max(0,h0);
	h1 = Math.max(0,h1);
	
	var l = Math.max(this.bounds[h0][0], this.bounds[h1][0]);
	var r = Math.min(this.bounds[h0][1], this.bounds[h1][1]);
	
	return [l, r];
}

Level.prototype.updateRow = function(row) {
	var dim = this.getRow(row.y);
	
	// draw the row
	row.innerHTML = "";
	var left = document.createElement("div");
	left.style.float = "left";
	left.style.backgroundColor = "#222";
	left.style.width = (dim[0] * settings.TILE_SIZE) + "px";
	left.style.height = "100%";
	row.appendChild(left);
	
	var center = document.createElement("div");
	center.style.float = "left";
	center.style.backgroundColor = "#888";
	center.style.width = (settings.LEVEL_WIDTH - dim[0] - dim[1]) * settings.TILE_SIZE + "px";
	center.style.height = "100%";
	row.appendChild(center);
	
	var right = document.createElement("div");
	right.style.float = "left";
	right.style.backgroundColor = "#222";
	right.style.width = (dim[1] * settings.TILE_SIZE) + "px";
	right.style.height = "100%";
	row.appendChild(right);
	
	// move the row
	var mat = new WebKitCSSMatrix();
	mat = mat.translate(0, -row.y * settings.TILE_SIZE);
	
	row.style.webkitTransform = mat;
}
