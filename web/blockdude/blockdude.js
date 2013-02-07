(function ($) {

/*  keys */
var RESET = 8;
	UP    = 38,
	DOWN  = 40,
	LEFT  = 37,
	RIGHT = 39;


var EMPTY  = 0,
	WALL   = 1,
	BLOCK  = 2,
	EXIT   = 3,
	PLAYER = 4;
	
	
var CLASS = [ "","wall","","exit",""];
	
/** helper functions **/
function getCell(r,c) {
	return document.getElementById("r"+r+"c"+c);
}

function getCellXY(x,y) {
	return document.getElementById("r"+y+"c"+x);
}

function cellHasBlock(cell) {
	return ( $(".block", cell).length==1 );
}

function cellHasBlockXY(x,y) {
	return $(".block", document.getElementById("r"+y+"c"+x)).length == 1;
}

function isBlockedXY(x,y) {
		return isBlocked( getCell(y,x) );
}
function isBlocked(cell) {
	if ( $(cell).hasClass("wall") )
		return true;
	else if ( cellHasBlock(cell) )
		return true;
	else
		return false;
}

function xy2id(x,y) {
	return "r"+y+"c"+x;
}

function id2xy(id) {
	var s = id.substring(1);
	var coord = s.split('c');
	return { x: parseInt(coord[1]), y: parseInt(coord[0]) };
}

function load(url) {
	$("#game").html("");
	// ajax request wraps function normally
	$.get(url, null, function (data) {
		build( data );
		reset();
	}, "text");
}

var map;
var game;
function build(data) {
	// strip excess white space and prep text
	data = jQuery.trim(data);
	data = data.split("\n");
	
	// parse the text 
	map = new Array();
	for ( var i=0; i<data.length; i++) {
		var row = new Array();
		for ( var j=0; j<data[i].length; j++) {
			switch( data[i].charAt(j) ) {
			case " ":
				row.push(EMPTY); break;
			case "#":
				row.push(WALL); break;
			case "O":
				row.push(BLOCK); break;
			case "P":
				row.push(PLAYER); break;
			case "@":
				row.push(EXIT); break;
			}
		}
		map.push(row);
	}
	
	// build the board
	for ( var i=0; i<map.length; i++) {
		var row = document.createElement("div");
		for ( var j=0; j<map[i].length; j++) {
			$(document.createElement("span"))
				.attr("id", "r"+i+"c"+j)
				.attr("class", CLASS[ map[i][j] ])
				.appendTo(row);
		}
		$(row)
			.addClass("row")
			.appendTo("#game");
	}
}

var hasBlock;
var direction;	
function reset() {
	$(".block",game).remove();
	$("#player").remove();
	direction = RIGHT;
	hasBlock = false;
	
	for ( var i=0; i<map.length; i++) {
		for ( var j=0; j<map[i].length; j++) {
			var cell = getCell(i,j);
			if (map[i][j] == BLOCK ) {
				$(document.createElement("div"))
					.addClass("block")
					.appendTo(cell);
			}
			else if (map[i][j] == PLAYER ) {
				$(document.createElement("div"))
					.attr("id","player")
					.attr("class", "right")
					.appendTo(cell);
			}
		}
	}
}


function move(key) {
	var $cell = $("#player").parent();
	var coord = id2xy( $cell.attr("id") );
	
	if (key!=UP && key!=DOWN)
		direction = key;
	var dx = direction==LEFT ? -1 : +1;
	var dy = key==UP ? -1: 0;
	
	if (key==DOWN) {
		// PICKUP
		if ( !hasBlock ) {
			var canPickup =  cellHasBlockXY(coord.x+dx, coord.y) 	// there is a block
			canPickup = canPickup && !isBlockedXY( coord.x, coord.y-1)			// no ceiling
			canPickup = canPickup && !isBlockedXY( coord.x+dx, coord.y-1)		// nothing on block
			if (canPickup) {
				$('.block', getCellXY(coord.x+dx, coord.y) )
					.appendTo( getCellXY( coord.x, coord.y-1 ) );
				hasBlock = true;
			}
		}
		// DROP
		else {
			var dy = -1;
			var canDrop = !isBlockedXY(coord.x+dx, coord.y+dy);
			if (canDrop) {
				while ( !isBlockedXY( coord.x+dx, coord.y+dy+1 ) ) {
					dy++;
				}
				$('.block', getCellXY(coord.x, coord.y-1) )
					.appendTo( getCellXY( coord.x+dx, coord.y+dy ) );
				hasBlock = false;
			}
		}
		return;
	}
	
	/** VALID MOVE **/
	// make sure we can perform the basic move
	var valid = !isBlockedXY(coord.x+dx, coord.y+dy);
	// if we can, check for specifics
	if (valid && key==UP) {
		valid = valid && isBlockedXY(coord.x+dx, coord.y);
		if(hasBlock) {
			valid = valid && !isBlockedXY(coord.x+dx, coord.y+dy-1)
		}
		else {
			valid = valid && !isBlockedXY(coord.x, coord.y-1);
		}
		
		if(valid) {
			$("#player").appendTo( getCellXY( coord.x+dx, coord.y+dy ) );
			if (hasBlock) {
				$('.block', getCellXY(coord.x, coord.y-1) )
						.appendTo( getCellXY( coord.x+dx, coord.y+dy-1 ) );
			}
		}
		return;
	}
	
	/** MOVE **/
	else if (valid) {
		// recalculate dy for falling
		while ( !isBlockedXY( coord.x+dx, coord.y+dy+1 ) ) {
			dy++;
		}
		$("#player").appendTo( getCellXY( coord.x+dx, coord.y+dy ) );
		
		if ( isBlockedXY( coord.x+dx, coord.y-1) && hasBlock) {
			var dy = 1;
			while ( !isBlockedXY( coord.x, coord.y+dy ) ) {
				dy++;
			}
			$('.block', getCellXY(coord.x, coord.y-1) )
				.appendTo( getCellXY( coord.x, coord.y+dy-1 ) );
			hasBlock = false;
		}
		else if (hasBlock) {
			$('.block', getCellXY(coord.x, coord.y-1) )
				.appendTo( getCellXY( coord.x+dx, coord.y+dy-1 ) );
		}
		
	}
}

function step(evt) {
	evt.preventDefault();
	var key = evt.keyCode;
	if (key == RESET) {
		
		reset();
	}
	else if (key == UP || key == DOWN || key == LEFT || key == RIGHT)
		move(key);
	
	if (direction == RIGHT) 
		$("#player").attr("class", "right");
	else
		$("#player").attr("class", "left");
}

$(document).ready( function() {	
	$(document.body).keydown(step);
	$("#levels a").click( function(evt) {
		evt.preventDefault();
		var href = $(this).attr("href");
		load(href);
	});
	load("levels/classic/01.map");
});
})(jQuery);
