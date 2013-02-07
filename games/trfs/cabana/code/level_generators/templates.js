function startRoom(prev) {
	var room = {};

	// snip
	room.width = 16;
	room.rows = [[1,1], [5,5], [5,5], [5,5], [5,5]];

	return room;
}

function chokeRoom(prev) { 
	var room = {};

	// snip
	room.width = 16;
	room.rows = [[7,7], [7,7]];

	return room;
}

function cavernRoom(prev) { 
	var room = {};

	// snip
	room.width = 16;
	room.rows = [[0,0], [0,0], [0,0], [2,2], [2,2], [2,2], [2,2], [4,4], [4,4], [4,4], [4,4], [5,5], [5,5]];
	room.rows = randomizer(room.rows);
	room.rows = removeOneGaps(room.rows); 
	
	return room;
}

function corridorRoom(prev) { 
	var room = {};

	// snip
	room.width = 16;
	room.rows = [[0,12], [0,12], [0,12], [0,12], [0,0], [0,0], [0,0], [12,0], [12,0], [12,0], [12,0]];
	room.rows = randomizer(room.rows);
	room.rows = removeOneGaps(room.rows); 
	
	return room;
}

function staircaseRoom(prev) { 
	var room = {};

	// snip
	room.width = 16;
	room.rows = [[0,10], [0,10], [0,8], [2,8], [4,6], [4,6], [4,4], [6,4], [6,2], [6,2]];
	room.rows = randomizer(room.rows);
	room.rows = removeOneGaps(room.rows); 
	
	return room;
}

function chimneyRoom(prev) { 
	var room = {};

	// snip
	room.width = 16;
	room.rows = [[4,4], [4,4], [4,4], [4,4], [4,4], [4,4], [4,4], [4,4], [4,4], [4,4]];
	room.rows = randomizer(room.rows);
	room.rows = removeOneGaps(room.rows); 
	
	return room;
} 
