// random %, higher means more randomization

settings = {}; 

settings.LVL_GAP = 3; 
settings.RM_CAVERN_WIDTH = [16,24];
settings.RM_CAVERN_HEIGHT = [15,20]; 
settings.RM_CAVERN_TAPER_WIDTH = .5; 
settings.RM_CAVERN_TAPER_HEIGHT = .5;


function linear(p, l, h) {
	return Math.round(p*(h-l)+l);
}

var randomChance = 40; 

function randomizer(rowArray) { 
	for(i=0;i<rowArray.length;i++) {
		var random = Math.floor(Math.random()*101);
		if(random < randomChance) 
		{
			var random = Math.round(Math.random()); 
			var randomSizeChange = Math.floor(Math.random()*3); 
			
			if(random == 1) 
			{
				var random = Math.round(Math.random());
				if(random == 1) {rowArray[0][1]-=randomSizeChange;
				if(rowArray.length > i+2) {
				rowArray[i+1][0]-=randomSizeChange;
				rowArray[i+2][0]-=randomSizeChange;}}
				else {rowArray[i][0]+=randomSizeChange;
				if(rowArray.length > i+2) {
				rowArray[i+1][0]+=randomSizeChange;
				rowArray[i+2][0]+=randomSizeChange;}}
			}
			else
			{
				var random = Math.round(Math.random());
				if(random == 1) {rowArray[i][1]-=randomSizeChange;
				if(rowArray.length > i+2) {
				rowArray[i+1][1]-=randomSizeChange;
				rowArray[i+2][1]-=randomSizeChange;}}
				else {rowArray[i][1]+=randomSizeChange;
				if(rowArray.length > i+2) {
				rowArray[i+1][1]+=randomSizeChange;
				rowArray[i+2][1]+=randomSizeChange;}}
			}
		
			
		}
		 
	}
	return rowArray;
}

function removeOneGaps(rowArray) {
	for(i=0;i<rowArray.length;i++) {
		if(i > 0 && i < rowArray.length-1) {
			var num1 = rowArray[i][0]; 
			var num2 = rowArray[i+1][0]; 
			var num3 = rowArray[i-1][0]; 
			
			if(num1 != num2 && num1 != num3) {
				rowArray[i][0] = rowArray[i-1][0]; 
			}
			
			var num4 = rowArray[i][1]; 
			var num5 = rowArray[i+1][1]; 
			var num6 = rowArray[i-1][1]; 
			
			if(num4 != num5 && num4 != num6) {
				rowArray[i][0] = rowArray[i-1][0]; 
				rowArray[i][1] = rowArray[i-1][1]; 
			}
		}
		
		else if(i == rowArray.length) {rowArray[i][1] = rowArray[i-1][1]; } 
	}
	
	return rowArray;
}
// Basic, only tests to see that everything is >= 1 space
function makePossible(rowArray, roomWidth) {
	for(i=0;i<rowArray.length;i++) {
		if(i<rowArray.length) {
		
			mySpace = [0,0]; //start point, end point of space
			mySpace[0] = rowArray[i][0]; //start point = left wall + 1 
			mySpace[1] = roomWidth - rowArray[i][1] - 1; //end point = room width - right wall - 1  
			mySpaceList = new Array(); 
			for(j=mySpace[0]; j<mySpace[1]; j++){
				mySpaceList.push(j); 
			}
			
			aboveSpace = [0,0]; //start point, end point of space
			aboveSpace[0] = rowArray[i+1][0]; //start point = left wall + 1 
			aboveSpace[1] = roomWidth - rowArray[i+1][1] - 1; //end point = room width - right wall - 1  
			aboveSpaceList = new Array(); 
			for(j=aboveSpace[0]; j<aboveSpace[1]; j++){
				aboveSpaceList.push(j); 
			}
			
		}
	}
}


var cavernWidth = [settings.RM_CAVERN_WIDTH[0], settings.RM_CAVERN_WIDTH[1]]; 
var cavernHeightPossible = [settings.RM_CAVERN_HEIGHT[0], settings.RM_CAVERN_HEIGHT[1]]; 
var cavernTaperMin = settings.LVL_GAP; 
var cavernTaperMax = settings.RM_CAVERN_TAPER_WIDTH;
var cavernTaperHeight = [.3, settings.RM_CAVERN_TAPER_HEIGHT]; 

register("cavern", function (prev) {
	var room = {};
	
	
	room.width = linear(Math.random(), cavernWidth[0], cavernWidth[1]);
	cavernHeight = linear(Math.random(), cavernHeightPossible[0], cavernHeightPossible[1]);
	cavernTaperWidth = linear(Math.random(), cavernTaperMin, Math.round(cavernTaperMax * room.width));
	cavernTaperOffset = parseInt(Math.random()*(room.width/3)) + 3;
	cavernTaperLength = linear(Math.random(), Math.round(cavernTaperHeight[0] * cavernHeight), Math.round(cavernTaperHeight[1] * cavernHeight));
	room.rows = []; 
	
	for(i=0;i < (cavernHeight-cavernTaperLength);i++)
	{
		room.rows.push([0, 0]); 
	}
	
	finalLeft = cavernTaperOffset; 
	finalRight = room.width - cavernTaperWidth - cavernTaperOffset;
	
	for(var j=1;j <= cavernTaperLength;j++) 
	{
		completion = j/cavernTaperLength; 
		if(completion < .33){completion *= 0.8;}
		if(completion > .33){completion *= 1.2;} 
		if(completion > 1){completion = 1;} 
		left = parseInt((completion * finalLeft)); 
		right = parseInt((completion * finalRight)); 
		room.rows.push([left, right]); 
		room.rows.push([left, right]); 
		
	}
	
	room.rows.pop(); 
	
	//room.rows = removeOneGaps(room.rows); 
	
	room.start = linear(Math.random(), 0, room.width); 
	room.exit = linear(Math.random(), cavernTaperOffset, (cavernTaperOffset + cavernTaperWidth));
	
	return room; 
});

var corridorWidth = [20,30]; 
var corridorEntranceWidthPossible = [2, 4]; 
var corridorExitWidthPossible = [2, 4];
var corridorEntranceExitHeight = 2;  
var corridorPointPercent = 1/3;

register("corridor", function (prev) {
	var room = {};
	room.rows = [];
	room.width = linear(Math.random(), corridorWidth[0], corridorWidth[1]);
	
	nCorridors = linear(Math.random(), 1, 4); 
	standardHeight = linear(Math.random(), 2, 3); 
	corridorPoints = [];
	side = parseInt(Math.random() * 2);
	
	
	corridorPoints.push(linear(Math.random(), 0, 10)); 
	corridorWidthPercent;
	for(i=0;i<=nCorridors+1;i++) {
		
		var x = linear(Math.random(), 	
		
		if(i%2 == 1) { 
			corridorPoints.push(linear(Math.random(), room.width-10, room.width-4)); 
		}
		else {
			corridorPoints.push(linear(Math.random(), 0, 10)); 
		}
	}
	
	for(i=1;i<=corridorPoints.length;i++) {
		if(i != corridorPoints.length) { 
			entranceHeight = linear(Math.random(), 2, 4);
			entranceWidth = linear(Math.random(), 2, 4);
			rand = linear(Math.random(), 1, 4); 
			if(rand == 1) { corridorHeight = linear(Math.random(), 2, 5);}
			else { corridorHeight = standardHeight;}
			
			for(j=0;j<entranceHeight;j++) { 
				if(i%2 == 1){room.rows.push([corridorPoints[i-1],(room.width - entranceWidth - corridorPoints[i-1])]);}
				else {room.rows.push([corridorPoints[i-1]-entranceWidth,(room.width - corridorPoints[i-1])]);}
			}
			
			for(k=0;k<corridorHeight;k++) { 
				if(i%2 == 1) {room.rows.push([corridorPoints[i-1],(room.width - corridorPoints[i])]);}
				else {room.rows.push([corridorPoints[i],(room.width - corridorPoints[i-1])]);} 
				
			}
		}
		
	}
	
	exitHeight = linear(Math.random(), 2, 5); 
	exitWidth = linear(Math.random(), 2, 5); 
	for(i=0;i<exitHeight;i++){
		if(nCorridors % 2 == 1) {room.rows.push([corridorPoints[nCorridors]-exitWidth,room.width-corridorPoints[nCorridors]]);}
		else {room.rows.push([corridorPoints[nCorridors],room.width-corridorPoints[nCorridors]-exitWidth]);}
	}

	
	room.rows.pop(); 
	
	//room.rows = removeOneGaps(room.rows); 
	
	room.start = 0; 
	room.exit = 0;
	
	return room; 
});








/*
register("Start", startRoom);

addRooms("Start", K.RM_START, 1);

register("Choke", chokeRoom);

addRooms("Choke", K.RM_CHOKE, 1);

register("Cavern", cavernRoom);

addRooms("Cavern", K.RM_CAVERN, 1);

register("Corridor", corridorRoom);

addRooms("Corridor", K.RM_CORRIDOR, 1);

register("Staircase", staircaseRoom);

addRooms("Staircase", K.RM_STAIRCASE, 1);

register("Chimney", chimneyRoom);

addRooms("Chimney", K.RM_CHIMNEY, 1);
*/ 


addRooms("Cavern", K.RM_CAVERN, 1);
addRooms("Corridor", K.RM_CORRIDOR, 1);