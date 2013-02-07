node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Level Rooms");

function Room(data) {
    this.type = data.type;
    this.prev = data.prev;
    this.start = data.start;
    this.exit = data.exit;
    this.width = data.width;
    this.height = data.rows.length;
    this.rows = data.rows;
    
    // calculate position from previous
    if (data.x === undefined) {
        this.x = this.prev.x + (this.prev.exit - this.start);
    } else {
        this.x = data.x;
    }

    if (this.prev) {
        this.y = this.prev.top;
    } else {
        this.y = 0;
    }
    
    this.top = this.y + this.height-1;
}

Room.prototype.getRects = function(y,h) {
    var h0 = Math.floor(y / trfs.tileSize);
    var h1 = Math.floor((y+h) / trfs.tileSize);
    
    h0 = Math.max(h0, this.y);
    h1 = Math.min(h1, this.top);
    return this.rects.slice(h0 - this.y, h1 - this.y + 1);
}

Room.prototype.build = function() {
    this.rects = [];
    for (var i=0; i<this.rows.length; i++) {
 			    var row = this.rows[i];
 			    var rects = [];
 			    
 			    rects.push(new Rect((this.x - 1) * trfs.tileSize - K.SCR_WIDTH, (i+this.y) * trfs.tileSize, (row[0] + 1) * trfs.tileSize + K.SCR_WIDTH, trfs.tileSize));
 			    for (var j=1; j<this.rows.length-1; j+=2) {
 			        // middle stuff
 			    }
 			    rects.push(new Rect((this.width - row.last() + this.x) * trfs.tileSize, (i+this.y)*trfs.tileSize, (row.last() + 1) * trfs.tileSize + K.SCR_WIDTH, trfs.tileSize));
			     this.rects.push(rects);
    }
}

Room.prototype.join = function(room) {
    var data = {};
    data.type = room.type;
    data.prev = this.prev;
    
    // calculate total width
    data.x = Math.min(this.x, room.x);
    data.y = this.y;
    data.width = Math.max(this.x + this.width, room.x + room.width) - data.x;
    data.rows = [];

    dobl.core.log(this.x + "," + room.x + "," + data.x);
    dobl.core.log(this.width + "," + room.width + "," + data.width);

    
    var off = this.x - data.x;
    var rOff = data.width - this.width - off;
    dobl.core.log(off,rOff);
    data.start = this.start + off;
    for (var i=0; i<this.rows.length-1; i++) {
        var row = this.rows[i];
        var nRow = [];
        for (var j=0; j<row.length-1; j++) {
            nRow.push(row[j] + off);
        }
        nRow.push(row.last() + rOff);
        data.rows.push(nRow);
    }
    
    off = room.x - data.x;
    var rOff = data.width - room.width - off;
    dobl.core.log(off,rOff);
    data.exit = room.exit + off;
    for (var i=0; i<room.rows.length-1; i++) {
        var row = room.rows[i];
        var nRow = [];
        for (var j=0; j<row.length-1; j++) {
            nRow.push(row[j] + off);
        }
        nRow.push(row.last())+rOff;

        data.rows.push(nRow);
    }
    
    return new Room(data);

    this.height = data.rows.length;
    this.top = this.y + (this.height - 1);
}

trfs.level.createRoom = function(prev, typ) {
    var data = trfs.level._generators[typ](prev);
    data.typ = typ;
    data.prev = prev;
    
    return new Room(data);
}

trfs.level.nextType = function(prev) {
    var t = Math.linear(this.prng.rand(), 1, this._generators.length - 1);
    return t;
}

trfs.level.addRoom = function(typ) {
    var prev = this._rooms.last();
    
    typ = typ !== undefined ? typ : this.nextType(prev);
    var room = this.createRoom(prev, typ);    
    
    while(room.rows.length < this.minRoomRows) {
        var next = this.createRoom(room, this.nextType(room));
        room = room.join(next);
    }
    
    room.build();
    room.index = this._rooms.length;
    this._rooms.push(room);
    
    return room;
}

trfs.level.getRoom = function(i) {
    while (this._rooms[i+1] === undefined) {
        var room = this.addRoom();
        this.drawRoom(room);
    }
    return this._rooms[i];
}

trfs.level.getRects = function(y,h) {
    y = Math.max(y,0);
    var h0 = Math.floor(y / trfs.tileSize);
    var h1 = Math.floor((y+h) / trfs.tileSize);

    // update current room according to current room height
    while (this.currentRoom.top * trfs.tileSize < y)
        this.currentRoom = this.getRoom(this.currentRoom.index + 1);
    while (this.currentRoom.y * trfs.tileSize > y)
        this.currentRoom = this.getRoom(this.currentRoom.index - 1);
    
    var rects = [];
    for (var h=h0; h<=h1; h++) {
        var room = (this.currentRoom.top < h) ? this.getRoom(this.currentRoom.index+1) : this.currentRoom;
        rects.push(room.rects[h - room.y]);
    }
    
    return rects.flatten();
}

}