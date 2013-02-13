node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Level Render");

trfs.level.drawRoom = function(room) {
    room.node = document.createElement("div");
    room.node.className = "room";
    
    var width = room.width * trfs.tileSize;
    var height = room.rows.length * trfs.tileSize;
    
    room.node.style.width = px(width);
    room.node.style.height = px(height);
    room.node.style.top = px(-trfs.tileSize * room.y - height);
    //room.node.style.left = px(trfs.tileSize * room.x);
	
    for (var i=0; i<room.rects.length; i++) {
        var row = room.rects[i];
        var rowNode = document.createElement("div");
        rowNode.className = "row";
        
        rowNode.style.width = px(width);
        rowNode.style.top = px(height-(i*trfs.tileSize));
        
        for (var j=0; j<row.length; j++) {
            var rect = document.createElement("div");
            rect.style.left = px(row[j].x);
            rect.style.width = px(row[j].w);
            rowNode.appendChild(rect);
        }
        room.node.appendChild(rowNode);
    }		
    /*
    var container = document.createElement("div");
    var day = room.node;
    var night = room.node.cloneNode(true);
    day.className += " day";
    night.className += " night";
    container.appendChild(night)
    container.appendChild(day)
    room.node = container;
    */
    trfs.level.node.appendChild(room.node);
    
    return room.node;
}

trfs.level.render = function() {

}

}