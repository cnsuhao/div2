node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Level Generators");

trfs.level.register("start", function(prev) {
    return { 
        prev: null, 
        x: 0, 
        width: 13, 
        start: 0, 
        exit: 9, 
        rows: [
            [0,0],
            [0,0],
            [8,0],
            [8,1],
            [8,1],
            [8,1]
        ]
    };
});

trfs.level.register("test_room1", function(prev) {
    var room = {};
    room.width = 18;
    room.rows = [ 
        [0,11],           // |      ************|
        [0,3],            // |               ***|
        [0,3],            // |               ***|
        [12, 3],          // |************   ***|
        [12, 3],          // |************   ***|
        [12, 3],          // |************   ***|
        [2, 3],           // |**             ***|
        [2, 3],           // |**             ***|
        [2, 3],           // |**             ***|
        [4, 7],           // |****       *******|
        [8, 7],           // |********   *******|
        [8, 7],           // |********   *******|
        [8, 7],           // |********   *******|
        [7, 6],           // |********   *******|
    ];
    room.start = 3;
    room.exit = 9;
    
    return room;
});

}