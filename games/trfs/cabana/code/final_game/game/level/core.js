node.onCreateEntity = function() {
node.observe(node, "load"); 
}
node.onLoadSignaled = function() {
setup();
node.properties.loaded.signal(); 
}
function setup() {
console.log("Level Core");

trfs.level = {};

trfs.level.minRoomRows = Math.ceil(K.SCR_HEIGHT / settings.TILE_SIZE);

trfs.level.build = function() {
    var elem = document.createElement("div");
    elem.id = "level";
    
    this.node = elem;
    
    this.sun = document.createElement("div");
    this.sun.id = "sun";
    trfs.world.appendChild(this.sun);
    trfs.world.appendChild(elem);
}

trfs.level.init = function() {
    // initialize if we have a new seed, else leave it
    if (this.seed != trfs.seed) {
        this.seed = trfs.seed;
        this.prng = new Math.Random(trfs.seed);
        
        this._rooms = [];
        this.addRoom(K.RM_START);
        this.drawRoom(this._rooms[0]);
        this.currentRoom = this.getRoom(0);
    }
}

trfs.level._generators = [];

trfs.level.register = function(name, func) {
    var k = "RM_" + name.replace(/([A-Z]+)/g, "_$1").toUpperCase();
    K[k] = trfs.level._generators.length;
    trfs.level._generators.push(func);
}

}