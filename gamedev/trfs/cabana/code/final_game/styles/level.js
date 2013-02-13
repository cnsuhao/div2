node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {
console.log("Level Style");

var tile = settings.TILE_SIZE;
var lvl = new JSS("level");

// temp level bounds for camera test
lvl.add("#level .room", {
    zIndex: 10,
});

lvl.add("#level  .row div", {
    height: px(tile),
    top: px(-tile),
    backgroundImage: url(assets.img.tileDay)
});

}