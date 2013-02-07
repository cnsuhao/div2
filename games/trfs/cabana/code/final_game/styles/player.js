node.onCreateEntity = function () {
node.observe(node, "load"); 
}
node.onLoadSignaled = function () { setup(); node.properties.loaded.signal(); }

function setup() {
console.log("Player Style");

var tile = settings.TILE_SIZE;
var ply = new JSS("player");

/*******************\
   BASE
\*******************/
ply.add("#player", {
    width: px(tile),
    height: px(2*tile),
    display: "block",
    top: px(-2*tile),
    overflow: "visible",
    zIndex: 20,
});

ply.add("#player" + " .sprite", {
    width: px(tile),
    height: px(tile*10),
    wbktAnimationTimingFunction: "linear",
    background: url(assets.img.troll),
    clip: "rect(10px, 36px, 72px, 0px)",
});

/*
ply.anim("spinCW", [
    [ 0, { wbktTransform: "rotate(0deg)" } ],
    [ 1, { wbktTransform: "rotate(360deg)" } ],
]);

ply.anim("spinCCW", [
    [ 0, { wbktTransform: "scale(-1,1) rotate(0deg)" } ],
    [ 1, { wbktTransform: "scale(-1,1) rotate(360deg)" } ],
]);
*/
/*******************\
   RUN
\*******************/
ply.add("#player" + attr("state", K.PLY_RUN) + " .sprite", {
    wbktAnimationName: "run",
    wbktAnimationIterationCount: "infinite",
    wbktAnimationTimingFunction: "cubic(1,0,0,1)",
    wbktAnimationDuration: "0.5s",
//    backgroundColor: "red",
    clip: "rect(10px, 36px, 72px, 0px)",
});

ply.add("#player" + attr("inSun", 0) + attr("state", K.PLY_RUN) + " .sprite", { 
    top: px(-2*tile),
    clip: "rect(46px, 36px, 108px, 0px)",
});
//ply.add("#player" + attr("inSun", 0) + attr("state", K.PLY_IDLE) + " .sprite", { top: px(-2*tile) });

ply.anim("run", [
    [  0.0,  { backgroundPosition: "-36px -33px" } ],
    [ -0.25, { backgroundPosition: "-72px -33px" } ],
    [ -0.5,  { backgroundPosition: "-36px -33px" } ],
    [ -0.75,  { backgroundPosition: "0px -33px" } ],
    [ -1.0,  { backgroundPosition: "-36px -33px" } ],
]);

ply.add("#player" + attr("state", K.PLY_RUN) + attr("facing", K.LEFT) + " .sprite", {
    wbktTransform: "scale(-1,1)"
});

ply.add("#player" + attr("inSun", 0) + attr("state", K.PLY_RUN) + " .sprite", { 
    top: px(-2*tile),
    clip: "rect(82px, 36px, 144px, 0px)",
});


/*******************\
   TURN
\*******************/
ply.add("#player" + attr("state", K.PLY_TURN) + " .sprite", {
    backgroundPosition: "-36px -33px",
//    backgroundColor: "yellow",
});

ply.add("#player" + attr("inSun", 0) + attr("state", K.PLY_TURN) + " .sprite", { 
    backgroundPosition: "-36px -105px",
});

ply.add("#player" + attr("state", K.PLY_TURN) + attr("facing", K.LEFT) + " .sprite", {
    wbktTransform: "scale(-1,1)"
});


/*******************\
   STUN
\*******************/
ply.add("#player" + attr("state", K.PLY_STUN) + " .sprite", {
    backgroundPosition: "-36px -33px",
//    backgroundColor: "orange",
});

ply.add("#player" + attr("inSun", 0) + attr("state", K.PLY_STUN) + " .sprite", { 
    backgroundPosition: "-36px -105px",
});

ply.add("#player" + attr("state", K.PLY_STUN) + attr("facing", K.RIGHT) + " .sprite", {
    wbktTransform: "scale(-1,1)"
});


/*******************\
   ROLL/JUMP/DASH
\*******************/
ply.add("#player" + attr("state", K.PLY_ROLL) + ", " +
        "#player" + attr("state", K.PLY_DASH) + ", " +
        "#player" + attr("state", K.PLY_JUMP), {
    height: px(tile),
    top: px(-tile),
});

ply.add("#player" + attr("state", K.PLY_ROLL) + " .sprite, " +
        "#player" + attr("state", K.PLY_JUMP) + " .sprite", {
    wbktAnimationIterationCount: "infinite",
    wbktAnimationTimingFunction: "cubic(1,0,0,1)",
    backgroundPosition: "0px 0px",
    wbktTransformOrigin: "18px 18px",
    wbktAnimationDuration: "0.8s",
    clip: "rect(0px, 36px, 36px, 0px)",
});

/*
ply.add("#player" + attr("state", K.PLY_ROLL) + " .sprite", { backgroundColor: "#f0f" });
ply.add("#player" + attr("state", K.PLY_JUMP) + " .sprite", { backgroundColor: "#08f" });
*/

ply.add("#player" + attr("state", K.PLY_DASH) + " .sprite", { 
//    backgroundColor: "#0ff",
    backgroundPosition: "-36px 0px",
    wbktAnimationDuration: "0.3s",
    wbktAnimationIterationCount: "infinite",
    wbktAnimationTimingFunction: "cubic(1,0,0,1)",
    clip: "rect(0px, 36px, 36px, 0px)", 
    wbktTransformOrigin: "18px 18px",
});

ply.add("#player" + attr("state", K.PLY_ROLL) + attr("inSun", 1) + " .sprite," + 
        "#player" + attr("state", K.PLY_JUMP) + attr("inSun", 1) + " .sprite", {
    backgroundPosition: "-73px 0px",
});


ply.add("#player" + attr("state", K.PLY_ROLL) + attr("facing", K.LEFT) + " .sprite," + 
        "#player" + attr("state", K.PLY_DASH) + attr("facing", K.LEFT) + " .sprite," +
        "#player" + attr("state", K.PLY_JUMP) + attr("facing", K.LEFT) + " .sprite", {
    wbktAnimationName: "spinCCW",
    
});

ply.add("#player" + attr("state", K.PLY_ROLL) + attr("facing", K.RIGHT) + " .sprite," + 
        "#player" + attr("state", K.PLY_DASH) + attr("facing", K.RIGHT) + " .sprite," +
        "#player" + attr("state", K.PLY_JUMP) + attr("facing", K.RIGHT) + " .sprite", {
    wbktAnimationName: "spinCW",
    
});


/*******************\
   WALL SLIDE
\*******************/
ply.add("#player" + attr("state", K.PLY_WALL) + " .sprite", {
//    backgroundColor: "#0f0",
    clip: "rect(16px, 33px, 72px, 0px)",
    backgroundPosition: "-1px -252px",
});

ply.add("#player" + attr("state", K.PLY_WALL) + attr("inSun", 0) + " .sprite", {
    backgroundPosition: "-37px -252px",
});

ply.add("#player" + attr("state", K.PLY_WALL) + attr("facing", K.LEFT) + " .sprite", {
    wbktTransform: "scale(-1,1)"
});


///////////
///////////   EOF
///////////
}
