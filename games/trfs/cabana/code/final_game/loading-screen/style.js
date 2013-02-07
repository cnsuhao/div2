node.onCreateEntity = function() { node.observe(node,"load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {

//assets.require("img.loading_hills", "img.loading_sun");
var css = new JSS("loadingScr");

css.anim("loading_sun", [
    [ 0.0,  { wbktTransform: "translate(0px,190px)" }],
    [ 0.75,  { wbktTransform: "translate(0px,0px)" }],
    [ 1.0,  { wbktTransform: "translate(0px,0px)" }],
]);


css.anim("loading_hills", [
    [ 0,  { opacity: 0 } ],
    [ 0.45, { opacity: 1 } ],
    [ 0.9, { opacity: 1 } ],
    [ 1,  { opacity: 0 }],
]);

var id = node.properties.loadingScreen;
var scr = document.getElementById(id);
var duration = 12+"s";

var ground = document.createElement("div");
ground.style.width = px(K.SCR_WIDTH);
ground.style.height = px(132);
ground.style.top = px(K.SCR_HEIGHT - 132);
ground.style.backgroundColor = "blacK";
ground.style.webkitMaskImage = url(assets.img.loading_hills);

var hill = document.createElement("div");
hill.className = "hill";
hill.style.width = px(K.SCR_WIDTH);
hill.style.height = px(132);
hill.style.background = url(assets.img.loading_hills);
hill.style.webkitAnimationDuration = duration;
hill.style.webkitAnimationIterationCount = 1;
hill.style.webkitAnimationTimingFunction= "linear";
hill.style.opacity = 0;
ground.appendChild(hill);

var sun = document.createElement("div");
sun.className = "sun";
sun.style.width = px(96);
sun.style.height = px(96);
sun.style.display = "none";
sun.style.background = url(assets.img.loading_sun);
sun.style.left = px(K.SCR_WIDTH/2 - 96/2);
sun.style.top = px(K.SCR_HEIGHT/2 - 96/2 - 20/2);
sun.style.webkitAnimationDuration = duration;
sun.style.webkitAnimationIterationCount = 1;
sun.style.webkitAnimationTimingFunction= "linear";
scr.appendChild(sun);
scr.appendChild(ground);

scr.style.position = "absolute";
sun.style.position = "absolute";
hill.style.position = "absolute";
ground.style.position = "absolute";

sun.style.webkitAnimationName = "loading_sun";
hill.style.webkitAnimationName = "loading_hills";
sun.style.display = "block";

setTimeout(function() { node.properties.skippable.signal(); }, 7000);
setTimeout(function() { node.properties.complete.signal(); }, 12000);

}