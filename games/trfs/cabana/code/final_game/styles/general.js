node.onCreateEntity = function() { node.observe(node, "load"); }
node.onLoadSignaled = function() { setup(); node.properties.loaded.signal(); }
function setup() {

var tile = settings.TILE_SIZE;
var css = new JSS("general");

css.add("#trfs div, #trfs canvas", {
    position: "absolute",
    padding: px(0),
    margin: px(0),
    border: px(0)
});

css.add("#trfs", {
    width: px(K.SCR_WIDTH),
    height: px(K.SCR_HEIGHT),
});

css.add("#world", {
    width: px(K.SCR_WIDTH),
    height: px(K.SCR_HEIGHT),
    top: px(K.SCR_HEIGHT - 2*tile),
    left: px(K.SCR_WIDTH/2),
});

/**************************\
 *    INITIAL OVERLAY     *
\**************************/
css.anim("pulse", [
    [ 0, { color: "rgba(255,255,255,0.02)" } ],
    [ 1, { color: "rgba(255,255,255,0.3)" } ],
]);


css.add("#overlay.ready", {
    width: px(K.SCR_WIDTH),
    height: px(K.SCR_HEIGHT),
    lineHeight: px(K.SCR_HEIGHT),
    backgroundColor: "rgba(0, 0, 0, 0.5)",
    fontWeight: "bold",
    fontSize: px(40),
    zIndex: 1000,
    wbktAnimationName: "pulse",
    wbktAnimationDuration: "2s",
    wbktAnimationIterationCount: "infinite",
    wbktAnimationDirection: "alternate",
    wbktAnimationTimingFunction: "linear",
});

}