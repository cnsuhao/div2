node.onCreateEntity = function() { node.observe(node, "newGame"); node.observe(node, "restart"); }
node.onNewGameSignaled = function() { trfs.build(); trfs.reset(); trfs.play(); node.properties.fired.signal(); }
node.onRestartSignaled = function() { trfs.reset(trfs.seed); trfs.play(); node.properties.fired.signal(); }
