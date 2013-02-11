var tests = {
	player: {
		update: function(init) {
			if (init != true) {
				var node = this;
				
				tests.player.props[node.name] = node.value;
			}
									
			tests.player.props.node.setAttribute("state", tests.player.props.state);
			tests.player.props.node.setAttribute("facing", tests.player.props.facing);
			tests.player.props.node.innerHTML = tests.player.props.node.innerHTML;
		},
		init: function() {
			// initialize state
			tests.player.props.state = "idle";
			tests.player.props.facing = "right";
			tests.player.props.node = document.getElementById("player");
			
			// create state nodes
			var stateDesc = document.createElement("h4");
			var stateList = document.createElement("ul");
			
			// set state title
			stateDesc.innerHTML = K.INFO.STATES._desc + ":";
			
			// setup state inputs
			for (var k in K.INFO.STATES) {
				// skip if a property param
				if (k.startsWith('_') || !K.INFO.STATES.hasOwnProperty(k)) continue;
				
				// make nodes
				var li = document.createElement("li");
				var input = document.createElement("input")
				var desc = document.createElement("label")
				
				// add nodes
				li.appendChild(input);
				li.appendChild(desc);
				stateList.appendChild(li);
				
				// update label
				desc.htmlFor = "state";
				desc.innerHTML = K.INFO.STATES[k]._desc;
				
				// update input node
				input.type = "radio";
				input.name = "state";
				input.value = k;
				input.addEventListener("click", tests.player.update, false);
				if (k == tests.player.props.state)
					input.checked = true;
			}
			// add state nodes to config
			document.playerConfig.appendChild(stateDesc);
			document.playerConfig.appendChild(stateList);

			// create facing nodes
			var facingDesc = document.createElement("h4");
			var facingList = document.createElement("ul");
			
			// set facing title
			facingDesc.innerHTML = K.INFO.FACING._desc + ":";
			
			// setup facing inputs
			for (var k in K.INFO.FACING) {
				// skip if a property param
				if (k.startsWith('_') || !K.INFO.FACING.hasOwnProperty(k)) continue;
				
				// make nodes
				var li = document.createElement("li");
				var input = document.createElement("input")
				var desc = document.createElement("label")
				
				// add nodes
				li.appendChild(input);
				li.appendChild(desc);
				facingList.appendChild(li);
				
				// update label
				desc.htmlFor = "state";
				desc.innerHTML = K.INFO.FACING[k]._desc;
				
				// update input node
				input.type = "radio";
				input.name = "facing";
				input.value = k;
				input.addEventListener("click", tests.player.update, false);
				if (k == tests.player.props.facing)
					input.checked = true;
			}
			
			// add facing nodes to config
			document.playerConfig.appendChild(facingDesc);
			document.playerConfig.appendChild(facingList);
			
			// initialize
			tests.player.update(true);
		}
	},
	
	platform: {
		props: {
			maxWidth: 16,
			maxHeight: 6
		},
		update: function(init) {
			var dim = { width: tests.platform.props.width, height: tests.platform.props.height };
			
			if (init != true) {
				try {
					dim[this.name] = parseInt(this.value);
				}
				finally {}
			}
			w = dim.width;
			h = dim.height;
			
			w = Math.max(1, w);
			h = Math.max(1, h);
			w = Math.min(w, tests.platform.props.maxWidth);
			h = Math.min(h, tests.platform.props.maxHeight);
			
			tests.platform.props.width = w;
			tests.platform.props.height = h;
			document.platformConfig.width.value = w;
			document.platformConfig.height.value = h;
			tests.platform.props.node.style.width = (w * 36) + "px"
			tests.platform.props.node.style.height = (h * 36) + "px"
		},
		init: function() {
			// initial value
			tests.platform.props.width = 4;
			tests.platform.props.height = 2;
			tests.platform.props.node = document.getElementById("platform");
			
			document.platformConfig.width.addEventListener("blur", tests.platform.update, false);
			document.platformConfig.height.addEventListener("blur", tests.platform.update, false);
			
			document.getElementById("pltWMax").innerHTML = tests.platform.props.maxWidth;
			document.getElementById("pltHMax").innerHTML = tests.platform.props.maxHeight;
			
			// initialize
			tests.platform.update(true);
		}
	}
}

// setup
function setup() {
	for (var k in tests) {
		if (tests[k].props == undefined)
			tests[k].props = {};
		tests[k].init();
	}
}
document.addEventListener("DOMContentLoaded", setup, false);