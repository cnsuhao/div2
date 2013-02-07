Importer.fetch("all")
	.require("base");

Importer.fetch("jge")
	.require("base")
	.add("js/jge/init.js")
	.add("js/jge/core/time.js");

Importer.fetch("util")
	.add("js/util/function.js")
	.add("js/util/logger.js")
	.add("js/util/object.js")
	.add("js/util/random.js")
	.add("js/util/string.js");

Importer.fetch("game")
	.require("jge")
	.require("jss")
	.add("js/game/init.js")
	.add("js/game/level.js")
	.add("js/game/player.js");

Importer.fetch("base")
	.add("js/constants.js")
	.add("js/settings.js")
	.require("util");

Importer.fetch("jss")
	.add("js/jss/init.js")
	.add("js/jss/animation.js");

