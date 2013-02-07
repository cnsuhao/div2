var logger = {};

logger.log = function(lvl, name, msg) {
	if(settings.DEBUG && lvl >= settings.LOG_LEVEL) {
		var s = logger.format(lvl,name,msg);
		logger.stdout(s);
	}
}

logger.debug = function(name, msg) { 
	logger.log(K.LOG_DEBUG, name, msg);
};
logger.info = function(name, msg) { 
	logger.log(K.LOG_INFO, name, msg);
};
logger.warn = function(name, msg) { 
	logger.log(K.LOG_WARN, name, msg);
};
logger.error = function(name, msg) { 
	logger.log(K.LOG_ERROR, name, msg);
};
logger.fatal = function(name, msg) { 
	logger.log(K.LOG_FATAL, name, msg);
};

logger.format = function(lvl, name, msg) {
	
	switch (lvl) {
	case K.LOG_DEBUG:
		lvl = "D";
		break;
	case K.LOG_INFO:
		lvl = "I";
		break;
	case K.LOG_WARN:
		lvl = "W";
		break;
	case K.LOG_ERROR:
		lvl = "E";
		break;
	case K.LOG_FATAL:
		lvl = "!"
		break;
	}
	
	var s = "["+lvl+"] "+name+": "+msg;
	return s
}

if (console != undefined)
	logger.stdout = function(msg) {
		console.log(msg);
	}
else
	logger.stdout = function() {};