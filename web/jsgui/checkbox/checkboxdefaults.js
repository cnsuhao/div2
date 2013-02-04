(function ($) {

// check to see if any of the checkboxes with given name are checked
function getCheckBoxes(name) {
	return $("input[name="+name+"][type=checkbox]");
}

function getChecked(name) {
	return getCheckBoxes(name).filter(":checked");
}

function anyChecked(name) {
	var nChecked = getChecked(name).length;
	return nChecked != 0;
}

$.fn.anyChecked = function() {
	var cb = this.get(0);
	if ( cb !== undefined && cb.tagName == "INPUT" && cb.type.toLowerCase() == "checkbox") {
		return anyChecked(cb.name);
	}
	return false;
}

$.fn.getCheckBoxes = function () {
	var cb = this.get(0);
	if ( cb !== undefined && cb.tagName == "INPUT" && cb.type.toLowerCase() == "checkbox") {
		return getCheckBoxes(cb.name);
	}
	return $();
}

$.fn.getChecked = function() {
	var cb = this.get(0);
	if ( cb !== undefined && cb.tagName == "INPUT" && cb.type.toLowerCase() == "checkbox") {
		return getChecked(cb.name);
	}
	return $();
}

$.fn.isActive = function() {
	var cb = this.get(0);
	if ( cb !== undefined) {
		if (cb.checked) return true;
	
		if (cb.name != "" && !anyChecked(cb.name)) {
			return cb.getAttribute("default") == "true" ? true : false;
		}
	}
	return false;
}

$.fn.getActive = function() {
	var cb = this.get(0);
	if ( cb !== undefined) {
		return getCheckBoxes(cb.name).filter(function() { return $(this).isActive(); });
	}
	return $();
}

$.fn.check = function(state) {
	state = state === false ? false : true;
	this.filter("input[type=checkbox]").each( function() {	
		this.checked = state;	 
	});
}

})(jQuery);