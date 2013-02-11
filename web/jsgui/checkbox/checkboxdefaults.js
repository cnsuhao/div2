(function ($) {

// basic checkbox plugin function template/decorator
function checkboxFn(fn, returned) {
    return function() {
        var chkbx = this.get(0);
        if (chkbx !== undefined && chkbx.tagName == "INPUT" && chkbx.type.toLowerCase() == "checkbox") 
            return fn(chkbx.name);
        else
            return returned;
    }
}


// Checkbox helper functions
function getCheckBoxes(name) { return $("input[name="+name+"][type=checkbox]"); }
function getChecked(name) { return getCheckBoxes(name).filter(":checked"); }
function anyChecked(name) { return (getChecked(name).length) != 0; }


// checkbox jquery functions
$.fn.anyChecked = checkboxFn(anyChecked, false);
$.fn.getCheckBoxes = checkboxFn(getCheckBoxes, $());
$.fn.getChecked = checkboxFn(getChecked, $());


// active as opposed to checked.  If a checkbox is active if nothing
// else in it's group is checked AND it has the default attribute
$.fn.isActive = function() {
	var chkbx = this.getCheckBoxes().get(0);
	if ( chkbx !== undefined) {
		if (chkbx.checked) return true;
	
		if (chkbx.name != "" && !anyChecked(chkbx.name)) {
			return chkbx.getAttribute("default") == "true";
		}
	}
	return false;
}

// fetch all active boxes
$.fn.getActive = function() {
    return this.getCheckBoxes().filter(function() { return $(this).isActive(); });
}

// check all selected boxes
$.fn.check = function(state) {
	state = (state === false) ? false : true; // type correction
	this.filter("input[type=checkbox]").each( function() {	
		this.checked = state;
	});
}

})(jQuery);
