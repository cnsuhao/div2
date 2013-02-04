(function($) {


function checkDefault(node) {
	var fieldset = $(node).parents("fieldset");
	if ( $(node).anyChecked() ) {
		fieldset.removeClass("default");
	} else {
		fieldset.addClass("default");
	}
}

function updateHash() {
	var vals = {};
	
	$(".filter").each(function() {
		var bits = $(this).find("input").getChecked().map(function(i,e) { 
			return 1 << ($(e.parentNode).index() - 1); 
		});
		var v = 0;
		for (var i=0; i<bits.length; i++) {
			v = v | bits[i];
		}
		
		if (v != 0) 
			vals[this.id] = v;
	});
	
	var hash = [];
	for (k in vals) {
		hash.push(k + "=" + vals[k]);
	}
	
	if (hash.length > 0)
		location.hash = "!" + hash.join(",");
	else
		location.hash = "";
}

function loadInfo() {
	var info = document.getElementById("info");
	info.innerHTML = "";
	$(".filter").each(function() {
		var p = document.createElement("p");
		p.innerHTML = $("legend",this).text();
		var labels = $("input", this).getActive().map(function(i,e) { return $(this).next().text(); });
		p.innerHTML += ": " + labels.toArray().join(", ");
		info.appendChild(p);
	});
}

function update(node) {
	checkDefault(node);
	updateHash();
	loadInfo();
}

$(document).ready(function() {
	// before anything else, load the hash
	var h = location.hash;
	if (h != "" && h.indexOf("!") != -1) {
		h = h.split("!")[1];
		chunks = h.split(",");

		// parse hash
		var d = {};
		for (var i=0; i<chunks.length; i++) {
			var parts = chunks[i].split("=");
			d[parts[0]] = parseInt(parts[1]);
		}
		
		// take our parts and go through each valid fieldset
		for (k in d) {
			var fieldset = document.getElementById(k);
			$(fieldset).find("input[type=checkbox]").map(function(i, e) {
				var idx = $(this.parentNode).index() - 1;
				this.checked = d[k] & (1 << idx);
			});
		}
	}

	/ * events */
	// when anything is checked, run update
	$(".filter input").change(function() { update(this); });
	
	// select label only
	$(".filter label").dblclick(function() {
		var chkbx = $(this).prev().get(0);
		$(this)
			.prev()
			.getCheckBoxes()
			.check(false);
		chkbx.checked = true;
		update(chkbx);
		// default event will check box
	});
	
	// toggle all on/off
	$(".filter legend").dblclick(function() {
		var fieldset = this.parentNode;
		var cb = $(fieldset).find("input").getCheckBoxes();
		var nInp = cb.length;
		var nChk = cb.getChecked().length;
		cb.check(nInp != nChk);
		update(cb.get(0));
	});
	
	$(".filter div:last-child input").each(function() { checkDefault(this); });
	updateHash();
	loadInfo();
});

})(jQuery);