$(function(){
	$.extend($.fn.disableTextSelect = function() {
		return this.each(function(){
			if($.browser.mozilla){//Firefox
				$(this).css('MozUserSelect','none');
			}else if($.browser.msie){//IE
				$(this).bind('selectstart',function(){return false;});
			}else{//Opera, etc.
				$(this).mousedown(function(){return false;});
			}
		});
	});
});

NODE_COUNTS = {};
function auto_id(node) {
	if (node.id !== "") return
	
	var id = node.className;
	if (id === "") {
	  id = node.tagName;
	}
	
	if (NODE_COUNTS[id] === undefined) 
		NODE_COUNTS[id]=0;
	
	var result = id+"_"+NODE_COUNTS[id].toString();
	NODE_COUNTS[id]++;
	return result;
}

var BUTTONS = {}
function setup_buttons() {
	var html = "<table>";
	for( var i=0; i<4; i++) {
		html+="<tr>";
		for ( var j=0; j<4; j++ ) {
			var c = (j+i*4+1).toString();
			if (c.length==1)
				c = '0'+c;
			html+="<td id='button_"+c+"'>"+c+"</td>";
			BUTTONS["button_"+c] = true;
		}
		html+="</tr>";
	}
	html+="</table>";
	$("#buttons").html(html);
	
	$("#buttons td").mousedown( function(evt) {
		$(evt.target).toggleClass("on");
	});
	$("body").mouseup( function(evt) {
		$("#buttons td").each( function() {
			if (BUTTONS[this.id])
				$(this).removeClass("on");
		});
	});
	
	$("#buttons td").addClass("keybind");
}

function setup_settings() {
	var html = "<form><table>";
	for( var i=0; i<4; i++) {
		html+="<tr>";
		for ( var j=0; j<4; j++ ) {
			var c = (j+i*4+1).toString();
			if (c.length==1)
				c = '0'+c;
			
			if (i<2) {
				var checked = " checked='true'";
				BUTTONS["button_"+c] = false;
			}
			else
				var checked = "";
			html+="<td>"+c+"<input type='checkbox' name='button_"+c+"' id='chk_button_"+c+"'"+checked+"/></td>";
		}
		html+="</tr>";
	}
	html += "</table></form>";
	$("#settings").html(html);
	
	$("#settings input").change( function(evt) {
		BUTTONS[evt.target.name] = !evt.target.checked;
		$("#"+evt.target.name).removeClass("on");
	});
}

function setup_overlay () {
	$(".keybind").each(function() {
		var div = document.createElement("div");
		$(div)
			.addClass("keybind_overlay")
			.height( $(this).outerHeight()-4 )
			.width( $(this).outerWidth()-4 )
			.attr("overlays", this.id)
			.css({
				top: parseInt($(this).offset().top),
				left: parseInt($(this).offset().left)
			});
		$("body").append(div);
	});

	$(".keybind_overlay").live("click", function(evt) {
		$(".selected").removeClass("selected");
		$(evt.target).addClass("selected");
	});
	
	$("#setup").click( function(evt) {
		$("body").toggleClass("keybinding");
		$(".on").removeClass("on");
		render_mapped();
	});
}

function setup_button(code,btn) {
	KEYFUNCTIONS[code] = btn;
}

function render_mapped() {
	$(".mapping").remove();
	if( ! $("body").hasClass("keybinding") ) return
	
	for(var k in KEYFUNCTIONS) {
		var id = "#"+KEYFUNCTIONS[k];
		
		var span = document.createElement("span");
		$(span)
			.text( KEYCODES[k] )
			.addClass("mapping");
		auto_id(span);
		
		var top  = $(id).offset().top;
		var left = $(id).offset().left;
		$("body").append(span);
		
		var w = $(id).outerWidth() - $(span).outerWidth();
		var h = parseInt($(id).outerHeight()) - parseInt($(span).outerHeight());
		top+=h;
		left+=w;
		
		$(span).css( {
			top:top,
			left:left
		});
	}
}

KEYFUNCTIONS = {}
$(document).ready(function () {
	setup_buttons();
	setup_settings();
	setup_overlay();
	
	$(window).keydown( function(evt) {
		if ( ! $("body").hasClass("keybinding") && evt.keyCode in KEYFUNCTIONS) {
			evt.preventDefault();
			var id = KEYFUNCTIONS[evt.keyCode]
			if (!BUTTONS[id])
				$("#"+id).toggleClass("on");
			else
				$("#"+id).addClass("on");
		}
		else if (evt.keyCode==27 && $("body").hasClass("keybinding")) {
			evt.preventDefault();
			$("body").removeClass("keybinding");
			$(".on").removeClass("on");
			render_mapped();
		}
		else if ( $("body").hasClass("keybinding") && evt.keyCode != 116) {
			evt.preventDefault();
			var selected = $(".selected");
			if (selected.length>0) {
				selected = selected[0];
				$(selected).removeClass("selected");
				setup_button(evt.keyCode,$(selected).attr("overlays"));
				render_mapped();
			}
		}
	});
	
	$(window).keyup( function(evt) {
		if ( ! $("body").hasClass("keybinding") && evt.keyCode in KEYFUNCTIONS) {
			evt.preventDefault();
			var id = KEYFUNCTIONS[evt.keyCode];
			if (BUTTONS[id])
				$("#"+id).removeClass("on");
		}
	});
	
	$("#buttons td").disableTextSelect();
});