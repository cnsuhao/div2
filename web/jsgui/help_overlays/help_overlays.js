function drawOverlays() {
	$('*[help]').each(function (i) {
		var overlay = document.createElement('div');
		var h = $(this).outerHeight();
		var w = $(this).outerWidth();
		var off = $(this).offset();
		$(overlay).height(h-4).width(w-4);
		$(overlay).css({top:off.top, left:off.left});
		$(overlay).appendTo("#overlays");
		$(overlay).addClass('help_overlay');
		$(overlay).data("node", this);
	});
}

function showHelp(node) {
	var node = $(node).data('node');
    
	var div = document.createElement('div');
	$(div).text($(node).attr("help"));
	$(div).prependTo("#instructions");
	$(div).show()
	.animate({opacity: 1.0}, 3000)
	.fadeOut("slow", function() { $(this).remove() });
}

function enableHelp(evt) {
	$('body').click(overlayClick);
	//draw overlays
	drawOverlays();
	$('#enable_help').each( function(i) {
		$(this).unbind('click',enableHelp);
	});
}

function overlayClick(evt) {
	$('body').addClass("get_help");
	
	if ($(evt.target).hasClass('help_overlay')) {
		showHelp(evt.target);
		disableHelp();
	}
}

function disableHelp() {
	$('#overlays').html("");
	$('body').removeClass("get_help").unbind('click',overlayClick);
	
	$('#enable_help').each( function(i) {
		$(this).click(enableHelp);
	});
}

$('document').ready(function () {
	$('#enable_help').each( function(i) {
		$(this).click(enableHelp);
	});
});
