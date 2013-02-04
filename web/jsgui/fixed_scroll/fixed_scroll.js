function build_lorem(data) {
	data = data["lipsum"];
	
	for (var i=0; i<data.length; i++) {
		var p = document.createElement("p");
		$(p).html(data[i]+"<a href='#top' class='btt'>Back To Top</a>");
		$("#content").append(p);
	}
}

var margin = 24
var SCROLL_THRESHOLD = 30;
var bottom_bound = 0;

var prevScroll = $(window).scrollTop();
function scroll2pos() {
	var scrollPos = $(window).scrollTop();
	if (scrollPos<300) {
		$("#floating_box").css({
			"position": "relative",
			"top":margin+"px"
		});
	}
	else if (scrollPos>=bottom_bound) {
		$("#floating_box").css({
			"position": "relative",
			"top": (bottom_bound-$("#content").offset().top+24)+"px"
		});
	}
	else if (scrollPos>=300) {
		$("#floating_box").css({
			"position": "fixed",
			"top": margin+"px"
		});
	}
	prevScroll = scrollPos;
}

$(document).ready(function () {
	$(window).scroll(scroll2pos);
	
	var h = $("#content").offset().top + $("#content").height();
	h -= $("#floating_box").outerHeight();
	h -= margin;
	
	bottom_bound = h;
	scroll2pos();
});