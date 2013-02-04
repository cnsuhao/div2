var SCROLL_POINT = 0;

function build_navbar(data) {
	data = data["lipsum"].split(".")[0];
	data = data.split(" ");
	var list = document.createElement("ul");
	
	var html = "";
	for (var i=0 ;i<data.length; i++) {
		html += "<li><a href='#'>"+data[i]+"</a></li>";
	}
	$(list).html(html);
	$("#navbar").append(list);
}

function build_lorem(data) {
	data = data["lipsum"];
	
	var html = "";
	for (var i=0; i<data.length; i++) {
		html += "<p>"+data[i]+"<br/><a class='btt' href='#top'>Back to Top</a></p>";
	}
	$("#lorem").html(html);
}

$(document).ready( function() {	
	// grab all back to top links
	$("a.btt").click(function(evt) {
		SCROLL_POINT = $("body").scrollTop();

        $(window).scrollTop(0);
        $("#return").fadeIn("slow");
	});

	// setup return button
	$("#return").click( function(evt) {
		$("#return").hide();
		$("body").scrollTop(SCROLL_POINT);
	});
});
