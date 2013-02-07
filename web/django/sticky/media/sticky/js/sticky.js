var tmp_note = '<div class="note resizeable" style="width: 300; height: 200; top: 10; left: 10;"> This is a sample note </div>'
var top_z = 5;

function setupNote(note) {
	$(note).resizable({
		handles: "all",
		stop: function() { ajaxSaveNote(this); },
		transparent: true,})
	.draggable({
		zIndex:3000,
		stop: function (ev, ui) {
			dropNote(ui.helper);
		},
	}).dblclick(function() { editNote(this) });
}



function editNote(note) {
	var text = $(note).text();
	$(note).html('<textarea>'+text+'</textarea>').children("textarea").focus().select()
		.keypress(function(e) { if(e.which==13) { e.preventDefault(); saveNote(this); } });
	$(".note").each(function() { $(this).draggable('disable'); });
}



function saveNote(ta) {
	var note = $(ta).parent();
	var text = $(ta).val();
	$(note).html(text);
	$(".note").each(function() { $(this).draggable('enable').resizable({ handles: "all", transparent: true, stop: function() { ajaxSaveNote(this) }}); });
	ajaxSaveNote(note);
}

function dropNote(note) {
	if ($(note).offset().top <= 100) {
		$(note).css('top', '0px')
	}
	$(".note").each( function() {
		var index = parseInt($(this).css('z-index'));
		$(this).css('z-index', index-1);
	});
	$(note).css('z-index',$(".note").length);
	ajaxSaveNote(note);
}

function ajaxSaveNote(note) {
	$.ajax({
		type: 'POST',
		url: '/sticky/save',
		data: { 'pk':$(note).attr('id').split('_')[1],
		    'x': $(note).offset().left,
		    'y': $(note).offset().top-100,
		    'w': $(note).css('width').split('px')[0],
		    'h': $(note).css('height').split('px')[0],
		    'text': $(note).text(),
			},
	});
}

function processNotes(data) {
	for(var n in data) {
		note = data[n]['fields'];
		var new_note = $("#notes").prepend('<div class="note">'+note['text']+'</div>').children(".note")[0];
		$(new_note).attr('id','note_'+data[n]['pk'])
			.css('top',note['y']+'px').css('left',note['x']+'px')
			.css('width',note['w']+'px').css('height',note['h']+'px')
			.css('z-index',note['ind']).html(note['text']);
		setupNote(new_note);
	}
}

function newNote() {
	$.ajax({
		type: 'GET',
		url: '/sticky/new',
		dataType: 'json',
		success:  function(data) { processNotes(data) }
	});
}

function trashNote(note) {
	$.ajax({
		type: 'POST',
		url: '/sticky/trash',
		data: { 'pk':$(note).attr('id').split('_')[1] },
	});
	$(note).remove();
}

function loadNotes() {
	$.ajax({
		type: 'GET',
		url: '/sticky/all',
		dataType: 'json',
		success: function(data) { processNotes(data) }
	});
}

function loadTrash() {
	$.ajax({
		type: 'GET',
		url: '/sticky/trashcan',
		dataType: 'json',
		success: function(data) { processTrash(data) }
	});
}

function openTrash() {
	$("body").prepend("<div id='trash_can'><h3 style='float:left'>Trash</h3>" + 
	     "<input type='button' id='close' style='float: right' value='Close'/>" + 
	     "<input type='button' id='empty' style='float: right' value='Empty'/>" + 
		"<table id='items'></table></div>");
	$("#trash_can #close").click(function() { $("#trash_can").remove() });
	$("#trash_can #empty").click(function() { $.ajax({ type:'GET', url:'/sticky/empty' }); $("#trash_can #close").trigger("click") });
	
	loadTrash();
}

function processTrash(data) {
	for(var n in data) {
		note = data[n]['fields'];
		var new_note = $("#trash_can #items").append('<tr><td colspan="3">'+note['text'].substring(0,20)+'</td><td><input class="restore" type="button" id="restore_'+data[n]['pk']+'" value="Restore"/></td><td><input type="button" class="delete" value="Delete" id="delete_'+data[n]['pk']+'"/></td></tr>');
	}
	$(".delete").click(function() {  deleteNote($(this).attr('id').split('_')[1]); $(this).parent().parent().remove() });
	$(".restore").click(function() { restoreNote($(this).attr('id').split('_')[1]) });
}

function restoreNote(id) {
	$.ajax({
		type: 'GET',
		url: '/sticky/restore',
		data: {'pk':id},
		dataType: 'json',
		success:  function(data) { processNotes(data) }
	});
}

function deleteNote(id) {
	$.ajax({
		type: 'POST',
		url: '/sticky/delete',
		data: {'pk':id},
	});
}

$(document).ready( function() {
	loadNotes();
	$("#new").click(newNote);
	$("#trash").droppable({ 
		accept: ".note", 
		tolerance: "pointer", 
		hoverClass: "inv", 
		drop: function(ev, ui) { 
			trashNote(ui.draggable);
			$("#trash_can #close").trigger('click');
		},
	}).dblclick(function(e) {
		openTrash();
	});
});
