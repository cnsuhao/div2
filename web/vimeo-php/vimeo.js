var PLAYER_ID = 0;
function VimeoPlayer(node,width,height) {
	this.player_id = PLAYER_ID;
	PLAYER_ID+=1;
	this.url = "http://www.vimeo.com/moogaloop.swf";
	this.version = "9.0.0";
	this.expressInstall = false;
	this.flashvars = {
		'clip_id':'',
		'server':'vimeo.com',
		'width':width+"",
		'height':height+"",
		'show_title':'0',
		'show_byline':'0',
		'show_portrait':'0',
		'autoplay':'0',
		'loop':'0',
	};
	this.params = {};
	this.attributes = {};
	
	// initialize the node
	if ($(node).attr('id') == undefined || $(node).attr('id')=='') $(node).attr('id','vimeo_player_'+this.player_id);
	this.node = $(node).attr('id');
}

VimeoPlayer.prototype.load_video = function(id) {
	this.flashvars.clip_id = id+"";
	swfobject.embedSWF(this.url, this.node, this.flashvars.width, this.flashvars.height, this.version, this.expressInstall, this.flashvars);
}

VimeoPlayer.prototype.setFlashVar  = function(key,value) { this.flashvars[key] = value+""; }
VimeoPlayer.prototype.setParam     = function(key,value) { this.params[key] = value+""; }
VimeoPlayer.prototype.setAttribute = function(key,value) { this.attributes[key] = value+""; }

var player;
$(document).ready(function() {
	player = new VimeoPlayer($(".video"),320,240);
	$('li.clip a').click(function(evt) {
		var id = $(this).attr('href').substring(1);
		player.load_video(id);
		$('.info_panel div.active').removeClass('active');
		$('#info_'+id).addClass('active');
	});
	
	// auto click first one
	var url = document.location.hash;
	if (url!='') {
		player.load_video(url,true);
		$('#info_'+url).addClass('active');
	}
	player.flashvars.autoplay = '1';
});