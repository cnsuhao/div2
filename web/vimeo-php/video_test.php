<?php
/// sample data
$sample_data = array(
	array('id'=>5181136,'type'=>'clip'),
	array('id'=>102789,'type'=>'album'),
	array('id'=>102787,'type'=>'album')
);
/// end sample data

$clips = array();

function shrink_title($title) { return $title; }

function insert_clip($clip) { 
	global $clips;
	
	$clips[] = $clip;
	$mins = (int)($clip->duration/60); $secs = sprintf("%02d",$clip->duration%60);
	
	echo '<li class="clip"><a href="#'.$clip->clip_id.'">'.shrink_title($clip->title).'</a><span class="duration">('.$mins.':'.$secs.')</span></li>';
}

function get_clip($id) {
	$clips = simplexml_load_file('http://www.vimeo.com/api/clip/'.$id.'.xml');
	foreach ($clips->clip as $clip) insert_clip($clip);
}

function get_album($id) {
	$info = simplexml_load_file('http://www.vimeo.com/api/album/'.$id.'/info.xml');
	
	echo '<li class="album">'.$info->album->title.'</li>';
	$clips = simplexml_load_file('http://www.vimeo.com/api/album/'.$id.'/clips.xml');
	echo '<ul>';
	foreach ($clips->clip as $clip) insert_clip($clip);
	echo '</ul>';
}
?>
<html>
<head>
<style>
.info_panel > div { display:none; }
.info_panel > div.active { display:block; }
table td { padding: 0px; }
table { position: absolute; }
</style>
</head>
<body>
<div class="vimeo_player">
	<ul class="playlist"><?php 
	foreach($sample_data as $entry) : ?>
		<?php if($entry['type']=='clip') : get_clip($entry['id']); ?>
		<?php elseif($entry['type']=='album') : get_album($entry['id']); ?>
		<?php endif; 
	endforeach;?>
	</ul>
	<center>
	<div class="content">
		<table class="player_panel">
			<tr>
				<td class="prev"><a>Prev</a></td>
				<td class="video"><div>Select a video</div></td>
				<td class="next"><a>Next</a></td>
			<tr><td colspan="3" class="controls"></td></tr>
			</tr>
		</table>
		<div class="info_panel">
			<?php foreach($clips as $clip) :?>
			<div id="info_<?php echo $clip->clip_id?>" class="info">
				<h3 class="title"><?php echo $clip->title;?></h3>
				<div class="description"><?php echo $clip->caption?></div>
			</div>
			<?php endforeach; ?>
		</div>
	</div>
	</center>
</div>
<script src="swfobject.js" type="text/javascript"></script>
<script src="jquery.js" type="text/javascript"></script>
<script src="vimeo.js" type="text/javascript"></script>
</body>
</html>
