#include "AdvCameraScripts.h"
#include "Game.h"

using namespace C4;


//////////////////////////
///////   Gradual Shift Script
///////
///////    Takes a start and end goal and sets the camera based on where player is in box
//////////////////////////

CamBoxTransitionMethod::CamBoxTransitionMethod() : Method(kMethodCamBoxTransition) {
	scale		=	5.f;
	axis		=	ky;

	strtDist	=	8.f;
	strtAzm		=	0.f;
	strtAlt		=	0.f;
	strtXY		=	1.f;
	strtZ		=	2.f;

	goalDist	=	8.f;
	goalAzm		=	0.f;
	goalAlt		=	0.f;
	goalXY		=	1.f;
	goalZ		=	2.f;

	boolFlags   =   31;
}

CamBoxTransitionMethod::CamBoxTransitionMethod(float s, long ax, float sd, float saz, float sal, float sxy, float sz, float gd, float gaz, float gal, float gxy, float gz, long bf) : Method(kMethodCamBoxTransition) {
	scale		=	s;
	axis		=	ax;

	strtDist	=	sd;
	strtAzm		=	saz;
	strtAlt		=	sal;
	strtXY		=	sxy;
	strtZ		=	sz;

	goalDist	=	gd;
	goalAzm		=	gaz;
	goalAlt		=	gal;
	goalXY		=	gxy;
	goalZ		=	gz;

	boolFlags	=	bf;
}

CamBoxTransitionMethod::CamBoxTransitionMethod(const CamBoxTransitionMethod& camBoxTransitionMethod) : Method(camBoxTransitionMethod) {
	scale		=	camBoxTransitionMethod.scale;
	axis		=	camBoxTransitionMethod.axis;

	strtDist	=	camBoxTransitionMethod.strtDist;
	strtAzm		=	camBoxTransitionMethod.strtAzm;
	strtAlt		=	camBoxTransitionMethod.strtAlt;
	strtXY		=	camBoxTransitionMethod.strtXY;
	strtZ		=	camBoxTransitionMethod.strtZ;

	goalDist	=	camBoxTransitionMethod.goalDist;
	goalAzm		=	camBoxTransitionMethod.goalAzm;
	goalAlt		=	camBoxTransitionMethod.goalAlt;
	goalXY		=	camBoxTransitionMethod.goalXY;
	goalZ		=	camBoxTransitionMethod.goalZ;

	boolFlags	=	camBoxTransitionMethod.boolFlags;
}

CamBoxTransitionMethod::~CamBoxTransitionMethod() {}


Method *CamBoxTransitionMethod::Replicate(void) const {
	return (new CamBoxTransitionMethod(*this));
}

void CamBoxTransitionMethod::Execute(const ScriptState *state) {
	// break if no triggers
	if(!TheGame->GetCamTriggers()) {
		CallCompletionProc();
		return;
	}

	Node *node = GetTargetNode(state);
	if (node)
	{	// get camera
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());

		// figure out which axis we are on
		Vector3D ofs;
		float trigVal;
		float plyVal;

		switch(axis) {
		case kx:
			trigVal = node->GetWorldPosition().x;
			plyVal  = side->GetTargetEntity()->GetWorldPosition().x;
			break;
		case ky:
			trigVal = node->GetWorldPosition().y;
			plyVal  = side->GetTargetEntity()->GetWorldPosition().y;
			break;
		case kz:
			trigVal = node->GetWorldPosition().z;
			plyVal  = side->GetTargetEntity()->GetWorldPosition().z;
			break;
		}

		// subtract the two to get the shift, then use scale to get percentage "through"
		float shift = trigVal - plyVal;
		float perc  = shift/scale;

		
		// calculate all deltas
		float dDist = strtDist + (strtDist-goalDist)*perc;
		float dAzm = strtAzm + (strtAzm-goalAzm)*perc;
		float dAlt = strtAlt +(strtAlt-goalAlt)*perc;
		float dXY = strtXY + (strtXY-goalXY)*perc;
		float dZ = strtZ + (strtZ-goalZ)*perc;

		// caps
		/*if (dDist < strtDist) dDist = strtDist;
		if (dAzm  < strtAzm)  dAzm  = strtAzm;
		if (dAlt  < strtAlt)  dAlt  = strtAlt;
		if (dXY   < strtXY)   dXY   = strtXY;
		if (dZ    < strtZ)    dZ    = strtZ;


		if (dDist > goalDist) dDist = goalDist;
		if (dAzm  > goalAzm)  dAzm  = goalAzm;
		if (dAlt  > goalAlt)  dAlt  = goalAlt;
		if (dXY   > goalXY)   dXY   = goalXY;
		if (dZ    > goalZ)    dZ    = goalZ;*/

		if(goalAzm<strtAzm) {
			if (dAzm  > strtAzm)  dAzm  = strtAzm;
			if (dAzm  < goalAzm)  dAzm  = goalAzm;
		}else {
			if (dAzm  < strtAzm)  dAzm  = strtAzm;
			if (dAzm  > goalAzm)  dAzm  = goalAzm;
		}

		if(goalDist < strtDist) {
			if (dDist > strtDist) dDist = strtDist;
			if (dDist < goalDist) dDist = goalDist;
			
		}else {
			if (dDist < strtDist) dDist = strtDist;
			if (dDist > goalDist) dDist = goalDist;
		}


		if(goalAlt < strtAlt) {
			if (dAlt  > strtAlt)  dAlt  = strtAlt;
			if (dAlt  < goalAlt)  dAlt  = goalAlt;
			
		}else {
			if (dAlt  < strtAlt)  dAlt  = strtAlt;
			if (dAlt  > goalAlt)  dAlt  = goalAlt;
		}

		if(goalXY < strtXY) {
			if (dXY   > strtXY)   dXY   = strtXY;
			if (dXY   < goalXY)   dXY   = goalXY;
		}else {
			if (dXY   < strtXY)   dXY   = strtXY;
			if (dXY   > goalXY)   dXY   = goalXY;
		}

		if(goalZ < strtZ) {
			if (dZ    > strtZ)    dZ    = strtZ;
			if (dZ    < goalZ)    dZ    = goalZ;
		}else {
			if (dZ    < strtZ)    dZ    = strtZ;
			if (dZ    > goalZ)    dZ    = goalZ;
		}

		// set new values
		if((boolFlags & kbDist) != 0) { side->SetDist(dDist); }
		if((boolFlags & kbAzm)  != 0) { side->SetRotationAzm(dAzm*K::radians); }
		if((boolFlags & kbAlt)  != 0) { side->SetRotationAlt(dAlt*K::radians); }
		if((boolFlags & kbXY)   != 0) { side->SetOffsetXY(dXY); }
		if((boolFlags & kbZ)    != 0) { side->SetOffsetZ(dZ); }
	}
	
	CallCompletionProc();
}

long CamBoxTransitionMethod::GetSettingCount(void) const {
	return 17;
}


Setting *CamBoxTransitionMethod::GetSetting(long index) const {
	StringTable *table = &StringTable("text/Camera");

	if (index == 0) {
		const char *title = table->GetString(StringID('CAMR', 'CSCL'));
		return (new TextSetting('SCLE', Text::FloatToString((float) scale), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 1) {
		static const unsigned long axisID[3] =	{ 'cmkx', 'cmky', 'cmkz' };
		
		long selection = 0;
		if (axis == ky) selection = 1;
		else if (axis == kz) selection = 2;
		
		const char *title = table->GetString(StringID('CAMR', 'CAXS', 'CPMT'));
		MenuSetting *menu = new MenuSetting('AXIS', selection, title, 3);
		for (natural a = 0; a < 3; a++) menu->SetMenuItemString(a, table->GetString(StringID('CAMR', 'CAXS', axisID[a])));
		
		return (menu);
	}

	// start
	else if(index == 2) {
		const char *title = table->GetString(StringID('CAMR', 'STRT', 'DIST'));
		return (new TextSetting('SDIS', Text::FloatToString((float) strtDist), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 3) {
		const char *title = table->GetString(StringID('CAMR', 'STRT', 'RAZM'));
		return (new TextSetting('SAZM', Text::FloatToString((float) strtAzm), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 4) {
		const char *title = table->GetString(StringID('CAMR', 'STRT', 'RALT'));
		return (new TextSetting('SALT', Text::FloatToString((float) strtAlt), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 5) {
		const char *title = table->GetString(StringID('CAMR', 'STRT', 'OFFX'));
		return (new TextSetting('SOFX', Text::FloatToString((float) strtXY), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 6) {
		const char *title = table->GetString(StringID('CAMR', 'STRT', 'OFFZ'));
		return (new TextSetting('SOFZ', Text::FloatToString((float) strtZ), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}

	// goal
	else if(index == 7) {
		const char *title = table->GetString(StringID('CAMR', 'GOAL', 'DIST'));
		return (new TextSetting('GDIS', Text::FloatToString((float) goalDist), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 8) {
		const char *title = table->GetString(StringID('CAMR', 'GOAL', 'RAZM'));
		return (new TextSetting('GAZM', Text::FloatToString((float) goalAzm), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 9) {
		const char *title = table->GetString(StringID('CAMR', 'GOAL', 'RALT'));
		return (new TextSetting('GALT', Text::FloatToString((float) goalAlt), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 10) {
		const char *title = table->GetString(StringID('CAMR', 'GOAL', 'OFFX'));
		return (new TextSetting('GOFX', Text::FloatToString((float) goalXY), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	else if(index == 11) {
		const char *title = table->GetString(StringID('CAMR', 'GOAL', 'OFFZ'));
		return (new TextSetting('GOFZ', Text::FloatToString((float) goalZ), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}

	// bools
	else if(index == 12) {
		const char *title = table->GetString(StringID('CAMR', 'BOOL', 'DIST'));
		return (new BooleanSetting('BDIS', ((boolFlags & kbDist) != 0), title));
	}
	else if(index == 13) {
		const char *title = table->GetString(StringID('CAMR', 'BOOL', 'RAZM'));
		return (new BooleanSetting('BAZM', ((boolFlags & kbAzm) != 0), title));
	}
	else if(index == 14) {
		const char *title = table->GetString(StringID('CAMR', 'BOOL', 'RALT'));
		return (new BooleanSetting('BALT', ((boolFlags & kbAlt) != 0), title));
	}
	else if(index == 15) {
		const char *title = table->GetString(StringID('CAMR', 'BOOL', 'OFFX'));
		return (new BooleanSetting('BOFX', ((boolFlags & kbXY) != 0), title));
	}
	else if(index == 16) {
		const char *title = table->GetString(StringID('CAMR', 'BOOL', 'OFFZ'));
		return (new BooleanSetting('BOFZ', ((boolFlags & kbZ) != 0), title));
	}
	return (nullptr);
}




void CamBoxTransitionMethod::SetSetting(const Setting *setting)
{
	const char *text = static_cast<const TextSetting *>(setting)->GetText();

	Type identifier = setting->GetSettingIdentifier();
	if (identifier == 'SCLE') {
		scale = Text::StringToFloat(text);
	}
	else if (identifier == 'AXIS') {
		static const unsigned long axisType[3] =	{ kx, ky, kz };
		axis = axisType[static_cast<const MenuSetting *>(setting)->GetMenuSelection()];
	}

	// start
	else if (identifier == 'SDIS') {
		strtDist = Text::StringToFloat(text);
	}
	else if (identifier == 'SAZM') {
		strtAzm = Text::StringToFloat(text);
	}
	else if (identifier == 'SALT') {
		strtAlt = Text::StringToFloat(text);
	}
	else if (identifier == 'SOFX') {
		strtXY = Text::StringToFloat(text);
	}
	else if (identifier == 'SOFZ') {
		strtZ = Text::StringToFloat(text);
	}

	// goal
	else if (identifier == 'GDIS') {
		goalDist = Text::StringToFloat(text);
	}
	else if (identifier == 'GAZM') {
		goalAzm = Text::StringToFloat(text);
	}
	else if (identifier == 'GALT') {
		goalAlt = Text::StringToFloat(text);
	}
	else if (identifier == 'GOFX') {
		goalXY = Text::StringToFloat(text);
	}
	else if (identifier == 'GOFZ') {
		goalZ = Text::StringToFloat(text);
	}

	// bool
	else if (identifier == 'BDIS') {
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		if(b)  boolFlags |= kbDist;
		else   boolFlags &= ~kbDist;
	}
	else if (identifier == 'BAZM') {
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		if(b)  boolFlags |= kbAzm;
		else   boolFlags &= ~kbAzm;
	}
	else if (identifier == 'BALT') {
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		if(b)  boolFlags |= kbAlt;
		else   boolFlags &= ~kbAlt;
	}
	else if (identifier == 'BOFX') {
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		if(b)  boolFlags |= kbXY;
		else   boolFlags &= ~kbXY;
	}
	else if (identifier == 'BOFZ') {
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		if(b)  boolFlags |= kbZ;
		else   boolFlags &= ~kbZ;
	}
}


unsigned long CamBoxTransitionMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(ChunkHeader)*14 + 4*13);
}

void CamBoxTransitionMethod::Pack(Packer& data, unsigned long packFlags) const
{
	Method::Pack(data, packFlags);
	
	data << ChunkHeader('SCLE', 4);
	data << scale;

	data << ChunkHeader('AXIS', 4);
	data << axis;

	//start
	data << ChunkHeader('SDIS', 4);
	data << strtDist;

	data << ChunkHeader('SAZM', 4);
	data << strtAzm;

	data << ChunkHeader('SALT', 4);
	data << strtAlt;

	data << ChunkHeader('SOFX', 4);
	data << strtXY;

	data << ChunkHeader('SOFZ', 4);
	data << strtZ;

	//goal
	data << ChunkHeader('GDIS', 4);
	data << goalDist;

	data << ChunkHeader('GAZM', 4);
	data << goalAzm;

	data << ChunkHeader('GALT', 4);
	data << goalAlt;

	data << ChunkHeader('GOFX', 4);
	data << goalXY;

	data << ChunkHeader('GOFZ', 4);
	data << goalZ;

	//bool
	data << ChunkHeader('FLAG', 4);
	data << boolFlags;

	data << TerminatorChunk;
}


void CamBoxTransitionMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	UnpackChunkList<CamBoxTransitionMethod>(data, unpackFlags, &CamBoxTransitionMethod::UnpackChunk);
}

		
bool CamBoxTransitionMethod::UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags)
{
	switch (chunkHeader->chunkType)	{
	case 'SCLE':
		data >> scale;
		return true;
	case 'AXIS':
		data >> axis;
		return true;

	// start
	case 'SDIS':
		data >> strtDist;
		return true;
	case 'SAZM':
		data >> strtAzm;
		return true;
	case 'SALT':
		data >> strtAlt;
		return true;
	case 'SOFX':
		data >> strtXY;
		return true;
	case 'SOFZ':
		data >> strtZ;
		return true;
	
	// goal
	case 'GDIS':
		data >> goalDist;
		return true;
	case 'GAZM':
		data >> goalAzm;
		return true;
	case 'GALT':
		data >> goalAlt;
		return true;
	case 'GOFX':
		data >> goalXY;
		return true;
	case 'GOFZ':
		data >> goalZ;
		return true;

	// goal
	case 'FLAG':
		data >> boolFlags;
		return true;
	}
	return (false);
}
