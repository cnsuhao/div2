#include "CameraScripts.h"
#include "Game.h"

using namespace C4;

//////////////////////////
///////   No Var
//////////////////////////

// reset cam method
ResetCameraMethod::ResetCameraMethod() : Method(kMethodCamReset) {}
ResetCameraMethod::ResetCameraMethod(const ResetCameraMethod& resetCameraMethod) : Method(resetCameraMethod) {}
ResetCameraMethod::~ResetCameraMethod() {}

Method *ResetCameraMethod::Replicate(void) const{
	return (new ResetCameraMethod(*this));
}

void ResetCameraMethod::Execute(const ScriptState *state) {
	if(TheGame->GetCamTriggers()) {
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
		side->ResetHeight();
	}
	
	CallCompletionProc();
}



//////////////////////////
///////   One Var
//////////////////////////

// set camera distance
CamDistMethod::CamDistMethod() : Method(kMethodCamDist) {
	dist = 8.f;
}

CamDistMethod::CamDistMethod(float d) : Method(kMethodCamDist) {
	dist = d;
}

CamDistMethod::CamDistMethod(const CamDistMethod& camDistMethod) :	Method(camDistMethod) {
	dist = camDistMethod.dist;
}

CamDistMethod::~CamDistMethod() {}

Method *CamDistMethod::Replicate(void) const {
	return (new CamDistMethod(*this));
}

unsigned long CamDistMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(float));
}

void CamDistMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
	data << dist;
}

void CamDistMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	data >> dist;
}

long CamDistMethod::GetSettingCount(void) const {
	return 1;
}

Setting *CamDistMethod::GetSetting(long index) const {
	StringTable *table = &StringTable("text/Camera");
	const char *title = table->GetString(StringID('CAMR', 'CDIS', 'CPMT'));
	return (new TextSetting('TIME', Text::FloatToString((float) dist), title, 7, &EditableTextElement::FloatNumberKeyFilter));
}

void CamDistMethod::SetSetting(const Setting *setting) {
	if (setting->GetSettingIdentifier() == 'TIME') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		dist = Text::StringToFloat(text);
	}
}

void CamDistMethod::Execute(const ScriptState *state) {
	if(TheGame->GetCamTriggers()) {
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
		side->SetDist(dist);
	}

	CallCompletionProc();
}




// set Track amount
CamTrackAmountMethod::CamTrackAmountMethod() : Method(kMethodCamTrackAmt) {
	amt = 1.f;
}

CamTrackAmountMethod::CamTrackAmountMethod(float a) : Method(kMethodCamTrackAmt) {
	amt = a;
}

CamTrackAmountMethod::CamTrackAmountMethod(const CamTrackAmountMethod& camTrackAmountMethod) :	Method(camTrackAmountMethod) {
	amt = camTrackAmountMethod.amt;
}

CamTrackAmountMethod::~CamTrackAmountMethod() {}

Method *CamTrackAmountMethod::Replicate(void) const {
	return (new CamTrackAmountMethod(*this));
}

unsigned long CamTrackAmountMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(float));
}

void CamTrackAmountMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
	data << amt;
}

void CamTrackAmountMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	data >> amt;
}

long CamTrackAmountMethod::GetSettingCount(void) const {
	return 1;
}

Setting *CamTrackAmountMethod::GetSetting(long index) const {
	StringTable *table = &StringTable("text/Camera");
	const char *title = table->GetString(StringID('CAMR', 'CTRK', 'CPMT'));
	return (new TextSetting('TRCK', Text::FloatToString((float) amt), title, 7, &EditableTextElement::FloatNumberKeyFilter));
}

void CamTrackAmountMethod::SetSetting(const Setting *setting) {
	if (setting->GetSettingIdentifier() == 'TRCK') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		amt = Text::StringToFloat(text);
	}
}

void CamTrackAmountMethod::Execute(const ScriptState *state) {
	if(TheGame->GetCamTriggers()) {
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
		side->SetTrackAmount(amt);
	}

	CallCompletionProc();
}






//////////////////////////
///////   Two Vars
//////////////////////////

// set offset amount
CamOffsetMethod::CamOffsetMethod() : Method(kMethodCamOffset) {
	xy = 1.f;
	z  = 2.f;
}

CamOffsetMethod::CamOffsetMethod(float xy, float z) : Method(kMethodCamOffset) {
	xy = xy;
	z = z;
}

CamOffsetMethod::CamOffsetMethod(const CamOffsetMethod& camOffsetMethod) :	Method(camOffsetMethod) {
	xy = camOffsetMethod.xy;
	z = camOffsetMethod.z;
}

CamOffsetMethod::~CamOffsetMethod() {}

Method *CamOffsetMethod::Replicate(void) const {
	return (new CamOffsetMethod(*this));
}

unsigned long CamOffsetMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(ChunkHeader)*3 + sizeof(float)*2);
}

void CamOffsetMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
			
	data << ChunkHeader('OFXY', 4);
	data << xy;
	
	data << ChunkHeader('OFFZ', 4);
	data << z;

	data << TerminatorChunk;
}

void CamOffsetMethod::Unpack(Unpacker& data, unsigned long unpackFlags) {
	Method::Unpack(data, unpackFlags);
	UnpackChunkList<CamOffsetMethod>(data, unpackFlags, &CamOffsetMethod::UnpackChunk);
}

bool CamOffsetMethod::UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags) {
	switch (chunkHeader->chunkType)	{
		case 'OFXY':
			data >> xy;
			return (true);
		
		case 'OFFZ':
			data >> z;
			return (true);
	}
	
	return (false);
}

long CamOffsetMethod::GetSettingCount(void) const {
	return 2;
}

Setting *CamOffsetMethod::GetSetting(long index) const {
	StringTable *table = &StringTable("text/Camera");
	
	if (index == 0)	{
		const char *title = table->GetString(StringID('CAMR', 'COFF', 'OFXY'));
		return (new TextSetting('OFXY', Text::FloatToString((float) xy), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	if (index == 1)	{
		const char *title = table->GetString(StringID('CAMR', 'COFF', 'OFFZ'));
		return (new TextSetting('OFFZ', Text::FloatToString((float) z), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}

	return nullptr;
}

void CamOffsetMethod::SetSetting(const Setting *setting) {
	const char *text = static_cast<const TextSetting *>(setting)->GetText();
	if (setting->GetSettingIdentifier() == 'OFXY') {
		xy = Text::StringToFloat(text);
	}
	if (setting->GetSettingIdentifier() == 'OFFZ') {
		z = Text::StringToFloat(text);
	}
}

void CamOffsetMethod::Execute(const ScriptState *state) {
	if(TheGame->GetCamTriggers()) {
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
		side->SetOffset(Vector2D(xy,z));
	}

	CallCompletionProc();
}



// set rotation amount
CamRotationMethod::CamRotationMethod() : Method(kMethodCamRotation) {
	azm = 0.f;
	alt = 0.f;
}

CamRotationMethod::CamRotationMethod(float x, float y) : Method(kMethodCamRotation) {
	azm = x;
	alt = y;
}

CamRotationMethod::CamRotationMethod(const CamRotationMethod& camRotationMethod) :	Method(camRotationMethod) {
	azm = camRotationMethod.azm;
	alt = camRotationMethod.alt;
}

CamRotationMethod::~CamRotationMethod() {}

Method *CamRotationMethod::Replicate(void) const {
	return (new CamRotationMethod(*this));
}

unsigned long CamRotationMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(ChunkHeader)*3 + sizeof(float)*2);
}

void CamRotationMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
			
	data << ChunkHeader('RAZM', 4);
	data << azm;
	
	data << ChunkHeader('RALT', 4);
	data << alt;

	data << TerminatorChunk;
}

void CamRotationMethod::Unpack(Unpacker& data, unsigned long unpackFlags) {
	Method::Unpack(data, unpackFlags);
	UnpackChunkList<CamRotationMethod>(data, unpackFlags, &CamRotationMethod::UnpackChunk);
}

bool CamRotationMethod::UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags) {
	switch (chunkHeader->chunkType)	{
		case 'RAZM':
			data >> azm;
			return (true);
		
		case 'RALT':
			data >> alt;
			return (true);
	}
	
	return (false);
}

long CamRotationMethod::GetSettingCount(void) const {
	return 2;
}

Setting *CamRotationMethod::GetSetting(long index) const {
	StringTable *table = &StringTable("text/Camera");
	
	if (index == 0)	{
		const char *title = table->GetString(StringID('CAMR', 'CROT', 'RAZM'));
		return (new TextSetting('RAZM', Text::FloatToString((float) azm), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}
	if (index == 1)	{
		const char *title = table->GetString(StringID('CAMR', 'CROT', 'RALT'));
		return (new TextSetting('RALT', Text::FloatToString((float) alt), title, 7, &EditableTextElement::FloatNumberKeyFilter));
	}

	return nullptr;
}

void CamRotationMethod::SetSetting(const Setting *setting) {
	const char *text = static_cast<const TextSetting *>(setting)->GetText();
	if (setting->GetSettingIdentifier() == 'RAZM') {
		azm = Text::StringToFloat(text);
	}
	if (setting->GetSettingIdentifier() == 'RALT') {
		alt = Text::StringToFloat(text);
	}
}

void CamRotationMethod::Execute(const ScriptState *state) {
	if(TheGame->GetCamTriggers()) {
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
		side->SetRotation(azm*K::radians,alt*K::radians);
	}

	CallCompletionProc();
}

//////////////////////////
///////   Bools
//////////////////////////
// set what flips with offset
CamFlagFlipMethod::CamFlagFlipMethod() : Method(kMethodCamFlagFlip) {
	flipFlags = 3;
}

CamFlagFlipMethod::CamFlagFlipMethod(unsigned long flags) : Method(kMethodCamFlagFlip) {
	flipFlags = flags;
}

CamFlagFlipMethod::CamFlagFlipMethod(const CamFlagFlipMethod& camFlipMethod) :	Method(camFlipMethod) {
	flipFlags = camFlipMethod.flipFlags;
}

CamFlagFlipMethod::~CamFlagFlipMethod() {}

Method *CamFlagFlipMethod::Replicate(void) const {
	return (new CamFlagFlipMethod(*this));
}

unsigned long CamFlagFlipMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(unsigned long));
}

void CamFlagFlipMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
	data << flipFlags;
}

void CamFlagFlipMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	data >> flipFlags;
}

long CamFlagFlipMethod::GetSettingCount(void) const {
	return 2;
}

Setting *CamFlagFlipMethod::GetSetting(long index) const {
	StringTable *table = &StringTable("text/Camera");
	
	if(index == 0) {
		const char *title = table->GetString(StringID('CAMR', 'CFLP', 'FAZM'));
		return (new BooleanSetting('FAZM', ((flipFlags & kFlipAzm) != 0), title));
	}
	else if(index == 1) {
		const char *title = table->GetString(StringID('CAMR', 'CFLP', 'FOXY'));
		return (new BooleanSetting('FOXY', ((flipFlags & kFlipXY) != 0), title));
	}

	return nullptr;
}

void CamFlagFlipMethod::SetSetting(const Setting *setting) {
	Type identifier = setting->GetSettingIdentifier();
	if (identifier == 'FAZM') {
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		if(b)  flipFlags |= kFlipAzm;
		else   flipFlags &= ~kFlipAzm;
	}
	else if (identifier == 'FOXY') {
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		if(b)  flipFlags |= kFlipXY;
		else   flipFlags &= ~kFlipXY;
	}
}

void CamFlagFlipMethod::Execute(const ScriptState *state) {
	if(TheGame->GetCamTriggers()) {
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
		side->SetFlipFlags(flipFlags);
	}

	CallCompletionProc();
}

//////////////////////////
///////   Camera Change Type
//////////////////////////
// constructors
CamChangeTypeMethod::CamChangeTypeMethod() : Method(kMethodCamChangeType) {
	cameraType = kCamNone;
}
CamChangeTypeMethod::CamChangeTypeMethod(const CamChangeTypeMethod& camChangeTypeMethod) : Method(camChangeTypeMethod) {
	cameraType = camChangeTypeMethod.cameraType;
}

CamChangeTypeMethod::CamChangeTypeMethod(long type) : Method(kMethodCamChangeType) {
	cameraType = type;
}

// deconstructor
CamChangeTypeMethod::~CamChangeTypeMethod() {}


Method *CamChangeTypeMethod::Replicate(void) const {
	return (new CamChangeTypeMethod(*this));
}

void CamChangeTypeMethod::Execute(const ScriptState *state) {
	if(TheGame->GetCamTriggers()) {
		SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
		side->SetTrackType(cameraType);
	}

	CallCompletionProc();
}

long CamChangeTypeMethod::GetSettingCount(void) const {
	return 1;
}

Setting *CamChangeTypeMethod::GetSetting(long index) const {

	StringTable *table = &StringTable("text/Camera");

	if (index == 0) {
		static const unsigned long camID[3] =	{ 'none', 'lock', 'trck' };
		
		long selection = 0;
		if (cameraType == kCamLock) selection = 1;
		else if (cameraType == kCamTrack) selection = 2;
		
		const char *title = table->GetString(StringID('CAMR', 'CTYP', 'CPMT'));
		MenuSetting *menu = new MenuSetting('CAMT', selection, title, 3);
		for (natural a = 0; a < 3; a++) menu->SetMenuItemString(a, table->GetString(StringID('CAMR', 'CTYP', camID[a])));
		
		return (menu);
	}
	
	return (nullptr);
}

void CamChangeTypeMethod::SetSetting(const Setting *setting)
{
	Type identifier = setting->GetSettingIdentifier();
	if (identifier == 'CAMT')
	{
		static const unsigned long camType[3] =	{ kCamNone, kCamLock, kCamTrack };
		
		cameraType = camType[static_cast<const MenuSetting *>(setting)->GetMenuSelection()];
	}
}



unsigned long CamChangeTypeMethod::GetPackSize(unsigned long packFlags) const
{
	return (Method::GetPackSize(packFlags) + sizeof(ChunkHeader) * 2 + 4);
}

void CamChangeTypeMethod::Pack(Packer& data, unsigned long packFlags) const
{
	Method::Pack(data, packFlags);
	
	data << ChunkHeader('CAMT', 4);
	data << cameraType;
	
	data << TerminatorChunk;
}

void CamChangeTypeMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	UnpackChunkList<CamChangeTypeMethod>(data, unpackFlags, &CamChangeTypeMethod::UnpackChunk);
}
			
bool CamChangeTypeMethod::UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags)
{
	switch (chunkHeader->chunkType)
	{
		case 'CAMT':
			
			data >> cameraType;
			return (true);
	}
	
	return (false);
}






// set player
PlayerAzmMethod::PlayerAzmMethod() : Method(kMethodPlayAzm) {
	azm = 0.f;
}

PlayerAzmMethod::PlayerAzmMethod(float a) : Method(kMethodPlayAzm) {
	azm = a;
}

PlayerAzmMethod::PlayerAzmMethod(const PlayerAzmMethod& playerAzmMethod) :	Method(playerAzmMethod) {
	azm = playerAzmMethod.azm;
}

PlayerAzmMethod::~PlayerAzmMethod() {}

Method *PlayerAzmMethod::Replicate(void) const {
	return (new PlayerAzmMethod(*this));
}

unsigned long PlayerAzmMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(float));
}

void PlayerAzmMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
	data << azm;
}

void PlayerAzmMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	data >> azm;
}

long PlayerAzmMethod::GetSettingCount(void) const {
	return 1;
}

Setting *PlayerAzmMethod::GetSetting(long index) const {
	const char *title = "Player Azm";
	return (new TextSetting('PAZM', Text::FloatToString((float) azm), title, 7, &EditableTextElement::FloatNumberKeyFilter));
}

void PlayerAzmMethod::SetSetting(const Setting *setting) {
	if (setting->GetSettingIdentifier() == 'PAZM') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		azm = Text::StringToFloat(text);
	}
}

void PlayerAzmMethod::Execute(const ScriptState *state) {
	TheGame->GetPlayerController()->SetPlayerAzimuth(azm*K::radians);
}