#include "DevRegistrar.h"

#include "Game.h"

using namespace C4;

DevRegistrar *C4::TheDevReg = nullptr;

DevRegistrar::DevRegistrar() : Singleton<DevRegistrar>(TheDevReg),
	cameraDemoCommand("cameraDemo", &CameraDemoCommand),
	startGameCommand("startGame", &StartGameCommand),
	gridMoveToggle("gridMoveToggle", &gridMoveToggleCmd),
	menuCommand("menu", &MenuCommand),
	
	

	loadWorldReg(kMethodLoadWorld, "Load World")
{}

DevRegistrar::~DevRegistrar() {}

void DevRegistrar::LoadCommands() {
	TheEngine->AddCommand(&cameraDemoCommand);
	TheEngine->AddCommand(&startGameCommand);
	TheEngine->AddCommand(&gridMoveToggle);
	TheEngine->AddCommand(&menuCommand);
	

	stopPlayerAction = new SwitchAction(kActionStopPlayer);
	TheInputMgr->AddAction(stopPlayerAction);
}

void DevRegistrar::CameraDemoCommand(const char *text) {
	TheGame->LoadWorld("levels/Demos/CameraDemo");
}

void DevRegistrar::StartGameCommand(const char *text) {
	TheGame->LoadWorld("ad2");
}

void DevRegistrar::gridMoveToggleCmd(const char *text) {
	TheGame->ToggleGridMove();
}

void DevRegistrar::MenuCommand(const char *text) {
	MainWindow::Open();
}





//** Code for loading the next level  *//


LoadWorldMethod::LoadWorldMethod() : Method(kMethodLoadWorld) {
	mapSelect = 1;
}

LoadWorldMethod::LoadWorldMethod(long mapSelection) : Method(kMethodLoadWorld) {
	mapSelect = mapSelection;
	
}

LoadWorldMethod::LoadWorldMethod(const LoadWorldMethod& loadWorldMethod) :	Method(loadWorldMethod) {
	mapSelect = loadWorldMethod.mapSelect;
	
}

LoadWorldMethod::~LoadWorldMethod() {}

Method *LoadWorldMethod::Replicate(void) const {
	return (new LoadWorldMethod(*this));
}





unsigned long LoadWorldMethod::GetPackSize(unsigned long packFlags) const
{
	return (Method::GetPackSize(packFlags) + sizeof(ChunkHeader) * 2 + 4);
}

void LoadWorldMethod::Pack(Packer& data, unsigned long packFlags) const
{
	Method::Pack(data, packFlags);
	
	data << ChunkHeader('NWLD', 4);
	data << mapSelect;
	
	data << TerminatorChunk;
}

void LoadWorldMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	UnpackChunkList<LoadWorldMethod>(data, unpackFlags, &LoadWorldMethod::UnpackChunk);
}
			
bool LoadWorldMethod::UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags)
{
	switch (chunkHeader->chunkType)
	{
		case 'NWLD':
			
			data >> mapSelect;
			return (true);
	}
	
	return (false);
}



/*
unsigned long LoadWorldMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(ChunkHeader)*2 + sizeof(long));
}

void LoadWorldMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
			
	data << ChunkHeader('NWLD', 100);
	data << mapSelect;
	
	data << TerminatorChunk;
}

void LoadWorldMethod::Unpack(Unpacker& data, unsigned long unpackFlags) {
	Method::Unpack(data, unpackFlags);
	UnpackChunkList<LoadWorldMethod>(data, unpackFlags, &LoadWorldMethod::UnpackChunk);
}

bool LoadWorldMethod::UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags) {
	switch (chunkHeader->chunkType)	{
		case 'NWLD':
			data >> mapSelect;
			return (true);
	}
	
	return (false);
}*/

long LoadWorldMethod::GetSettingCount(void) const {
	return 1;
}

Setting *LoadWorldMethod::GetSetting(long index) const {
	
	if (index == 0) {
		
		
		long selection = 0;

		if (mapSelect == kMap1) selection = 0;
		else if (mapSelect == kMap2) selection = 1;
		else if (mapSelect == kMap3) selection = 2;
		else if (mapSelect == kMap4) selection = 3;
		
		
		const char *title = "Map";
		MenuSetting *menu = new MenuSetting('MAPS', selection, title, 4);
		for (natural a = 0; a < 4; a++) {
			if(a == 0)
				menu->SetMenuItemString(a, "Rafters");
			if(a == 1)
				menu->SetMenuItemString(a, "Dungeon");
			if(a == 2)
				menu->SetMenuItemString(a, "Aztec Temple");
			if(a == 3)
				menu->SetMenuItemString(a, "Starting Level");
		}
		
		return (menu);
	}
	
	return (nullptr);


	/*
	const StringTable *table = TheInterfaceMgr->GetStringTable();

	if (index == 0)
	{
		const char *title = "Path to World File";
		return (new TextSetting('NWLD', nextWorld, title, 100));
	}
	
	return nullptr;*/
}

void LoadWorldMethod::SetSetting(const Setting *setting) {
	Type identifier = setting->GetSettingIdentifier();
	if (identifier == 'MAPS')
	{
		static const unsigned long mapType[4] =	{ kMap1, kMap2, kMap3, kMap4 };
		
		mapSelect = mapType[static_cast<const MenuSetting *>(setting)->GetMenuSelection()];
	}
	
	/*const char *text = static_cast<const TextSetting *>(setting)->GetText();
	if (setting->GetSettingIdentifier() == 'NWLD') {
		nextWorld = const_cast<char *>(text);
	}*/

}

void LoadWorldMethod::Execute(const ScriptState *state) {
	char* newWorld;
	if(mapSelect == kMap1)
		newWorld = "Rafters";
	if(mapSelect == kMap2)
		newWorld = "ad2";
	if(mapSelect == kMap3)
		newWorld = "at3";
	if(mapSelect == kMap4)
		newWorld = "startingLevel";

	TheGame->SetNextWorld(newWorld);//pass the name for the new world
	TheGame->SetWorldLoad(true);//set load world to true so it loads

	CallCompletionProc();
}