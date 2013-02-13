#include "MonkeyPart.h"
#include "Player.h"
#include "C4Engine.h"
#include "Game.h"
#include "Player.h"
#include "Interface.h"

using namespace C4;




MonkeyPartMethod::MonkeyPartMethod() : Method(kMethodMonkeyPart) {
	monkeyPart = 0;
}
MonkeyPartMethod::MonkeyPartMethod(const MonkeyPartMethod& monkeyPartMethod) : Method(monkeyPartMethod) { monkeyPart = monkeyPartMethod.monkeyPart;}

MonkeyPartMethod::~MonkeyPartMethod() {}

Method *MonkeyPartMethod::Replicate(void) const{
	return (new MonkeyPartMethod(*this));
}

void MonkeyPartMethod::Execute(const ScriptState *state) {
	
	TheGame->GetPlayerController()->AddMonkey(monkeyPart);//monkeyPart);
	TheDisplayInterface->UpdatePlayerMonkey();
	CallCompletionProc();
}


//Serialization stuff
/*
A custom controller must implement the GetPackSize, Pack, and Unpack functions so that its data can be written to a file and later restored. 
(These functions override the virtual functions in the Packable class.) 
Each of these functions needs to first call its counterpart in the Controller base class. 
*/


void MonkeyPartMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
	//Write the travel distance
	//data << monkeyPart;
	data << ChunkHeader('MKPT', 4);
	data << monkeyPart;

	data << TerminatorChunk;
}

void MonkeyPartMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
/*	Method::Unpack(data, unpackFlags);
	// Read the travel distance
    data >> monkeyPart;*/

	Method::Unpack(data, unpackFlags);
	UnpackChunkList<MonkeyPartMethod>(data, unpackFlags, &MonkeyPartMethod::UnpackChunk);
}

bool MonkeyPartMethod::UnpackChunk(const ChunkHeader *chunkHeader, Unpacker& data, unsigned long unpackFlags)
{
	switch (chunkHeader->chunkType)	{
	case 'MKPT':
		data >> monkeyPart;
		return true;
	}
	return (false);
}

//User interface stuff to allow options in the world editor

long MonkeyPartMethod::GetSettingCount(void) const {
	return 1;
}

Setting *MonkeyPartMethod::GetSetting(long index) const {
	if (index == 0)
    {
		return (new TextSetting('mpar', Text::IntegerToString(monkeyPart),
            "Monkey Part", 7, &EditableTextElement::NumberKeyFilter));
    }

    return (nullptr);
}

void MonkeyPartMethod::SetSetting(const Setting *setting) {
	
	if (setting->GetSettingIdentifier() == 'mpar') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		monkeyPart = Text::StringToInteger(text);
		
	}
}