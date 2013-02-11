#include "Coin.h"
#include "Player.h"
#include "C4Engine.h"
#include "Game.h"
#include "Player.h"
#include "Interface.h"

using namespace C4;




CollectCoinMethod::CollectCoinMethod() : Method(kMethodCollectCoin) {
	amount = 1;
}
CollectCoinMethod::CollectCoinMethod(const CollectCoinMethod& collectCoinMethod) : Method(collectCoinMethod) {
	amount = collectCoinMethod.amount;
}

CollectCoinMethod::~CollectCoinMethod() {}

Method *CollectCoinMethod::Replicate(void) const{
	return (new CollectCoinMethod(*this));
}

void CollectCoinMethod::Execute(const ScriptState *state) {
	
	TheGame->GetPlayerController()->AddCoin(amount);
	TheDisplayInterface->UpdatePlayerCoin();
	CallCompletionProc();
}


//Serialization stuff
/*
A custom controller must implement the GetPackSize, Pack, and Unpack functions so that its data can be written to a file and later restored. 
(These functions override the virtual functions in the Packable class.) 
Each of these functions needs to first call its counterpart in the Controller base class. 
*/

unsigned long CollectCoinMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(long));
}

void CollectCoinMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
	//Write the travel distance
	data << amount;
}

void CollectCoinMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	// Read the travel distance
    data >> amount;
}


//User interface stuff to allow options in the world editor

long CollectCoinMethod::GetSettingCount(void) const {
	return 1;
}

Setting *CollectCoinMethod::GetSetting(long index) const {
	if (index == 0)
    {
		return (new TextSetting('amnt', Text::IntegerToString(amount),
            "Gold Value", 7, &EditableTextElement::NumberKeyFilter));
    }

    return (nullptr);
}

void CollectCoinMethod::SetSetting(const Setting *setting) {
	
	if (setting->GetSettingIdentifier() == 'amnt') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		amount = Text::StringToInteger(text);
	}
}