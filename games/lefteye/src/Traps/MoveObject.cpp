#include "MoveObject.h"
#include "C4Engine.h"
#include "Player.h"

using namespace C4;


MoveObjectMethod::MoveObjectMethod() : Method(kMethodMoveObject) {
	travelDistance = 0.0;
	travelDirection = kOriNorth;
}
MoveObjectMethod::MoveObjectMethod(const MoveObjectMethod& moveObjectMethod) : Method(moveObjectMethod) {}
MoveObjectMethod::~MoveObjectMethod() {}

Method *MoveObjectMethod::Replicate(void) const{
	return (new MoveObjectMethod(*this));
}

void MoveObjectMethod::Execute(const ScriptState *state) {
	Point3D currentPos = this->GetTargetNode(state)->GetNodePosition();//GetWorldPosition();
	this->GetTargetNode(state)->SetNodePosition(Point3D(currentPos.x, currentPos.y,currentPos.z+1));

	CallCompletionProc();
}


/*
void MoveObjectMethod::Move(void)
{
	Controller::Move();
	
	Entity *entity = GetTargetNode();
	if (!(entity->GetNodeFlags() & kNodeDisabled))
	{
		Matrix3D	rotator;
		
		float dt = TheTimeMgr->GetFloatDeltaTime();
		phaseAngle = Fmod(phaseAngle + dt * 0.0025F, K::four_pi);
		
		rotator.SetRotationAboutZ(phaseAngle);
		entity->SetNodeMatrix3D(rotator);
		entity->SetNodePosition(Point3D(centerPosition.x, centerPosition.y, centerPosition.z + kCollectableAmplitude * Sin(phaseAngle * 0.5F)));
		entity->Invalidate();
	}
	else
	{
		if ((TheMessageMgr->Server()) && ((respawnTime -= TheTimeMgr->GetDeltaTime()) <= 0))
		{
			TheMessageMgr->SendMessageAll(ControllerMessage(kControllerMessageRespawnItem, GetControllerIndex()));
		}
	}
}*/





//Serialization stuff
/*
A custom controller must implement the GetPackSize, Pack, and Unpack functions so that its data can be written to a file and later restored. 
(These functions override the virtual functions in the Packable class.) 
Each of these functions needs to first call its counterpart in the Controller base class. 
*/

unsigned long MoveObjectMethod::GetPackSize(unsigned long packFlags) const {
	return (Method::GetPackSize(packFlags) + sizeof(float));
}

void MoveObjectMethod::Pack(Packer& data, unsigned long packFlags) const {
	Method::Pack(data, packFlags);
	//Write the travel distance
	data << travelDistance;
}

void MoveObjectMethod::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Method::Unpack(data, unpackFlags);
	// Read the travel distance
    data >> travelDistance;
}


//User interface stuff to allow options in the world editor

long MoveObjectMethod::GetSettingCount(void) const {
	return 2;
}

Setting *MoveObjectMethod::GetSetting(long index) const {
	if (index == 0)
    {
       return (new TextSetting('tdis', Text::FloatToString(travelDistance),
            "Travel Distance", 7, &EditableTextElement::FloatNumberKeyFilter));
    }

	if (index == 1) {
		static const unsigned long orientationID[4] =	{ 'nort', 'sout', 'east', 'west'};
		
		long selection = 0;
		if (travelDirection == kOriNorth) selection = 1;
		else if (travelDirection == kOriSouth) selection = 2;
		else if (travelDirection == kOriEast) selection = 3;
		else if (travelDirection == kOriWest) selection = 4;
		
		const char *title = "North South East West";
		MenuSetting *menu = new MenuSetting('ATTR', selection, title, 4);
		for (natural a = 0; a < 4; a++){
			if(a == 0) menu->SetMenuItemString(a, "North");
			if(a == 1) menu->SetMenuItemString(a, "South");
			if(a == 2) menu->SetMenuItemString(a, "East");
			if(a == 3) menu->SetMenuItemString(a, "West");

		}
		
		return (menu);
	}

    return (nullptr);
}

void MoveObjectMethod::SetSetting(const Setting *setting) {
	//are we setting the travel distances?
	if (setting->GetSettingIdentifier() == 'tdis') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		travelDistance = Text::StringToFloat(text);
	}
}
