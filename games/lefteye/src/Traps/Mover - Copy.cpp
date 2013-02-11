#include "Mover.h"
#include "C4Engine.h"
#include "Player.h"
#include "Game.h"

using namespace C4;

	enum {
		kAxisX  = 'xaxi',
		kAxisY  = 'yaxi',
		kAxisZ = 'zaxi'
	};

/**
 * Default constructor makes a 1,1,1 size square trigger
 */
MoverController::MoverController() : Controller(kControllerMover)
{
		travelDistance = 3;
		speed = 0.001f;
		reverseDirection = false;
		travelAxis = kAxisZ; 
}

MoverController::MoverController(float newSpeed) : Controller(kControllerMover)
{
		travelDistance = 3;
		speed = newSpeed;
		reverseDirection = false;
		travelAxis = kAxisZ;
}

Controller *MoverController::Replicate(void) const
{
    return (new MoverController(*this));
}

/**
 * Destructor
 */
MoverController::~MoverController()
{
	
}


MoverController::MoverController(const MoverController& moverController) : Controller(moverController)
{
	travelDistance = moverController.travelDistance;
    speed = 0.001F;
}


/**
 * Prepares the controller for entry into the world
 */
void MoverController::Preprocess()
{
	// This function is called once before the target node is ever
	// rendered or moved. The base class Preprocess() function should
	// always be called first, and then the subclass can do whatever
	// preprocessing it needs to do. In this case, we don't do anything
	// extra, so this function is only overridden for illustrative purposes.

	Controller::Preprocess();

	//get the original Position
	originalPosition = GetTargetNode()->GetNodePosition();

	//start at 0 offset
	offset = 0.0f;


}


/**
 * Prepares the controller for entry into the world
 */
void MoverController::Move()
{
	Node *node = GetTargetNode();
	const CollisionData *data = TheGame->GetPlayerController()->GetCollisionData();
	CollisionState playerState = TheGame->GetPlayerController()->GetCollisionState();
	
	if(playerState == kCollisionStateNone){
		TheEngine->Report("No Collision");
			if(offset >= travelDistance)
				reverseDirection = true;

			if(offset <= 0.0)
				reverseDirection = false;

			if(reverseDirection) //if direction is reversed, subtract
				offset -= TheTimeMgr->GetFloatDeltaTime() * speed;
			else 
				offset += TheTimeMgr->GetFloatDeltaTime() * speed;
			

			if(travelAxis == kAxisZ)
				node->SetNodePosition(Point3D(originalPosition.x, originalPosition.y, originalPosition.z + offset));
			else if(travelAxis == kAxisY)
				node->SetNodePosition(Point3D(originalPosition.x, originalPosition.y + offset, originalPosition.z));
			else if(travelAxis == kAxisX)
				node->SetNodePosition(Point3D(originalPosition.x + offset, originalPosition.y, originalPosition.z));

			node->Invalidate();
	}else if(playerState == kCollisionStateGeometry)
		TheEngine->Report("there was collision");
	else
		TheEngine->Report("Other");
}

/**
 * Responds to the trigger being activated by the player
 *
 * @param trigger		the trigger that was activated
 * @param activator		the node that activated the trigger
 */
void MoverController::Activate(Node *trigger, Node *activator)
{
	// Called when the controller is activated by a trigger.
//	TheEngine->Report("you died");
	
	//trigger->Disable(); finishMove
	//GetTargetNode()->Disable();
}



//Serialization stuff
/*
A custom controller must implement the GetPackSize, Pack, and Unpack functions so that its data can be written to a file and later restored. 
(These functions override the virtual functions in the Packable class.) 
Each of these functions needs to first call its counterpart in the Controller base class. 
*/

unsigned long MoverController::GetPackSize(unsigned long packFlags) const {
	//create enough spaces for all the current variables
	return (Controller::GetPackSize(packFlags) + (sizeof(float) * 3) + sizeof(long) + sizeof(Point3D) + sizeof(bool));
}

void MoverController::Pack(Packer& data, unsigned long packFlags) const {
	Controller::Pack(data, packFlags);
	
	data << originalPosition;

	data << travelAxis;

	data << offset;
	data << speed;
	data << travelDistance;

	data << reverseDirection; 
}

void MoverController::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Controller::Unpack(data, unpackFlags);
	// Read the travel distance
   	data >> originalPosition;

	data >> travelAxis;

	data >> offset;
	data >> speed;
	data >> travelDistance;

	data >> reverseDirection; 
}


//User interface stuff to allow options in the world editor

long MoverController::GetSettingCount(void) const {
	return 2;
}

Setting *MoverController::GetSetting(long index) const {
	if (index == 0)
    {
       return (new TextSetting('tdis', Text::FloatToString(travelDistance),
            "Travel Distance", 7, &EditableTextElement::FloatNumberKeyFilter));
    }

	if (index == 1) {
		//static const unsigned long axisID[3] =	{ 'xaxi', 'yaxi', 'zaxi'};
		
		long selection = 0;
		if (travelAxis == kAxisX) selection = 1;
		else if (travelAxis == kAxisY) selection = 2;
		else if (travelAxis == kAxisZ) selection = 3;
		
		
		const char *title = "X-Axis Y-Axis Z-Axis";
		MenuSetting *menu = new MenuSetting('axis', selection, title, 3);
		for (natural a = 0; a < 3; a++){
			if(a == 0) menu->SetMenuItemString(a, "X-Axis");
			if(a == 1) menu->SetMenuItemString(a, "Y-Axis");
			if(a == 2) menu->SetMenuItemString(a, "Z-Axis");
		}
		
		return (menu);
	}

    return (nullptr);
}

void MoverController::SetSetting(const Setting *setting) {
	//get the type of identifier
	Type identifier = setting->GetSettingIdentifier();

	//are we setting the travel distances?
	if (identifier == 'tdis') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		travelDistance = Text::StringToFloat(text);
	}

	//are we setting movement axis
	if (identifier == 'axis') {
		static const unsigned long axisID[3] =	{ kAxisX, kAxisY, kAxisZ };
		travelAxis = axisID[static_cast<const MenuSetting *>(setting)->GetMenuSelection()];
	}
}
