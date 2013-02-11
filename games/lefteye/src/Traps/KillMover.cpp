#include "KillMover.h"
#include "C4Engine.h"
#include "Player.h"
#include "Game.h"


using namespace C4;



/**
* Default constructor makes a 1,1,1 size square trigger
*/
KillMoverController::KillMoverController() : SphereCollider(kControllerKillMover, kColliderKillMover, 1.0F)//Controller(kControllerMover)
{
	travelDistance = 3;
	speed = 0.001f;
	reverseDirection = false;
	travelAxis = kAxisZ;
	doesPatrol = false;
	repeatMovement = true;
	stopMoving = false;
	hasTrigger = false;
	killPlayer = false;
}

KillMoverController::KillMoverController(float newSpeed) : SphereCollider(kControllerKillMover, kColliderKillMover, 1.0F)//Controller(kControllerMover)
{
	travelDistance = 3;
	speed = newSpeed;
	reverseDirection = false;
	travelAxis = kAxisZ;
	doesPatrol = false;
	repeatMovement = true;
	stopMoving = false;
	hasTrigger = false;
	killPlayer = false;
}

Controller *KillMoverController::Replicate(void) const
{
	return (new KillMoverController(*this));
}

/**
* Destructor
*/
KillMoverController::~KillMoverController()
{

}


KillMoverController::KillMoverController(const KillMoverController& killMoverController) : SphereCollider(kControllerKillMover, kColliderKillMover, 1.0F)//Controller(killMoverController)
{
	travelDistance = killMoverController.travelDistance;
	speed = killMoverController.speed;
	travelAxis = killMoverController.travelAxis;
	doesPatrol = killMoverController.doesPatrol;
	repeatMovement = killMoverController.repeatMovement;
	hasTrigger = killMoverController.hasTrigger;
	killPlayer = killMoverController.killPlayer;
}


/**
* Prepares the controller for entry into the world
*/
void KillMoverController::Preprocess() {
	if(hasTrigger)
		SetControllerFlags(kControllerAsleep);


	Controller::Preprocess();



	GameWorld* world= static_cast<GameWorld*>(GetTargetNode()->GetWorld());

	if (world) {
		world->AddCollider(this);
	}

	GetTargetNode()->Update();

	//get the original Position
	originalPosition = GetTargetNode()->GetNodePosition();

	//start at 0 offset
	offset = 0.0f;
}


/**
* Prepares the controller for entry into the world
*/
void KillMoverController::Move() {

	Node *node = GetTargetNode();

	if(!stopMoving){

		//if the object is suppose to patrol
		//make sure its going the right directions
		if(doesPatrol){
			//if it should not repeat moving, and its already patroled once. stop.
			if(!repeatMovement && (offset < 0.0))
				stopMoving = true;

			//if its gone past target distance start going other direction
			if(offset >= travelDistance)
				reverseDirection = true;
			//if its gone back to its start position, start going up again
			if(offset <= 0.0)
				reverseDirection = false;
		} 

		if(reverseDirection) //if direction is reversed, subtract
			offset -= TheTimeMgr->GetFloatDeltaTime() * speed;
		else 
			offset += TheTimeMgr->GetFloatDeltaTime() * speed;

		//if the object is suppose to repeat its movement and NOT patrol
		//and its gone past its travel distance
		if(repeatMovement && !doesPatrol && (offset >= travelDistance))
			offset -= offset; //have the offset send the object back to original position

		if(!doesPatrol && (offset >= travelDistance))
			stopMoving = true;


		//calculate the objects position then set it
		position = CalculatePosition(travelAxis, offset, reverseDirection);
	}
}

Point3D KillMoverController::CalculatePosition(long axis, float newOffset, bool reverse){
	Point3D returnPosition;
	if(travelAxis == kAxisZ){
		//offset Position for given axis
		returnPosition = Point3D(originalPosition.x, originalPosition.y, originalPosition.z + newOffset);

		//if the direction needs to be reversed use the right velocity
		if(reverse) 
			velocity = speed * Vector3D(0,0,-1);
		else
			velocity = speed * Vector3D(0,0,1);

		SetColliderPosition(returnPosition);
		SetColliderDelta(velocity*TheTimeMgr->GetFloatDeltaTime());
	}else if(travelAxis == kAxisY){
		returnPosition = Point3D(originalPosition.x, originalPosition.y + newOffset, originalPosition.z);

		if(reverse)
			velocity = speed * Vector3D(0,-1,0);
		else
			velocity = speed * Vector3D(0,1,0);

		SetColliderPosition(returnPosition);//node->SetNodePosition(position);
		SetColliderDelta(velocity*TheTimeMgr->GetFloatDeltaTime());
	}else if(travelAxis == kAxisX){
		returnPosition = Point3D(originalPosition.x + newOffset, originalPosition.y, originalPosition.z);

		if(reverse)
			velocity = speed * Vector3D(-1,0,0);
		else
			velocity = speed * Vector3D(1,0,0);

		SetColliderPosition(returnPosition);//node->SetNodePosition(position);
		SetColliderDelta(velocity*TheTimeMgr->GetFloatDeltaTime());
	}
	
	else if(travelAxis == kAxisZneg){
		//offset Position for given axis
		returnPosition = Point3D(originalPosition.x, originalPosition.y, originalPosition.z - newOffset);

		//if the direction needs to be reversed use the right velocity
		if(reverse) 
			velocity = speed * Vector3D(0,0,-1);
		else
			velocity = speed * Vector3D(0,0,1);

		SetColliderPosition(returnPosition);
		SetColliderDelta(velocity*TheTimeMgr->GetFloatDeltaTime());
	}else if(travelAxis == kAxisYneg){
		returnPosition = Point3D(originalPosition.x, originalPosition.y - newOffset, originalPosition.z);

		if(reverse)
			velocity = speed * Vector3D(0,-1,0);
		else
			velocity = speed * Vector3D(0,1,0);

		SetColliderPosition(returnPosition);//node->SetNodePosition(position);
		SetColliderDelta(velocity*TheTimeMgr->GetFloatDeltaTime());
	}else if(travelAxis == kAxisXneg){
		returnPosition = Point3D(originalPosition.x - newOffset, originalPosition.y, originalPosition.z);

		if(reverse)
			velocity = speed * Vector3D(-1,0,0);
		else
			velocity = speed * Vector3D(1,0,0);

		SetColliderPosition(returnPosition);//node->SetNodePosition(position);
		SetColliderDelta(velocity*TheTimeMgr->GetFloatDeltaTime());
	}

	return returnPosition;
}


void KillMoverController::Travel() {

	CollisionState state = GetCollisionState();
	const CollisionData* data = GetCollisionData();
	Point3D newPosition;

	Node *node = GetTargetNode();
	position = (GetColliderPosition() + GetColliderDelta());
	node->SetNodePosition(node->GetSuperNode()->GetInverseWorldTransform() *  position);
	node->Invalidate();
}


//Serialization stuff
/*
A custom controller must implement the GetPackSize, Pack, and Unpack functions so that its data can be written to a file and later restored. 
(These functions override the virtual functions in the Packable class.) 
Each of these functions needs to first call its counterpart in the Controller base class. 
*/

unsigned long KillMoverController::GetPackSize(unsigned long packFlags) const {
	//create enough spaces for all the current variables
	return (Controller::GetPackSize(packFlags) + (sizeof(float) * 3) + sizeof(long) + sizeof(Point3D) + sizeof(bool));
}

void KillMoverController::Pack(Packer& data, unsigned long packFlags) const {
	Controller::Pack(data, packFlags);

	data << travelAxis;

	data << speed;
	data << travelDistance;

	data << doesPatrol;
	data << repeatMovement;
	data << hasTrigger;
	data << killPlayer;
	
}

void KillMoverController::Unpack(Unpacker& data, unsigned long unpackFlags)
{
	Controller::Unpack(data, unpackFlags);
	// Read the travel distance
	data >> travelAxis;

	data >> speed;
	data >> travelDistance;

	data >> doesPatrol;
	data >> repeatMovement;
	data >> hasTrigger;
	data >> killPlayer;
}


//User interface stuff to allow options in the world editor

long KillMoverController::GetSettingCount(void) const {
	return 7;
}

Setting *KillMoverController::GetSetting(long index) const {
	if (index == 0)
	{
		return (new TextSetting('tdis', Text::FloatToString(travelDistance),
			"Travel Distance", 7, &EditableTextElement::FloatNumberKeyFilter));
	}

	if (index == 1)
	{
		return (new TextSetting('mspd', Text::FloatToString(speed),
			"Speed(0.001 default)", 7, &EditableTextElement::FloatNumberKeyFilter));
	}

	if (index == 2) {
		//static const unsigned long axisID[3] =	{ 'xaxi', 'yaxi', 'zaxi'};

		long selection = 0;
		if (travelAxis == kAxisX) selection = 0;
		else if (travelAxis == kAxisY) selection = 1;
		else if (travelAxis == kAxisZ) selection = 2;
		else if (travelAxis == kAxisXneg) selection = 3;
		else if (travelAxis == kAxisYneg) selection = 4;
		else if (travelAxis == kAxisZneg) selection = 5;


		const char *title = "Axis";
		MenuSetting *menu = new MenuSetting('axis', selection, title, 6);
		for (natural a = 0; a < 6; a++){
			if(a == 0) menu->SetMenuItemString(a, "X-Axis");
			if(a == 1) menu->SetMenuItemString(a, "Y-Axis");
			if(a == 2) menu->SetMenuItemString(a, "Z-Axis");
			if(a == 3) menu->SetMenuItemString(a, "Negative X-Axis");
			if(a == 4) menu->SetMenuItemString(a, "Negative Y-Axis");
			if(a == 5) menu->SetMenuItemString(a, "Negative Z-Axis");
		}

		return (menu);
	}

	if(index == 3){
		return (new BooleanSetting('ptrl', doesPatrol, "Patrols"));
	}

	if(index == 4){
		return (new BooleanSetting('mrpt', repeatMovement, "Repeat Movement"));
	}

	if(index == 5){
		return (new BooleanSetting('trgr', hasTrigger, "Triggerable"));
	}

	if(index == 6){
		return (new BooleanSetting('kplr', killPlayer, "Kill Player"));
	}

	return (nullptr);
}

void KillMoverController::SetSetting(const Setting *setting) {
	//get the type of identifier
	Type identifier = setting->GetSettingIdentifier();

	//are we setting the travel distances?
	if (identifier == 'tdis') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		travelDistance = Text::StringToFloat(text);
	}

		//are we setting the travel distances?
	if (identifier == 'mspd') {
		const char *text = static_cast<const TextSetting *>(setting)->GetText();
		speed = Text::StringToFloat(text);
	}

	//are we setting movement axis
	if (identifier == 'axis') {
		static const unsigned long axisID[6] =	{ kAxisX, kAxisY, kAxisZ, kAxisXneg, kAxisYneg, kAxisZneg};
		travelAxis = axisID[static_cast<const MenuSetting *>(setting)->GetMenuSelection()];
	}

	if (identifier == 'ptrl'){
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		doesPatrol = b;
	}

	if (identifier == 'mrpt'){
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		repeatMovement = b;
	}

	if(identifier == 'trgr'){
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		hasTrigger = b;
	}

	if(identifier == 'kplr'){
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		killPlayer = b;
	}
}
