#include "SpinController.h"
#include "Game.h"
#include "KillMover.h"


using namespace C4;


SpinController::SpinController() : Controller(kControllerSpin)
{
    // Set a default value for the spin rate of one revolution per second
    spinRate = K::two_pi / 1000.0F;
    spinAngle = 0.0F;

	startAngle = 0 * K::degrees;
	endAngle = K::two_pi * K::degrees;
	reverseDirection = false;

	rotationAxis = kAxisZ;
}

SpinController::SpinController(float rate) : Controller(kControllerSpin)
{
    spinRate = rate;
    spinAngle = 0.0F;

	startAngle = 0 * K::degrees;
	endAngle = K::two_pi * K::degrees;
	reverseDirection = false;
	rotationAxis = kAxisZ;
}

SpinController::~SpinController()
{
}

SpinController::SpinController(const SpinController& spinController) : Controller(spinController)
{
    spinRate = spinController.spinRate;
    spinAngle = 0.0F;
	startAngle = spinController.startAngle;
	endAngle = spinController.endAngle;
	reverseDirection = spinController.reverseDirection;
	rotationAxis = spinController.rotationAxis;
}

Controller *SpinController::Replicate(void) const
{
    return (new SpinController(*this));
}



void SpinController::Pack(Packer& data, unsigned long packFlags) const
{
    Controller::Pack(data, packFlags);

    // Write the spin rate
    data << spinRate;

    // Write the current angle
    data << spinAngle;

    // Write the original transform
    data << originalTransform;
	
	data << startAngle;
	data << endAngle;

	data << shouldSwing;
	data << rotationAxis;
}

void SpinController::Unpack(Unpacker& data, unsigned long unpackFlags)
{
    Controller::Unpack(data, unpackFlags);

    // Read the spin rate
    data >> spinRate;

    // Read the current angle
    data >> spinAngle;

    // Read the original transform
    data >> originalTransform;

	data >> startAngle;
	data >> endAngle;

	data >> shouldSwing;
	data >> rotationAxis;
}

long SpinController::GetSettingCount(void) const
{
    // There's only one setting
    return (5);
}

Setting *SpinController::GetSetting(long index) const
{
    // Is it asking for the first setting?
    if (index == 0)
    {
        // Yes, return a new text setting and set its value
        return (new TextSetting('rate', Text::FloatToString(spinRate * 1000.0F / K::two_pi),
            "Spin rate", 7, &EditableTextElement::FloatNumberKeyFilter));
    }
	if (index == 1)
    {
        // Yes, return a new text setting and set its value
        return (new TextSetting('sang', Text::FloatToString(startAngle),
            "Start Angle(Degrees)", 7, &EditableTextElement::FloatNumberKeyFilter));
    }
	if (index == 2)
    {
        // Yes, return a new text setting and set its value
        return (new TextSetting('eang', Text::FloatToString(endAngle),
            "End Angle(Degrees)", 7, &EditableTextElement::FloatNumberKeyFilter));
    }
	
	if (index == 3)
    {
        return (new BooleanSetting('swng', shouldSwing, "Swing back and forth"));
    }

	if (index == 4){
		
		long selection = 0;
		if (rotationAxis == kAxisX) selection = 0;
		else if (rotationAxis == kAxisY) selection = 1;
		else if (rotationAxis == kAxisZ) selection = 2;



		const char *title = "Rotation Axis";
		MenuSetting *menu = new MenuSetting('raxi', selection, title, 3);
		for (natural a = 0; a < 3; a++){
			if(a == 0) menu->SetMenuItemString(a, "X-Axis");
			if(a == 1) menu->SetMenuItemString(a, "Y-Axis");
			if(a == 2) menu->SetMenuItemString(a, "Z-Axis");
		}

		return (menu);
	}

    return (nullptr);
}

void SpinController::SetSetting(const Setting *setting)
{
    // Are we setting the spin rate?
    if (setting->GetSettingIdentifier() == 'rate')
    {
        // Yes, grab the value from the setting and convert it back to radians per millisecond
        const char *text = static_cast<const TextSetting *>(setting)->GetText();
        spinRate = Text::StringToFloat(text) * K::two_pi / 1000.0F;
    }

	if (setting->GetSettingIdentifier() == 'sang')
    {
        // Yes, grab the value from the setting and convert it back to radians per millisecond
        const char *text = static_cast<const TextSetting *>(setting)->GetText();
        startAngle = Text::StringToFloat(text);
	}

	if (setting->GetSettingIdentifier() == 'eang')
    {
        // Yes, grab the value from the setting and convert it back to radians per millisecond
        const char *text = static_cast<const TextSetting *>(setting)->GetText();
        endAngle = Text::StringToFloat(text);
    }

	if (setting->GetSettingIdentifier() == 'swng'){
		bool b = static_cast<const BooleanSetting *>(setting)->GetBooleanValue();
		shouldSwing = b;
	}

	if (setting->GetSettingIdentifier() == 'raxi') {
		static const unsigned long axisID[3] =	{ kAxisX, kAxisY, kAxisZ};
		rotationAxis = axisID[static_cast<const MenuSetting *>(setting)->GetMenuSelection()];
	}
}

void SpinController::Preprocess(void)
{
    Controller::Preprocess();

    // Grab the original transform of the target node
    const Node *target = GetTargetNode();
    originalTransform = target->GetNodeTransform();

    // Set the kGeometryDynamic flag for any geometry nodes
    const Node *node = target;
    do
    {
        if (node->GetNodeType() == kNodeGeometry)
        {
            // Node is a geometry, so grab its object
            GeometryObject *object = static_cast<const Geometry *>(node)->GetObject();

            // Set the kGeometryDynamic flag
            object->SetGeometryFlags(object->GetGeometryFlags() | kGeometryDynamic);
        }

        // Iterate through entire subtree
        node = target->GetNextNode(node);
    } while (node);
}

void SpinController::Move(void)
{
    Matrix3D    rotator;

    // Calculate the new spin angle based on how much time has passed
    float angle = spinAngle + spinRate * TheTimeMgr->GetFloatDeltaTime();

    // Make sure it's in the [-pi, pi] range
   if (angle > K::pi) angle -= K::two_pi;
    else if (angle < -K::pi) angle += K::two_pi;

	
	if(shouldSwing){
		float degree = angle * K::degrees;
		if(degree < 0) degree += 359;//360 causes issues
		//dont pick a degree of 180 use 179 instead


			//if its gone past target distance start going other direction
		if((angle * K::degrees) >= endAngle)
				reverseDirection = true;
			//if its gone back to its start position, start going up again
		if((angle * K::degrees) <= startAngle)
				reverseDirection = false;


		if(reverseDirection)
			angle = spinAngle - spinRate * TheTimeMgr->GetFloatDeltaTime();
		else
			angle = spinAngle + spinRate * TheTimeMgr->GetFloatDeltaTime();
	}

    spinAngle = angle;

	
    // Now make a 3x3 rotation matrix
	if(rotationAxis == kAxisX)		rotator.SetRotationAboutX(angle);
	else if(rotationAxis == kAxisY)	rotator.SetRotationAboutY(angle);
	else if(rotationAxis == kAxisZ)	rotator.SetRotationAboutZ(angle);

    // We'll rotate about the center of the target node's bounding sphere
    Node *target = GetTargetNode();
    const Point3D& worldCenter = target->GetBoundingSphere()->GetCenter();
    Point3D objectCenter = target->GetInverseWorldTransform() * worldCenter; 

    // Make a 3x4 transform that rotates about the center point
    Transform4D transform(rotator, objectCenter - rotator * objectCenter);

    // Apply the rotation transform to the original transform and
    // assign it to the node as its new transform
    target->SetNodeTransform(originalTransform * transform);

    // Invalidate the target node so that it gets updated properly
    target->Invalidate();
}

