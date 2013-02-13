#ifndef RotatorController_h
#define RotatorController_h

#include "C4Controller.h"
#include "C4Effects.h"

namespace C4
{
	// This is the type for the controller that we use for a mushroom
	enum
	{
		kControllerRotator = 'rota'
	};


	/// A controller for a rotator
	class RotatorController : public Controller
	{
	private:
		

	public:
		RotatorController();

		void Preprocess();
		void HandleInteractionEvent(InteractionEventType type, const Point3D *position, Node *activator);
	};
}


#endif
