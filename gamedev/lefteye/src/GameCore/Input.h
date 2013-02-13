#ifndef Input_h
#define Input_h

#include "C4Input.h"

#include "Player.h"

namespace C4 {
	// These are action types used to define action bindings in the
	// Input Manager. If the four-character code for an action is
	// 'abcd', then any input control (there can be more than one)
	// bound to %abcd triggers the associated action.	
	enum {
		kButtonUp				= 'upbt',
		kButtonDown				= 'down',
		kButtonLeft				= 'left',
		kButtonRight			= 'rght',
		kButtonJump				= 'jump',
		kButtonSprint			= 'spnt',

		kActionRotate			= 'rota',
		kActionRotateLeft		= 'rotl',
		kActionRotateRight		= 'rotr',
		kActionStopPlayer		= 'stop'
	};
	

	// These are input flags used by the player controller. They are set or cleared
	// by the Begin() and End() functions in the MovementAction class.


	class MovementAction : public Action {
		private:
			unsigned long inputFlag;
			
		public:
			MovementAction(unsigned long type, unsigned long flag);
			~MovementAction();
			
			void Begin(void);
			void End(void);
	};



	class SwitchAction : public Action
	{
		public:
			SwitchAction(unsigned long type);
			~SwitchAction();
			
			void Begin(void);
	};
}

#endif