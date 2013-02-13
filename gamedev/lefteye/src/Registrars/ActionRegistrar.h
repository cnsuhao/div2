#ifndef ActionRegistrar_h
#define ActionRegistrar_h

#include "C4Application.h"
#include "C4Engine.h"

#include "Input.h"

namespace C4 {

	class ActionRegistrar : public Singleton<ActionRegistrar> {
		private:
			MovementAction					*upAction;
			MovementAction					*downAction;
			MovementAction					*leftAction;
			MovementAction					*rightAction;
			MovementAction					*jumpAction;
			MovementAction					*sprintAction;

		public:
			ActionRegistrar();
			~ActionRegistrar();

			void LoadActions(void);
	};

	extern ActionRegistrar *TheActReg;
}

#endif