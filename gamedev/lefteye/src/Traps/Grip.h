#ifndef Grip_h
#define Grip_h

#include "C4Scripts.h"

#include "Player.h"

namespace C4 {
	enum {
		kMethodGrip = 'grip'
	};

	class GripMethod : public Method	{
		private:
			GripMethod(const GripMethod& pivotMethod);
			Method *Replicate(void) const;
		
		public:
			GripMethod();
			~GripMethod();
			void Execute(const ScriptState *state);
	};
}

#endif