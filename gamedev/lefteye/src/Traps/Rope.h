#ifndef Rope_h
#define Rope_h

#include "C4Scripts.h"

#include "Player.h"

namespace C4 {
	enum {
		kMethodRope = 'rope'
	};

	class RopeMethod : public Method	{
		private:
			RopeMethod(const RopeMethod& RopeMethod);
			Method *Replicate(void) const;
		
		public:
			RopeMethod();
			~RopeMethod();
			void Execute(const ScriptState *state);
	};
}

#endif