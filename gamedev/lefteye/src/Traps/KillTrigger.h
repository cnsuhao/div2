#ifndef KillTrigger_h
#define KillTrigger_h

#include "C4Scripts.h"

#include "Player.h"

namespace C4 {
	enum {
		kMethodKill = 'kllt'
	};

	class KillMethod : public Method {
		private:
			KillMethod(const KillMethod& killMethod);
			Method *Replicate(void) const;
		
		public:
			KillMethod();
			~KillMethod();
			void Execute(const ScriptState *state);
	};
}

#endif