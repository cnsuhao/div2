#ifndef Pivot_h
#define Pivot_h

#include "C4Scripts.h"

#include "Player.h"

namespace C4 {
	enum {
		kMethodPivotA = 'pvta',
		kMethodPivotB = 'pvtb'
	};

	class PivotAMethod : public Method	{
		private:
			PivotAMethod(const PivotAMethod& pivotMethod);
			Method *Replicate(void) const;
		
		public:
			PivotAMethod();
			~PivotAMethod();
			void Execute(const ScriptState *state);
	};

	class PivotBMethod : public Method	{
		private:
			PivotBMethod(const PivotBMethod& pivotMethod);
			Method *Replicate(void) const;
		
		public:
			PivotBMethod();
			~PivotBMethod();
			void Execute(const ScriptState *state);
	};
}

#endif