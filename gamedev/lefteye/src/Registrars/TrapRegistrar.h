#ifndef TrapRegistrar_h
#define TrapRegistrar_h

#include "C4Engine.h"
#include "Pivot.h"
#include "Grip.h"
#include "Rope.h"
#include "KillTrigger.h"
#include "RotatorController.h"
#include "KillMover.h"
#include "SpinController.h"
#include "Coin.h"
#include "MonkeyPart.h"


namespace C4 {

	class TrapRegistrar : public Singleton<TrapRegistrar> {
		private:
			// Items which need to be registered here
			MethodReg<CollectCoinMethod>		coinObjectReg;
			MethodReg<MonkeyPartMethod>			monkeyObjectReg;

			MethodReg<PivotAMethod>				pivotAReg;
			MethodReg<PivotBMethod>				pivotBReg;
			MethodReg<GripMethod>				gripReg;
			MethodReg<RopeMethod>				ropeReg;

			MethodReg<KillMethod>				killReg;

			ControllerReg<KillMoverController> killMoverControllerReg;
			ControllerReg<SpinController> spinControllerReg;
			ControllerReg<RotatorController> rotatorControllerReg;
			//ControllerReg<CoinController> coinControllerReg;



		public:
			TrapRegistrar();
			~TrapRegistrar();

			// executed on World creation
			void RegisterTraps(void);
	};

	extern TrapRegistrar *TheTrapReg;
}

#endif