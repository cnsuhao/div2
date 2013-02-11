#ifndef CamControlRegistrar_h
#define CamControlRegistrar_h

#include "C4Engine.h"

#include "Cameras.h"
#include "CameraCommands.h"
#include "CameraScripts.h"
#include "AdvCameraScripts.h"

#include "Input.h"

namespace C4 {

	class CamControlRegistrar : public Singleton<CamControlRegistrar> {
		private:
			const StringTable				camTable;

			// Camera Console Commands
			// No variables
			Command		camResetHeightCmd;
			Command		camRotateCmd;
			Command		camToggleTriggersCmd;

			// type
			Command		camTypeNoneCmd;
			Command		camTypeLockCmd;
			Command		camTypeTrackCmd;

			// flipping
			Command		camFlipXYCmd;
			Command		camFlipAzmCmd;

			// standard
			Command		camDistCmd;
			Command		camRotAzmCmd;
			Command		camRotAltCmd;
			Command		camOffsetXYCmd;
			Command		camOffsetZCmd;
			Command		camTrackAmtCmd;

			// script registration for triggers
			MethodReg<CamChangeTypeMethod>	  camChangeTypeReg;
			MethodReg<ResetCameraMethod>	  camResetReg;
			MethodReg<CamDistMethod>		  camDistReg;
			MethodReg<CamTrackAmountMethod>   camTrackAmtReg;
			MethodReg<CamOffsetMethod>		  camOffsetReg;
			MethodReg<CamRotationMethod>	  camRotationReg;
			MethodReg<CamFlagFlipMethod>	  camFlagFlipReg;
			MethodReg<CamBoxTransitionMethod> camBoxTransReg;
			MethodReg<PlayerAzmMethod>		  playerAzmReg;

			// Action for Rotation
			SwitchAction					*rotateAction;
			SwitchAction					*rotateLeftAction;
			SwitchAction					*rotateRightAction;

		public:
			CamControlRegistrar();
			~CamControlRegistrar();

			void LoadCommands(void);
			void LoadScripts(void);
	};

	extern CamControlRegistrar *TheCamCtrlReg;
}

#endif