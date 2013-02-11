#ifndef CameraCommands_h
#define CameraCommands_h

#include "Game.h"
#include "Cameras.h"

/**
 *  Add all the camera control commands
 */

namespace C4 {
	class CameraCommands {
		public:
			static void ResetHeightCommand(const char* text);
			static void RotatePlyCommand(const char* text);
			static void ToggleCamTriggersCmd(const char* text);

			static void LockTypeNoneCmd(const char* text);
			static void LockTypeLockCmd(const char* text);
			static void LockTypeTrackCmd(const char* text);

			static void FlagFlipXYCmd(const char* text);
			static void FlagFlipAzmCmd(const char* text);

			static void SetDistCmd(const char* text);
			static void SetRotationAzmCmd(const char* text);
			static void SetRotationAltCmd(const char* text);
			static void SetOffsetXYCmd(const char* text);
			static void SetOffsetZCmd(const char* text);
			static void SetTrackAmtCmd(const char* text);
	};
}


#endif