#include "CamControlRegistrar.h"

using namespace C4;

CamControlRegistrar *C4::TheCamCtrlReg = nullptr;

CamControlRegistrar::CamControlRegistrar() : Singleton<CamControlRegistrar>(TheCamCtrlReg),	
	camTable("text/Camera"),
	camResetHeightCmd("camResetHeight", &CameraCommands::ResetHeightCommand),
	camRotateCmd("camRotate", &CameraCommands::RotatePlyCommand),
	camToggleTriggersCmd("camToggleTriggers", &CameraCommands::ToggleCamTriggersCmd),

	camTypeNoneCmd("camTypeNone", &CameraCommands::LockTypeNoneCmd),
	camTypeLockCmd("camTypeLock", &CameraCommands::LockTypeLockCmd),
	camTypeTrackCmd("camTypeTrack", &CameraCommands::LockTypeTrackCmd),

	camFlipXYCmd("camFlipXY", &CameraCommands::FlagFlipXYCmd),
	camFlipAzmCmd("camFlipAzm", &CameraCommands::FlagFlipAzmCmd),

	camDistCmd("camDist", &CameraCommands::SetDistCmd),
	camRotAzmCmd("camRotAzm", &CameraCommands::SetRotationAzmCmd),
	camRotAltCmd("camRotAlt", &CameraCommands::SetRotationAltCmd),
	camOffsetXYCmd("camOffsetXY", &CameraCommands::SetOffsetXYCmd),
	camOffsetZCmd("camOffsetZ", &CameraCommands::SetOffsetZCmd),
	camTrackAmtCmd("camTrackAmt", &CameraCommands::SetTrackAmtCmd),

	playerAzmReg(kMethodPlayAzm, "Player Azm"),

	camChangeTypeReg(kMethodCamChangeType, camTable.GetString(StringID('CAMR', 'CTYP', 'CPMT' ))),
	camResetReg(kMethodCamReset, camTable.GetString(StringID('CAMR', 'CRES'))),
	camDistReg(kMethodCamDist, camTable.GetString(StringID('CAMR', 'CDIS'))),
	camTrackAmtReg(kMethodCamTrackAmt, camTable.GetString(StringID('CAMR', 'CTRK'))),
	camOffsetReg(kMethodCamOffset, camTable.GetString(StringID('CAMR', 'COFF'))),
	camRotationReg(kMethodCamRotation, camTable.GetString(StringID('CAMR', 'CROT'))),
	camFlagFlipReg(kMethodCamFlagFlip, camTable.GetString(StringID('CAMR', 'CFLP', 'CPMT' ))),
	camBoxTransReg(kMethodCamBoxTransition, camTable.GetString(StringID('CAMR', 'BOXT')))
{}

CamControlRegistrar::~CamControlRegistrar() {}

void CamControlRegistrar::LoadCommands() {
	// register camera commands
	TheEngine->AddCommand(&camResetHeightCmd);
	TheEngine->AddCommand(&camRotateCmd);
	TheEngine->AddCommand(&camToggleTriggersCmd);

	TheEngine->AddCommand(&camTypeNoneCmd);
	TheEngine->AddCommand(&camTypeLockCmd);
	TheEngine->AddCommand(&camTypeTrackCmd);

	TheEngine->AddCommand(&camFlipXYCmd);
	TheEngine->AddCommand(&camFlipAzmCmd);

	TheEngine->AddCommand(&camDistCmd);
	TheEngine->AddCommand(&camRotAzmCmd);
	TheEngine->AddCommand(&camRotAltCmd);
	TheEngine->AddCommand(&camOffsetXYCmd);
	TheEngine->AddCommand(&camOffsetZCmd);
	TheEngine->AddCommand(&camTrackAmtCmd);

	rotateAction = new SwitchAction(kActionRotate);
	TheInputMgr->AddAction(rotateAction);

	rotateLeftAction = new SwitchAction(kActionRotateLeft);
	TheInputMgr->AddAction(rotateLeftAction);

	rotateRightAction = new SwitchAction(kActionRotateRight);
	TheInputMgr->AddAction(rotateRightAction);
}

void CamControlRegistrar::LoadScripts() {
	// scripts actually already registered, included in case any new actions must be made
}