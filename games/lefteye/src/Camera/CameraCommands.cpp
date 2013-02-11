#include "CameraCommands.h"

using namespace C4;

void CameraCommands::ResetHeightCommand(const char *text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->ResetHeight();
}

void CameraCommands::RotatePlyCommand(const char* text) {
	PlayerController* sc = TheGame->GetPlayerController();
	sc->SetPlayerAzimuth(sc->GetPlayerAzimuth() + K::pi_over_2);
}

void CameraCommands::ToggleCamTriggersCmd(const char* text) {
	TheGame->ToggleCamTriggers();
	if(TheGame->GetCamTriggers()) Engine::Report("Triggers On");
	else Engine::Report("Triggers Off");
}



// lock commands
void CameraCommands::LockTypeNoneCmd(const char *text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetTrackType(kCamNone);
}

void CameraCommands::LockTypeLockCmd(const char *text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetTrackType(kCamLock);
}

void CameraCommands::LockTypeTrackCmd(const char *text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetTrackType(kCamTrack);
}

// variable setting
void CameraCommands::SetDistCmd(const char* text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetDist(Text::StringToFloat(text));
}

void CameraCommands::SetRotationAzmCmd(const char* text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetRotationAzm(Text::StringToFloat(text)*K::radians);
}

void CameraCommands::SetRotationAltCmd(const char* text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetRotationAlt(Text::StringToFloat(text)*K::radians);
}

void CameraCommands::SetOffsetXYCmd(const char* text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetOffsetXY(Text::StringToFloat(text));
}

void CameraCommands::SetOffsetZCmd(const char* text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetOffsetZ(Text::StringToFloat(text));
}

void CameraCommands::SetTrackAmtCmd(const char* text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetTrackAmount(Text::StringToFloat(text));
}



void CameraCommands::FlagFlipXYCmd(const char *text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetFlipFlags(side->GetFlipFlags() ^ kFlipXY);
}

void CameraCommands::FlagFlipAzmCmd(const char *text) {
	SideCamera* side = static_cast<SideCamera *>(TheWorldMgr->GetWorld()->GetCamera());
	side->SetFlipFlags(side->GetFlipFlags() ^ kFlipAzm);
}